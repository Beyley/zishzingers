//! Generates a script file from a type resolved AST

const std = @import("std");

const Parser = @import("parser.zig");
const Resolvinator = @import("resolvinator.zig");
const MMTypes = @import("MMTypes.zig");

const Genny = @This();

pub const S64ConstantTable = std.AutoArrayHashMap(i64, void);
pub const FloatConstantTable = std.AutoArrayHashMap(f32, void);
pub const TypeReferenceTable = std.AutoArrayHashMap(MMTypes.TypeReference, void);
pub const FieldReferenceTable = std.AutoArrayHashMap(MMTypes.FieldReference, void);
pub const FunctionReferenceTable = std.AutoArrayHashMap(MMTypes.FunctionReference, void);
pub const BytecodeList = std.ArrayList(MMTypes.Bytecode);
pub const ArgumentList = std.ArrayList(MMTypes.Argument);
pub const LineNumberList = std.ArrayList(u16);
pub const LocalVariableTable = std.StringArrayHashMap(MMTypes.LocalVariable);
pub const LocalVariableList = std.ArrayList(MMTypes.LocalVariable);

ast: Parser.Tree,
a_string_table: *Resolvinator.AStringTable,
w_string_table: *Resolvinator.WStringTable,
s64_constants: S64ConstantTable,
f32_constants: FloatConstantTable,
type_references: TypeReferenceTable,
function_references: FunctionReferenceTable,
field_references: FieldReferenceTable,
bytecode: BytecodeList,
arguments: ArgumentList,
line_numbers: LineNumberList,
local_variables: LocalVariableList,

pub fn init(
    ast: Parser.Tree,
    a_string_table: *Resolvinator.AStringTable,
    w_string_table: *Resolvinator.WStringTable,
) Genny {
    return .{
        .ast = ast,
        .a_string_table = a_string_table,
        .w_string_table = w_string_table,
        .s64_constants = S64ConstantTable.init(ast.allocator),
        .f32_constants = FloatConstantTable.init(ast.allocator),
        .type_references = TypeReferenceTable.init(ast.allocator),
        .bytecode = BytecodeList.init(ast.allocator),
        .arguments = ArgumentList.init(ast.allocator),
        .line_numbers = LineNumberList.init(ast.allocator),
        .local_variables = LocalVariableList.init(ast.allocator),
        .function_references = FunctionReferenceTable.init(ast.allocator),
        .field_references = FieldReferenceTable.init(ast.allocator),
    };
}

pub fn deinit(self: *Genny) void {
    self.s64_constants.deinit();
    self.f32_constants.deinit();
    self.type_references.deinit();
    self.bytecode.deinit();
    self.arguments.deinit();
    self.line_numbers.deinit();
    self.local_variables.deinit();
    self.function_references.deinit();
    self.field_references.deinit();

    self.* = undefined;
}

const Codegen = struct {
    pub const RegisterAllocator = struct {
        const FreeSpace = struct {
            start: u16,
            size: u16,
        };

        const FreeSpaceList = std.DoublyLinkedList(FreeSpace);

        allocator: std.mem.Allocator,
        /// A linked list of the free spaces
        free_spaces: FreeSpaceList,
        /// The highest register in use
        highest_register: u16,

        pub fn init(allocator: std.mem.Allocator) !RegisterAllocator {
            var free_spaces = FreeSpaceList{};

            //Add the initial block of free space
            const free_space_node = try allocator.create(FreeSpaceList.Node);
            free_space_node.* = .{
                .data = .{
                    .start = 0,
                    .size = std.math.maxInt(u16),
                },
            };
            free_spaces.append(free_space_node);

            return .{
                .free_spaces = free_spaces,
                .highest_register = 0,
                .allocator = allocator,
            };
        }

        /// Allocates a regester from the memory space, and returns the start register for the passed data type
        pub fn allocate(self: *RegisterAllocator, data_type: MMTypes.MachineType) !u16 {
            const size = data_type.size();

            var item = self.free_spaces.first;
            while (item) |node| : (item = node.next) {
                //Skip spaces which are too small to fit our data
                if (node.data.size < size)
                    continue;

                const start = node.data.start;

                node.data.start += size;
                node.data.size -= size;

                self.highest_register = @max(self.highest_register, start + size - 1);

                //If this free space is all taken up now, just remove this node
                if (node.data.size == 0) {
                    self.free_spaces.remove(node);
                    self.allocator.destroy(node);
                }

                return start;
            }

            @panic("Ran out of register space... this is probably a bug, you shouldnt be using 16kb of stack space.");
        }

        pub fn free(self: *RegisterAllocator, freeing_start: u16, data_type: MMTypes.MachineType) !void {
            const size = data_type.size();

            const freeing_end = freeing_start + size;

            var item_before: ?*FreeSpaceList.Node = null;

            var item = self.free_spaces.first;
            while (item) |node| : (item = node.next) {
                const start = node.data.start;
                const end = start + node.data.size;

                if (freeing_start >= start and freeing_end <= end) {
                    std.debug.panic("freeing already freed range, bad.", .{});
                }

                //Find if our end is directly before the start boundary of another free space, if so we can just extend that one
                if (freeing_end == start) {
                    node.data.start -= size;
                    node.data.size += size;
                    return;
                }

                //Find if our start is directly after the end boundary of another free space, if so we can just extend that one
                if (freeing_start == end) {
                    node.data.size += size;
                    return;
                }

                if (start <= freeing_start)
                    item_before = item;
            }

            const new_space = try self.allocator.create(FreeSpaceList.Node);
            new_space.* = .{
                .data = .{
                    .start = freeing_start,
                    .size = size,
                },
            };

            self.free_spaces.insertAfter(item_before.?, new_space);
        }
    };

    bytecode: BytecodeList,
    line_numbers: LineNumberList,
    register_allocator: RegisterAllocator,
    genny: *Genny,

    fn appendBytecode(self: *Codegen, bytecode: MMTypes.Bytecode) !void {
        try self.bytecode.append(bytecode);
        //TODO: pass line numbers all the way down
        try self.line_numbers.append(0);
    }

    pub fn emitLoadConstStringWide(self: *Codegen, str: []const u16, dst_idx: u16) !void {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LCsw = .{
            .constant_idx = @intCast((try self.genny.w_string_table.getOrPut(str)).index),
            .dst_idx = dst_idx,
        } }, .void));
    }

    pub fn emitAssert(self: *Codegen, src_idx: u16) !void {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .ASSERT = .{ .src_idx = src_idx } }, .void));
    }
};

fn compileFunction(self: *Genny, function: *Parser.Node.Function, class: *Parser.Node.Class) !MMTypes.FunctionDefinition {
    var arguments = ArgumentList.init(self.ast.allocator);
    _ = &arguments;
    var local_variables = LocalVariableTable.init(self.ast.allocator);
    _ = &local_variables;

    var codegen: Codegen = .{
        .bytecode = BytecodeList.init(self.ast.allocator),
        .line_numbers = LineNumberList.init(self.ast.allocator),
        .register_allocator = try Codegen.RegisterAllocator.init(self.ast.allocator),
        .genny = self,
    };
    _ = &codegen;

    //If the function isnt static, then we need to allocate `r0-r3` for the `this` reference
    if (!function.modifiers.static) {
        const self_reg = try codegen.register_allocator.allocate(.object_ref);

        std.debug.assert(self_reg == 0);

        const type_reference: u32 = @intCast((try self.type_references.getOrPut(class.type_reference.?)).index);

        try local_variables.put("this", .{
            .modifiers = .{},
            .name = @intCast((try self.a_string_table.getOrPut("this")).index),
            .offset = self_reg,
            .type_reference = type_reference,
        });
    }

    for (function.parameters) |parameter| {
        const register = try codegen.register_allocator.allocate(parameter.type.resolved.runtime_type.machine_type);
        const type_reference: u32 = @intCast((try self.type_references.getOrPut(parameter.type.resolved.runtime_type)).index);

        try local_variables.put(parameter.name, .{
            .name = @intCast((try self.a_string_table.getOrPut(parameter.name)).index),
            .modifiers = .{},
            .type_reference = type_reference,
            .offset = register,
        });
        try arguments.append(.{
            .type_reference = type_reference,
            .offset = register,
        });
    }

    //TODO: let users put this safety feature under a debug flag
    {
        const str_idx = try codegen.register_allocator.allocate(.object_ref);
        try codegen.emitLoadConstStringWide(std.unicode.utf8ToUtf16LeStringLiteral("function overrun protection..."), str_idx);
        try codegen.emitAssert(str_idx);
        try codegen.register_allocator.free(str_idx, .object_ref);
    }

    // Stack size is the highest used register + 1, since registers are zero indexed
    const stack_size = codegen.register_allocator.highest_register + 1;

    return .{
        .name = @intCast((try self.a_string_table.getOrPut(function.name)).index),
        .modifiers = function.modifiers,
        .stack_size = stack_size,
        .type_reference = @intCast((try self.type_references.getOrPut(function.return_type.resolved.runtime_type)).index),
        .bytecode = blk: {
            const start = self.bytecode.items.len;

            try self.bytecode.appendSlice(codegen.bytecode.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + codegen.bytecode.items.len),
            };
        },
        .arguments = blk: {
            const start = self.arguments.items.len;

            try self.arguments.appendSlice(arguments.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + arguments.items.len),
            };
        },
        .line_numbers = blk: {
            const start = self.line_numbers.items.len;

            try self.line_numbers.appendSlice(codegen.line_numbers.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + codegen.line_numbers.items.len),
            };
        },
        .local_variables = blk: {
            const start = self.local_variables.items.len;

            const local_variable_values = local_variables.values();

            try self.local_variables.appendSlice(local_variable_values);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + local_variable_values.len),
            };
        },
    };
}

pub fn generate(self: *Genny) !MMTypes.Script {
    for (self.ast.root_elements.items) |node| {
        //Skip non class nodes
        if (node != .class)
            continue;

        const class = node.class;

        const functions = try self.ast.allocator.alloc(MMTypes.FunctionDefinition, class.functions.len);

        for (class.functions, functions) |ast_function, *function| {
            function.* = try self.compileFunction(ast_function, class);
        }

        return .{
            .up_to_date_script = null,
            .class_name = class.name,
            .super_class_script = if (class.base_class == .resolved) class.base_class.resolved.ident else null,
            //TODO: implement modifiers
            .modifiers = .{},
            //TODO: is this even needed to be specified?
            .depending_guids = &.{},
            .functions = functions,
            .bytecode = self.bytecode.items,
            .arguments = self.arguments.items,
            .line_numbers = self.line_numbers.items,
            .local_variables = self.local_variables.items,
            .function_references = self.function_references.keys(),
            .constant_table_s64 = self.s64_constants.keys(),
            .constant_table_float = self.f32_constants.keys(),
            .type_references = self.type_references.keys(),
            .field_references = self.field_references.keys(),
            .a_string_table = blk: {
                var buf = std.ArrayList(u8).init(self.ast.allocator);
                var strings = std.ArrayList([:0]const u8).init(self.ast.allocator);

                for (self.a_string_table.keys()) |key| {
                    try buf.appendSlice(key);
                    try buf.append(0);
                }

                var start: usize = 0;
                for (self.a_string_table.keys()) |key| {
                    try strings.append(buf.items[start .. start + key.len :0]);

                    start += key.len + 1;
                }

                break :blk .{
                    .buf = try buf.toOwnedSlice(),
                    .strings = try strings.toOwnedSlice(),
                };
            },
            .w_string_table = blk: {
                var buf = std.ArrayList(u16).init(self.ast.allocator);
                var strings = std.ArrayList([:0]const u16).init(self.ast.allocator);

                for (self.w_string_table.keys()) |key| {
                    try buf.appendSlice(key);
                    try buf.append(0);
                }

                var start: usize = 0;
                for (self.w_string_table.keys()) |key| {
                    try strings.append(buf.items[start .. start + key.len :0]);

                    start += key.len + 1;
                }

                break :blk .{
                    .buf = try buf.toOwnedSlice(),
                    .strings = try strings.toOwnedSlice(),
                };
            },
            .field_definitions = blk: {
                var field_definitions = std.ArrayList(MMTypes.FieldDefinition).init(self.ast.allocator);

                for (class.fields) |field| {
                    try field_definitions.append(.{
                        .name = @intCast((try self.a_string_table.getOrPut(field.name)).index),
                        .type_reference = @intCast((try self.type_references.getOrPut(field.type.resolved.runtime_type)).index),
                        .modifiers = field.modifiers,
                    });
                }

                break :blk try field_definitions.toOwnedSlice();
            },
            .property_definitions = blk: {
                var property_definitions = std.ArrayList(MMTypes.PropertyDefinition).init(self.ast.allocator);

                for (class.properties) |property| {
                    _ = property; // autofix
                    @panic("TODO: properties");
                    // try property_definitions.append(.{
                    //     .name = @intCast((try self.a_string_table.getOrPut(property.name)).index),
                    //     .type_reference = @intCast((try self.type_reference_table.getOrPut(property.type.resolved.runtime_type)).index),
                    // });
                }

                break :blk try property_definitions.toOwnedSlice();
            },
        };
    }

    @panic("no class?");
}
