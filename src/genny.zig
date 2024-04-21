//! Generates a script file from a type resolved AST

const std = @import("std");

const Parser = @import("parser.zig");
const Resolvinator = @import("resolvinator.zig");
const MMTypes = @import("MMTypes.zig");

const Self = @This();

pub const S64ConstantTable = std.AutoArrayHashMap(i64, void);
pub const FloatConstantTable = std.AutoArrayHashMap(f32, void);
pub const TypeReferenceTable = std.AutoArrayHashMap(MMTypes.TypeReference, void);
pub const FieldReferenceTable = std.AutoArrayHashMap(MMTypes.FieldReference, void);
pub const FunctionReferenceTable = std.AutoArrayHashMap(MMTypes.FunctionReference, void);
pub const BytecodeList = std.ArrayList(MMTypes.Bytecode);
pub const ArgumentList = std.ArrayList(MMTypes.Argument);
pub const LineNumberList = std.ArrayList(u16);
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
) Self {
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

pub fn deinit(self: *Self) void {
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

fn compileFunction(self: *Self, function: *Parser.Node.Function) !MMTypes.FunctionDefinition {
    var bytecode = BytecodeList.init(self.ast.allocator);
    _ = &bytecode;
    var arguments = ArgumentList.init(self.ast.allocator);
    _ = &arguments;
    var line_numbers = LineNumberList.init(self.ast.allocator);
    _ = &line_numbers;
    var local_variables = LocalVariableList.init(self.ast.allocator);
    _ = &local_variables;

    //TODO: Add an ASSERT at the end of the function as a safety barrier to prevent buffer overruns if somehow a RET is dropped at the end
    //TODO: let users put this under a debug flag, once this is implemented

    return .{
        .name = @intCast((try self.a_string_table.getOrPut(function.name)).index),
        .modifiers = function.modifiers,
        .stack_size = undefined,
        .type_reference = @intCast((try self.type_references.getOrPut(function.return_type.resolved.runtime_type)).index),
        .bytecode = blk: {
            const start = self.bytecode.items.len;

            try self.bytecode.appendSlice(bytecode.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + bytecode.items.len),
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

            try self.line_numbers.appendSlice(line_numbers.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + line_numbers.items.len),
            };
        },
        .local_variables = blk: {
            const start = self.local_variables.items.len;

            try self.local_variables.appendSlice(local_variables.items);

            break :blk .{
                .begin = @intCast(start),
                .end = @intCast(start + local_variables.items.len),
            };
        },
    };
}

pub fn generate(self: *Self) !MMTypes.Script {
    for (self.ast.root_elements.items) |node| {
        //Skip non class nodes
        if (node != .class)
            continue;

        const class = node.class;

        const functions = try self.ast.allocator.alloc(MMTypes.FunctionDefinition, class.functions.len);

        for (class.functions, functions) |ast_function, *function| {
            function.* = try self.compileFunction(ast_function);
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

                for (self.w_string_table.keys()) |key|
                    try buf.appendSlice(key);

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
