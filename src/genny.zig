//! Generates a script file from a type resolved AST

const std = @import("std");

const Parser = @import("parser.zig");
const Resolvinator = @import("resolvinator.zig");
const MMTypes = @import("MMTypes.zig");

const Genny = @This();

pub const Error = std.mem.Allocator.Error || error{InvalidUtf8};

pub const S64ConstantTable = std.AutoArrayHashMap(i64, void);
pub const FloatConstantTable = std.AutoArrayHashMap([4]f32, void);
pub const TypeReferenceTable = std.AutoArrayHashMap(MMTypes.TypeReference, void);
pub const FieldReferenceTable = std.AutoArrayHashMap(MMTypes.FieldReference, void);
pub const FunctionReferenceTable = std.AutoArrayHashMap(*Parser.Node.Function, MMTypes.FunctionReference);
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
revision: MMTypes.Revision,

pub fn init(
    ast: Parser.Tree,
    a_string_table: *Resolvinator.AStringTable,
    w_string_table: *Resolvinator.WStringTable,
    revision: MMTypes.Revision,
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
        .revision = revision,
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
        revision: MMTypes.Revision,

        pub fn init(allocator: std.mem.Allocator, revision: MMTypes.Revision) !RegisterAllocator {
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
                .revision = revision,
            };
        }

        /// Allocates a regester from the memory space, and returns the start register for the passed data type
        pub fn allocate(self: *RegisterAllocator, machine_type: MMTypes.MachineType) !Register {
            const size = machine_type.size();

            var item = self.free_spaces.first;
            while (item) |node| : (item = node.next) {
                const node_start = node.data.start;

                //Calculate the amount of alignment needed, since we need to align all registers to the size of the underlying data type
                const alignment_needed = blk: {
                    if (node_start % size == 0) {
                        break :blk 0;
                    }

                    break :blk size - (node_start % size);
                };

                //Skip spaces which are too small to fit our data
                if (node.data.size < size + alignment_needed)
                    continue;

                if (alignment_needed != 0) {
                    const new_node = try self.allocator.create(FreeSpaceList.Node);
                    new_node.* = .{
                        .data = .{
                            .start = node_start,
                            .size = alignment_needed,
                        },
                    };
                    self.free_spaces.insertBefore(node, new_node);
                }

                const start = node_start + alignment_needed;

                node.data.start += size + alignment_needed;
                node.data.size -= size + alignment_needed;

                self.highest_register = @max(self.highest_register, start + size - 1);

                //If this free space is all taken up now, just remove this node
                if (node.data.size == 0) {
                    self.free_spaces.remove(node);
                    self.allocator.destroy(node);
                }

                return .{ start, machine_type };
            }

            @panic("Ran out of register space... this is probably a bug, you shouldnt be using 16kb of stack space.");
        }

        pub fn free(self: *RegisterAllocator, register: Register) !void {
            _ = register; // autofix
            _ = self; // autofix

            // TODO: register re-use
        }
    };

    bytecode: BytecodeList,
    line_numbers: LineNumberList,
    register_allocator: RegisterAllocator,
    genny: *Genny,
    revision: MMTypes.Revision,

    fn ensureAlignment(address: u16, machine_type: MMTypes.MachineType) void {
        // 4 % 0 is UB
        if (machine_type.size() == 0)
            return;

        if (address % machine_type.size() > 0) {
            std.debug.panic("BUG: Bad alignment! Memory address {d} fails alignment check {d}!", .{ address, machine_type.size() });
        }
    }

    fn appendBytecode(self: *Codegen, bytecode: MMTypes.Bytecode) !void {
        try self.bytecode.append(bytecode);
        //TODO: pass line numbers all the way down
        try self.line_numbers.append(0);
    }

    pub fn lastEmitBytecodeIndex(self: *Codegen) usize {
        return self.bytecode.items.len - 1;
    }

    pub fn nextEmitBytecodeIndex(self: *Codegen) usize {
        return self.bytecode.items.len;
    }

    pub fn emitLoadConstStringWide(self: *Codegen, dst_idx: u16, str: []const u16) !void {
        ensureAlignment(dst_idx, .object_ref);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LCsw = .{
            .constant_idx = @intCast((try self.genny.w_string_table.getOrPut(str)).index),
            .dst_idx = dst_idx,
        } }, .void));
    }

    pub fn emitAssert(self: *Codegen, src_idx: u16) !void {
        ensureAlignment(src_idx, .object_ref);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .ASSERT = .{ .src_idx = src_idx } }, .void));
    }

    pub fn emitCallVo(self: *Codegen, dst_idx: u16, call_idx: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(dst_idx, machine_type);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .CALLVo = .{ .dst_idx = dst_idx, .call_idx = call_idx } }, machine_type));
    }

    pub fn emitArg(self: *Codegen, arg_idx: u16, src_idx: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(arg_idx, machine_type);
        ensureAlignment(src_idx, machine_type);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .ARG = .{ .src_idx = src_idx, .arg_idx = arg_idx } }, machine_type));
    }

    pub fn emitCall(self: *Codegen, dst_idx: u16, call_idx: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(dst_idx, machine_type);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .CALL = .{ .dst_idx = dst_idx, .call_idx = call_idx } }, machine_type));
    }

    pub fn emitLoadConstInt(self: *Codegen, dst_idx: u16, s32: i32) !void {
        ensureAlignment(dst_idx, .s32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LCi = .{ .dst_idx = dst_idx, .constant_idx = @bitCast(s32) } }, .s32));
    }

    pub fn emitLoadConstBool(self: *Codegen, dst_idx: u16, boolean: bool) !void {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LCb = .{ .dst_idx = dst_idx, .constant_idx = if (boolean) 0x80000000 else 0 } }, .s32));
    }

    pub fn emitLoadConstNullSafePtr(self: *Codegen, dst_idx: u16) !void {
        ensureAlignment(dst_idx, .safe_ptr);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LC_NULLsp = .{ .constant_idx = 0, .dst_idx = dst_idx } }, .void));
    }

    pub fn emitSetObjectMember(self: *Codegen, src_idx: u16, base_idx: u16, field_ref: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(src_idx, machine_type);
        ensureAlignment(base_idx, .object_ref);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .SET_OBJ_MEMBER = .{
            .src_idx = src_idx,
            .base_idx = base_idx,
            .field_ref = field_ref,
        } }, machine_type));
    }

    pub fn emitGetObjectMember(self: *Codegen, dst_idx: u16, base_idx: u16, field_ref: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(dst_idx, machine_type);
        ensureAlignment(base_idx, .object_ref);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .GET_OBJ_MEMBER = .{
            .dst_idx = dst_idx,
            .base_idx = base_idx,
            .field_ref = field_ref,
        } }, machine_type));
    }

    pub fn emitBoolToS32(self: *Codegen, dst_idx: u16, src_idx: u16) !void {
        ensureAlignment(dst_idx, .s32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .INTb = .{ .src_idx = src_idx, .dst_idx = dst_idx } }, .void));
    }

    pub fn emitRet(self: *Codegen, src_idx: u16, machine_type: MMTypes.MachineType) !void {
        ensureAlignment(src_idx, machine_type);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .RET = .{ .src_idx = src_idx } }, machine_type));
    }

    pub fn emitBranchNotEqualZero(self: *Codegen, src_idx: u16) !usize {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .BNEZ = .{
            .branch_offset = undefined,
            .src_idx = src_idx,
        } }, .void));

        //Return the index of the last item inserted, which is our branch instruction, so that the branch offset can be filled in later
        return self.bytecode.items.len - 1;
    }

    pub fn emitBranchEqualZero(self: *Codegen, src_idx: u16) !usize {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .BEZ = .{
            .branch_offset = undefined,
            .src_idx = src_idx,
        } }, .void));

        //Return the index of the last item inserted, which is our branch instruction, so that the branch offset can be filled in later
        return self.bytecode.items.len - 1;
    }

    pub fn emitBranch(self: *Codegen) !usize {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .B = .{
            .src_idx = 0xFFFF,
            .branch_offset = undefined,
        } }, .void));

        //Return the index of the last item inserted, which is our branch instruction, so that the branch offset can be filled in later
        return self.bytecode.items.len - 1;
    }

    pub fn emitLogicalNegationBoolean(self: *Codegen, dst_idx: u16, src_idx: u16) !void {
        try self.appendBytecode(MMTypes.Bytecode.init(
            .{ .LOG_NEGb = .{ .dst_idx = dst_idx, .src_idx = src_idx } },
            .void,
        ));
    }

    pub fn emitLoadConstFloat(self: *Codegen, dst_idx: u16, value: f32) !void {
        ensureAlignment(dst_idx, .f32);

        try self.appendBytecode(MMTypes.Bytecode.init(
            .{ .LCf = .{ .constant_idx = @bitCast(value), .dst_idx = dst_idx } },
            .void,
        ));
    }

    pub fn emitLoadConstVector4(self: *Codegen, dst_idx: u16, value: [4]f32) !void {
        ensureAlignment(dst_idx, .v4);

        const constant_idx: u32 = @intCast((try self.genny.f32_constants.getOrPut(value)).index);

        try self.appendBytecode(MMTypes.Bytecode.init(
            .{ .LCv4 = .{
                .constant_idx = constant_idx,
                .dst_idx = dst_idx,
            } },
            .void,
        ));
    }

    pub fn emitSetVectorElement(self: *Codegen, dst_idx: u16, element: u2, src_idx: u16) !void {
        ensureAlignment(dst_idx, .v4);
        ensureAlignment(src_idx, .f32);

        switch (element) {
            inline else => |element_val| {
                const tag_name = comptime switch (element_val) {
                    0 => @tagName(MMTypes.InstructionType.SET_V4_X),
                    1 => @tagName(MMTypes.InstructionType.SET_V4_Y),
                    2 => @tagName(MMTypes.InstructionType.SET_V4_Z),
                    3 => @tagName(MMTypes.InstructionType.SET_V4_W),
                };

                try self.appendBytecode(MMTypes.Bytecode.init(
                    @unionInit(MMTypes.TaggedInstruction, tag_name, .{
                        .src_idx = src_idx,
                        .base_idx = dst_idx,
                    }),
                    .void,
                ));
            },
        }
    }

    pub fn emitIntNotEqual(self: *Codegen, dst_idx: u16, left_idx: u16, right_idx: u16) !void {
        ensureAlignment(left_idx, .s32);
        ensureAlignment(right_idx, .s32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .NEi = .{
            .dst_idx = dst_idx,
            .src_a_idx = left_idx,
            .src_b_idx = right_idx,
        } }, .void));
    }

    pub fn emitIntBitwiseAnd(self: *Codegen, dst_idx: u16, left_idx: u16, right_idx: u16) !void {
        ensureAlignment(left_idx, .s32);
        ensureAlignment(right_idx, .s32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .BIT_ANDi = .{
            .dst_idx = dst_idx,
            .src_a_idx = left_idx,
            .src_b_idx = right_idx,
        } }, .void));
    }

    pub fn emitBoolBitwiseAnd(self: *Codegen, dst_idx: u16, lefthand: u16, righthand: u16) !void {
        try self.appendBytecode(MMTypes.Bytecode.init(.{ .BIT_ANDb = .{
            .dst_idx = dst_idx,
            .src_a_idx = lefthand,
            .src_b_idx = righthand,
        } }, .void));
    }

    pub fn emitFloatGreaterThan(self: *Codegen, dst_idx: u16, lefthand: u16, righthand: u16) !void {
        ensureAlignment(lefthand, .f32);
        ensureAlignment(righthand, .f32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .GTf = .{
            .dst_idx = dst_idx,
            .src_a_idx = lefthand,
            .src_b_idx = righthand,
        } }, .void));
    }

    pub fn emitFloatLessThanOrEqual(self: *Codegen, dst_idx: u16, lefthand: u16, righthand: u16) !void {
        ensureAlignment(lefthand, .f32);
        ensureAlignment(righthand, .f32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .LTEf = .{
            .dst_idx = dst_idx,
            .src_a_idx = lefthand,
            .src_b_idx = righthand,
        } }, .void));
    }

    pub fn emitAddFloat(self: *Codegen, dst_idx: u16, lefthand: u16, righthand: u16) !void {
        ensureAlignment(dst_idx, .f32);
        ensureAlignment(lefthand, .f32);
        ensureAlignment(righthand, .f32);

        try self.appendBytecode(MMTypes.Bytecode.init(.{ .ADDf = .{
            .dst_idx = dst_idx,
            .src_a_idx = lefthand,
            .src_b_idx = righthand,
        } }, .void));
    }
};

const Register = struct { u16, MMTypes.MachineType };

/// Compiles an expression, returning the register and the resulting machine type
fn compileExpression(
    codegen: *Codegen,
    function_local_variables: *LocalVariableTable,
    scope_local_variables: *LocalVariableTable,
    expression: *Parser.Node.Expression,
    discard_result: bool,
    result_register: ?Register,
) Error!?Register {
    return switch (expression.contents) {
        .assignment => |assignment| blk: {
            switch (assignment.destination.contents) {
                .variable_access => |variable_access| {
                    const variable = scope_local_variables.get(variable_access) orelse std.debug.panic("missing variable {s}", .{variable_access});

                    const register: Register = .{ @intCast(variable.offset), codegen.genny.type_references.keys()[variable.type_reference].machine_type };

                    _ = try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        assignment.value,
                        false,
                        register,
                    );

                    break :blk if (discard_result) null else register;
                },
                .field_access => |field_access| {
                    const source_variable = scope_local_variables.get(field_access.source.contents.variable_access).?;
                    const source_variable_type = codegen.genny.type_references.keys()[source_variable.type_reference];

                    const machine_type = expression.type.resolved.runtime_type.machine_type;

                    const register = (try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        assignment.value,
                        false,
                        result_register,
                    )).?;

                    if (register[1] != machine_type)
                        std.debug.panic(
                            "BUG: register type is {s} when it should be {s}",
                            .{ @tagName(register[1]), @tagName(machine_type) },
                        );

                    const field_reference: u16 = @intCast((try codegen.genny.field_references.getOrPut(.{
                        .name = @intCast((try codegen.genny.a_string_table.getOrPut(field_access.field)).index),
                        .type_reference = @intCast((try codegen.genny.type_references.getOrPut(source_variable_type)).index),
                    })).index);

                    switch (source_variable_type.machine_type) {
                        .object_ref => {
                            try codegen.emitSetObjectMember(
                                register[0],
                                @intCast(source_variable.offset),
                                field_reference,
                                machine_type,
                            );
                        },
                        else => |tag| std.debug.panic("unable to do field access on machine type {s}", .{@tagName(tag)}),
                    }

                    // If we the result is being discarded
                    if (result_register == null and discard_result) {
                        try codegen.register_allocator.free(register);

                        break :blk null;
                    }

                    break :blk register;
                },
                else => |tag| std.debug.panic("TODO: codegen for assignment to {s}", .{@tagName(tag)}),
            }
        },
        // We can just lower this into a LCi
        .integer_literal_to_s32 => |integer_literal| blk: {
            if (discard_result)
                break :blk null;

            const value: i32 = if (integer_literal.contents.integer_literal.base != .decimal) int: {
                const unsigned: u64 = @bitCast(integer_literal.contents.integer_literal.value);

                const unsigned_u32: u32 = @intCast(unsigned);

                break :int @bitCast(unsigned_u32);
            } else @intCast(integer_literal.contents.integer_literal.value);

            const register = result_register orelse try codegen.register_allocator.allocate(.s32);

            try codegen.emitLoadConstInt(register[0], value);

            break :blk register;
        },
        .integer_literal_to_f32 => |int_literal| blk: {
            if (discard_result)
                break :blk null;

            const value: f32 = @floatFromInt(int_literal.contents.integer_literal.value);

            const register = result_register orelse try codegen.register_allocator.allocate(.f32);

            try codegen.emitLoadConstFloat(register[0], value);

            break :blk register;
        },
        .float_literal_to_f32 => |float_literal| blk: {
            if (discard_result)
                break :blk null;

            const value = float_literal.contents.float_literal.value;

            const register = result_register orelse try codegen.register_allocator.allocate(.f32);

            try codegen.emitLoadConstFloat(register[0], @floatCast(value));

            break :blk register;
        },
        .bool_literal => |bool_literal| blk: {
            if (discard_result)
                break :blk null;

            const register = result_register orelse try codegen.register_allocator.allocate(.bool);

            try codegen.emitLoadConstBool(register[0], bool_literal);

            break :blk register;
        },
        .null_literal_to_safe_ptr => blk: {
            if (discard_result)
                break :blk null;

            const register = result_register orelse try codegen.register_allocator.allocate(.safe_ptr);

            try codegen.emitLoadConstNullSafePtr(register[0]);

            break :blk register;
        },
        .wide_string_literal => |wide_string_literal| blk: {
            if (discard_result)
                break :blk null;

            const wide_string = try std.unicode.utf8ToUtf16LeAlloc(codegen.register_allocator.allocator, wide_string_literal);

            //If the result register is known, just load directly into that
            if (result_register) |result_idx| {
                try codegen.emitLoadConstStringWide(result_idx[0], wide_string);

                break :blk result_idx;
            }
            // Else, allocate a new register and use that
            else {
                const result_idx = try codegen.register_allocator.allocate(.object_ref);

                try codegen.emitLoadConstStringWide(result_idx[0], wide_string);

                break :blk result_idx;
            }
        },
        // If the result register is unspecified, we do not lower this at all,
        // however if there is a result register specified, we lower this as a simple move.
        .variable_access => |variable_access| blk: {
            const variable = scope_local_variables.get(variable_access).?;

            const variable_machine_type = codegen.genny.type_references.keys()[variable.type_reference].machine_type;

            if (result_register) |result_idx| {
                _ = result_idx; // autofix

                switch (variable_machine_type) {
                    else => |tag| std.debug.panic("TODO: lower variable access move of {s}", .{@tagName(tag)}),
                }
            } else {
                break :blk .{ @intCast(variable.offset), variable_machine_type };
            }
        },
        // We lower this as a simple set of `ARG` instructions followed by a CALL instruction
        .function_call => |function_call| blk: {
            std.debug.assert(function_call.function == .function);

            const function = function_call.function.function;

            const called_function_idx: u16 = @intCast((try codegen.genny.function_references.getOrPutValue(function.function, .{
                //TODO: this name needs to be mangled
                .name = @intCast((try codegen.genny.a_string_table.getOrPut(function.function.mangled_name.?)).index),
                .type_reference = @intCast((try codegen.genny.type_references.getOrPut(function.owning_type)).index),
            })).index);

            const return_type = expression.type.resolved.runtime_type.machine_type;

            const parameter_registers = try codegen.register_allocator.allocator.alloc(Register, function_call.parameters.len);

            for (function_call.parameters, parameter_registers) |parameter, *parameter_register| {
                if (try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    parameter,
                    false,
                    null,
                )) |parameter_result_register| {
                    parameter_register.* = parameter_result_register;
                } else {
                    @panic("BUG: function call parameter has no result register");
                }
            }

            var curr_arg_register: u16 = 0;

            // If this is a member function call, we need to add the source as the arg0 reg
            if (function_call.source) |source| {
                // We only want to put arg0 as the `this` param if we are calling a method on a variable, and not a class
                if (source.contents == .variable_access) {
                    const source_variable = scope_local_variables.get(source.contents.variable_access).?;

                    const source_machine_type = codegen.genny.type_references.keys()[source_variable.type_reference].machine_type;

                    try codegen.emitArg(0, @intCast(source_variable.offset), source_machine_type);

                    curr_arg_register += source_machine_type.size();
                }
            }

            for (parameter_registers) |parameter_register| {
                const parameter_size = parameter_register[1].size();

                // Align to machine type
                curr_arg_register += (parameter_size - (curr_arg_register % parameter_size)) % parameter_size;

                try codegen.emitArg(curr_arg_register, parameter_register[0], parameter_register[1]);

                curr_arg_register += parameter_size;
            }

            const call_result_register = if (result_register) |result_register_idx|
                result_register_idx
            else if (discard_result)
                .{ std.math.maxInt(u16), .void }
            else
                try codegen.register_allocator.allocate(return_type);

            try codegen.emitCall(call_result_register[0], called_function_idx, call_result_register[1]);

            for (parameter_registers) |parameter_register|
                try codegen.register_allocator.free(parameter_register);

            break :blk call_result_register;
        },
        .bool_to_s32 => |bool_to_s32| blk: {
            if (try compileExpression(
                codegen,
                function_local_variables,
                scope_local_variables,
                bool_to_s32,
                discard_result,
                null,
            )) |source| {
                const register = result_register orelse try codegen.register_allocator.allocate(.s32);

                if (source[1] != .bool)
                    std.debug.panic(
                        "BUG: source register should have been bool, was {s}",
                        .{@tagName(source[1])},
                    );

                // Emit the bool to s32 instruction
                try codegen.emitBoolToS32(register[0], source[0]);

                return register;
            }

            break :blk null;
        },
        .field_access => |field_access| blk: {
            if (discard_result)
                break :blk null;

            const source_variable = scope_local_variables.get(field_access.source.contents.variable_access).?;

            const source_variable_type = codegen.genny.type_references.keys()[source_variable.type_reference];

            const machine_type = expression.type.resolved.runtime_type.machine_type;

            const register = result_register orelse try codegen.register_allocator.allocate(machine_type);

            const field_reference: u16 = @intCast((try codegen.genny.field_references.getOrPut(.{
                .name = @intCast((try codegen.genny.a_string_table.getOrPut(field_access.field)).index),
                .type_reference = @intCast((try codegen.genny.type_references.getOrPut(source_variable_type)).index),
            })).index);

            switch (source_variable_type.machine_type) {
                .object_ref => {
                    try codegen.emitGetObjectMember(
                        register[0],
                        @intCast(source_variable.offset),
                        field_reference,
                        machine_type,
                    );
                },
                else => |tag| std.debug.panic("unable to do field access on machine type {s}", .{@tagName(tag)}),
            }

            break :blk register;
        },
        .logical_negation => |logical_negation| blk: {
            if (try compileExpression(
                codegen,
                function_local_variables,
                scope_local_variables,
                logical_negation,
                false,
                null,
            )) |source_register| {
                const register = result_register orelse try codegen.register_allocator.allocate(.bool);

                try codegen.emitLogicalNegationBoolean(register[0], source_register[0]);

                if (discard_result and result_register == null) {
                    try codegen.register_allocator.free(register);
                    break :blk null;
                }

                break :blk register;
            } else @panic("BUG: logical negation source has no register");
        },
        .block => |block| blk: {
            std.debug.assert(result_register == null);

            try compileBlock(
                codegen,
                function_local_variables,
                scope_local_variables,
                block,
                false,
                .void,
            );

            break :blk null;
        },
        .vec2_construction => |vec2_construction| blk: {
            if (discard_result)
                break :blk null;

            const register = result_register orelse try codegen.register_allocator.allocate(.v4);

            for (vec2_construction, 0..) |element_expression, i| {
                const element_register = (try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    element_expression,
                    false,
                    null,
                )).?;

                try codegen.emitSetVectorElement(register[0], @intCast(i), element_register[0]);

                try codegen.register_allocator.free(element_register);
            }

            break :blk register;
        },
        inline .bitwise_and, .addition, .not_equal, .greater_than, .less_than_or_equal => |binary, binary_type| blk: {
            //Assert the types are equal
            std.debug.assert(binary.lefthand.type.resolved.eql(binary.righthand.type.resolved));

            const hand_type = binary.lefthand.type.resolved;

            // Allocate a result register, in the case of bitwise ops, we need to use the machine type, else use a bool as the result
            const register = result_register orelse try codegen.register_allocator.allocate(switch (binary_type) {
                .bitwise_and, .addition => binary.lefthand.type.resolved.runtime_type.machine_type,
                .not_equal, .greater_than, .less_than_or_equal => .bool,
                else => @compileError("Missing register type resolution"),
            });

            switch (hand_type) {
                .runtime_type => |runtime_type| {
                    const lefthand = (try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        binary.lefthand,
                        false,
                        null,
                    )).?;
                    const righthand = (try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        binary.righthand,
                        false,
                        null,
                    )).?;

                    std.debug.assert(lefthand[1] == righthand[1]);
                    std.debug.assert(lefthand[1] == runtime_type.machine_type);

                    switch (runtime_type.machine_type) {
                        .s32 => switch (binary_type) {
                            .not_equal => try codegen.emitIntNotEqual(register[0], lefthand[0], righthand[0]),
                            .bitwise_and => try codegen.emitIntBitwiseAnd(register[0], lefthand[0], righthand[0]),
                            else => std.debug.panic("TODO: {s} binary op type for s32", .{@tagName(binary_type)}),
                        },
                        .f32 => switch (binary_type) {
                            .greater_than => try codegen.emitFloatGreaterThan(register[0], lefthand[0], righthand[0]),
                            .less_than_or_equal => try codegen.emitFloatLessThanOrEqual(register[0], lefthand[0], righthand[0]),
                            .addition => try codegen.emitAddFloat(register[0], lefthand[0], righthand[0]),
                            else => std.debug.panic("TODO: {s} binary op type for f32", .{@tagName(binary_type)}),
                        },
                        else => |tag| std.debug.panic("TODO: comparisons for machine type {s}", .{@tagName(tag)}),
                    }

                    try codegen.register_allocator.free(lefthand);
                    try codegen.register_allocator.free(righthand);
                },
                .type => @panic("BUG: i dont think this should be allowed?"),
                .integer_literal => {
                    @panic("TODO: int literal comparison");
                },
                .float_literal => {
                    @panic("TODO: float literal comparison");
                },
                .null_literal => {
                    @panic("TODO: null literal comparison");
                },
            }

            if (discard_result and result_register != null) {
                try codegen.register_allocator.free(register);
                break :blk null;
            }

            break :blk register;
        },
        .logical_and => |logical_and| blk: {
            const lefthand = (try compileExpression(
                codegen,
                function_local_variables,
                scope_local_variables,
                logical_and.lefthand,
                false,
                null,
            )).?;
            std.debug.assert(lefthand[1] == .bool);

            const righthand = (try compileExpression(
                codegen,
                function_local_variables,
                scope_local_variables,
                logical_and.righthand,
                false,
                null,
            )).?;
            std.debug.assert(righthand[1] == .bool);

            if (!discard_result) {
                const register = result_register orelse try codegen.register_allocator.allocate(.bool);

                try codegen.emitBoolBitwiseAnd(register[0], lefthand[0], righthand[0]);

                break :blk register;
            }

            try codegen.register_allocator.free(lefthand);
            try codegen.register_allocator.free(righthand);

            break :blk null;
        },
        else => |tag| std.debug.panic("cant codegen for expression {s} yet\n", .{@tagName(tag)}),
    };
}

fn compileBlock(
    codegen: *Codegen,
    function_local_variables: *LocalVariableTable,
    scope_local_variables: *LocalVariableTable,
    block: []const Parser.Node,
    top_level: bool,
    return_type: MMTypes.MachineType,
) Error!void {
    var local_variables_from_this_scope = std.ArrayList([]const u8).init(codegen.register_allocator.allocator);

    var return_emit = false;

    for (block) |node| block_loop: {
        switch (node) {
            .variable_declaration => |variable_declaration| {
                const runtime_type = variable_declaration.type.resolved.runtime_type;

                //Allocate the register that will be used for this local variable
                const register = try codegen.register_allocator.allocate(runtime_type.machine_type);

                try local_variables_from_this_scope.append(variable_declaration.name);

                const local_variable: MMTypes.LocalVariable = .{
                    .type_reference = @intCast((try codegen.genny.type_references.getOrPut(runtime_type)).index),
                    .name = @intCast((try codegen.genny.a_string_table.getOrPut(variable_declaration.name)).index),
                    .modifiers = .{},
                    .offset = register[0],
                };

                if (variable_declaration.value) |variable_value| {
                    if (try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        variable_value,
                        false,
                        register,
                    )) |result_register| {
                        if (result_register[0] != register[0])
                            @panic("BUG: result register of variable assignment was bad");
                    } else {
                        @panic("BUG: expression for variable value didnt return anything?");
                    }
                }

                try function_local_variables.putNoClobber(variable_declaration.name, local_variable);
                try scope_local_variables.putNoClobber(variable_declaration.name, local_variable);
            },
            .expression => |expression| {
                if (try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    expression,
                    true,
                    null,
                )) |result_register| {
                    try codegen.register_allocator.free(result_register);
                }
            },
            .return_statement => |return_statement| {
                if (return_statement.expression) |return_value| {
                    const return_register = (try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        return_value,
                        false,
                        null,
                    )).?;

                    try codegen.emitRet(return_register[0], return_register[1]);

                    try codegen.register_allocator.free(return_register);
                } else {
                    try codegen.emitRet(0, .void);
                }

                return_emit = true;

                // At a return statement, immediately break out of the block, we dont need to generate code after a return statement
                break :block_loop;
            },
            .if_statement => |if_statement| {
                const condition_register = (try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    if_statement.condition,
                    false,
                    null,
                )).?;

                // If the condition is false (0), we need to skip over the main body,
                // this will either skip ahead to after the main body and continue execution as normal,
                // or it will skip to the else body, which is emit after the main body
                const skip_body_instruction = try codegen.emitBranchEqualZero(condition_register[0]);

                // Now that we are done with the condition, we can get rid of that register
                try codegen.register_allocator.free(condition_register);

                // Compile the main body expression
                std.debug.assert(try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    if_statement.body,
                    true,
                    null,
                ) == null);

                // If we have an else body, we need to emit an instruction after the main body to skip over the else block
                const skip_else_instruction: ?usize = if (if_statement.else_body != null) try codegen.emitBranch() else null;

                // Set the branch offset of the instruction to the next instruction to be emit
                codegen.bytecode.items[skip_body_instruction].params.BEZ.branch_offset =
                    @intCast(codegen.bytecode.items.len - skip_body_instruction);

                // If we have an else body, we need to emit that
                if (if_statement.else_body) |else_body| {
                    // Emit the actual body
                    std.debug.assert(try compileExpression(
                        codegen,
                        function_local_variables,
                        scope_local_variables,
                        else_body,
                        true,
                        null,
                    ) == null);

                    // Set the skip else target to the instruction after the current instruction
                    codegen.bytecode.items[skip_else_instruction.?].params.B.branch_offset =
                        @intCast(codegen.bytecode.items.len - skip_else_instruction.?);
                }
            },
            //
            // alloc condition register
            // start: calculate condition
            // if(!condition) goto end;
            // body;
            // goto start;
            // end:
            // free condition register
            //
            .while_statement => |while_statement| {
                // Get the index which starts the condition check
                const condition_start = codegen.nextEmitBytecodeIndex();

                const condition_register = (try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    while_statement.condition,
                    false,
                    null,
                )).?;
                std.debug.assert(condition_register[1] == .bool);

                // Emit the branch which will conditionally skip to after the loop if the condition is zero
                const skip_to_end_instruction = try codegen.emitBranchEqualZero(condition_register[0]);

                // Emit the body of the while loop
                std.debug.assert(try compileExpression(
                    codegen,
                    function_local_variables,
                    scope_local_variables,
                    while_statement.body,
                    true,
                    null,
                ) == null);

                std.debug.print("HASNEOSHANEO {d} - {d}\n", .{ codegen.nextEmitBytecodeIndex(), condition_start });

                // Generate a branch which branches directly back to the condition check
                codegen.bytecode.items[try codegen.emitBranch()].params.B.branch_offset =
                    @as(i32, @intCast(condition_start)) - @as(i32, @intCast(codegen.nextEmitBytecodeIndex()));

                // Update the "skip to end" jump to go to the next instruction emit
                codegen.bytecode.items[skip_to_end_instruction].params.BEZ.branch_offset =
                    @intCast(codegen.nextEmitBytecodeIndex() - skip_to_end_instruction);

                try codegen.register_allocator.free(condition_register);
            },
            else => |tag| std.debug.panic("cant codegen for block {s} yet\n", .{@tagName(tag)}),
        }
    }

    //Free all the allocated registers for these variables
    for (local_variables_from_this_scope.items) |local_variable_to_free| {
        const local_variable = function_local_variables.get(local_variable_to_free).?;

        try codegen.register_allocator.free(.{
            @intCast(local_variable.offset),
            codegen.genny.type_references.keys()[local_variable.type_reference].machine_type,
        });
        _ = scope_local_variables.swapRemove(local_variable_to_free);
    }

    if (top_level and !return_emit) {
        if (return_type == .void) {
            //On void return functions, we can emit an implicit RET
            try codegen.emitRet(0, .void);
        } else {
            //On non void return functions, we need to error
            std.debug.panic("non void return function does not return at end of function!", .{});
        }
    }
}

fn compileFunction(self: *Genny, function: *Parser.Node.Function, class: *Parser.Node.Class) !MMTypes.FunctionDefinition {
    var arguments = ArgumentList.init(self.ast.allocator);
    _ = &arguments;
    var local_variables = LocalVariableTable.init(self.ast.allocator);
    var scope_local_variables = LocalVariableTable.init(self.ast.allocator);
    _ = &local_variables;

    var codegen: Codegen = .{
        .bytecode = BytecodeList.init(self.ast.allocator),
        .line_numbers = LineNumberList.init(self.ast.allocator),
        .register_allocator = try Codegen.RegisterAllocator.init(self.ast.allocator, self.revision),
        .revision = self.revision,
        .genny = self,
    };
    _ = &codegen;

    //If the function isnt static, then we need to allocate `r0-r3` for the `this` reference
    if (!function.modifiers.static) {
        const self_reg = try codegen.register_allocator.allocate(.object_ref);

        std.debug.assert(self_reg[0] == 0);

        const type_reference: u32 = @intCast((try self.type_references.getOrPut(class.type_reference.?)).index);

        const self_variable: MMTypes.LocalVariable = .{
            .modifiers = .{},
            .name = @intCast((try self.a_string_table.getOrPut("this")).index),
            .offset = self_reg[0],
            .type_reference = type_reference,
        };

        try local_variables.put("this", self_variable);
        try scope_local_variables.put("this", self_variable);
    }

    for (function.parameters) |parameter| {
        const register = try codegen.register_allocator.allocate(parameter.type.resolved.runtime_type.machine_type);
        const type_reference: u32 = @intCast((try self.type_references.getOrPut(parameter.type.resolved.runtime_type)).index);

        const parameter_variable: MMTypes.LocalVariable = .{
            .name = @intCast((try self.a_string_table.getOrPut(parameter.name)).index),
            .modifiers = .{},
            .type_reference = type_reference,
            .offset = register[0],
        };

        try local_variables.put(parameter.name, parameter_variable);
        try scope_local_variables.put(parameter.name, parameter_variable);
        try arguments.append(.{
            .type_reference = type_reference,
            .offset = register[0],
        });
    }

    //TODO: arrow expression functions
    try compileBlock(
        &codegen,
        &local_variables,
        &scope_local_variables,
        function.body.?.contents.block,
        true,
        function.return_type.resolved.runtime_type.machine_type,
    );

    //TODO: let users put this safety feature under a debug flag
    {
        //TODO: let users force ASSERT to be used, since that can give nice names/text
        //TODO: use WRITE if available, else try to use a patched runtime to call into that game's printf,
        //      if that fails, THEN fall back to the CALLVo crash

        // Allocate a register which will just have 0
        const call_idx = try codegen.register_allocator.allocate(.object_ref);

        // Load 0 into that instruction
        try codegen.emitLoadConstInt(call_idx[0], 0);

        // Emit a call instruction which uses a nonsense target and 0 source, causing a script exception
        try codegen.emitCallVo(call_idx[0], 0, .void);

        try codegen.register_allocator.free(call_idx);
    }

    // Stack size is the highest used register + 1, since registers are zero indexed
    const stack_size = codegen.register_allocator.highest_register + 1;

    return .{
        //TODO: this name needs to be mangled
        .name = @intCast((try self.a_string_table.getOrPut(function.mangled_name.?)).index),
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
            // Convert all the GUID type references into the list of depening GUIDs
            .depending_guids = blk: {
                var depending_guids = std.ArrayList(u32).init(self.ast.allocator);

                for (self.type_references.keys()) |type_reference|
                    if (type_reference.script) |script|
                        switch (script) {
                            .guid => |guid| try depending_guids.append(guid),
                            .hash => {},
                        };

                break :blk depending_guids.items;
            },
            .functions = functions,
            .bytecode = self.bytecode.items,
            .arguments = self.arguments.items,
            .line_numbers = self.line_numbers.items,
            .local_variables = self.local_variables.items,
            .function_references = self.function_references.values(),
            .constant_table_s64 = self.s64_constants.keys(),
            .constant_table_float = std.mem.bytesAsSlice(f32, std.mem.sliceAsBytes(self.f32_constants.keys())),
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
