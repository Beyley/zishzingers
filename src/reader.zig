const std = @import("std");
const builtin = @import("builtin");

const MMTypes = @import("MMTypes.zig");

pub fn MMReader(comptime Reader: type) type {
    _ = Reader; // autofix
    return struct {
        reader: std.fs.File.Reader,
        compression_flags: MMTypes.CompressionFlags,
        revision: MMTypes.Revision,

        const Self = @This();

        pub fn readInt(self: Self, comptime T: type) !T {
            //If the file has compressed integers, read the varint
            if (self.compression_flags.compressed_integers and @typeInfo(T).Int.bits > 16)
                return try self.readVarInt(T);

            //Else read the int as normal
            return try self.reader.readInt(T, .big);
        }

        pub fn readFloat(self: Self, comptime T: type) !T {
            return @bitCast(try self.reader.readInt(std.meta.Int(.unsigned, @typeInfo(T).Float.bits), .big));
        }

        pub fn readSha1(self: Self) ![std.crypto.hash.Sha1.digest_length]u8 {
            var ret: [std.crypto.hash.Sha1.digest_length]u8 = undefined;

            _ = try self.reader.readAtLeast(&ret, std.crypto.hash.Sha1.digest_length);

            return ret;
        }

        fn readVarInt(self: Self, comptime IntType: type) !IntType {
            //Do all the work in unsigned space to prevent signed-ness shenanigans
            var result: std.meta.Int(.unsigned, @bitSizeOf(IntType)) = 0;
            var i: std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(IntType))) = 0;

            while (true) {
                const b: u8 = try self.reader.readByte();

                result |= @as(@TypeOf(result), b & 0b01111111) << i;

                //If the MSB is not set, break out
                if (b & 0b10000000 == 0) {
                    break;
                }

                //Mark that we are now filling out the next 7 bits
                i += 7;
            }

            return @bitCast(result);
        }

        fn readBytes(self: Self, buf: []u8) !usize {
            return try self.reader.readAll(buf);
        }

        fn readString(self: Self, allocator: std.mem.Allocator) ![]const u8 {
            const len =
                if (self.compression_flags.compressed_integers)
                @divExact(try self.readInt(u32), 2)
            else
                try self.readInt(u32);

            //Allocate the space for the new string
            const str = try allocator.alloc(u8, len);
            errdefer allocator.free(str);

            //Make sure that we read the whole string
            if (try self.reader.readAll(str) < len)
                return error.EndOfStream;

            return str;
        }

        pub fn readScript(self: Self, allocator: std.mem.Allocator) !MMTypes.Script {
            const up_to_date_script: ?MMTypes.ResourceIdentifier = if (self.revision.head <= 0x33a)
                try self.readResource(false)
            else
                null;

            const class_name = try self.readString(allocator);
            const super_class_script = try self.readResource(false);

            const read_type_is_u16 = self.revision.head >= 0x3d9;

            switch (read_type_is_u16) {
                inline else => |val| {
                    const ScriptReadType = if (val) u16 else u32;

                    const modifiers: ?u32 = if (self.revision.head >= 0x1e5)
                        try self.readInt(ScriptReadType)
                    else
                        null;

                    const type_references = try self.readArray(MMTypes.TypeReference, allocator, null, ScriptReadType);
                    const field_references = try self.readArray(MMTypes.FieldReference, allocator, null, ScriptReadType);
                    const function_references = try self.readArray(MMTypes.FunctionReference, allocator, null, ScriptReadType);
                    const field_definitions = try self.readArray(MMTypes.FieldDefinition, allocator, null, ScriptReadType);
                    const property_definitions = try self.readArray(MMTypes.PropertyDefinition, allocator, null, ScriptReadType);

                    if (self.revision.head < 0x1ec) @panic("AAAA");

                    const functions = try self.readArray(MMTypes.FunctionDefinition, allocator, null, ScriptReadType);
                    const shared_arguments = try self.readArray(MMTypes.Argument, allocator, null, ScriptReadType);
                    defer allocator.free(shared_arguments);
                    const shared_bytecode = try self.readArray(MMTypes.Bytecode, allocator, null, ScriptReadType);
                    defer allocator.free(shared_bytecode);
                    const shared_line_numbers = try self.readArray(u16, allocator, null, ScriptReadType);
                    defer allocator.free(shared_line_numbers);
                    const shared_local_variables = try self.readArray(MMTypes.LocalVariable, allocator, null, ScriptReadType);
                    defer allocator.free(shared_local_variables);

                    // In revision 0x3d9, range ends were made to be relative, not absolute, this is probably to make save storage on large scripts
                    if (self.revision.head >= 0x3d9) {
                        for (functions) |*function_definition| {
                            function_definition.arguments.idx.end += function_definition.arguments.idx.begin;
                            function_definition.bytecode.idx.end += function_definition.bytecode.idx.begin;
                            function_definition.line_numbers.idx.end += function_definition.line_numbers.idx.begin;
                            function_definition.local_variables.idx.end += function_definition.local_variables.idx.begin;
                        }
                    }

                    const a_str_table: MMTypes.AStringTable = blk: {
                        const indices = try self.readTable(allocator);
                        defer allocator.free(indices);

                        const str_buf_len = try self.readInt(u32);

                        //Read the full string buffer
                        const str_buf = try allocator.alloc(u8, str_buf_len);
                        if (try self.readBytes(str_buf) < str_buf_len)
                            return error.EndOfStream;

                        const str_count = std.mem.count(u8, str_buf, &.{0});

                        const strings: [][]const u8 = try allocator.alloc([]const u8, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u8, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end];

                            i = end + 1;
                        }

                        break :blk MMTypes.AStringTable{
                            .buf = str_buf,
                            .strings = strings,
                        };
                    };

                    const w_str_table: MMTypes.WStringTable = blk: {
                        const indices = try self.readTable(allocator);
                        defer allocator.free(indices);

                        const str_buf_len = try self.readInt(u32);

                        //Read the full string buffer
                        const str_buf = try allocator.alloc(u16, str_buf_len);
                        if (try self.readBytes(std.mem.sliceAsBytes(str_buf)) < str_buf_len)
                            return error.EndOfStream;

                        const str_count = std.mem.count(u16, str_buf, &.{0});

                        const strings: [][]u16 = try allocator.alloc([]u16, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u16, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end];

                            // If we are on a LE machine, byte swap the data, since LBP uses BE UTF-16 here.
                            if (builtin.cpu.arch.endian() == .little)
                                for (str.*) |*c| {
                                    c.* = @byteSwap(c.*);
                                };

                            i = end + 1;
                        }

                        break :blk MMTypes.WStringTable{
                            .buf = str_buf,
                            .strings = strings,
                        };
                    };

                    const constant_table_s64: ?[]u32 =
                        if (self.revision.head >= 0x3e2)
                        try self.readArray(u32, allocator, null, ScriptReadType)
                    else
                        null;

                    const constant_table_float: []f32 = try self.readArray(f32, allocator, null, ScriptReadType);

                    const depending_guids: ?[]u32 =
                        if (self.revision.head >= 0x1ec)
                        try self.readArray(u32, allocator, null, ScriptReadType)
                    else
                        null;

                    { //start resolving shit
                        for (type_references) |*type_reference| {
                            if (type_reference.type_name.idx != 0xFFFFFFFF)
                                type_reference.type_name = .{ .string = &a_str_table.strings[type_reference.type_name.idx] }
                            else
                                type_reference.type_name = .{ .string = null };
                        }

                        for (field_references) |*field_reference| {
                            if (field_reference.name.idx != 0xFFFFFFFF)
                                field_reference.name = .{ .string = &a_str_table.strings[field_reference.name.idx] }
                            else
                                field_reference.name = .{ .string = null };

                            if (field_reference.type_reference.idx != 0xFFFFFFFF)
                                field_reference.type_reference = .{ .type_reference = &type_references[field_reference.type_reference.idx] };
                        }

                        for (function_references) |*function_reference| {
                            if (function_reference.name.idx != 0xFFFFFFFF)
                                function_reference.name = .{ .string = &a_str_table.strings[function_reference.name.idx] }
                            else
                                function_reference.name = .{ .string = null };

                            if (function_reference.type_reference.idx != 0xFFFFFFFF)
                                function_reference.type_reference = .{ .type_reference = &type_references[function_reference.type_reference.idx] };
                        }

                        for (field_definitions) |*field_definition| {
                            if (field_definition.name.idx != 0xFFFFFFFF)
                                field_definition.name = .{ .string = &a_str_table.strings[field_definition.name.idx] }
                            else
                                field_definition.name = .{ .string = null };

                            if (field_definition.type_reference.idx != 0xFFFFFFFF)
                                field_definition.type_reference = .{ .type_reference = &type_references[field_definition.type_reference.idx] };
                        }

                        for (property_definitions) |*property_definition| {
                            if (property_definition.name.idx != 0xFFFFFFFF)
                                property_definition.name = .{ .string = &a_str_table.strings[property_definition.name.idx] }
                            else
                                property_definition.name = .{ .string = null };

                            if (property_definition.type_reference.idx != 0xFFFFFFFF)
                                property_definition.type_reference = .{ .type_reference = &type_references[property_definition.type_reference.idx] };

                            if (property_definition.set_function.idx != 0xFFFFFFFF)
                                property_definition.set_function = .{ .function = &functions[property_definition.set_function.idx] };

                            if (property_definition.get_function.idx != 0xFFFFFFFF)
                                property_definition.get_function = .{ .function = &functions[property_definition.get_function.idx] };
                        }

                        for (shared_arguments) |*argument| {
                            if (argument.type_reference.idx != 0xFFFFFFFF)
                                argument.type_reference = .{ .type_reference = &type_references[argument.type_reference.idx] };
                        }

                        for (shared_local_variables) |*local_variable| {
                            if (local_variable.type_reference.idx != 0xFFFFFFFF)
                                local_variable.type_reference = .{ .type_reference = &type_references[local_variable.type_reference.idx] };

                            if (local_variable.name.idx != 0xFFFFFFFF)
                                local_variable.name = .{ .string = &a_str_table.strings[local_variable.name.idx] }
                            else
                                local_variable.name = .{ .string = null };
                        }

                        for (functions) |*function| {
                            if (function.name.idx != 0xFFFFFFFF)
                                function.name = .{ .string = &a_str_table.strings[function.name.idx] }
                            else
                                function.name = .{ .string = null };

                            if (function.type_reference.idx != 0xFFFFFFFF)
                                function.type_reference = .{ .type_reference = &type_references[function.type_reference.idx] };

                            function.arguments = .{ .slice = try allocator.dupe(MMTypes.Argument, shared_arguments[function.arguments.idx.begin..function.arguments.idx.end]) };
                            function.bytecode = .{ .slice = try allocator.dupe(MMTypes.Bytecode, shared_bytecode[function.bytecode.idx.begin..function.bytecode.idx.end]) };
                            function.line_numbers = .{ .slice = try allocator.dupe(u16, shared_line_numbers[function.line_numbers.idx.begin..function.line_numbers.idx.end]) };
                            function.local_variables = .{ .slice = try allocator.dupe(MMTypes.LocalVariable, shared_local_variables[function.local_variables.idx.begin..function.local_variables.idx.end]) };
                        }
                    }

                    return MMTypes.Script{
                        .up_to_date_script = up_to_date_script,
                        .class_name = class_name,
                        .super_class_script = super_class_script,
                        .modifiers = modifiers,
                        .type_references = type_references,
                        .field_references = field_references,
                        .function_references = function_references,
                        .field_definitions = field_definitions,
                        .property_definitions = property_definitions,
                        .functions = functions,
                        .a_string_table = a_str_table,
                        .w_string_table = w_str_table,
                        .constant_table_s64 = constant_table_s64,
                        .constant_table_float = constant_table_float,
                        .depending_guids = depending_guids,
                    };
                },
            }
        }

        pub fn readResource(self: Self, skip_flags: bool) !?MMTypes.ResourceIdentifier {
            const hash: u32, const guid: u32 = if (self.revision.head <= 0x18b) .{ 2, 1 } else .{ 1, 2 };

            const flags: u32 = if (self.revision.head > 0x22e and !skip_flags) try self.readInt(u32) else 0;
            _ = flags; // idk what flags does, seems to just be ignored

            const ident_type = try self.readInt(u8);

            if (ident_type == hash) {
                return .{
                    .hash = try self.readSha1(),
                };
            } else if (ident_type == guid) {
                return .{
                    .guid = try self.readInt(u32),
                };
            }

            //TODO: should this be an error?
            return null;
        }

        pub fn readArray(self: Self, comptime T: type, allocator: std.mem.Allocator, length: ?usize, comptime ScriptReadType: type) ![]T {
            const len: usize = length orelse try self.readInt(u32);

            const arr = try allocator.alloc(T, len);
            errdefer allocator.free(arr);

            for (arr) |*item| {
                if (@typeInfo(T) == .Int) {
                    item.* = try self.readInt(T);
                } else if (@typeInfo(T) == .Float) {
                    item.* = try self.readFloat(T);
                } else item.* = switch (T) {
                    MMTypes.Bytecode => @bitCast(try self.readInt(u64)),
                    // Bytecode => byteSwapFromBe(Bytecode, @bitCast(try self.readInt(u64))),
                    MMTypes.TypeReference => .{
                        .machine_type = try self.reader.readEnum(MMTypes.MachineType, .big),
                        .fish_type = try self.reader.readEnum(MMTypes.FishType, .big),
                        .dimension_count = try self.readInt(u8),
                        .array_base_machine_type = try self.reader.readEnum(MMTypes.MachineType, .big),
                        .script = try self.readResource(false),
                        .type_name = .{ .idx = try self.readInt(u32) },
                    },
                    MMTypes.FieldReference, MMTypes.FunctionReference => .{
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                    },
                    MMTypes.FieldDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                    },
                    MMTypes.PropertyDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .get_function = .{ .idx = try self.readInt(u32) },
                        .set_function = .{ .idx = try self.readInt(u32) },
                    },
                    MMTypes.FunctionDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .arguments = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .bytecode = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .line_numbers = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .local_variables = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .stack_size = try self.readInt(u32),
                    },
                    MMTypes.Argument => .{
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .offset = try self.readInt(u32),
                    },
                    MMTypes.LocalVariable => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .offset = try self.readInt(u32),
                    },
                    else => @compileError("Unknown type " ++ @typeName(T)),
                };
            }

            return arr;
        }

        pub fn readTable(self: Self, allocator: std.mem.Allocator) ![]u32 {
            if (!self.compression_flags.compressed_vectors) {
                return self.readArray(u32, allocator, null, undefined);
            }

            const index_count = try self.readInt(u32);

            //If theres no indices, return nothing
            if (index_count == 0) return &.{};

            const table_count = try self.readInt(u32);

            //If theres no tables, return a single 0 item
            if (table_count == 0) return try allocator.dupe(u32, &.{0});

            const values = try allocator.alloc(u32, index_count);

            for (0..index_count) |i|
                values[i] = try self.readInt(u8);

            for (1..table_count) |_|
                for (0..index_count) |j| {
                    values[j] += @as(u32, try self.readInt(u8)) * 0x100;
                };

            return values;
        }
    };
}

/// Demangles a function name
pub fn demangleFunctionName(orig: []const u8, extract_args: bool, allocator: std.mem.Allocator) ![]const u8 {
    const sep_index = std.mem.indexOf(u8, orig, "__").?;

    const name = orig[0..sep_index];

    if (!extract_args)
        return try allocator.dupe(u8, name);

    const args_str = orig[sep_index + 2 ..];

    var demangled = std.ArrayList(u8).init(allocator);
    defer demangled.deinit();

    try demangled.appendSlice(name);
    try demangled.appendSlice("(");

    var i: usize = 0;
    while (i < args_str.len) : (i += 1) {
        if (i > 0) try demangled.appendSlice(", ");

        // std.debug.print("i: {d}", .{i});

        const c = args_str[i];
        if (c == 'Q') {
            var digit_count: usize = 0;

            //count the amount of numeric chars
            for (args_str[i + 1 ..]) |d| {
                if (std.ascii.isDigit(d))
                    digit_count += 1
                else
                    break;
            }

            const count = try std.fmt.parseInt(usize, args_str[i + 1 .. i + digit_count + 1], 10);

            const type_name = args_str[i + 1 + digit_count .. i + 1 + digit_count + count];

            try demangled.appendSlice(type_name);

            //Skip the whole parameter
            i += digit_count + count;
            continue;
        }

        const fish_type = fishTypeFromMangledId(c);

        try demangled.appendSlice(@tagName(fish_type));
    }

    try demangled.appendSlice(")");

    return demangled.toOwnedSlice();
}

fn fishTypeFromMangledId(id: u8) MMTypes.FishType {
    return switch (id) {
        'v' => .void,
        'b' => .bool,
        'w' => .char,
        'i' => .s32,
        'f' => .f32,
        'p' => .v2,
        'q' => .v3,
        'r' => .v4,
        'm' => .m44,
        'g' => .guid,
        'j' => .s64,
        'd' => .f64,
        else => std.debug.panic("Unknown mangling ID {c}", .{id}),
    };
}

// /// Byteswap's all the fields of a struct individually from BE to LE
// fn byteSwapFromBe(comptime T: type, val: T) T {
//     //Short circuit the function on big endian, since no byte swap needs to take place
//     if (builtin.cpu.arch.endian() == .big)
//         return val;

//     const type_info: std.builtin.Type = @typeInfo(T);

//     var ret: T = val;

//     //we do the byteswap earlier :)
//     if (T == Bytecode) {
//         return ret;
//     }

//     switch (type_info) {
//         .Struct => |data| {
//             inline for (data.fields) |field| {
//                 @field(ret, field.name) = byteSwapFromBe(field.type, @field(ret, field.name));
//             }
//         },
//         .Int => {
//             ret = @byteSwap(ret);
//         },
//         .Enum => |data| {
//             ret = @enumFromInt(byteSwapFromBe(data.tag_type, @intFromEnum(ret)));
//         },
//         .Float => |float| {
//             const IntermediaryIntType = std.meta.Int(.unsigned, float.bits);

//             ret = @bitCast(@byteSwap(@as(IntermediaryIntType, ret)));
//         },
//         else => @compileError("Unsupported type " ++ @typeName(T)),
//     }

//     return ret;
// }
