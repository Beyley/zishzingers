const std = @import("std");
const builtin = @import("builtin");

const MMTypes = @import("MMTypes.zig");

pub fn MMStream(comptime Stream: type) type {
    return struct {
        stream: Stream,
        compression_flags: MMTypes.CompressionFlags,
        revision: MMTypes.Revision,

        const Self = @This();

        pub fn readInt(self: *Self, comptime T: type) !T {
            //If the file has compressed integers, read the varint
            if (self.compression_flags.compressed_integers and @typeInfo(T).Int.bits > 16)
                return try self.readVarInt(T);

            //Else read the int as normal
            return try self.stream.reader().readInt(T, .big);
        }

        pub fn readFloat(self: *Self, comptime T: type) !T {
            return @bitCast(try self.stream.reader().readInt(std.meta.Int(.unsigned, @typeInfo(T).Float.bits), .big));
        }

        pub fn readSha1(self: *Self) ![std.crypto.hash.Sha1.digest_length]u8 {
            var ret: [std.crypto.hash.Sha1.digest_length]u8 = undefined;

            _ = try self.stream.reader().readAtLeast(&ret, std.crypto.hash.Sha1.digest_length);

            return ret;
        }

        fn readVarInt(self: *Self, comptime IntType: type) !IntType {
            //Do all the work in unsigned space to prevent signed-ness shenanigans
            var result: std.meta.Int(.unsigned, @bitSizeOf(IntType)) = 0;
            var i: std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(IntType))) = 0;

            while (true) {
                const b: u8 = try self.stream.reader().readByte();

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

        fn readBytes(self: *Self, buf: []u8) !usize {
            return try self.stream.reader().readAll(buf);
        }

        fn readString(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
            const len = if (self.compression_flags.compressed_integers)
                @divExact(try self.readInt(u32), 2)
            else
                try self.readInt(u32);

            //Allocate the space for the new string
            const str = try allocator.alloc(u8, len);
            errdefer allocator.free(str);

            //Make sure that we read the whole string
            if (try self.stream.reader().readAll(str) < len)
                return error.EndOfStream;

            return str;
        }

        pub fn readScript(self: *Self, allocator: std.mem.Allocator) !MMTypes.Script {
            const up_to_date_script: ?MMTypes.ResourceIdentifier = if (self.revision.head <= 0x33a)
                try self.readResource(false)
            else
                null;

            const class_name = try self.readString(allocator);
            errdefer allocator.free(class_name);
            const super_class_script = try self.readResource(false);

            switch (self.revision.head >= 0x3d9) {
                inline else => |read_type_is_u16| {
                    const ScriptReadType = if (read_type_is_u16) u16 else u32;

                    const modifiers: ?u32 = if (self.revision.head >= 0x1e5)
                        try self.readInt(ScriptReadType)
                    else
                        null;

                    const type_references = try self.readArray(MMTypes.TypeReference, allocator, null, ScriptReadType);
                    errdefer allocator.free(type_references);
                    const field_references = try self.readArray(MMTypes.FieldReference, allocator, null, ScriptReadType);
                    errdefer allocator.free(field_references);
                    const function_references = try self.readArray(MMTypes.FunctionReference, allocator, null, ScriptReadType);
                    errdefer allocator.free(function_references);
                    const field_definitions = try self.readArray(MMTypes.FieldDefinition, allocator, null, ScriptReadType);
                    errdefer allocator.free(field_definitions);
                    const property_definitions = try self.readArray(MMTypes.PropertyDefinition, allocator, null, ScriptReadType);
                    errdefer allocator.free(property_definitions);

                    const functions, const shared_arguments, const shared_bytecode, const shared_line_numbers, const shared_local_variables =
                        if (self.revision.head < 0x1ec)
                    blk: {
                        const functions = try self.readArray(MMTypes.Function, allocator, null, ScriptReadType);
                        defer {
                            for (functions) |function| {
                                function.deinit(allocator);
                            }

                            allocator.free(functions);
                        }

                        const function_definitions = try allocator.alloc(MMTypes.FunctionDefinition, functions.len);
                        errdefer allocator.free(functions);
                        var shared_arguments = std.ArrayList(MMTypes.Argument).init(allocator);
                        defer shared_arguments.deinit();
                        var shared_bytecode = std.ArrayList(MMTypes.Bytecode).init(allocator);
                        defer shared_bytecode.deinit();
                        var shared_line_numbers = std.ArrayList(u16).init(allocator);
                        defer shared_line_numbers.deinit();
                        var shared_local_variables = std.ArrayList(MMTypes.LocalVariable).init(allocator);
                        defer shared_local_variables.deinit();

                        for (functions, 0..) |function, i| {
                            function_definitions[i] = MMTypes.FunctionDefinition{
                                .name = function.name,
                                .modifiers = function.modifiers,
                                .stack_size = function.stack_size,
                                .type_reference = function.type_reference,
                                .arguments = .{ .begin = @intCast(shared_arguments.items.len), .end = @intCast(shared_arguments.items.len + function.arguments.len) },
                                .bytecode = .{ .begin = @intCast(shared_bytecode.items.len), .end = @intCast(shared_bytecode.items.len + function.bytecode.len) },
                                .line_numbers = .{ .begin = @intCast(shared_line_numbers.items.len), .end = @intCast(shared_line_numbers.items.len + function.line_numbers.len) },
                                .local_variables = .{ .begin = @intCast(shared_local_variables.items.len), .end = @intCast(shared_local_variables.items.len + function.local_variables.len) },
                            };
                            try shared_arguments.appendSlice(function.arguments);
                            try shared_bytecode.appendSlice(function.bytecode);
                            try shared_line_numbers.appendSlice(function.line_numbers);
                            try shared_local_variables.appendSlice(function.local_variables);
                        }

                        break :blk .{
                            function_definitions,
                            try shared_arguments.toOwnedSlice(),
                            try shared_bytecode.toOwnedSlice(),
                            try shared_line_numbers.toOwnedSlice(),
                            try shared_local_variables.toOwnedSlice(),
                        };
                    } else blk: {
                        const functions = try self.readArray(MMTypes.FunctionDefinition, allocator, null, ScriptReadType);
                        errdefer allocator.free(functions);
                        const shared_arguments = try self.readArray(MMTypes.Argument, allocator, null, ScriptReadType);
                        errdefer allocator.free(shared_arguments);
                        const shared_bytecode = try self.readArray(MMTypes.Bytecode, allocator, null, ScriptReadType);
                        errdefer allocator.free(shared_bytecode);
                        const shared_line_numbers = try self.readArray(u16, allocator, null, ScriptReadType);
                        errdefer allocator.free(shared_line_numbers);
                        const shared_local_variables = try self.readArray(MMTypes.LocalVariable, allocator, null, ScriptReadType);
                        errdefer allocator.free(shared_local_variables);

                        break :blk .{
                            functions,
                            shared_arguments,
                            shared_bytecode,
                            shared_line_numbers,
                            shared_local_variables,
                        };
                    };
                    errdefer allocator.free(functions);
                    errdefer allocator.free(shared_arguments);
                    errdefer allocator.free(shared_bytecode);
                    errdefer allocator.free(shared_line_numbers);
                    errdefer allocator.free(shared_local_variables);

                    // In revision 0x3d9, range ends were made to be relative, not absolute, this is probably to make save storage on large scripts
                    if (self.revision.head >= 0x3d9) {
                        for (functions) |*function_definition| {
                            function_definition.arguments.end += function_definition.arguments.begin;
                            function_definition.bytecode.end += function_definition.bytecode.begin;
                            function_definition.line_numbers.end += function_definition.line_numbers.begin;
                            function_definition.local_variables.end += function_definition.local_variables.begin;
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

                        const strings: [][:0]const u8 = try allocator.alloc([:0]const u8, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u8, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end :0];

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

                        const strings: [][:0]u16 = try allocator.alloc([:0]u16, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u16, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end :0];

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

                    const constant_table_s64: ?[]i64 =
                        if (self.revision.head >= 0x30c)
                        try self.readArray(i64, allocator, null, ScriptReadType)
                    else
                        null;

                    const constant_table_float: []f32 = try self.readArray(f32, allocator, null, ScriptReadType);

                    const depending_guids: ?[]u32 =
                        if (self.revision.head >= 0x1ec)
                        try self.readArray(u32, allocator, null, ScriptReadType)
                    else
                        null;

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
                        .bytecode = shared_bytecode,
                        .local_variables = shared_local_variables,
                        .arguments = shared_arguments,
                        .line_numbers = shared_line_numbers,
                    };
                },
            }
        }

        pub fn readResource(self: *Self, skip_flags: bool) !?MMTypes.ResourceIdentifier {
            const hash: u8, const guid: u8 = if (self.revision.head <= 0x191) .{ 2, 1 } else .{ 1, 2 };

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

        pub fn readArray(self: *Self, comptime T: type, allocator: std.mem.Allocator, length: ?usize, comptime ScriptReadType: type) ![]T {
            const len: usize = length orelse try self.readInt(u32);

            const arr = try allocator.alloc(T, len);
            errdefer allocator.free(arr);

            const TypeInfo = @typeInfo(T);

            for (arr) |*item| {
                if (TypeInfo == .Int) {
                    item.* = try self.readInt(T);
                } else if (TypeInfo == .Float) {
                    item.* = try self.readFloat(T);
                } else item.* = switch (T) {
                    MMTypes.Bytecode => @bitCast(try self.readInt(u64)),
                    MMTypes.TypeReference => .{
                        .machine_type = try self.stream.reader().readEnum(MMTypes.MachineType, .big),
                        .fish_type = try self.stream.reader().readEnum(MMTypes.FishType, .big),
                        .dimension_count = try self.readInt(u8),
                        .array_base_machine_type = try self.stream.reader().readEnum(MMTypes.MachineType, .big),
                        .script = try self.readResource(false),
                        .type_name = try self.readInt(u32),
                    },
                    MMTypes.FieldReference, MMTypes.FunctionReference => .{
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                    },
                    MMTypes.FieldDefinition => .{
                        .modifiers = @bitCast(@as(u32, try self.readInt(ScriptReadType))),
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                    },
                    MMTypes.PropertyDefinition => .{
                        .modifiers = @bitCast(@as(u32, try self.readInt(ScriptReadType))),
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                        .get_function = try self.readInt(u32),
                        .set_function = try self.readInt(u32),
                    },
                    MMTypes.FunctionDefinition => .{
                        .modifiers = @bitCast(@as(u32, try self.readInt(ScriptReadType))),
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                        .arguments = .{
                            .begin = try self.readInt(ScriptReadType),
                            .end = try self.readInt(ScriptReadType),
                        },
                        .bytecode = .{
                            .begin = try self.readInt(ScriptReadType),
                            .end = try self.readInt(ScriptReadType),
                        },
                        .line_numbers = .{
                            .begin = try self.readInt(ScriptReadType),
                            .end = try self.readInt(ScriptReadType),
                        },
                        .local_variables = .{
                            .begin = try self.readInt(ScriptReadType),
                            .end = try self.readInt(ScriptReadType),
                        },
                        .stack_size = try self.readInt(u32),
                    },
                    MMTypes.Argument => .{
                        .type_reference = try self.readInt(u32),
                        .offset = try self.readInt(u32),
                    },
                    MMTypes.LocalVariable => .{
                        .modifiers = @bitCast(@as(u32, try self.readInt(ScriptReadType))),
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                        .offset = try self.readInt(u32),
                    },
                    MMTypes.Function => .{
                        .modifiers = @bitCast(try self.readInt(u32)),
                        .type_reference = try self.readInt(u32),
                        .name = try self.readInt(u32),
                        .arguments = try self.readArray(MMTypes.Argument, allocator, null, ScriptReadType),
                        .bytecode = try self.readArray(MMTypes.Bytecode, allocator, null, ScriptReadType),
                        .line_numbers = try self.readArray(u16, allocator, null, ScriptReadType),
                        .local_variables = try self.readArray(MMTypes.LocalVariable, allocator, null, ScriptReadType),
                        .stack_size = try self.readInt(u32),
                    },
                    else => @compileError("Unknown type " ++ @typeName(T)),
                };
            }

            return arr;
        }

        pub fn readTable(self: *Self, allocator: std.mem.Allocator) ![]u32 {
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

        pub fn writeInt(self: *Self, comptime T: type, val: T) !void {
            const bit_count = @typeInfo(T).Int.bits;

            //If the int is >16 bits and we have compressed integers enabled, use them
            if (bit_count > 16 and self.compression_flags.compressed_integers) {
                const UnsignedType = std.meta.Int(.unsigned, bit_count);

                //Bitcast the number to unsigned, this is so the shifts are all done as unsigned shifts
                var cur: UnsignedType = @bitCast(val);

                if (cur == 0) {
                    return self.stream.writer().writeByte(0);
                }

                // While we still have more bits to write
                while (cur > 0) {
                    //Get the last seven bits of the int
                    var b: u8 = @intCast(cur & 0b01111111);

                    //Shift value right by 7 bits to work on the next bits
                    cur >>= 7;

                    //If theres more bits to write
                    if (cur > 0) {
                        //Set the uppermost bit, to mark theres more data after this
                        b |= 0b10000000;
                    }

                    try self.stream.writer().writeByte(b);
                }
            } else {
                return self.stream.writer().writeInt(T, val, .big);
            }
        }

        pub fn writeFloat(self: *Self, val: anytype) !void {
            return self.stream.writer().writeInt(
                std.meta.Int(.unsigned, @typeInfo(@TypeOf(val)).Float.bits),
                @bitCast(val),
                .big,
            );
        }

        pub fn writeSha1(self: *Self, val: [std.crypto.hash.Sha1.digest_length]u8) !void {
            return self.stream.writer().writeAll(&val);
        }

        pub fn writeString(self: *Self, str: []const u8) !void {
            if (self.compression_flags.compressed_integers) {
                try self.writeInt(u32, @intCast(str.len * 2));
            } else {
                try self.writeInt(u32, @intCast(str.len));
            }

            return self.stream.writer().writeAll(str);
        }

        pub fn writeScript(self: *Self, script: MMTypes.Script, allocator: std.mem.Allocator) !void {
            //In revision 0x33a, the "up to date script" field was removed
            if (self.revision.head <= 0x33a)
                try self.writeResource(false, script.up_to_date_script);

            try self.writeString(script.class_name);

            try self.writeResource(false, script.super_class_script);

            //In revision 0x3d9, most script fields are now serialized as `u16` instead of `u32`
            switch (self.revision.head >= 0x3d9) {
                inline else => |write_type_is_u16| {
                    const ScriptWriteType = if (write_type_is_u16) u16 else u32;

                    //In revision 0x1e5 modifiers were added to scripts
                    if (self.revision.head >= 0x1e5)
                        try self.writeInt(ScriptWriteType, @intCast(script.modifiers orelse 0));

                    try self.writeArray(MMTypes.TypeReference, script.type_references, false, ScriptWriteType);
                    try self.writeArray(MMTypes.FieldReference, script.field_references, false, ScriptWriteType);
                    try self.writeArray(MMTypes.FunctionReference, script.function_references, false, ScriptWriteType);
                    try self.writeArray(MMTypes.FieldDefinition, script.field_definitions, false, ScriptWriteType);
                    try self.writeArray(MMTypes.PropertyDefinition, script.property_definitions, false, ScriptWriteType);

                    // In revision 0x1ed, functions were made to no longer own their data,
                    // and instead switched to a shared set of data between all functions
                    if (self.revision.head < 0x1ec)
                        @panic("Unimplemented writing of revision < 0x1ec");

                    if (self.revision.head >= 0x3d9) {
                        const functions = try allocator.dupe(MMTypes.FunctionDefinition, script.functions);
                        defer allocator.free(functions);

                        for (functions) |*function_definition| {
                            function_definition.arguments.end -= function_definition.arguments.begin;
                            function_definition.bytecode.end -= function_definition.bytecode.begin;
                            function_definition.line_numbers.end -= function_definition.line_numbers.begin;
                            function_definition.local_variables.end -= function_definition.local_variables.begin;
                        }

                        try self.writeArray(MMTypes.FunctionDefinition, functions, false, ScriptWriteType);
                    } else {
                        try self.writeArray(MMTypes.FunctionDefinition, script.functions, false, ScriptWriteType);
                    }

                    try self.writeArray(MMTypes.Argument, script.arguments, false, ScriptWriteType);
                    try self.writeArray(MMTypes.Bytecode, script.bytecode, false, ScriptWriteType);
                    try self.writeArray(u16, script.line_numbers, false, ScriptWriteType);
                    try self.writeArray(MMTypes.LocalVariable, script.local_variables, false, ScriptWriteType);

                    { //Write the AStringTable
                        const indices = try allocator.alloc(u32, script.a_string_table.strings.len);
                        defer allocator.free(indices);

                        var offset: u32 = 0;
                        for (script.a_string_table.strings, 0..) |string, i| {
                            indices[i] = offset;

                            offset += @intCast(string.len + 1);
                        }

                        try self.writeTable(indices);
                        try self.writeInt(u32, offset);
                        try self.stream.writer().writeAll(script.a_string_table.buf);
                    }

                    { //Write the WStringTable
                        const indices = try allocator.alloc(u32, script.w_string_table.strings.len);
                        defer allocator.free(indices);

                        var offset: u32 = 0;
                        for (script.w_string_table.strings, 0..) |string, i| {
                            indices[i] = offset;

                            offset += @intCast(string.len + 1);
                        }

                        try self.writeTable(indices);
                        try self.writeInt(u32, offset);
                        //Write each char by hand, this will byte-swap if needed
                        for (script.w_string_table.buf) |c| {
                            try self.writeInt(u16, c);
                        }
                    }

                    // In revision 0x30c a s64 constant table was added
                    if (self.revision.head >= 0x30c) {
                        try self.writeArray(
                            i64,
                            script.constant_table_s64 orelse &.{},
                            false,
                            undefined,
                        );
                    }

                    try self.writeArray(f32, script.constant_table_float, false, undefined);

                    if (self.revision.head >= 0x1ec) {
                        try self.writeArray(
                            u32,
                            script.depending_guids orelse &.{},
                            false,
                            undefined,
                        );
                    }
                },
            }
        }

        pub fn writeTable(self: *Self, indices: []const u32) !void {
            //If we dont have compressed vectors, just write the array directly
            if (!self.compression_flags.compressed_vectors) {
                return self.writeArray(u32, indices, false, undefined);
            }

            //If theres no indices, just write 0
            if (indices.len == 0)
                return self.writeInt(u32, 0);

            const needs_second_table = blk: {
                for (indices) |index|
                    if (index > 0xFF) break :blk true;

                break :blk false;
            };

            //Write the length
            try self.writeInt(u32, @intCast(indices.len));

            //If theres only one element and its a zero, just write a zero marking no tables
            if (indices.len == 1 and indices[0] == 0)
                return self.writeInt(u32, 0);

            //Write the amount of tables needed
            try self.writeInt(u32, @as(u32, @intFromBool(needs_second_table)) + 1);

            { //Write the first table
                var loop: u32 = 0;
                for (indices) |index| {
                    if (index - (loop * 0x100) > 0x100)
                        loop += 1;

                    try self.writeInt(u8, @intCast(index - (loop * 0x100)));
                }
            }

            if (needs_second_table) {
                var loop: u32 = 0;
                for (indices) |index| {
                    if (index - (loop * 0x100) > 0x100)
                        loop += 1;

                    try self.writeInt(u8, @intCast(loop));
                }
            }
        }

        pub fn writeArray(self: *Self, comptime T: type, arr: []const T, known_length: bool, comptime ScriptWriteType: type) !void {
            if (!known_length)
                try self.writeInt(u32, @intCast(arr.len));

            const TypeInfo = @typeInfo(T);

            for (arr) |item| {
                if (TypeInfo == .Int) {
                    try self.writeInt(T, item);
                } else if (TypeInfo == .Float) {
                    try self.writeFloat(item);
                } else switch (T) {
                    MMTypes.Bytecode => try self.writeInt(u64, @bitCast(item)),
                    MMTypes.TypeReference => {
                        try self.writeInt(u8, @intFromEnum(item.machine_type));
                        try self.writeInt(u8, @intFromEnum(item.fish_type));
                        try self.writeInt(u8, item.dimension_count);
                        try self.writeInt(u8, @intFromEnum(item.array_base_machine_type));
                        try self.writeResource(false, item.script);
                        try self.writeInt(u32, item.type_name);
                    },
                    MMTypes.FieldReference, MMTypes.FunctionReference => {
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.name);
                    },
                    MMTypes.FieldDefinition => {
                        try self.writeInt(ScriptWriteType, @intCast(@as(u32, @bitCast(item.modifiers))));
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.name);
                    },
                    MMTypes.PropertyDefinition => {
                        try self.writeInt(ScriptWriteType, @intCast(@as(u32, @bitCast(item.modifiers))));
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.name);
                        try self.writeInt(u32, item.get_function);
                        try self.writeInt(u32, item.set_function);
                    },
                    MMTypes.FunctionDefinition => {
                        try self.writeInt(ScriptWriteType, @intCast(@as(u32, @bitCast(item.modifiers))));
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.name);
                        try self.writeInt(ScriptWriteType, @intCast(item.arguments.begin));
                        try self.writeInt(ScriptWriteType, @intCast(item.arguments.end));
                        try self.writeInt(ScriptWriteType, @intCast(item.bytecode.begin));
                        try self.writeInt(ScriptWriteType, @intCast(item.bytecode.end));
                        try self.writeInt(ScriptWriteType, @intCast(item.line_numbers.begin));
                        try self.writeInt(ScriptWriteType, @intCast(item.line_numbers.end));
                        try self.writeInt(ScriptWriteType, @intCast(item.local_variables.begin));
                        try self.writeInt(ScriptWriteType, @intCast(item.local_variables.end));
                        try self.writeInt(u32, item.stack_size);
                    },
                    MMTypes.Argument => {
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.offset);
                    },
                    MMTypes.LocalVariable => {
                        try self.writeInt(ScriptWriteType, @intCast(@as(u32, @bitCast(item.modifiers))));
                        try self.writeInt(u32, item.type_reference);
                        try self.writeInt(u32, item.name);
                        try self.writeInt(u32, item.offset);
                    },
                    else => @compileError("Unknown type " ++ @typeName(T)),
                }
            }
        }

        pub fn writeResource(self: *Self, skip_flags: bool, resource_ident: ?MMTypes.ResourceIdentifier) !void {
            const hash: u8, const guid: u8 = if (self.revision.head <= 0x18b) .{ 2, 1 } else .{ 1, 2 };

            //write a default number for the flags
            if (self.revision.head > 0x22e and !skip_flags) {
                try self.writeInt(u32, 0);
            }

            if (resource_ident) |ident| {
                switch (ident) {
                    .guid => |asset_guid| {
                        try self.writeInt(u8, guid);
                        try self.writeInt(u32, asset_guid);
                    },
                    .hash => |asset_hash| {
                        try self.writeInt(u8, hash);
                        try self.stream.writer().writeAll(&asset_hash);
                    },
                }
            } else {
                try self.writeInt(u8, 0);
            }
        }

        pub fn readFileDB(self: *Self, child_allocator: std.mem.Allocator) !MMTypes.FileDB {
            var arena = std.heap.ArenaAllocator.init(child_allocator);
            errdefer arena.deinit();

            const allocator = arena.allocator();

            const header = try self.readInt(i32);

            const db_type: MMTypes.FileDB.Type = switch (header) {
                256 => .pre_lbp3,
                21496064 => .lbp3,
                936 => .vita,
                else => .unknown,
            };

            const count = try self.readInt(u32);

            var entries = std.ArrayList(MMTypes.FileDB.Entry).init(allocator);
            defer entries.deinit();

            var hash_lookup = MMTypes.FileDB.HashLookupMap.init(allocator);
            errdefer hash_lookup.deinit();
            var guid_lookup = MMTypes.FileDB.GuidLookupMap.init(allocator);
            errdefer guid_lookup.deinit();

            for (0..count) |_| {
                //Read the path, length is i16 on LBP3, i32 on LBP1/2/Vita
                const path = try allocator.alloc(u8, if (db_type == .lbp3) try self.readInt(u16) else try self.readInt(u32));
                _ = try self.readBytes(path);

                //Skip 4 bytes on non lbp3
                if (db_type != .lbp3)
                    _ = try self.readInt(i32);

                const timestamp = try self.readInt(i32);

                const size = try self.readInt(u32);

                const hash = try self.readSha1();
                const guid = try self.readInt(u32);

                const entry: MMTypes.FileDB.Entry = .{
                    .path = path,
                    .timestamp = timestamp,
                    .size = size,
                };

                try hash_lookup.put(hash, entry);
                try guid_lookup.put(guid, entry);
            }

            return .{
                .hash_lookup = hash_lookup,
                .guid_lookup = guid_lookup,
                .allocator = arena,
            };
        }
    };
}
