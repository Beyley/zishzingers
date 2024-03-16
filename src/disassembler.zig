const std = @import("std");

const Stream = @import("stream.zig");

const MMTypes = @import("MMTypes.zig");

fn serializeResourceIdentifier(writer: anytype, resource_ident: MMTypes.ResourceIdentifier) !void {
    switch (resource_ident) {
        .guid => |guid| try std.fmt.format(writer, "g{d}", .{guid}),
        .hash => |hash| try std.fmt.format(writer, "h{s}", .{hash}),
    }
}

fn serializeTypeReference(writer: anytype, type_reference: MMTypes.TypeReference) !void {
    try writer.writeAll("${");
    try std.fmt.format(writer, "{s}, ", .{(type_reference.type_name.string orelse @as(*const []const u8, &"")).*});
    if (type_reference.script) |script|
        try serializeResourceIdentifier(writer, script)
    else
        try writer.writeAll("null");
    try std.fmt.format(writer, ", {s}", .{@tagName(type_reference.machine_type)});
    try std.fmt.format(writer, ", {s}", .{@tagName(type_reference.fish_type)});
    try std.fmt.format(writer, ", {d}", .{type_reference.dimension_count});
    try std.fmt.format(writer, ", {s}", .{@tagName(type_reference.array_base_machine_type)});
    try writer.writeAll("}");
}

pub fn serializeImportTable(writer: anytype, script: MMTypes.Script) !void {
    try std.fmt.format(writer, "; Autogenerated import table from script {s}\n\n", .{script.class_name});

    for (script.function_references) |function_reference| {
        try std.fmt.format(writer, "func {s} ", .{function_reference.name.string.?.*});
        try serializeTypeReference(writer, function_reference.type_reference.type_reference.*);
        try writer.writeByte('\n');
    }

    try writer.writeByte('\n');

    // for (script.type_references) |type_reference| {
    //     try writer.writeAll("type ");
    //     try serializeTypeReference(writer, type_reference);
    //     try writer.writeByte('\n');
    // }

    // try writer.writeByte('\n');
}

fn serializeModifiers(writer: anytype, modifiers: MMTypes.Modifiers) !void {
    const ModifiersTypeInfo: std.builtin.Type = @typeInfo(MMTypes.Modifiers);
    inline for (ModifiersTypeInfo.Struct.fields) |field| {
        //skip
        if (comptime std.mem.eql(u8, field.name, "_unused"))
            continue;

        if (@field(modifiers, field.name)) {
            try writer.writeByte(' ');
            try writer.writeAll(field.name);
        }
    }
}

pub fn serializeDisassembly(writer: anytype, script: MMTypes.Script, allocator: std.mem.Allocator) !void {
    try std.fmt.format(writer, "; Autogenerated disassembly from script {s}\n\n", .{script.class_name});

    try std.fmt.format(writer, "class {s}\n", .{script.class_name});

    if (script.super_class_script) |super_class_script| {
        try writer.writeAll("super ");
        try serializeResourceIdentifier(writer, super_class_script);
        try writer.writeAll("\n");
    }

    if (script.modifiers) |modifiers|
        try std.fmt.format(writer, "modifiers {d}\n", .{modifiers});

    try writer.writeAll("\n");

    for (script.field_definitions) |field_definition| {
        try writer.writeAll("field");
        try serializeModifiers(writer, field_definition.modifiers);
        try std.fmt.format(writer, ", {s}, ", .{field_definition.name.string.?.*});
        try serializeTypeReference(writer, field_definition.type_reference.type_reference.*);

        try writer.writeByte('\n');
    }

    try writer.writeByte('\n');

    var branches = std.AutoHashMap(usize, void).init(allocator);
    defer branches.deinit();

    for (script.functions) |function| {
        defer branches.clearRetainingCapacity();
        for (function.bytecode.slice, 0..) |bytecode, i| {
            switch (bytecode.op) {
                inline else => |op| {
                    const params = @field(bytecode.params, @tagName(op));

                    const ParamsType = @TypeOf(params);

                    switch (ParamsType) {
                        MMTypes.BranchClass => try branches.put(@intCast(params.branch_offset + @as(i32, @intCast(i))), {}),
                        else => {},
                    }
                },
            }
        }

        const demangled_name = try Stream.demangleFunctionName(function.name.string.?.*, false, allocator);
        defer allocator.free(demangled_name);

        try writer.writeAll("func");
        try serializeModifiers(writer, function.modifiers);
        try std.fmt.format(writer, ", {s}", .{demangled_name});

        for (function.arguments.slice) |argument| {
            try std.fmt.format(writer, ", {d} ", .{argument.offset});
            try serializeTypeReference(writer, argument.type_reference.type_reference.*);
        }

        try writer.writeByte('\n');

        for (function.bytecode.slice, 0..) |bytecode, i| {
            if (branches.get(i) != null) {
                try std.fmt.format(writer, "      l branch_{d}:\n", .{i});
            }

            try std.fmt.format(writer, "        {s} ", .{@tagName(bytecode.op)});

            switch (bytecode.op) {
                inline else => |op| {
                    const params = @field(bytecode.params, @tagName(op));

                    const ParamsType = @TypeOf(params);

                    switch (ParamsType) {
                        MMTypes.NopClass => {},
                        MMTypes.LoadConstClass => try std.fmt.format(writer, "r{d}, cid{d}", .{ params.dst_idx, params.constant_idx }),
                        MMTypes.UnaryClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.dst_idx, params.src_idx }),
                        MMTypes.BinaryClass => try std.fmt.format(writer, "r{d}, r{d}, r{d}", .{ params.dst_idx, params.src_a_idx, params.src_b_idx }),
                        MMTypes.GetBuiltinMemberClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                        MMTypes.SetBuiltinMemberClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                        MMTypes.GetMemberClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                        MMTypes.SetMemberClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                        MMTypes.GetElementClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                        MMTypes.SetElementClass => try std.fmt.format(writer, "r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                        MMTypes.NewArrayClass => try std.fmt.format(writer, "r{d}, t{d}[r{d}]", .{ params.dst_idx, params.type_idx, params.size_idx }),
                        MMTypes.WriteClass => try std.fmt.format(writer, "r{d}", .{params.src_idx}),
                        MMTypes.ArgClass => try std.fmt.format(writer, "a{d}, r{d}", .{ params.arg_idx, params.src_idx }),
                        MMTypes.CallClass => try std.fmt.format(writer, "r{d}, c{d}", .{ params.dst_idx, params.call_idx }),
                        MMTypes.ReturnClass => try std.fmt.format(writer, "r{d}", .{params.src_idx}),
                        MMTypes.BranchClass => try std.fmt.format(writer, "r{d}, branch_{d}", .{ params.src_idx, params.branch_offset + @as(i32, @intCast(i)) }),
                        MMTypes.CastClass => try std.fmt.format(writer, "r{d}, t{d}, r{d}", .{ params.dst_idx, params.type_idx, params.src_idx }),
                        MMTypes.NewObjectClass => try std.fmt.format(writer, "r{d}, t{d}", .{ params.dst_idx, params.type_idx }),
                        else => @compileError("Unhandled class type " ++ @typeName(ParamsType)),
                    }
                },
            }

            try writer.writeByte('\n');
        }
    }

    try writer.writeByte('\n');
}
