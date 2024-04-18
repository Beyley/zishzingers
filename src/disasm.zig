const std = @import("std");

const MMTypes = @import("MMTypes.zig");

pub fn disassembleScript(writer: anytype, script: MMTypes.Script) !void {
    try std.fmt.format(writer, " --- Class Name: {s} ---\n", .{script.class_name});
    try std.fmt.format(writer, " --- Up to date script: {?} ---\n", .{script.up_to_date_script});
    try std.fmt.format(writer, " --- Super class script: {?} ---\n", .{script.super_class_script});
    try std.fmt.format(writer, " --- Modifiers: {?d} ---\n", .{script.modifiers});
    try std.fmt.format(writer, "\n", .{});

    try std.fmt.format(writer, " --- Type References ---\n", .{});
    for (script.type_references) |*type_reference| {
        try std.fmt.format(writer, " - Type {*} \n", .{type_reference});
        if (type_reference.type_name != 0xFFFFFFFF) {
            try std.fmt.format(writer, "   - Type Name: {s}\n", .{script.a_string_table.strings[type_reference.type_name]});
        }
        try std.fmt.format(writer, "   - Machine Type: {s}\n", .{@tagName(type_reference.machine_type)});
        try std.fmt.format(writer, "   - Fish Type: {s}\n", .{@tagName(type_reference.fish_type)});
        try std.fmt.format(writer, "   - Dimension Count: {d}\n", .{type_reference.dimension_count});
        try std.fmt.format(writer, "   - Array Base Machine Type: {s}\n", .{@tagName(type_reference.array_base_machine_type)});
        try std.fmt.format(writer, "   - Script: {?}\n", .{type_reference.script});
    }
    try std.fmt.format(writer, "\n", .{});

    try std.fmt.format(writer, " --- Field References ---\n", .{});
    for (script.field_references) |*field_reference| {
        try std.fmt.format(writer, " - Field {*} \n", .{field_reference});
        if (field_reference.name != 0xFFFFFFFF) {
            try std.fmt.format(writer, "   - Name: {s}\n", .{script.a_string_table.strings[field_reference.name]});
        }
        try std.fmt.format(writer, "   - Type: {*}\n", .{&script.type_references[field_reference.type_reference]});
    }
    try std.fmt.format(writer, "\n", .{});

    try std.fmt.format(writer, " --- Functions ---\n", .{});
    for (script.functions) |*function| {
        try std.fmt.format(writer, " - Function {*} \n", .{function});
        try std.fmt.format(writer, "   - Modifiers: {}\n", .{function.modifiers});
        try std.fmt.format(writer, "   - Stack size: {d}\n", .{function.stack_size});
        if (function.name != 0xFFFFFFFF) {
            try std.fmt.format(writer, "   - Name: {s}\n", .{script.a_string_table.strings[function.name]});
        }
        try std.fmt.format(writer, "   - Type: {*}\n", .{&script.type_references[function.type_reference]});

        if (function.arguments.len() > 0) {
            try std.fmt.format(writer, "   - Arguments\n", .{});
            for (function.arguments.slice(script.arguments)) |argument| {
                try std.fmt.format(writer, "      - Offset: {d}\n", .{argument.offset});
                try std.fmt.format(writer, "      - Type: {*}\n", .{&script.type_references[argument.type_reference]});
            }
        }

        if (function.local_variables.len() > 0) {
            try std.fmt.format(writer, "   - Local Variables\n", .{});
            for (function.local_variables.slice(script.local_variables)) |local_variable| {
                try std.fmt.format(writer, "      - Name: {s}\n", .{script.a_string_table.strings[local_variable.name]});
                try std.fmt.format(writer, "      - Offset: {d}\n", .{local_variable.offset});
                try std.fmt.format(writer, "      - Modifiers: {d}\n", .{local_variable.modifiers});
                try std.fmt.format(writer, "      - Type: {*}\n", .{&script.type_references[local_variable.type_reference]});
            }
        }

        if (function.bytecode.len() > 0) {
            try std.fmt.format(writer, "   - Bytecode\n", .{});

            for (function.bytecode.slice(script.bytecode), function.line_numbers.slice(script.line_numbers), 0..) |bytecode, line_number, i| {
                _ = line_number; // autofix

                try std.fmt.format(writer, "      {d:0>4}: 0x{x:0>16} {s} ", .{
                    i,
                    @as(u64, @bitCast(bytecode)),
                    @tagName(bytecode.op),
                });
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
                            MMTypes.BranchClass => try std.fmt.format(writer, "r{d}, @{d}", .{ params.src_idx, params.branch_offset + @as(i32, @intCast(i)) }),
                            MMTypes.CastClass => try std.fmt.format(writer, "r{d}, t{d}, r{d}", .{ params.dst_idx, params.type_idx, params.src_idx }),
                            MMTypes.NewObjectClass => try std.fmt.format(writer, "r{d}, t{d}", .{ params.dst_idx, params.type_idx }),
                            MMTypes.ExternalInvokeClass => try std.fmt.format(writer, "r{d}, 0x{x}", .{ params.dst_idx, params.call_address }),
                            else => @compileError("Unhandled class type " ++ @typeName(ParamsType)),
                        }
                    },
                }

                if (bytecode.type != .void)
                    try std.fmt.format(writer, " ({s})\n", .{@tagName(bytecode.type)})
                else
                    try std.fmt.format(writer, "\n", .{});
            }
        }
    }

    try std.fmt.format(writer, "\n", .{});

    try std.fmt.format(writer, " --- Constant table s64 ---\n", .{});
    if (script.constant_table_s64) |constant_table_s64| {
        for (constant_table_s64, 0..) |constant, i| {
            try std.fmt.format(writer, "    {d}: {d}\n", .{ i, constant });
        }
    }

    try std.fmt.format(writer, " --- Constant table f32 ---\n", .{});
    for (script.constant_table_float, 0..) |constant, i| {
        try std.fmt.format(writer, "    {d}: {d}\n", .{ i, constant });
    }

    // try std.fmt.format(writer, "\n", .{});
    // try std.fmt.format(writer, "script class_name: {s}\n", .{script.class_name});
    // try std.fmt.format(writer, "{}\n", .{script});
}
