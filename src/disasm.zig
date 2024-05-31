const std = @import("std");

const MMTypes = @import("MMTypes.zig");

pub fn disassembleScript(writer: anytype, allocator: std.mem.Allocator, script: MMTypes.Script) !void {
    try writer.print(" --- Class Name: {s} ---\n", .{script.class_name});
    try writer.print(" --- Up to date script: {?} ---\n", .{script.up_to_date_script});
    try writer.print(" --- Super class script: {?} ---\n", .{script.super_class_script});
    try writer.print(" --- Modifiers: {?} ---\n", .{script.modifiers});
    try writer.print(" --- Depending guids: {?any} ---\n", .{script.depending_guids});
    try writer.print("\n", .{});

    try writer.print(" --- Type References ---\n", .{});
    for (script.type_references) |*type_reference| {
        try writer.print(" - Type {*} \n", .{type_reference});
        if (type_reference.type_name != 0xFFFFFFFF) {
            try writer.print("   - Type Name: {s}\n", .{script.a_string_table.strings[type_reference.type_name]});
        }
        try writer.print("   - Machine Type: {s}\n", .{@tagName(type_reference.machine_type)});
        try writer.print("   - Fish Type: {s}\n", .{@tagName(type_reference.fish_type)});
        try writer.print("   - Dimension Count: {d}\n", .{type_reference.dimension_count});
        try writer.print("   - Array Base Machine Type: {s}\n", .{@tagName(type_reference.array_base_machine_type)});
        try writer.print("   - Script: {?}\n", .{type_reference.script});
    }
    try writer.print("\n", .{});

    try writer.print(" --- Field References ---\n", .{});
    for (script.field_references) |*field_reference| {
        try writer.print(" - Field {*} \n", .{field_reference});
        if (field_reference.name != 0xFFFFFFFF) {
            try writer.print("   - Name: {s}\n", .{script.a_string_table.strings[field_reference.name]});
        }
        try writer.print("   - Type: {*}\n", .{&script.type_references[field_reference.type_reference]});
    }
    try writer.print("\n", .{});

    try writer.print(" --- Field Definitions ---\n", .{});
    for (script.field_definitions) |*field_definition| {
        try writer.print(" - Field {*} \n", .{field_definition});
        if (field_definition.name != 0xFFFFFFFF) {
            try writer.print("   - Name: {s}\n", .{script.a_string_table.strings[field_definition.name]});
        }
        try writer.print("   - Type: {*}\n", .{&script.type_references[field_definition.type_reference]});
        try writer.print("   - Modifiers: {}\n", .{field_definition.modifiers});
    }
    try writer.print("\n", .{});

    try writer.print(" --- Function References ---\n", .{});
    for (script.function_references) |*function_reference| {
        try writer.print(" - Function {*} \n", .{function_reference});
        if (function_reference.name != 0xFFFFFFFF) {
            try writer.print("   - Name: {s}\n", .{script.a_string_table.strings[function_reference.name]});
        }
        try writer.print("   - Type: {*}\n", .{&script.type_references[function_reference.type_reference]});
    }
    try writer.print("\n", .{});

    try writer.print(" --- Functions ---\n", .{});
    for (script.functions) |*function| {
        try writer.print(" - Function {*} \n", .{function});
        try writer.print("   - Modifiers: {}\n", .{function.modifiers});
        try writer.print("   - Stack size: {d}\n", .{function.stack_size});
        if (function.name != 0xFFFFFFFF) {
            try writer.print("   - Name: {s}\n", .{script.a_string_table.strings[function.name]});
        }
        try writer.print("   - Type: {*}\n", .{&script.type_references[function.type_reference]});

        if (function.arguments.len() > 0) {
            try writer.print("   - Arguments\n", .{});
            for (function.arguments.slice(script.arguments)) |argument| {
                try writer.print("      - Offset: {d}\n", .{argument.offset});
                try writer.print("      - Type: {*}\n", .{&script.type_references[argument.type_reference]});
            }
        }

        if (function.local_variables.len() > 0) {
            try writer.print("   - Local Variables\n", .{});
            for (function.local_variables.slice(script.local_variables)) |local_variable| {
                try writer.print("      - Name: {s}\n", .{script.a_string_table.strings[local_variable.name]});
                try writer.print("      - Offset: {d}\n", .{local_variable.offset});
                try writer.print("      - Modifiers: {d}\n", .{local_variable.modifiers});
                try writer.print("      - Type: {s}\n", .{@tagName(script.type_references[local_variable.type_reference].machine_type)});
            }
        }

        if (function.bytecode.len() > 0) {
            try writer.print("   - Bytecode\n", .{});

            try disassembleBytecode(writer, allocator, script, function, 0, false);
        }
    }

    try writer.print("\n", .{});

    try writer.print(" --- Constant table s64 ---\n", .{});
    if (script.constant_table_s64) |constant_table_s64| {
        for (constant_table_s64, 0..) |constant, i| {
            try writer.print("    {d}: {d}\n", .{ i, constant });
        }
    }

    try writer.print(" --- Constant table f32 ---\n", .{});
    for (script.constant_table_float, 0..) |constant, i| {
        try writer.print("    {d}: {d}\n", .{ i, constant });
    }

    // try writer.print("\n", .{});
    // try writer.print("script class_name: {s}\n", .{script.class_name});
    // try writer.print("{}\n", .{script});
}

pub fn disassembleBytecode(
    writer: anytype,
    allocator: std.mem.Allocator,
    script: MMTypes.Script,
    function: *const MMTypes.FunctionDefinition,
    indentation: usize,
    inline_asm: bool,
) !void {
    var branch_targets = std.AutoHashMap(i32, void).init(allocator);
    defer branch_targets.deinit();

    if (inline_asm) {
        for (function.bytecode.slice(script.bytecode), 0..) |bytecode, i| {
            switch (bytecode.op) {
                inline else => |op| {
                    const params = @field(bytecode.params, @tagName(op));

                    const ParamsType = @TypeOf(params);

                    if (ParamsType == MMTypes.BranchClass) {
                        try branch_targets.put(params.branch_offset + @as(i32, @intCast(i)), {});
                    }
                },
            }
        }
    }

    for (function.bytecode.slice(script.bytecode), 0..) |bytecode, i| {
        if (inline_asm) {
            if (branch_targets.get(@intCast(i)) != null) {
                for (0..indentation) |_| {
                    try writer.writeAll("    ");
                }
                try writer.print("branch_{d}:\n", .{i});
            }

            for (0..indentation) |_| {
                try writer.writeAll("    ");
            }

            try writer.print("    {s} ", .{@tagName(bytecode.op)});
        } else try writer.print("      {d:0>4}: 0x{x:0>16} {s} ", .{
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
                    MMTypes.LoadConstClass => {
                        switch (op) {
                            .LCsw => {
                                try writer.print("r{d}, '", .{
                                    params.dst_idx,
                                });

                                var iter = std.unicode.Utf16LeIterator.init(script.w_string_table.strings[params.constant_idx]);
                                while (try iter.nextCodepoint()) |codepoint| {
                                    switch (codepoint) {
                                        '\r' => try writer.writeAll("\\r"),
                                        '\'' => try writer.writeAll("\\'"),
                                        '\n' => try writer.writeAll("\\n"),
                                        else => try writer.print("{u}", .{codepoint}),
                                    }
                                }

                                try writer.writeAll("'");
                            },
                            .LCsa => {
                                try writer.print("r{d}, '", .{
                                    params.dst_idx,
                                });

                                var iter = std.unicode.Utf8Iterator{
                                    .bytes = script.a_string_table.strings[params.constant_idx],
                                    .i = 0,
                                };

                                while (iter.nextCodepoint()) |codepoint| {
                                    switch (codepoint) {
                                        '\r' => try writer.writeAll("\\r"),
                                        '\n' => try writer.writeAll("\\n"),
                                        '\'' => try writer.writeAll("\\'"),
                                        else => try writer.print("{u}", .{codepoint}),
                                    }
                                }

                                try writer.writeAll("'");
                            },
                            .LCi => try writer.print("r{d}, {d}", .{ params.dst_idx, @as(i32, @bitCast(params.constant_idx)) }),
                            .LCb => try writer.print("r{d}, {}", .{ params.dst_idx, (params.constant_idx >> 31) > 0 }),
                            .LCf => try writer.print("r{d}, {d}", .{ params.dst_idx, @as(f32, @bitCast(params.constant_idx)) }),
                            .LC_NULLo, .LC_NULLsp => try writer.print("r{d}", .{params.dst_idx}),
                            else => try writer.print("r{d}, cid{d}", .{
                                params.dst_idx,
                                params.constant_idx,
                            }),
                            .LCv4 => try writer.print("r{d}, {d}, {d}, {d}, {d}", .{
                                params.dst_idx,
                                script.constant_table_float[params.constant_idx],
                                script.constant_table_float[params.constant_idx + 1],
                                script.constant_table_float[params.constant_idx + 2],
                                script.constant_table_float[params.constant_idx + 3],
                            }),
                        }
                    },
                    MMTypes.UnaryClass => try writer.print("r{d}, r{d}", .{ params.dst_idx, params.src_idx }),
                    MMTypes.BinaryClass => try writer.print("r{d}, r{d}, r{d}", .{
                        params.dst_idx,
                        params.src_a_idx,
                        params.src_b_idx,
                    }),
                    MMTypes.GetBuiltinMemberClass => try writer.print("r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                    MMTypes.SetBuiltinMemberClass => try writer.print("r{d}, r{d}", .{ params.base_idx, params.src_idx }),
                    MMTypes.GetMemberClass => {
                        if (op == .GET_RP_MEMBER) {
                            try writer.print("r{d}, r{d}, {s}.{s}", .{
                                params.dst_idx,
                                params.base_idx,
                                script.a_string_table.strings[script.type_references[script.field_references[params.field_ref].type_reference].type_name],
                                script.a_string_table.strings[script.field_references[params.field_ref].name],
                            });
                        } else if (script.type_references[script.field_references[params.field_ref].type_reference].type_name == 0xFFFFFFFF) {
                            try writer.print("r{d}, r{d}, g{d}.{s}", .{
                                params.dst_idx,
                                params.base_idx,
                                script.type_references[script.field_references[params.field_ref].type_reference].script.?.guid,
                                script.a_string_table.strings[script.field_references[params.field_ref].name],
                            });
                        } else try writer.print("r{d}, r{d}, {s}.{s}", .{
                            params.dst_idx,
                            params.base_idx,
                            script.a_string_table.strings[script.type_references[script.field_references[params.field_ref].type_reference].type_name],
                            script.a_string_table.strings[script.field_references[params.field_ref].name],
                        });
                    },
                    MMTypes.SetMemberClass => {
                        if (op == .SET_RP_MEMBER) {
                            try writer.print("r{d}, {s}.{s}, r{d}", .{
                                params.base_idx,
                                script.a_string_table.strings[script.type_references[script.field_references[params.field_ref].type_reference].type_name],
                                script.a_string_table.strings[script.field_references[params.field_ref].name],
                                params.src_idx,
                            });
                        } else if (script.type_references[script.field_references[params.field_ref].type_reference].type_name == 0xFFFFFFFF) {
                            try writer.print("r{d}, g{d}.{s}, r{d}", .{
                                params.base_idx,
                                script.type_references[script.field_references[params.field_ref].type_reference].script.?.guid,
                                script.a_string_table.strings[script.field_references[params.field_ref].name],
                                params.src_idx,
                            });
                        } else try writer.print("r{d}, {s}.{s}, r{d}", .{
                            params.base_idx,
                            script.a_string_table.strings[script.type_references[script.field_references[params.field_ref].type_reference].type_name],
                            script.a_string_table.strings[script.field_references[params.field_ref].name],
                            params.src_idx,
                        });
                    },
                    MMTypes.GetElementClass => try writer.print("r{d}, r{d}, r{d}", .{ params.dst_idx, params.base_idx, params.src_or_index_idx }),
                    MMTypes.SetElementClass => try writer.print("r{d}, r{d}, r{d}", .{ params.base_idx, params.src_idx, params.index_idx }),
                    MMTypes.NewArrayClass => {
                        const type_reference = script.type_references[params.type_idx];
                        if (type_reference.machine_type != .safe_ptr and type_reference.machine_type != .object_ref)
                            try writer.print("r{d}, {s}, r{d}", .{
                                params.dst_idx,
                                @tagName(type_reference.machine_type),
                                params.size_idx,
                            })
                        else if (type_reference.type_name == 0xFFFFFFFF) try writer.print("r{d}, g{d}, r{d}", .{
                            params.dst_idx,
                            type_reference.script.?.guid,
                            params.size_idx,
                        }) else try writer.print("r{d}, {s}, r{d}", .{
                            params.dst_idx,
                            script.a_string_table.strings[type_reference.type_name],
                            params.size_idx,
                        });
                    },
                    MMTypes.WriteClass => try writer.print("r{d}", .{params.src_idx}),
                    MMTypes.ArgClass => try writer.print("a{d}, r{d}", .{ params.arg_idx, params.src_idx }),
                    MMTypes.CallClass => {
                        if (params.call_idx >= script.function_references.len)
                            try writer.print("r{d}, ??? {d}", .{ params.dst_idx, params.call_idx })
                        else if (script.type_references[script.function_references[params.call_idx].type_reference].type_name == 0xFFFFFFFF)
                            try writer.print("r{d}, g{d}.{s}", .{
                                params.dst_idx,
                                script.type_references[script.function_references[params.call_idx].type_reference].script.?.guid,
                                script.a_string_table.strings[script.function_references[params.call_idx].name],
                            })
                        else
                            try writer.print("r{d}, {s}.{s}", .{
                                params.dst_idx,
                                script.a_string_table.strings[script.type_references[script.function_references[params.call_idx].type_reference].type_name],
                                script.a_string_table.strings[script.function_references[params.call_idx].name],
                            });
                    },
                    MMTypes.ReturnClass => try writer.print("r{d}", .{params.src_idx}),
                    MMTypes.BranchClass => {
                        if (op == .B) {
                            if (inline_asm) try writer.print("branch_{d}", .{
                                params.branch_offset + @as(i32, @intCast(i)),
                            }) else try writer.print("@{d}", .{
                                params.branch_offset + @as(i32, @intCast(i)),
                            });
                        } else {
                            if (inline_asm) try writer.print("branch_{d}, r{d}", .{
                                params.branch_offset + @as(i32, @intCast(i)),
                                params.src_idx,
                            }) else try writer.print("r{d}, @{d}", .{
                                params.src_idx,
                                params.branch_offset + @as(i32, @intCast(i)),
                            });
                        }
                    },
                    MMTypes.CastClass => {
                        if (script.type_references[params.type_idx].type_name == 0xFFFFFFFF)
                            try writer.print("r{d}, g{d}, r{d}", .{
                                params.dst_idx,
                                script.type_references[params.type_idx].script.?.guid,
                                params.src_idx,
                            })
                        else
                            try writer.print("r{d}, {s}, r{d}", .{
                                params.dst_idx,
                                script.a_string_table.strings[script.type_references[params.type_idx].type_name],
                                params.src_idx,
                            });
                    },
                    MMTypes.NewObjectClass => if (script.type_references[params.type_idx].type_name == 0xFFFFFFFF)
                        try writer.print("r{d}, g{d}", .{
                            params.dst_idx,
                            script.type_references[params.type_idx].script.?.guid,
                        })
                    else
                        try writer.print("r{d}, {s}", .{
                            params.dst_idx,
                            script.a_string_table.strings[script.type_references[params.type_idx].type_name],
                        }),
                    MMTypes.ExternalInvokeClass => try writer.print("r{d}, 0x{x}, {d}", .{
                        params.dst_idx,
                        params.call_address,
                        params.toc_index,
                    }),
                    else => @compileError("Unhandled class type " ++ @typeName(ParamsType)),
                }
            },
        }

        if (bytecode.type != .void)
            try writer.print(" ({s})\n", .{@tagName(bytecode.type)})
        else
            try writer.print("\n", .{});
    }
}
