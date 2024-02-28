const std = @import("std");

const Reader = @import("reader.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("MEMORY LEAK");

    const allocator = gpa.allocator();

    // const file = try std.fs.openFileAbsolute("/home/beyley/sackboyanim.ff.dec", .{});
    const file = try std.fs.openFileAbsolute("/home/beyley/world.ff.dec", .{});
    // const file = try std.fs.openFileAbsolute("/home/beyley/Documents/inventorycache.ff.dec", .{});
    defer file.close();

    const MMReader = Reader.MMReader(std.fs.File.Reader);

    const reader = MMReader{
        .compression_flags = .{
            .compressed_integers = true,
            .compressed_matrices = true,
            .compressed_vectors = true,
        },
        .reader = file.reader(),
        .revision = .{
            .head = 0x272,
            .branch_id = 0,
            .branch_revision = 0,
        },
    };

    const script = try reader.readScript(allocator);
    defer script.deinit(allocator);

    std.debug.print(" --- Class Name: {s} ---\n", .{script.class_name});
    std.debug.print(" --- Up to date script: {?} ---\n", .{script.up_to_date_script});
    std.debug.print(" --- Super class script: {?} ---\n", .{script.super_class_script});
    std.debug.print(" --- Modifiers: {?d} ---\n", .{script.modifiers});
    std.debug.print("\n", .{});

    // std.debug.print(" --- Type References ---\n", .{});
    // for (script.type_references) |*type_reference| {
    //     std.debug.print(" - Type {*} \n", .{type_reference});
    //     if (type_reference.type_name.string) |type_name| {
    //         std.debug.print("   - Type Name: {s}\n", .{type_name.*});
    //     }
    //     std.debug.print("   - Machine Type: {s}\n", .{@tagName(type_reference.machine_type)});
    //     std.debug.print("   - Fish Type: {s}\n", .{@tagName(type_reference.fish_type)});
    //     std.debug.print("   - Dimension Count: {d}\n", .{type_reference.dimension_count});
    //     std.debug.print("   - Array Base Machine Type: {d}\n", .{type_reference.array_base_machine_type});
    //     std.debug.print("   - Script: {?}\n", .{type_reference.script});
    // }
    // std.debug.print("\n", .{});

    // std.debug.print(" --- Field References ---\n", .{});
    // for (script.field_references) |*field_reference| {
    //     std.debug.print(" - Field {*} \n", .{field_reference});
    //     if (field_reference.name.string) |type_name| {
    //         std.debug.print("   - Name: {s}\n", .{type_name.*});
    //     }
    //     std.debug.print("   - Type: {*}\n", .{field_reference.type_reference.type_reference});
    // }
    // std.debug.print("\n", .{});

    std.debug.print(" --- Functions ---\n", .{});
    for (script.functions) |*function| {
        std.debug.print(" - Function {*} \n", .{function});
        std.debug.print("   - Modifiers: {d}\n", .{function.modifiers});
        std.debug.print("   - Stack size: {d}\n", .{function.stack_size});
        if (function.name.string) |type_name| {
            std.debug.print("   - Name: {s}\n", .{type_name.*});
        }
        std.debug.print("   - Type: {*}\n", .{function.type_reference.type_reference});

        if (function.arguments.slice.len > 0) {
            std.debug.print("   - Arguments\n", .{});
            for (function.arguments.slice) |argument| {
                std.debug.print("      - Offset: {d}\n", .{argument.offset});
                std.debug.print("      - Type: {*}\n", .{argument.type_reference.type_reference});
            }
        }

        if (function.bytecode.slice.len > 0) {
            std.debug.print("   - Bytecode\n", .{});

            for (function.bytecode.slice, function.line_numbers.slice, 0..) |bytecode, line_number, i| {
                _ = line_number; // autofix

                std.debug.print("      {d:0>4}: 0x{x:0>16} {s} ", .{
                    i,
                    @as(u64, @bitCast(bytecode)),
                    @tagName(bytecode.op),
                });
                switch (bytecode.op) {
                    inline else => |op| {
                        const params = @field(bytecode.params, @tagName(op));

                        const ParamsType = @TypeOf(params);

                        switch (ParamsType) {
                            Reader.NopClass => {},
                            Reader.LoadConstClass => std.debug.print("r{d}, cid{d}", .{ params.dst_idx, params.constant_idx }),
                            Reader.UnaryClass => std.debug.print("r{d}, r{d}", .{ params.dst_idx, params.src_idx }),
                            Reader.BinaryClass => std.debug.print("r{d}, r{d}, r{d}", .{ params.dst_idx, params.src_a_idx, params.src_b_idx }),
                            Reader.GetBuiltinMemberClass => std.debug.print("r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                            Reader.SetBuiltinMemberClass => std.debug.print("r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                            Reader.GetMemberClass => std.debug.print("r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                            Reader.SetMemberClass => std.debug.print("r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                            Reader.GetElementClass => std.debug.print("r{d}, r{d}", .{ params.dst_idx, params.base_idx }),
                            Reader.SetElementClass => std.debug.print("r{d}, r{d}", .{ params.src_idx, params.base_idx }),
                            Reader.NewArrayClass => std.debug.print("r{d}, t{d}[r{d}]", .{ params.dst_idx, params.type_idx, params.size_idx }),
                            Reader.WriteClass => std.debug.print("r{d}", .{params.src_idx}),
                            Reader.ArgClass => std.debug.print("a{d}, r{d}", .{ params.arg_idx, params.src_idx }),
                            Reader.CallClass => std.debug.print("r{d}, c{d}", .{ params.dst_idx, params.call_idx }),
                            Reader.ReturnClass => std.debug.print("r{d}", .{params.src_idx}),
                            Reader.BranchClass => std.debug.print("r{d}, @{d}", .{ params.src_idx, params.branch_offset + @as(i32, @intCast(i)) }),
                            Reader.CastClass => std.debug.print("r{d}, t{d}, r{d}", .{ params.dst_idx, params.type_idx, params.src_idx }),
                            Reader.NewObjectClass => std.debug.print("r{d}, t{d}", .{ params.dst_idx, params.type_idx }),
                            else => @compileError("Unhandled class type " ++ @typeName(ParamsType)),
                        }
                    },
                }

                if (bytecode.type != .void)
                    std.debug.print(" ({s})\n", .{@tagName(bytecode.type)})
                else
                    std.debug.print("\n", .{});
            }
        }
    }
    std.debug.print("\n", .{});
    // std.debug.print("script class_name: {s}\n", .{script.class_name});
    // std.debug.print("{}\n", .{script});
}
