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
                std.debug.print("      {d:0>4}: 0x{x:0>16} {s} (ln: {d})\n", .{
                    i,
                    bytecode,
                    @tagName(try std.meta.intToEnum(
                        Reader.InstructionType,
                        @as(u8, @truncate(bytecode)),
                    )),
                    line_number,
                });
            }
        }
    }
    std.debug.print("\n", .{});
    // std.debug.print("script class_name: {s}\n", .{script.class_name});
    // std.debug.print("{}\n", .{script});
}
