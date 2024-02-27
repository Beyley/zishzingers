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

    // std.debug.print("script class_name: {s}\n", .{script.class_name});
    // std.debug.print("{}\n", .{script});
}
