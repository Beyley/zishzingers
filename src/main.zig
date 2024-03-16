const std = @import("std");

const Stream = @import("stream.zig");
const MMTypes = @import("MMTypes.zig");
const Debug = @import("debug.zig");
const Resource = @import("resource.zig");
const ArrayListStreamSource = @import("ArrayListStreamSource.zig");

const Disassembler = @import("disassembler.zig");

pub fn main() !void {
    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer stdout_buffer.flush() catch unreachable;
    const stdout = stdout_buffer.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("MEMORY LEAK");

    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const out = try std.fs.cwd().createFile("out.ff", .{});
    defer out.close();

    const file_contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_contents);

    var resource = try Resource.readResource(file_contents, allocator);
    defer resource.deinit();

    const script = try resource.stream.readScript(allocator);
    defer script.deinit(allocator);

    var stream: Stream.MMStream(ArrayListStreamSource) = .{
        .compression_flags = resource.stream.compression_flags,
        .revision = resource.stream.revision,
        .stream = .{
            .array_list = std.ArrayList(u8).init(allocator),
            .pos = 0,
        },
    };
    defer stream.stream.array_list.deinit();

    try stream.writeScript(script, allocator);

    var out_stream: std.io.StreamSource = .{ .file = out };

    try Resource.writeResource(
        resource.type,
        resource.stream.compression_flags,
        resource.dependencies orelse &.{},
        resource.stream.revision,
        &out_stream,
        stream.stream.array_list.items,
        allocator,
    );

    try Debug.dumpScript(stdout, script);
}
