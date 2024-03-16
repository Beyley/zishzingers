const std = @import("std");

const Stream = @import("stream.zig");
const MMTypes = @import("MMTypes.zig");
const Debug = @import("debug.zig");
const Resource = @import("resource.zig");

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

    const file_contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(file_contents);

    var resource_stream = try Resource.readResource(file_contents, allocator);
    defer allocator.free(resource_stream.stream.buffer);

    const script = try resource_stream.readScript(allocator);
    defer script.deinit(allocator);

    try Debug.dumpScript(stdout, script);

    // const MMWriter = Writer.MMWriter(std.fs.File.Writer);

    // const writer = MMWriter{
    //     .compression_flags = .{
    //         .compressed_integers = true,
    //         .compressed_matrices = true,
    //         .compressed_vectors = true,
    //     },
    //     .writer = out.writer(),
    //     .revision = .{
    //         .head = 0x272,
    //         .branch_id = 0,
    //         .branch_revision = 0,
    //     },
    // };

    // try writer.writeScript(script);

    // const import_table_file = try std.fs.cwd().createFile("import.ffi", .{});
    // defer import_table_file.close();

    // const script_file = try std.fs.cwd().createFile("script.ff", .{});
    // defer script_file.close();

    // try Disassembler.serializeImportTable(import_table_file.writer(), script);
    // try Disassembler.serializeDisassembly(script_file.writer(), script, allocator);

}
