const std = @import("std");
const clap = @import("clap");
const builtin = @import("builtin");

const Frontend = @import("frontend.zig");

const no_command_error =
    \\No command specified.
    \\
    \\Commands:
    \\
    \\    disasm              Disassembles a script file into a human readable format.
    \\    compile             Compiles an A# source file into a script.
    \\    generate_library    Generates a library of stubs from a MAP and a extracted folder.
    \\
    \\
;

const Subcommand = enum {
    disasm,
    compile,
    generate_library,
};

pub fn main() !void {
    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer stdout_buffer.flush() catch unreachable;
    const stdout = stdout_buffer.writer();

    var stderr_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());
    defer stderr_buffer.flush() catch unreachable;
    const stderr = stderr_buffer.writer();

    // If we have runtime safety, use GPA
    var gpa = if (builtin.mode == .Debug)
        std.heap.GeneralPurposeAllocator(.{
            .stack_trace_frames = 20,
        }){}
    else {};

    // Only leak check GPA when runtime safety
    defer if (builtin.mode == .Debug and gpa.deinit() == .leak)
        @panic("MEMORY LEAK");

    // GPA on runtime safety
    const allocator = if (builtin.mode == .Debug)
        gpa.allocator()
    else
        std.heap.c_allocator;

    var arg_iter = try std.process.ArgIterator.initWithAllocator(allocator);
    defer arg_iter.deinit();

    _ = arg_iter.next(); //skip exe name

    const command_str = arg_iter.next() orelse {
        try stderr.print(no_command_error, .{});

        return error.MissingCommandArgument;
    };
    const command = std.meta.stringToEnum(Subcommand, command_str) orelse {
        try stderr.print("Unknown command specified\n\n", .{});

        return error.UnknownCommand;
    };

    switch (command) {
        .disasm => try Frontend.disasm(allocator, &arg_iter, stderr, stdout),
        .compile => try Frontend.compile(allocator, &arg_iter, stderr, stdout),
        .generate_library => try Frontend.generateLibrary(allocator, &arg_iter, stderr, stdout),
    }
}
