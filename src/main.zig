const std = @import("std");
const clap = @import("clap");

const Stream = @import("stream.zig");
const MMTypes = @import("MMTypes.zig");
const Debug = @import("debug.zig");
const Resource = @import("resource.zig");
const ArrayListStreamSource = @import("ArrayListStreamSource.zig");
const Parser = @import("parser.zig");

const no_command_error =
    \\No command specified.
    \\
    \\Commands:
    \\
    \\    disasm      Disassembles a script file into a human readable format.
    \\    compile     Compiles an A# source file into a script.
    \\    dump_map    Dumps the contents of an LBP map file.
    \\
    \\
;

const Subcommand = enum {
    disasm,
    compile,
    dump_map,
};

pub fn main() !void {
    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer stdout_buffer.flush() catch unreachable;
    const stdout = stdout_buffer.writer();

    var stderr_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());
    defer stderr_buffer.flush() catch unreachable;
    const stderr = stderr_buffer.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("MEMORY LEAK");

    const allocator = gpa.allocator();

    var arg_iter = try std.process.ArgIterator.initWithAllocator(allocator);
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
        .disasm => {
            const params = comptime clap.parseParamsComptime(
                \\-h, --help                       Display this help and exit.
                \\-n, --no-container               Specifies that the file is a raw script file, not inside the resource container. This flag requires -r.
                \\-r, --revision         <u16>     If loading a raw script file, load with this revision.
                \\-c, --compression-flag <str>...  Specifies a compression flag, possible values are "integers", "vectors", or "matrices".
                \\<str>                            The file to disassemble
                \\
            );

            var diag = clap.Diagnostic{};
            var res = clap.parseEx(clap.Help, &params, clap.parsers.default, &arg_iter, .{
                .diagnostic = &diag,
                .allocator = gpa.allocator(),
            }) catch |err| {
                // Report useful error and exit
                diag.report(std.io.getStdErr().writer(), err) catch {};
                return err;
            };
            defer res.deinit();

            //If no arguments are passed or the user requested the help menu, display the help menu
            if (res.args.help != 0 or res.positionals.len == 0) {
                try clap.help(stderr, clap.Help, &params, .{});

                return;
            }

            if (res.args.@"no-container" != 0) {
                // Revision is required when theres no container
                if (res.args.revision == null) {
                    try clap.help(stderr, clap.Help, &params, .{});
                    return;
                }

                var compression_flags = MMTypes.CompressionFlags{
                    .compressed_integers = false,
                    .compressed_matrices = false,
                    .compressed_vectors = false,
                };

                //Enable any of the compression flags if specified
                for (res.args.@"compression-flag") |flag| {
                    if (std.mem.eql(u8, flag, "integers"))
                        compression_flags.compressed_integers = true
                    else if (std.mem.eql(u8, flag, "matrices"))
                        compression_flags.compressed_matrices = true
                    else if (std.mem.eql(u8, flag, "vectors"))
                        compression_flags.compressed_vectors = true;
                }

                for (res.positionals) |path| {
                    const file = try std.fs.cwd().openFile(path, .{});
                    defer file.close();

                    var resource_stream = Stream.MMStream(std.io.StreamSource){
                        .stream = std.io.StreamSource{ .file = file },
                        .compression_flags = compression_flags,
                        .revision = .{
                            .head = res.args.revision.?,
                            .branch_revision = 0,
                            .branch_id = 0,
                        },
                    };

                    const script = try resource_stream.readScript(allocator);
                    defer script.deinit(allocator);

                    try Debug.disassembleScript(stdout, script);
                }
            } else {
                //Iterate over all paths and disassemble them
                for (res.positionals) |path| {
                    const file = try std.fs.cwd().openFile(path, .{});
                    defer file.close();

                    const file_contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
                    defer allocator.free(file_contents);

                    var resource = try Resource.readResource(file_contents, allocator);
                    defer resource.deinit();

                    const script = try resource.stream.readScript(allocator);
                    defer script.deinit(allocator);

                    try Debug.disassembleScript(stdout, script);
                }
            }
        },
        .compile => {
            const params = comptime clap.parseParamsComptime(
                \\-h, --help                       Display this help and exit.
                \\-o, --out-file <str>             The output path for the compilation, defaults to "inputname.ff"
                \\<str>                            The source file
                \\
            );

            var diag = clap.Diagnostic{};
            var res = clap.parseEx(clap.Help, &params, clap.parsers.default, &arg_iter, .{
                .diagnostic = &diag,
                .allocator = gpa.allocator(),
            }) catch |err| {
                // Report useful error and exit
                diag.report(std.io.getStdErr().writer(), err) catch {};
                return err;
            };
            defer res.deinit();

            //If no arguments are passed or the user requested the help menu, display the help menu
            if (res.args.help != 0 or res.positionals.len == 0) {
                try clap.help(stderr, clap.Help, &params, .{});

                return;
            }

            const source_code: []const u8 = try std.fs.cwd().readFileAlloc(allocator, res.positionals[0], std.math.maxInt(usize));
            defer allocator.free(source_code);

            //Get all the lexemes into a single big array
            const lexemes = blk: {
                var lexemes = std.ArrayList([]const u8).init(allocator);
                defer lexemes.deinit();

                var lexizer = Parser.Lexemeizer{ .source = source_code };

                while (try lexizer.next()) |lexeme| {
                    try lexemes.append(lexeme);
                }

                break :blk try lexemes.toOwnedSlice();
            };
            defer allocator.free(lexemes);

            const tokens = try Parser.parse(allocator, lexemes);
            defer tokens.deinit();

            for (tokens.root_elements.items) |item| {
                switch (item) {
                    .class => |class| {
                        std.debug.print("{}\n", .{class.*});

                        for (class.fields) |field| {
                            std.debug.print("field {}\n", .{field.*});
                        }

                        for (class.functions) |function| {
                            std.debug.print("function {}\n", .{function.*});
                            std.debug.print("function body {}\n", .{function.body});
                        }
                    },
                    inline else => |ptr| {
                        std.debug.print("{}\n", .{ptr.*});
                    },
                }
            }
        },
        .dump_map => {
            const params = comptime clap.parseParamsComptime(
                \\-h, --help                       Display this help and exit.
                \\<str>                            The MAP file
                \\
            );

            var diag = clap.Diagnostic{};
            var res = clap.parseEx(clap.Help, &params, clap.parsers.default, &arg_iter, .{
                .diagnostic = &diag,
                .allocator = gpa.allocator(),
            }) catch |err| {
                // Report useful error and exit
                diag.report(std.io.getStdErr().writer(), err) catch {};
                return err;
            };
            defer res.deinit();

            //If no arguments are passed or the user requested the help menu, display the help menu
            if (res.args.help != 0 or res.positionals.len == 0) {
                try clap.help(stderr, clap.Help, &params, .{});

                return;
            }

            const map_file = try std.fs.cwd().openFile(res.positionals[0], .{});
            defer map_file.close();

            const MMStream = Stream.MMStream(std.io.StreamSource);

            var stream = MMStream{
                .stream = std.io.StreamSource{ .file = map_file },
                // nonsense compression files, since its not important for MAP files
                .compression_flags = .{
                    .compressed_integers = false,
                    .compressed_matrices = false,
                    .compressed_vectors = false,
                },
                // nonsense revision, since its not important for MAP files
                .revision = .{
                    .branch_id = 0,
                    .branch_revision = 0,
                    .head = 0,
                },
            };

            const file_db = try stream.readFileDB(allocator);
            defer file_db.deinit();

            var iter = file_db.guid_lookup.iterator();
            while (iter.next()) |entry| {
                try stdout.print("{d}: {s}\n", .{ entry.key_ptr.*, entry.value_ptr.path });
            }
        },
    }
}
