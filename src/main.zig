const std = @import("std");
const clap = @import("clap");

const Stream = @import("stream.zig");
const MMTypes = @import("MMTypes.zig");
const Debug = @import("debug.zig");
const Resource = @import("resource.zig");
const ArrayListStreamSource = @import("ArrayListStreamSource.zig");
const Parser = @import("parser.zig");
const Stubinator = @import("stubinator.zig");
const Resolvinator = @import("resolvinator.zig");

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
                \\-l, --library <str>...           A library to import, with the syntax `name:path`
                \\-i, --identifier <u32>           The GUID of the script being compiled, 
                \\                                 this overrides the GUID specified in the file.
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

            const script_identifier: ?MMTypes.ResourceIdentifier = if (res.args.identifier) |identifier|
                .{ .guid = identifier }
            else
                null;

            var defined_libraries = Resolvinator.Libraries.init(allocator);
            defer {
                var iter = defined_libraries.valueIterator();
                while (iter.next()) |dir|
                    dir.close();

                defined_libraries.deinit();
            }

            for (res.args.library) |library| {
                var iter = std.mem.split(u8, library, ":");
                const name = iter.next() orelse @panic("no library name specified");
                const path = iter.next() orelse @panic("no library path specified");

                try defined_libraries.putNoClobber(name, try std.fs.cwd().openDir(path, .{}));
            }

            for (res.positionals) |source_file| {
                std.debug.print("{s}\n", .{source_file});
                const source_code: []const u8 = try std.fs.cwd().readFileAlloc(allocator, source_file, std.math.maxInt(usize));
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

                var arena = std.heap.ArenaAllocator.init(allocator);
                defer arena.deinit();

                const ast_allocator = arena.allocator();

                const ast = try Parser.parse(ast_allocator, lexemes);

                var a_string_table = Resolvinator.AStringTable.init(allocator);
                defer a_string_table.deinit();

                try Resolvinator.resolve(
                    ast,
                    defined_libraries,
                    &a_string_table,
                    script_identifier,
                );

                const string_table_keys = a_string_table.keys();
                for (string_table_keys, 0..) |str, i| {
                    std.debug.print("string {d}: {s}\n", .{ i, str });
                }

                // for (ast.root_elements.items) |item| {
                //     switch (item) {
                //         .class => |class| {
                //             std.debug.print("{}\n", .{class.*});

                //             for (class.fields) |field| {
                //                 std.debug.print("field {}\n", .{field.*});
                //             }

                //             for (class.functions) |function| {
                //                 std.debug.print("function {}\n", .{function.*});
                //                 std.debug.print("function body {?}\n", .{function.body});
                //             }

                //             std.debug.print("{?any}\n", .{class.constructors});
                //         },
                //         inline else => |ptr| {
                //             std.debug.print("{}\n", .{ptr.*});
                //         },
                //     }
                // }
            }
        },
        .generate_library => {
            const params = comptime clap.parseParamsComptime(
                \\-h, --help             Display this help and exit.
                \\-m, --map       <str>  The MAP file to use. Required
                \\-f, --folder    <str>  The game data folder to use. Required
                \\-o, --output    <str>  The output folder of the library. Required
                \\-s, --namespace <str>  The namespace to put the generated files in.
                \\-n, --name      <str>  The name of the library
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
            if (res.args.help != 0 or
                res.args.map == null or
                res.args.folder == null or
                res.args.name == null or
                res.args.output == null)
            {
                try clap.help(stderr, clap.Help, &params, .{});

                return;
            }

            var output_dir = std.fs.cwd().openDir(res.args.output.?, .{}) catch |err| blk: {
                if (err == error.FileNotFound) {
                    try std.fs.cwd().makeDir(res.args.output.?);

                    break :blk try std.fs.cwd().openDir(res.args.output.?, .{});
                }

                return err;
            };
            defer output_dir.close();

            //If a namespace is specified, create that dir
            if (res.args.namespace) |namespace| {
                //Make the path for the namespace
                output_dir.makeDir(namespace) catch |err| {
                    if (err != error.PathAlreadyExists)
                        return err;
                };
            }

            var game_data_dir = try std.fs.cwd().openDir(res.args.folder.?, .{});
            defer game_data_dir.close();

            const map_file = try std.fs.cwd().openFile(res.args.map.?, .{});
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

            var scripts = std.AutoHashMap(u32, MMTypes.Script).init(allocator);
            defer {
                var iter = scripts.iterator();
                while (iter.next()) |entry| {
                    entry.value_ptr.deinit(allocator);
                }

                scripts.deinit();
            }

            var iter = file_db.guid_lookup.iterator();
            while (iter.next()) |entry| {
                if (!std.mem.endsWith(u8, entry.value_ptr.path, ".ff"))
                    continue;

                std.debug.print("handling g{d}: {s}\n", .{ entry.key_ptr.*, entry.value_ptr.path });
                // try stdout.print("handling g{d}: {s}\n", .{ entry.key_ptr.*, entry.value_ptr.path });

                const file_data = try game_data_dir.readFileAlloc(allocator, entry.value_ptr.path, std.math.maxInt(usize));
                defer allocator.free(file_data);

                var resource = try Resource.readResource(file_data, allocator);
                defer resource.deinit();

                const script = try resource.stream.readScript(allocator);
                errdefer script.deinit(allocator);

                try scripts.putNoClobber(entry.key_ptr.*, script);
            }

            var script_iter = scripts.iterator();
            while (script_iter.next()) |entry| {
                const lower_class_name = try std.ascii.allocLowerString(allocator, entry.value_ptr.class_name);
                defer allocator.free(lower_class_name);

                const out_name = if (res.args.namespace) |namespace|
                    try std.fmt.allocPrint(allocator, "{s}/{s}.as", .{ namespace, lower_class_name })
                else
                    try std.fmt.allocPrint(allocator, "{s}.as", .{lower_class_name});
                defer allocator.free(out_name);
                const out_file = try output_dir.createFile(out_name, .{});
                defer out_file.close();

                try Stubinator.generateStubs(
                    out_file.writer(),
                    allocator,
                    entry.value_ptr.*,
                    entry.key_ptr.*,
                    scripts,
                    res.args.namespace,
                    res.args.name.?,
                );
            }
        },
    }
}
