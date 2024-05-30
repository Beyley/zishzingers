const std = @import("std");

const clap = @import("clap");

const Stream = @import("stream.zig");
const MMTypes = @import("MMTypes.zig");
const Disasm = @import("disasm.zig");
const Resource = @import("resource.zig");
const Resolvinator = @import("resolvinator.zig");
const Parser = @import("parser.zig");
const Debug = @import("debug.zig");
const Genny = @import("genny.zig");
const ArrayListStreamSource = @import("ArrayListStreamSource.zig");
const Stubinator = @import("stubinator.zig");

pub fn disasm(
    allocator: std.mem.Allocator,
    arg_iter: *std.process.ArgIterator,
    stderr: anytype,
    stdout: anytype,
) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help                       Display this help and exit.
        \\-n, --no-container               Specifies that the file is a raw script file, not inside the resource container. This flag requires -r.
        \\-r, --revision         <u16>     If loading a raw script file, load with this revision.
        \\-c, --compression-flag <str>...  Specifies a compression flag, possible values are "integers", "vectors", or "matrices".
        \\<str>                            The file to disassemble
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, arg_iter, .{
        .diagnostic = &diag,
        .allocator = allocator,
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

            try Disasm.disassembleScript(stdout, allocator, script);
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

            if (resource.dependencies) |dependencies| {
                for (dependencies) |dependency| {
                    try stdout.print("Script has dependency {}\n", .{dependency.ident});
                }
            }

            // std.debug.print("compression {}\n", .{resource.stream.compression_flags});
            // std.debug.print("revision {}\n", .{resource.stream.revision});

            const script = try resource.stream.readScript(allocator);
            defer script.deinit(allocator);

            try Disasm.disassembleScript(stdout, allocator, script);
        }
    }
}

pub fn compile(
    allocator: std.mem.Allocator,
    arg_iter: *std.process.ArgIterator,
    stderr: anytype,
    stdout: anytype,
) !void {
    _ = stdout; // autofix
    const params = comptime clap.parseParamsComptime(
        \\-h, --help                       Display this help and exit.
        \\-o, --out-file <str>             The output path for the compilation, defaults to "inputname.ff"
        \\-l, --library <str>...           A library to import, with the syntax `name:path`
        \\-i, --identifier <u32>           The GUID of the script being compiled,
        \\-r, --revision <u32>             The revision of the asset to be serialized
        \\-z, --branch-id <u16>            The branch ID of the asset to be serialized
        \\-y, --branch-revision <u16>      The branch revision of the asset to be serialized
        \\--optimize <str>                 Specify the compilation mode used, defaults to Debug (Debug, ReleaseSafe, ReleaseFast, ReleaseSmall)
        \\                                 this overrides the GUID specified in the file.
        \\<str>                            The source file
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, arg_iter, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    //If no arguments are passed or the user requested the help menu, display the help menu
    if (res.args.help != 0 or res.positionals.len == 0 or res.args.revision == null) {
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
        // std.debug.print("{s}\n", .{source_file});
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

        var type_intern_pool = Parser.TypeInternPool{
            .hash_map = Parser.TypeInternPool.HashMap.init(allocator),
        };
        defer type_intern_pool.deinit();

        const parser = try Parser.parse(
            ast_allocator,
            lexemes,
            &type_intern_pool,
        );

        // try Debug.dumpAst(stdout, ast);

        var a_string_table = Resolvinator.AStringTable.init(allocator);
        defer a_string_table.deinit();

        var w_string_table = Resolvinator.WStringTable.init(allocator);
        defer w_string_table.deinit();

        try Resolvinator.resolve(
            parser.tree,
            defined_libraries,
            &a_string_table,
            script_identifier,
            &type_intern_pool,
        );

        // var debug = Debug{
        //     .indent = 0,
        //     .writer = stdout.any(),
        //     .a_string_table = &a_string_table,
        //     .type_intern_pool = &type_intern_pool,
        // };
        // try debug.dumpAst(parser.tree);

        const compilation_options: Genny.CompilationOptions = .{
            .optimization_mode = if (res.args.optimize) |optimize| std.meta.stringToEnum(std.builtin.OptimizeMode, optimize).? else .Debug,
            .revision = .{
                .head = res.args.revision.?,
                .branch_revision = res.args.@"branch-revision" orelse 0,
                .branch_id = res.args.@"branch-id" orelse 0,
            },
        };

        var genny = Genny.init(
            parser.tree,
            &a_string_table,
            &w_string_table,
            compilation_options,
            &type_intern_pool,
        );
        defer genny.deinit();

        const script = try genny.generate();

        //TODO: actually use `output` parameter
        const out_path = try std.fmt.allocPrint(allocator, "{s}.ff", .{std.fs.path.stem(source_file)});
        defer allocator.free(out_path);
        const out = try std.fs.cwd().createFile(res.args.@"out-file" orelse out_path, .{});
        defer out.close();

        const compression_flags = .{
            .compressed_integers = true,
            .compressed_matrices = true,
            .compressed_vectors = true,
        };

        var resource_stream = Stream.MMStream(ArrayListStreamSource){
            .stream = .{
                .array_list = std.ArrayList(u8).init(allocator),
                .pos = 0,
            },
            .compression_flags = compression_flags,
            .revision = compilation_options.revision,
        };
        defer resource_stream.stream.array_list.deinit();

        try resource_stream.writeScript(script, allocator);

        var dependencies = std.AutoArrayHashMap(Resource.Dependency, void).init(allocator);
        defer dependencies.deinit();

        for (script.type_references) |type_reference| {
            if (type_reference.script) |script_depentency| {
                try dependencies.put(.{ .ident = script_depentency, .type = .script }, {});
            }
        }
        var file_stream: std.io.StreamSource = .{ .file = out };
        try Resource.writeResource(
            .script,
            compression_flags,
            dependencies.keys(), //TODO: write out dependencies by pulling referenced files
            compilation_options.revision,
            &file_stream,
            resource_stream.stream.array_list.items,
            allocator,
        );

        // try Disasm.disassembleScript(stdout, allocator, script);
    }
}

pub fn generateLibrary(
    allocator: std.mem.Allocator,
    arg_iter: *std.process.ArgIterator,
    stderr: anytype,
    stdout: anytype,
) !void {
    _ = stdout; // autofix

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
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, arg_iter, .{
        .diagnostic = &diag,
        .allocator = allocator,
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
            try std.fs.cwd().makePath(res.args.output.?);

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

        // std.debug.print("handling g{d}: {s}\n", .{ entry.key_ptr.*, entry.value_ptr.path });
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

        // std.debug.print("handling g{d}: {s}\n", .{ entry.key_ptr.*, entry.value_ptr.class_name });
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
}
