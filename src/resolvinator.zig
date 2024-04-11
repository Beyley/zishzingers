const std = @import("std");

pub const MMTypes = @import("MMTypes.zig");
pub const Parser = @import("parser.zig");

pub const Libraries = std.StringHashMap(std.fs.Dir);

pub const ParsedScript = struct {
    pub const ImportedTypes = std.StringHashMap(void);

    ast: Parser.Tree,
    class_name: []const u8,
    imported_types: ImportedTypes,
    imported_libraries: Libraries,
};

pub const ParsedScriptTable = std.StringHashMap(*ParsedScript);

pub const Error = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError || Parser.Error;

fn getScriptClassName(tree: Parser.Tree) ?[]const u8 {
    for (tree.root_elements.items) |node| {
        switch (node) {
            .class => |class| return class.class_name,
            else => {},
        }
    }

    return null;
}

fn collectImportedTypes(class_name: []const u8, defined_libraries: Libraries, script_table: *ParsedScriptTable) Error!void {
    const script = script_table.get(class_name) orelse @panic("tsheointeonhsaoi");

    for (script.ast.root_elements.items) |item| {
        switch (item) {
            .import => |import| {
                var import_path = try script.ast.allocator.alloc(u8, import.target.len + 3);

                @memcpy(import_path[0..import.target.len], import.target);
                @memcpy(import_path[import.target.len..], ".as");

                //Replace all the `:` with `/` in the import path
                std.mem.replaceScalar(u8, import_path, ':', '/');

                var found = false;

                var iter = script.imported_libraries.valueIterator();
                while (iter.next()) |library_dir| {
                    const import_file = library_dir.openFile(import_path, .{}) catch |err| {
                        if (err == std.fs.Dir.OpenError.FileNotFound)
                            continue;

                        return err;
                    };
                    defer import_file.close();

                    std.debug.print("reading {s}\n", .{import_path});

                    //Get all the lexemes into a single big array
                    const lexemes = blk: {
                        var lexemes = std.ArrayList([]const u8).init(script.ast.allocator);
                        defer lexemes.deinit();

                        var lexizer = Parser.Lexemeizer{ .source = try import_file.readToEndAlloc(script.ast.allocator, std.math.maxInt(usize)) };

                        while (try lexizer.next()) |lexeme| {
                            try lexemes.append(lexeme);
                        }

                        break :blk try lexemes.toOwnedSlice();
                    };

                    const ast = try Parser.parse(script.ast.allocator, lexemes);

                    if (script_table.get(getScriptClassName(ast) orelse @panic("thsa")) == null)
                        try recursivelyResolveScript(ast, defined_libraries, script_table);

                    try script.imported_types.put(getScriptClassName(ast) orelse @panic("thsanye"), {});

                    found = true;
                }

                if (!found) std.debug.panic("could not find import {s}", .{import_path});
            },
            else => {},
        }
    }
}

fn collectImportedLibraries(script: *ParsedScript, defined_libraries: Libraries) Error!void {
    for (script.ast.root_elements.items) |item| {
        switch (item) {
            .using => |using| {
                if (using.type != .library)
                    @panic("unimplemented non-library using");

                try script.imported_libraries.putNoClobber(using.target, defined_libraries.get(using.target) orelse std.debug.panic("missing library {s}", .{using.target}));
            },
            else => {},
        }
    }
}

fn recursivelyResolveScript(tree: Parser.Tree, defined_libraries: Libraries, script_table: *ParsedScriptTable) Error!void {
    const script = try tree.allocator.create(ParsedScript);
    script.* = ParsedScript{
        .ast = tree,
        .class_name = getScriptClassName(tree) orelse @panic("thsane"),
        .imported_libraries = Libraries.init(tree.allocator),
        .imported_types = ParsedScript.ImportedTypes.init(tree.allocator),
    };
    try script_table.putNoClobber(script.class_name, script);

    std.debug.print("resolving script {s}\n", .{script.class_name});

    //Collect all the libraries which are imported by the script
    try collectImportedLibraries(script, defined_libraries);

    //Collect all the script types which are imported
    try collectImportedTypes(script.class_name, defined_libraries, script_table);
}

pub fn resolve(tree: Parser.Tree, defined_libraries: Libraries) Error!void {
    var script_table = ParsedScriptTable.init(tree.allocator);
    defer script_table.deinit();

    try recursivelyResolveScript(tree, defined_libraries, &script_table);
}
