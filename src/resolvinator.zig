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
    resource_identifier: MMTypes.ResourceIdentifier,
    is_thing: bool,
};

pub const ParsedScriptTable = std.StringHashMap(*ParsedScript);

pub const Error = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError || Parser.Error;

fn getScriptClassNode(tree: Parser.Tree) *Parser.Node.Class {
    for (tree.root_elements.items) |node| {
        switch (node) {
            .class => |class| return class,
            else => {},
        }
    }

    @panic("wathtsh");
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

                    if (script_table.get(getScriptClassNode(ast).class_name) == null)
                        try recursivelyResolveScript(ast, defined_libraries, script_table, null);

                    try script.imported_types.put(getScriptClassNode(ast).class_name, {});

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

fn recursivelyResolveScript(tree: Parser.Tree, defined_libraries: Libraries, script_table: *ParsedScriptTable, script_identifier: ?MMTypes.ResourceIdentifier) Error!void {
    const class = getScriptClassNode(tree);

    const script = try tree.allocator.create(ParsedScript);
    script.* = ParsedScript{
        .ast = tree,
        .class_name = getScriptClassNode(tree).class_name,
        .imported_libraries = Libraries.init(tree.allocator),
        .imported_types = ParsedScript.ImportedTypes.init(tree.allocator),
        //Get the identifier for this script, whether it be passed in as a parameter or pulled from the class details
        .resource_identifier = script_identifier orelse switch ((class.identifier orelse @panic("you need to specify a identifier somewhere")).contents) {
            .guid_literal => |guid_literal| .{
                .guid = guid_literal,
            },
            else => @panic("TODO"),
        },
        .is_thing = undefined,
    };
    try script_table.putNoClobber(script.class_name, script);

    std.debug.print("resolving script {s}\n", .{script.class_name});

    //Collect all the libraries which are imported by the script
    try collectImportedLibraries(script, defined_libraries);

    //Collect all the script types which are imported
    try collectImportedTypes(script.class_name, defined_libraries, script_table);

    script.is_thing = isScriptThing(script, script_table);

    std.debug.print("script {s} is {} thing/notthing\n", .{ script.class_name, script.is_thing });
}

///Figures out if a script extends Thing, checks the full inheritance chain
fn isScriptThing(script: *const ParsedScript, script_table: *const ParsedScriptTable) bool {
    if (std.mem.eql(u8, script.class_name, "Thing"))
        return true;

    const class = getScriptClassNode(script.ast);

    if (class.base_class) |base_class| {
        return isScriptThing(script_table.get(base_class).?, script_table);
    } else {
        // This class is not Thing, and extends no other classes, therefor it cannot be a Thing
        return false;
    }
}

pub const AStringTable = std.StringArrayHashMap(void);

pub fn resolve(
    tree: Parser.Tree,
    defined_libraries: Libraries,
    a_string_table: *AStringTable,
    script_identifier: ?MMTypes.ResourceIdentifier,
) Error!void {
    var script_table = ParsedScriptTable.init(tree.allocator);
    defer script_table.deinit();

    try recursivelyResolveScript(tree, defined_libraries, &script_table, script_identifier);

    //Get the class of the script
    const class = getScriptClassNode(tree);

    const script = script_table.get(class.class_name) orelse unreachable;

    std.debug.print("type resolving {s}\n", .{script.class_name});

    for (class.fields) |field| {
        try resolveField(field, script, &script_table, a_string_table);
    }

    for (class.functions) |function| {
        try resolveFunction(function, script, &script_table, a_string_table);
    }
}

fn resolveFunction(
    function: *Parser.Node.Function,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) !void {
    function.return_type = .{
        .resolved = try resolveParsedType(
            function.return_type.parsed,
            script,
            script_table,
            a_string_table,
        ),
    };

    std.debug.print("resolved function return type as {}\n", .{function.return_type.resolved});

    for (function.parameters.parameters) |*parameter| {
        parameter.type = .{
            .resolved = try resolveParsedType(
                parameter.type.parsed,
                script,
                script_table,
                a_string_table,
            ),
        };

        std.debug.print("resolved function parameter {s} as {}\n", .{ parameter.name, parameter.type.resolved });
    }

    //TODO: once i parse the `=>` syntax for function bodies, this `null` for target type needs to be made correct!!!
    //      should i make function_body a special expression type? im not sure yet.
    //      maybe this could be as simple as "if block, target type == void, if not block, target type is the function return type" that should work
    try resolveExpression(function.body.?, null, script, script_table, a_string_table);

    std.debug.print("resolved function {s}\n", .{function.name});
}

fn resolveExpression(
    expression: *Parser.Node.Expression,
    target_type: ?Parser.Type,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) !void {
    //If this expression has already been resolved, do nothing
    if (expression.type == .resolved)
        return;

    switch (expression.contents) {
        .s32_literal => {
            expression.type = .{
                .resolved = try resolveParsedType(
                    Parser.Type.ParsedType.S32,
                    script,
                    script_table,
                    a_string_table,
                ),
            };
        },
        .block => |block| {
            expression.type = .{
                .resolved = try resolveParsedType(
                    Parser.Type.ParsedType.Void,
                    script,
                    script_table,
                    a_string_table,
                ),
            };

            //Resolve the contents
            for (block) |node| {
                switch (node) {
                    .variable_declaration => |variable_declaration| {
                        if (variable_declaration.type == .unknown) {
                            //If the type of the variable declaration is unspecified, we need to resolve the value expression first
                            if (variable_declaration.value) |value| {
                                try resolveExpression(
                                    value,
                                    null,
                                    script,
                                    script_table,
                                    a_string_table,
                                );

                                //Then we can use the type of the value expression for the type of the variable declaration
                                variable_declaration.type = variable_declaration.value.?.type;
                            }
                            //This should be an impossible scenario, the parser gets mad about this
                            else unreachable;
                        } else {
                            //Resolve the variable declaration type
                            variable_declaration.type = .{
                                .resolved = try resolveParsedType(
                                    variable_declaration.type.parsed,
                                    script,
                                    script_table,
                                    a_string_table,
                                ),
                            };

                            //If the variable declaration has a value set, resolve the value expression to the type of the variable
                            if (variable_declaration.value) |value| {
                                try resolveExpression(
                                    value,
                                    variable_declaration.type,
                                    script,
                                    script_table,
                                    a_string_table,
                                );
                            }
                        }
                    },
                    else => |node_type| std.debug.panic("TODO: resolution of expression type {s}", .{@tagName(node_type)}),
                }
            }
        },
        else => |contents| std.debug.panic("TODO: resolution of expression type {s}", .{@tagName(contents)}),
    }

    if (target_type) |target_parsed_type| {
        std.debug.assert(target_parsed_type.resolved.eql(expression.type.resolved));
    }
}

fn resolveParsedType(
    parsed_type: Parser.Type.ParsedType,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) !MMTypes.TypeReference {
    if (std.meta.stringToEnum(MMTypes.FishType, parsed_type.name)) |fish_type| {
        return if (parsed_type.dimension_count > 0)
            MMTypes.TypeReference{
                .array_base_machine_type = fish_type.toMachineType(),
                .dimension_count = parsed_type.dimension_count,
                .fish_type = .void,
                .machine_type = .object_ref,
                //null
                .type_name = 0xFFFFFFFF,
                .script = null,
            }
        else
            MMTypes.TypeReference{
                .array_base_machine_type = .void,
                .dimension_count = 0,
                .fish_type = fish_type,
                .machine_type = fish_type.toMachineType(),
                //null
                .type_name = 0xFFFFFFFF,
                .script = null,
            };
    } else {
        var iter = script.imported_types.keyIterator();
        while (iter.next()) |imported_type| {
            if (std.mem.eql(u8, parsed_type.name, imported_type.*)) {
                const referenced_script = script_table.get(imported_type.*).?;

                //Get the idx of the name of this script, or put into the string table
                const name_idx = try a_string_table.getOrPut(imported_type.*);

                return if (parsed_type.dimension_count > 0)
                    MMTypes.TypeReference{
                        .array_base_machine_type = if (referenced_script.is_thing) .safe_ptr else .object_ref,
                        .dimension_count = parsed_type.dimension_count,
                        .fish_type = .void,
                        .machine_type = .object_ref,
                        .type_name = @intCast(name_idx.index),
                        .script = referenced_script.resource_identifier,
                    }
                else
                    MMTypes.TypeReference{
                        .array_base_machine_type = .void,
                        .dimension_count = 0,
                        .fish_type = .void,
                        .machine_type = if (referenced_script.is_thing) .safe_ptr else .object_ref,
                        .type_name = @intCast(name_idx.index),
                        .script = referenced_script.resource_identifier,
                    };
            }
        }

        @panic("no script found.");
    }
}

fn resolveField(
    field: *Parser.Node.Field,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) !void {
    std.debug.print("type resolving field {s}\n", .{field.name});

    switch (field.type) {
        .parsed => |parsed_type| {
            field.type = .{
                .resolved = try resolveParsedType(
                    parsed_type,
                    script,
                    script_table,
                    a_string_table,
                ),
            };
        },
        .unknown => {
            if (field.default_value) |default_value| {
                //Resolve the type of the default value, with no specific target type in mind
                try resolveExpression(
                    default_value,
                    null,
                    script,
                    script_table,
                    a_string_table,
                );

                //Set the type of the field to the resolved default value type
                field.type = default_value.type;
            } else unreachable;
        },
        else => unreachable,
    }

    std.debug.print("resolved as {}\n", .{field.type});

    std.debug.assert(field.type == .resolved);
}
