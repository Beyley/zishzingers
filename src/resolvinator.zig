//! Crawls an AST and recursively type resolves all expressions

const std = @import("std");

pub const MMTypes = @import("MMTypes.zig");
pub const Parser = @import("parser.zig");

pub const AStringTable = std.StringArrayHashMap(void);
pub const WStringTable = std.ArrayHashMap([]const u16, void, struct {
    pub fn hash(self: @This(), s: []const u16) u32 {
        _ = self;
        return hashString(s);
    }

    pub fn eql(self: @This(), a: []const u16, b: []const u16, b_index: usize) bool {
        _ = self;
        _ = b_index;
        return eqlString(a, b);
    }

    pub fn hashString(s: []const u16) u32 {
        return @as(u32, @truncate(std.hash.Wyhash.hash(0, std.mem.sliceAsBytes(s))));
    }

    pub fn eqlString(a: []const u16, b: []const u16) bool {
        return std.mem.eql(u16, a, b);
    }
}, true);

pub const Libraries = std.StringHashMap(std.fs.Dir);

pub const ParsedScript = struct {
    pub const ImportedTypes = std.StringHashMap(void);
    /// Map of "imported name" to "original name + script"
    pub const ImportedFunctions = std.StringHashMap(struct {
        original_name: []const u8,
        script: []const u8,
    });

    /// The AST of the parsed script.
    ast: Parser.Tree,
    /// The name of the script.
    class_name: []const u8,
    /// Maps all the types which are imported.
    imported_types: ImportedTypes,
    /// Maps all the libraries imported.
    imported_libraries: Libraries,
    /// Maps all the imported functions which use the `from 'std:lams' import Translate;` syntax.
    imported_functions: ImportedFunctions,
    /// The identifier of the script.
    resource_identifier: MMTypes.ResourceIdentifier,
    /// Marks whether the script derives `Thing`, whether directly or indirectly.
    is_thing: bool,
};

pub const ParsedScriptTable = std.StringHashMap(*ParsedScript);

pub const Error = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError || Parser.Error;

const Self = @This();

type_intern_pool: *Parser.TypeInternPool,

fn getScriptClassNode(tree: Parser.Tree) *Parser.Node.Class {
    for (tree.root_elements.items) |node| {
        switch (node) {
            .class => |class| return class,
            else => {},
        }
    }

    @panic("wathtsh");
}

fn getImportPathFromImportTarget(allocator: std.mem.Allocator, target: []const u8) Error![]const u8 {
    var import_path = try allocator.alloc(u8, target.len + 3);

    @memcpy(import_path[0..target.len], target);
    @memcpy(import_path[target.len..], ".as");

    //Replace all the `:` with `/` in the import path
    std.mem.replaceScalar(u8, import_path, ':', '/');

    return import_path;
}

fn collectImportedTypes(
    self: *Self,
    class_name: []const u8,
    defined_libraries: Libraries,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    const script = script_table.get(class_name) orelse @panic("tsheointeonhsaoi");

    // const progress_node = parent_progress_node.start("Collecting imported types", blk: {
    //     var i: usize = 0;

    //     for (script.ast.root_elements.items) |item| {
    //         if (item == .import or item == .from_import)
    //             i += 1;
    //     }

    //     break :blk i;
    // });
    // defer progress_node.end();

    //TODO: we need to make sure to prevent two imported scripts from sharing the same GUID in the global script table
    for (script.ast.root_elements.items) |item| {
        switch (item) {
            inline .import, .from_import => |import, import_type| {
                const import_path = try getImportPathFromImportTarget(script.ast.allocator, import.target);

                var found = false;

                var iter = script.imported_libraries.valueIterator();
                while (iter.next()) |library_dir| {
                    const import_file = library_dir.openFile(import_path, .{}) catch |err| {
                        if (err == std.fs.Dir.OpenError.FileNotFound)
                            continue;

                        return err;
                    };
                    defer import_file.close();

                    // std.debug.print("reading {s}\n", .{import_path});

                    //TODO: let the user specify a "system allocator" separate from the script allocator
                    var lexeme_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);

                    //Get all the lexemes into a single big array
                    const lexemes = blk: {
                        var lexemes = std.ArrayList([]const u8).init(lexeme_allocator.allocator());
                        defer lexemes.deinit();

                        var lexizer = Parser.Lexemeizer{ .source = try import_file.readToEndAlloc(script.ast.allocator, std.math.maxInt(usize)) };

                        while (try lexizer.next()) |lexeme| {
                            try lexemes.append(lexeme);
                        }

                        break :blk try lexemes.toOwnedSlice();
                    };

                    const parser = try Parser.parse(
                        script.ast.allocator,
                        lexemes,
                        self.type_intern_pool,
                        parent_progress_node,
                    );
                    lexeme_allocator.deinit();

                    const class = getScriptClassNode(parser.tree);

                    if (script_table.get(class.name) == null)
                        try self.recursivelyResolveScript(
                            parser.tree,
                            defined_libraries,
                            script_table,
                            null,
                            a_string_table,
                            parent_progress_node,
                        );

                    switch (comptime import_type) {
                        .import => {
                            try script.imported_types.putNoClobber(class.name, {});
                        },
                        .from_import => {
                            switch (import.wanted) {
                                .all => {
                                    for (class.functions) |function| {
                                        //TODO: this *needs* to be putNoClobber, I think these keys need to be the mangled names
                                        try script.imported_functions.put(function.name, .{
                                            .original_name = function.name,
                                            .script = class.name,
                                        });
                                    }
                                },
                                .multiple => |multiple_imports| {
                                    //TODO: check to make sure the original names actually exist in the script
                                    for (multiple_imports) |imported_function| {
                                        //TODO: this *needs* to be putNoClobber, I think these keys need to be the mangled names
                                        try script.imported_functions.put(imported_function.name, .{
                                            .original_name = imported_function.original_name,
                                            .script = class.name,
                                        });
                                    }
                                },
                                .single => |single_import| {
                                    //TODO: this *needs* to be putNoClobber, I think these keys need to be the mangled names
                                    try script.imported_functions.put(single_import.name, .{
                                        .original_name = single_import.original_name,
                                        .script = class.name,
                                    });
                                },
                            }
                        },
                        else => @compileError("THAE"),
                    }

                    found = true;
                }

                if (!found) std.debug.panic("could not find import {s}", .{import_path});
            },
            else => {},
        }
    }
}

fn collectImportedLibraries(
    script: *ParsedScript,
    defined_libraries: Libraries,
    parent_progress_node: std.Progress.Node,
) Error!void {
    _ = parent_progress_node; // autofix
    // const progress_node = parent_progress_node.start("Collecting imported libraries", blk: {
    //     var i: usize = 0;

    //     for (script.ast.root_elements.items) |item| {
    //         if (item == .using)
    //             i += 1;
    //     }

    //     break :blk i;
    // });
    // defer progress_node.end();

    for (script.ast.root_elements.items) |item| {
        switch (item) {
            .using => |using| {
                // defer progress_node.completeOne();

                if (using.type != .library)
                    @panic("unimplemented non-library using");

                try script.imported_libraries.putNoClobber(using.target, defined_libraries.get(using.target) orelse std.debug.panic("missing library {s}", .{using.target}));
            },
            else => {},
        }
    }
}

fn recursivelyResolveScript(
    self: *Self,
    tree: Parser.Tree,
    defined_libraries: Libraries,
    script_table: *ParsedScriptTable,
    script_identifier: ?MMTypes.ResourceIdentifier,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    const class = getScriptClassNode(tree);

    // var progress_node_name_buf: [256]u8 = undefined;
    // const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving dependencies for {s}", .{class.name}), 0);
    // defer progress_node.end();

    const script = try tree.allocator.create(ParsedScript);
    script.* = .{
        .ast = tree,
        .class_name = getScriptClassNode(tree).name,
        .imported_libraries = Libraries.init(tree.allocator),
        .imported_types = ParsedScript.ImportedTypes.init(tree.allocator),
        .imported_functions = ParsedScript.ImportedFunctions.init(tree.allocator),
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

    // Add the script to its own import table, so it can reference itself
    try script.imported_types.putNoClobber(script.class_name, {});

    //Collect all the libraries which are imported by the script
    try collectImportedLibraries(
        script,
        defined_libraries,
        parent_progress_node,
    );

    //Collect all the script types which are imported
    try self.collectImportedTypes(
        script.class_name,
        defined_libraries,
        script_table,
        a_string_table,
        parent_progress_node,
    );

    script.is_thing = isScriptThing(script, script_table);

    if (class.base_class == .parsed) {
        const parsed_base_class = class.base_class.parsed;

        _ = script.imported_types.get(parsed_base_class) orelse std.debug.panic("base class {s} not found\n", .{parsed_base_class});

        const base_script = script_table.get(parsed_base_class).?;

        class.base_class = .{
            .resolved = .{
                .ident = base_script.resource_identifier,
                .name = parsed_base_class,
                .has_parameterless_constructor = blk: {
                    const base_class = getScriptClassNode(base_script.ast);

                    // If its static, theres no constructors at all
                    if (base_class.modifiers.static)
                        break :blk false;

                    // If theres no constructor, then it has a default parameterless one
                    if (base_class.constructors.len == 0)
                        break :blk true;

                    // If the base classes constructor has no parameters, then we've found it
                    for (base_class.constructors) |constructor| {
                        if (constructor.parameters.len == 0)
                            break :blk true;
                    }

                    break :blk false;
                },
                .type_reference = null,
            },
        };
    }

    // std.debug.print("script {s} is {} thing/notthing\n", .{ script.class_name, script.is_thing });
}

///Figures out if a script extends Thing, checks the full inheritance chain
fn isScriptThing(script: *const ParsedScript, script_table: *const ParsedScriptTable) bool {
    if (std.mem.eql(u8, script.class_name, "Thing"))
        return true;

    const class = getScriptClassNode(script.ast);

    if (class.base_class != .none) {
        const base_class_name = switch (class.base_class) {
            .parsed => |parsed| parsed,
            .resolved => |resolved| resolved.name,
            else => unreachable,
        };

        return isScriptThing(script_table.get(base_class_name).?, script_table);
    } else {
        // This class is not Thing, and extends no other classes, therefor it cannot be a Thing
        return false;
    }
}

pub fn resolve(
    tree: Parser.Tree,
    defined_libraries: Libraries,
    a_string_table: *AStringTable,
    script_identifier: ?MMTypes.ResourceIdentifier,
    type_intern_pool: *Parser.TypeInternPool,
    parent_progress_node: std.Progress.Node,
) Error!void {
    //Get the class of the script
    const class = getScriptClassNode(tree);

    const progress_node = parent_progress_node.start(
        "Type resolving",
        // all the fields, constructors, and functions plus the type fixup stage
        4,
    );
    defer progress_node.end();

    var script_table = ParsedScriptTable.init(tree.allocator);
    defer script_table.deinit();

    var self: Self = .{
        .type_intern_pool = type_intern_pool,
    };

    try self.recursivelyResolveScript(
        tree,
        defined_libraries,
        &script_table,
        script_identifier,
        a_string_table,
        progress_node,
    );

    class.type_reference = (try typeReferenceFromScript(
        script_table.get(class.name).?,
        0,
        a_string_table,
    ));

    const base_class_type_fixup_progress_node = progress_node.start("Fill in base class type references", script_table.count());
    // fill in the type references of all the base classes
    var scripts = script_table.valueIterator();
    while (scripts.next()) |script| {
        defer base_class_type_fixup_progress_node.completeOne();

        const script_class = getScriptClassNode(script.*.ast);

        if (script_class.base_class != .none) {
            const base_class = script_class.base_class.resolved;

            script_class.base_class.resolved.type_reference = try typeReferenceFromScript(script_table.get(base_class.name).?, 0, a_string_table);
        }
    }
    base_class_type_fixup_progress_node.end();

    const script = script_table.get(class.name) orelse unreachable;

    // std.debug.print("type resolving {s}\n", .{script.class_name});

    const fields_progress_node = progress_node.start("Fields", class.fields.len);
    for (class.fields) |field| {
        try self.resolveField(
            field,
            script,
            &script_table,
            a_string_table,
            fields_progress_node,
        );
    }
    fields_progress_node.end();

    const constructors_progress_node = progress_node.start("Constructors", class.constructors.len * 2);
    for (class.constructors) |constructor| {
        try self.resolveConstructorHead(
            constructor,
            script,
            &script_table,
            a_string_table,
            constructors_progress_node,
        );
        try self.resolveConstructorBody(
            constructor,
            script,
            &script_table,
            a_string_table,
            constructors_progress_node,
        );
    }
    constructors_progress_node.end();

    const functions_progress_node = progress_node.start("Functions", class.functions.len * 2);
    for (class.functions) |function| {
        try self.resolveFunctionHead(
            function,
            script,
            &script_table,
            a_string_table,
            functions_progress_node,
        );
        try self.resolveFunctionBody(
            function,
            script,
            &script_table,
            a_string_table,
            functions_progress_node,
        );
    }
    functions_progress_node.end();
}

fn resolveConstructorHead(
    self: *Self,
    constructor: *Parser.Node.Constructor,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving constructor head with params {d}", .{constructor.parameters.len}), 0);
    defer progress_node.end();

    for (constructor.parameters) |*parameter| {
        try self.resolveParsedType(
            parameter.type,
            script,
            script_table,
            a_string_table,
            false,
            progress_node,
        );
    }
}

fn resolveConstructorBody(
    self: *Self,
    constructor: *Parser.Node.Constructor,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving constructor body with params {d}", .{constructor.parameters.len}), 0);
    defer progress_node.end();

    var variable_stack = FunctionVariableStack.init(script.ast.allocator);
    defer variable_stack.deinit();

    var stack_info: FunctionVariableStackInfo = .{
        .current_level = 0,
        .stack = &variable_stack,
        .function = .{ .constructor = constructor },
    };

    try self.resolveExpression(
        constructor.body.?,
        null,
        script,
        script_table,
        a_string_table,
        &stack_info,
        progress_node,
    );
}

fn resolveFunctionBody(
    self: *Self,
    function: *Parser.Node.Function,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving function body {s}", .{function.name}), 0);
    defer progress_node.end();

    if (function.body) |body| {
        var variable_stack = FunctionVariableStack.init(script.ast.allocator);
        defer variable_stack.deinit();

        var stack_info: FunctionVariableStackInfo = .{
            .current_level = 0,
            .stack = &variable_stack,
            .function = .{ .function = function },
        };

        if (body.type == .unknown or self.type_intern_pool.get(body.type).* != .resolved)
            //TODO: once i parse the `=>` syntax for function bodies, this `null` for target type needs to be made correct!!!
            //      should i make function_body a special expression type? im not sure yet.
            //      maybe this could be as simple as "if block, target type == void, if not block, target type is the function return type" that should work
            try self.resolveExpression(
                body,
                null,
                script,
                script_table,
                a_string_table,
                &stack_info,
                progress_node,
            );
    }

    // std.debug.print("resolved function body {s}\n", .{function.name});
}

fn resolveFunctionHead(
    self: *Self,
    function: *Parser.Node.Function,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving function head {s}", .{function.name}), 0);
    defer progress_node.end();

    // std.debug.print("resolving function head {s}\n", .{function.name});

    try self.resolveParsedType(
        function.return_type,
        script,
        script_table,
        a_string_table,
        false,
        progress_node,
    );

    // std.debug.print("resolved function return type as {}\n", .{return_type.resolved});

    for (function.parameters) |*parameter| {
        const parameter_type = self.type_intern_pool.get(parameter.type);
        _ = parameter_type; // autofix

        try self.resolveParsedType(
            parameter.type,
            script,
            script_table,
            a_string_table,
            false,
            progress_node,
        );

        // std.debug.print("resolved function parameter {s} as {}\n", .{ parameter.name, parameter_type.resolved });
    }

    // std.debug.print("resolved function head {s}\n", .{function.name});

    var mangled_name = std.ArrayList(u8).init(script_table.allocator);
    try self.mangleFunctionName(
        mangled_name.writer(),
        a_string_table,
        function.name,
        function.parameters,
    );
    function.mangled_name = mangled_name.items;
}

const FunctionVariableStack = std.StringArrayHashMap(struct {
    level: u8,
    type: Parser.TypeInternPool.Index,
});

const FunctionVariableStackInfo = struct {
    const StackLevel = u8;

    function: union(enum) {
        function: *Parser.Node.Function,
        constructor: *Parser.Node.Constructor,
    },
    current_level: StackLevel,
    stack: *FunctionVariableStack,
};

fn resolveExpression(
    self: *Self,
    expression: *Parser.Node.Expression,
    target_type: ?Parser.TypeInternPool.Index,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    function_variable_stack: ?*FunctionVariableStackInfo,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;
    const progress_node = parent_progress_node.start(
        try std.fmt.bufPrint(&progress_node_name_buf, "Expression of type {s}", .{@tagName(expression.contents)}),
        0,
    );
    defer progress_node.end();

    if (expression.type != .unknown) {
        const expression_type = self.type_intern_pool.get(expression.type);

        //If this expression has already been resolved, do nothing
        if (expression_type.* == .resolved) {
            if (target_type) |target_parsed_index| {
                const target_parsed_type = self.type_intern_pool.get(target_parsed_index);

                if (!target_parsed_type.resolved.eql(expression_type.resolved))
                    std.debug.panic("wanted type {}, got type {}", .{ target_parsed_type.resolved, expression_type.resolved });
            }
        }
    }

    if (target_type) |intern_target_type| {
        try self.resolveParsedType(
            intern_target_type,
            script,
            script_table,
            a_string_table,
            false,
            progress_node,
        );
    }

    // std.debug.print("resolving {}\n", .{expression.contents});

    switch (expression.contents) {
        .integer_literal => {
            expression.type = try self.type_intern_pool.integerLiteral();
        },
        .float_literal => {
            expression.type = try self.type_intern_pool.floatLiteral();
        },
        .null_literal => {
            expression.type = try self.type_intern_pool.nullLiteral();
        },
        .assignment => |assignment| {
            //Resolve the type of the destination
            try self.resolveExpression(
                assignment.destination,
                target_type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const assignment_destination_type = self.type_intern_pool.get(assignment.destination.type);

            std.debug.assert(assignment_destination_type.resolved == .fish or assignment_destination_type.resolved == .pointer);

            //Resolve the type of the value, which should be the same type as the destination
            try self.resolveExpression(
                assignment.value,
                assignment.destination.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            // The type of the expression is the type of the destination value
            // This is to allow constructs like `func(a = 2);`
            expression.type = assignment.destination.type;
        },
        .field_access => |field_access| {
            //Resolve the source expression
            try self.resolveExpression(
                field_access.source,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const field_access_source_type = self.type_intern_pool.get(field_access.source.type);

            const field = switch (field_access_source_type.resolved.fish.machine_type) {
                .object_ref, .safe_ptr => field_resolution: {
                    const source_class_name = a_string_table.keys()[field_access_source_type.resolved.fish.type_name];

                    const source_script = script_table.get(source_class_name).?;

                    const source_class = getScriptClassNode(source_script.ast);

                    for (source_class.fields) |field| {
                        //If the field name is wrong, continue
                        if (!std.mem.eql(u8, field.name, field_access.field))
                            continue;

                        // Type resolve the field
                        try self.resolveField(
                            field,
                            source_script,
                            script_table,
                            a_string_table,
                            progress_node,
                        );

                        //Break out now that we have the field
                        break :field_resolution field;
                    }

                    //TODO: this should be a proper compile error, not `unreachable`
                    unreachable;
                },
                else => |machine_type| std.debug.panic(
                    "you cannot do field access on a variable with machine type {s}. must be object_ref or safe_ptr",
                    .{@tagName(machine_type)},
                ),
            };

            expression.type = field.type;
        },
        .variable_or_class_access => |variable_or_class_access| {
            if (script.imported_types.get(variable_or_class_access)) |_| {
                const class = script_table.get(variable_or_class_access).?;

                const intern_type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                    .name = variable_or_class_access,
                    .base_type = null,
                    .dimension_count = 0,
                    .indirection_count = 0,
                } });
                try self.resolveParsedType(
                    intern_type,
                    script,
                    script_table,
                    a_string_table,
                    false,
                    progress_node,
                );

                expression.* = .{
                    .contents = .{ .class_access = class.class_name },
                    .type = intern_type,
                };
            } else {
                const func_modifiers = switch (function_variable_stack.?.function) {
                    .function => |function| function.modifiers,
                    .constructor => |constructor| constructor.modifiers,
                };

                // If we arent static and we are accessing a variable called `this`, then its a self access
                if (!func_modifiers.static and std.mem.eql(u8, variable_or_class_access, "this")) {
                    // Update the contents to a variable access
                    expression.contents = .{ .variable_access = variable_or_class_access };

                    // Set the type of the variable to the self class
                    expression.type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                        .name = script.class_name,
                        .base_type = null,
                        .dimension_count = 0,
                        .indirection_count = 0,
                    } });

                    // Resolve the parsed type
                    try self.resolveParsedType(
                        expression.type,
                        script,
                        script_table,
                        a_string_table,
                        false,
                        progress_node,
                    );
                } else {
                    const variable = function_variable_stack.?.stack.get(variable_or_class_access) orelse std.debug.panic(
                        "could not find variable {s}",
                        .{variable_or_class_access},
                    );

                    expression.* = .{
                        .contents = .{ .variable_access = variable_or_class_access },
                        .type = variable.type,
                    };
                }
            }

            std.debug.assert(expression.contents != .variable_or_class_access);
        },
        .bool_literal => {
            // Set the type to a bool
            expression.type = try self.type_intern_pool.fromFishType(.bool);
            // Resolve the type
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .wide_string_literal => {
            expression.type = try self.type_intern_pool.wideStringType();
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .int_string_literal => {
            expression.type = try self.type_intern_pool.intStringLiteral();
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .ascii_string_literal => {
            expression.type = try self.type_intern_pool.asciiStringType();
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .function_call => |*function_call| {
            if (function_call.source != null) {
                //Resolve the source expression
                try self.resolveExpression(
                    function_call.source.?,
                    null,
                    script,
                    script_table,
                    a_string_table,
                    function_variable_stack,
                    progress_node,
                );

                const function = try self.findFunction(
                    function_call.function.name,
                    function_call.parameters,
                    script.ast.allocator,
                    function_call.source,
                    script,
                    script_table,
                    a_string_table,
                    progress_node,
                );
                // std.debug.print("found function {s}, special cased? {}\n", .{ function.found_function.name, function.special_cased });

                const function_parameters_to_check = if (function.special_cased)
                    function.found_function.parameters[1..]
                else
                    function.found_function.parameters;

                for (function_parameters_to_check, function_call.parameters) |function_parameter, call_parameter| {
                    try self.resolveExpression(
                        call_parameter,
                        function_parameter.type,
                        script,
                        script_table,
                        a_string_table,
                        function_variable_stack,
                        progress_node,
                    );
                }

                function_call.function = .{ .function = .{
                    .function = function.found_function,
                    .owning_type = function.owning_type,
                } };

                expression.type = function.found_function.return_type;
            } else {
                //First, figure out which function we want to call here...
                const function, const function_script = blk: {
                    const local_class = getScriptClassNode(script.ast);

                    // Iterate over all the functions defined in the local class, and prioritize them
                    for (local_class.functions) |local_function| {
                        if (std.mem.eql(u8, local_function.name, function_call.function.name)) {
                            break :blk .{ local_function, script };
                        }
                    }

                    //TODO: handle calling overloads by checking param types... this code does not account for overloads *at all*. this is Not Good.
                    //      why do people have to add overloads to things it *only* makes things harder. JUST NAME THINGS BETTER.
                    //      im going to have to type resolve every imported function in every fucking imported script to be able to track overloads.
                    //      like MAYBE i could only do the resolution when theres a conflict,
                    //      but why the fuck should i have to? thats so much extra state tracking.
                    //      just Dont Put Overloads In Your Language/VM you Bitch.
                    if (script.imported_functions.get(function_call.function.name)) |imported_function| {
                        const original_script = script_table.get(imported_function.script).?;

                        const script_class = getScriptClassNode(original_script.ast);

                        for (script_class.functions) |imported_script_function| {
                            if (std.mem.eql(u8, function_call.function.name, imported_script_function.name))
                                break :blk .{ imported_script_function, original_script };
                        }

                        unreachable;
                    }

                    std.debug.panic("unable to find function {s}", .{function_call.function.name});
                };

                // std.debug.print("found func {s} for call {s}\n", .{ function.name, function_call.function.name });

                try self.resolveFunctionHead(
                    function,
                    function_script,
                    script_table,
                    a_string_table,
                    progress_node,
                );

                function_call.function = .{
                    .function = .{
                        .function = function,
                        .owning_type = try typeReferenceFromScript(
                            function_script,
                            0,
                            a_string_table,
                        ),
                    },
                };

                if (function.parameters.len != function_call.parameters.len) {
                    std.debug.panic("function {s} wanted parameter count {d} but got {d}", .{ function.name, function.parameters.len, function_call.parameters.len });
                }

                // Resolve all the call parameter expressions to the types of the function parameters
                for (function.parameters, function_call.parameters) |parameter, call_parameter| {
                    try self.resolveExpression(
                        call_parameter,
                        parameter.type,
                        script,
                        script_table,
                        a_string_table,
                        function_variable_stack,
                        progress_node,
                    );
                }

                expression.type = function.return_type;
            }

            if (function_call.function != .function) {
                std.debug.panic("function call is not resolved correctly {s}", .{function_call.function.name});
            }
        },
        .logical_negation => |logical_negation| {
            // Resolve the sub-expression of the negation
            try self.resolveExpression(
                logical_negation,
                try self.type_intern_pool.fromFishType(.bool),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = logical_negation.type;
        },
        .numeric_negation => |numeric_negation| {
            // Resolve the sub-expression of the negation
            try self.resolveExpression(
                numeric_negation,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            if (!isNumberLike(self.type_intern_pool.get(numeric_negation.type)))
                std.debug.panic("numeric negation is not number-like", .{});

            expression.type = numeric_negation.type;
        },
        .block => |block| {
            expression.type = try self.type_intern_pool.fromFishType(.void);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );

            function_variable_stack.?.current_level += 1;
            defer {
                var to_remove = std.ArrayList([]const u8).init(script.ast.allocator);
                defer to_remove.deinit();

                var iter = function_variable_stack.?.stack.iterator();
                while (iter.next()) |variable| {
                    if (variable.value_ptr.level == function_variable_stack.?.current_level) {
                        to_remove.append(variable.key_ptr.*) catch @panic("OOM");
                    }
                }

                for (to_remove.items) |item|
                    std.debug.assert(function_variable_stack.?.stack.orderedRemove(item));

                function_variable_stack.?.current_level -= 1;
            }

            const func_params = switch (function_variable_stack.?.function) {
                .function => |function| function.parameters,
                .constructor => |constructor| constructor.parameters,
            };

            //Only add the function parameters on the first scope level (eg. function scope)
            if (function_variable_stack.?.current_level == 1)
                for (func_params) |parameter| {
                    try function_variable_stack.?.stack.put(parameter.name, .{
                        .level = function_variable_stack.?.current_level,
                        .type = parameter.type,
                    });
                };

            //Resolve the contents
            for (block) |node| {
                switch (node) {
                    .variable_declaration => |variable_declaration| {
                        if (variable_declaration.type == .unknown) {
                            //If the type of the variable declaration is unspecified, we need to resolve the value expression first
                            if (variable_declaration.value) |value| {
                                try self.resolveExpression(
                                    value,
                                    null,
                                    script,
                                    script_table,
                                    a_string_table,
                                    function_variable_stack,
                                    progress_node,
                                );

                                const value_type = self.type_intern_pool.get(value.type);

                                //Panic if the resolved type is not knowable at runtime
                                if (value_type.resolved != .fish and value_type.resolved != .pointer)
                                    std.debug.panic(
                                        "variable declaration {s}'s default value's is unknown at runtime, currently is {s}",
                                        .{ variable_declaration.name, @tagName(value_type.resolved) },
                                    );

                                //Then we can use the type of the value expression for the type of the variable declaration
                                variable_declaration.type = value.type;
                            }
                            //This should be an impossible scenario, the parser gets mad about this
                            else unreachable;
                        } else {
                            //Resolve the variable declaration type
                            try self.resolveParsedType(
                                variable_declaration.type,
                                script,
                                script_table,
                                a_string_table,
                                false,
                                progress_node,
                            );

                            //If the variable declaration has a value set, resolve the value expression to the type of the variable
                            if (variable_declaration.value) |value| {
                                try self.resolveExpression(
                                    value,
                                    variable_declaration.type,
                                    script,
                                    script_table,
                                    a_string_table,
                                    function_variable_stack,
                                    progress_node,
                                );
                            }
                        }

                        try function_variable_stack.?.stack.put(variable_declaration.name, .{
                            .level = function_variable_stack.?.current_level,
                            .type = variable_declaration.type,
                        });
                    },
                    .expression => |node_expression| {
                        try self.resolveExpression(
                            node_expression,
                            target_type,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                            progress_node,
                        );
                    },
                    .return_statement => |return_statement| {
                        const return_machine_type: MMTypes.MachineType = switch (function_variable_stack.?.function) {
                            .function => |function| switch (self.type_intern_pool.get(function.return_type).resolved) {
                                .fish => |fish| fish.machine_type,
                                .pointer => .s32,
                                else => unreachable,
                            },
                            .constructor => .void,
                        };

                        if (return_machine_type != .void and return_statement.expression == null)
                            std.debug.panic(
                                "you cant return nothing when the function wants {}",
                                .{self.type_intern_pool.get(function_variable_stack.?.function.function.return_type).resolved},
                            );

                        //If this return statement has an expression, type resolve that to the return type of the function
                        if (return_statement.expression) |return_value| {
                            try self.resolveExpression(
                                return_value,
                                switch (function_variable_stack.?.function) {
                                    .function => |function| function.return_type,
                                    .constructor => try self.type_intern_pool.fromFishType(.void),
                                },
                                script,
                                script_table,
                                a_string_table,
                                function_variable_stack,
                                progress_node,
                            );
                        }
                    },
                    .if_statement => |if_statement| {
                        // std.debug.print("condition {}\n", .{if_statement.condition});

                        //Resolve the condition
                        try self.resolveExpression(
                            if_statement.condition,
                            try self.type_intern_pool.fromFishType(.bool),
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                            progress_node,
                        );

                        try self.resolveExpression(
                            if_statement.body,
                            null,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                            progress_node,
                        );

                        if (if_statement.else_body) |else_body| {
                            try self.resolveExpression(
                                else_body,
                                null,
                                script,
                                script_table,
                                a_string_table,
                                function_variable_stack,
                                progress_node,
                            );
                        }
                    },
                    .while_statement => |while_statement| {
                        try self.resolveExpression(
                            while_statement.condition,
                            try self.type_intern_pool.fromFishType(.bool),
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                            progress_node,
                        );

                        try self.resolveExpression(
                            while_statement.body,
                            null,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                            progress_node,
                        );
                    },
                    .inline_asm_statement => |inline_asm| {
                        for (inline_asm.bytecode) |*bytecode| {
                            switch (bytecode.op) {
                                .CALL, .CALLVo, .CALLVsp => |*call_bytecode| {
                                    const call_bytecode_type = self.type_intern_pool.getKey(call_bytecode.type);

                                    const type_name = call_bytecode_type.parsed.name;

                                    try self.resolveParsedType(
                                        call_bytecode.type,
                                        script,
                                        script_table,
                                        a_string_table,
                                        false,
                                        progress_node,
                                    );

                                    call_bytecode.function = blk: {
                                        const referenced_script = script_table.get(type_name).?;

                                        const class = getScriptClassNode(referenced_script.ast);

                                        if (std.mem.eql(u8, call_bytecode.function.name, ".init__")) {
                                            if (class.modifiers.static) {
                                                std.debug.panic("cannot call init func on static class {s}", .{class.name});
                                            }

                                            break :blk .initializer;
                                        }

                                        if (std.mem.startsWith(u8, call_bytecode.function.name, ".ctor__")) {
                                            if (class.modifiers.static) {
                                                std.debug.panic("cannot call ctor on static class", .{});
                                            }

                                            break :blk .{ .constructor = call_bytecode.function.name };
                                        }

                                        for (class.functions) |function| {
                                            try self.resolveFunctionHead(
                                                function,
                                                referenced_script,
                                                script_table,
                                                a_string_table,
                                                progress_node,
                                            );

                                            if (std.mem.eql(u8, call_bytecode.function.name, function.mangled_name.?)) {
                                                break :blk .{ .function = function };
                                            }
                                        }

                                        std.debug.panic("could not find function {s} on type {s}", .{ call_bytecode.function.name, type_name });
                                    };
                                },
                                .GET_SP_NATIVE_MEMBER, .GET_OBJ_MEMBER, .GET_SP_MEMBER, .GET_RP_MEMBER => |get_member_bytecode| {
                                    try self.resolveParsedType(
                                        get_member_bytecode.type,
                                        script,
                                        script_table,
                                        a_string_table,
                                        bytecode.op == .GET_RP_MEMBER,
                                        parent_progress_node,
                                    );
                                },
                                else => {},
                            }
                        }
                    },
                    .@"unreachable" => {},
                    else => |node_type| std.debug.panic("TODO: resolution of node type {s}", .{@tagName(node_type)}),
                }
            }
        },
        .vec2_construction => |vec2_construction| {
            //Resolve the expressions into f32s
            for (vec2_construction) |param| {
                try self.resolveExpression(
                    param,
                    try self.type_intern_pool.fromFishType(.f32),
                    script,
                    script_table,
                    a_string_table,
                    function_variable_stack,
                    progress_node,
                );
            }

            expression.type = try self.type_intern_pool.fromFishType(.vec2);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .vec4_construction => |vec4_construction| {
            //Resolve the expressions into f32s
            for (vec4_construction) |param| {
                try self.resolveExpression(
                    param,
                    try self.type_intern_pool.fromFishType(.f32),
                    script,
                    script_table,
                    a_string_table,
                    function_variable_stack,
                    progress_node,
                );
            }

            expression.type = try self.type_intern_pool.fromFishType(.vec4);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .less_than, .less_than_or_equal, .greater_than, .greater_than_or_equal => |comparison| {
            try self.resolveExpression(
                comparison.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            try self.resolveExpression(
                comparison.righthand,
                comparison.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = try self.type_intern_pool.fromFishType(.bool);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .bitwise_and => |bitwise_and| {
            //Resolve the lefthand expression to whatever type it naturally wants to be
            try self.resolveExpression(
                bitwise_and.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const bitwise_and_lefthand_type = self.type_intern_pool.get(bitwise_and.lefthand.type);

            switch (bitwise_and_lefthand_type.resolved.fish.machine_type) {
                .s32, .bool, .s64 => {},
                else => |tag| std.debug.panic(
                    "lefthand side of bitwise type must be s32, bool, or s64, currently is {s}",
                    .{@tagName(tag)},
                ),
            }

            //Resolve the righthand expression to the same type as the lefthand expression
            try self.resolveExpression(
                bitwise_and.righthand,
                bitwise_and.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = bitwise_and.lefthand.type;
        },
        .not_equal, .equal => |equality| {
            //Resolve the lefthand expression to whatever type it naturally wants to be
            try self.resolveExpression(
                equality.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const equality_lefthand_type = self.type_intern_pool.get(equality.lefthand.type);

            switch (equality_lefthand_type.resolved) {
                .fish => |fish| switch (fish.machine_type) {
                    //NEb   NEc    NEi   NEf   NEs64 NErp      NEo          NEsp
                    .bool, .char, .s32, .f32, .s64, .raw_ptr, .object_ref, .safe_ptr => {},
                    else => |tag| std.debug.panic(
                        "lefthand side of not equal must be .bool, .char, .s32, .f32, .s64, .raw_ptr, .object_ref, currently is {s}",
                        .{@tagName(tag)},
                    ),
                },
                .pointer => {},
                else => unreachable,
            }

            //Resolve the righthand expression to the same type as the lefthand expression
            try self.resolveExpression(
                equality.righthand,
                equality.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = try self.type_intern_pool.fromFishType(.bool);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .logical_and, .logical_or => |logical_op| {
            try self.resolveExpression(
                logical_op.lefthand,
                try self.type_intern_pool.fromFishType(.bool),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            try self.resolveExpression(
                logical_op.righthand,
                try self.type_intern_pool.fromFishType(.bool),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = try self.type_intern_pool.fromFishType(.bool);
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .addition, .subtraction => |math_op| {
            try self.resolveExpression(
                math_op.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const math_op_lefthand_type = self.type_intern_pool.get(math_op.lefthand.type);

            try self.resolveExpression(
                math_op.righthand,
                // For simple math ops, the righthand side should always be an s32
                if (math_op_lefthand_type.resolved == .pointer) try self.type_intern_pool.fromFishType(.s32) else math_op.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            expression.type = math_op.lefthand.type;
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .cast => |cast_expression| {
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );

            try self.resolveExpression(
                cast_expression,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            if (try self.coerceExpression(
                script.ast.allocator,
                expression.type,
                cast_expression,
                script_table,
                a_string_table,
                true,
            )) |new_expression| {
                expression.* = new_expression;
            } else {
                expression.type = expression.type;
            }
        },
        .dereference => |dereference| {
            try self.resolveExpression(
                dereference,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                progress_node,
            );

            const dereference_type = self.type_intern_pool.get(dereference.type);

            std.debug.assert(dereference_type.resolved == .pointer);

            expression.type = if (dereference_type.resolved.pointer.indirection_count > 1) blk: {
                var new_parsed = self.type_intern_pool.getKey(dereference.type).parsed;
                new_parsed.indirection_count -= 1;

                const new_type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = new_parsed });

                break :blk new_type;
            } else try self.type_intern_pool.fromFishType(dereference_type.resolved.pointer.type.fish);

            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .native_strcpy => |native_strcpy| {
            try self.resolveExpression(
                native_strcpy.dst,
                try self.type_intern_pool.fishTypePtr(.s32, 1),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                parent_progress_node,
            );

            // Resolve the string type
            try self.resolveParsedType(
                try self.type_intern_pool.asciiStringType(),
                script,
                script_table,
                a_string_table,
                false,
                parent_progress_node,
            );

            try self.resolveExpression(
                native_strcpy.src,
                try self.type_intern_pool.asciiStringType(),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                parent_progress_node,
            );

            expression.type = try self.type_intern_pool.fromFishType(.void);

            // Resolve the string type
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                parent_progress_node,
            );
        },
        .new_array => |new_array| {
            // Resolve the array size into an s32
            try self.resolveExpression(
                new_array.size,
                try self.type_intern_pool.fromFishType(.s32),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                parent_progress_node,
            );

            // Resolve the child type of the array
            try self.resolveParsedType(
                new_array.child,
                script,
                script_table,
                a_string_table,
                false,
                parent_progress_node,
            );

            // Resolve the type of the array
            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                parent_progress_node,
            );
        },
        .array_access => |array_access| {
            try self.resolveExpression(
                array_access.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                parent_progress_node,
            );

            try self.resolveExpression(
                array_access.righthand,
                try self.type_intern_pool.fromFishType(.s32),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
                parent_progress_node,
            );

            const array_type = self.type_intern_pool.get(array_access.lefthand.type);

            expression.type = if (array_type.resolved.fish.dimension_count > 1) blk: {
                var new_parsed = self.type_intern_pool.getKey(array_access.lefthand.type).parsed;
                new_parsed.indirection_count -= 1;

                const new_type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = new_parsed });

                break :blk new_type;
            } else try self.type_intern_pool.fromFishType(MMTypes.FishType.guessFromMachineType(array_type.resolved.fish.array_base_machine_type));

            try self.resolveParsedType(
                expression.type,
                script,
                script_table,
                a_string_table,
                false,
                parent_progress_node,
            );
        },
        else => |contents| std.debug.panic("TODO: resolution of expression type {s}", .{@tagName(contents)}),
    }

    std.debug.assert(self.type_intern_pool.get(expression.type).* == .resolved);

    if (target_type) |target_parsed_type_index| {
        const target_parsed_type = self.type_intern_pool.get(target_parsed_type_index);

        switch (target_parsed_type.*) {
            .resolved => {
                //If we have a target type, try to coerce to that type
                if (try self.coerceExpression(
                    script.ast.allocator,
                    target_parsed_type_index,
                    expression,
                    script_table,
                    a_string_table,
                    false,
                )) |coersion_expression|
                    expression.* = coersion_expression;
            },
            else => |tag| std.debug.panic("target type must be a resolved type, cannot be {s}", .{@tagName(tag)}),
        }

        const expression_type = self.type_intern_pool.get(expression.type);

        //If we couldnt coerce, then we've hit an unresolvable situation
        if (!target_parsed_type.resolved.eql(expression_type.resolved))
            std.debug.panic("wanted type {}, got type {}", .{ target_parsed_type.resolved, expression_type.resolved });
    }

    std.debug.assert(self.type_intern_pool.get(expression.type).* == .resolved);
}

fn isNumberLike(resolved_type: *const Parser.TypeInternPool.Type) bool {
    return switch (resolved_type.resolved) {
        .null_literal => false,
        .float_literal, .integer_literal, .int_string_literal => true,
        .fish => |fish| switch (fish.machine_type) {
            .s32, .s64, .f32, .f64 => true,
            else => false,
        },
        .pointer => false,
    };
}

fn resolveComptimeInteger(self: *Self, expression: *Parser.Node.Expression) !void {
    std.debug.assert(self.type_intern_pool.get(expression.type).resolved == .integer_literal);

    switch (expression.contents) {
        // Do nothing if its already just an integer literal
        .integer_literal => {},
        .numeric_negation => |negation| {
            try self.resolveComptimeInteger(negation);

            const literal = negation.contents.integer_literal;
            expression.contents = .{ .integer_literal = .{
                .base = literal.base,
                .value = -literal.value,
            } };
            expression.type = negation.type;
        },
        else => |tag| std.debug.panic("cannot resolve integer expression type {s}", .{@tagName(tag)}),
    }
}

fn resolveComptimeFloat(self: *Self, expression: *Parser.Node.Expression) !void {
    std.debug.assert(self.type_intern_pool.get(expression.type).resolved == .float_literal);

    switch (expression.contents) {
        // Do nothing if its already just a float literal
        .float_literal => {},
        //At compile time, resolve the numeric negation
        .numeric_negation => |numeric_negation| {
            try self.resolveComptimeFloat(numeric_negation);

            expression.contents = .{
                .float_literal = .{
                    //Negate the value
                    .value = -numeric_negation.contents.float_literal.value,
                    //Same base as the child type
                    .base = numeric_negation.contents.float_literal.base,
                },
            };
        },
        else => |tag| std.debug.panic("cannot resolve float expression type {s}", .{@tagName(tag)}),
    }
}

///Attempts to coerce the expression to the target type, modifying the expression in the meantime
fn coerceExpression(
    self: *Self,
    allocator: std.mem.Allocator,
    intern_target_type: Parser.TypeInternPool.Index,
    expression: *Parser.Node.Expression,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    hard_cast: bool,
) !?Parser.Node.Expression {
    const expression_type = self.type_intern_pool.get(expression.type).resolved;

    const target_type = self.type_intern_pool.get(intern_target_type).resolved;

    //If the types already match, then do nothing
    if (expression_type.eql(target_type))
        return null;

    switch (target_type) {
        .pointer => |pointer| {
            _ = pointer; // autofix

            // Integer literal -> ptr conversion
            if (expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_ptr = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            // Null literal -> ptr conversion
            if (expression_type == .null_literal) {
                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .null_literal_to_ptr,
                    .type = intern_target_type,
                };
            }
        },
        .fish => |target_fish_type| {
            // Iterate the base classes of the source expression, to try to see if we can implicitly cast the type to its base type
            if (expression_type == .fish and target_fish_type.script != null) {
                var script_name = a_string_table.keys()[expression_type.fish.type_name];

                base_check: while (true) {
                    const parsed_script = script_table.get(script_name).?;

                    const class = getScriptClassNode(parsed_script.ast);

                    // If the GUID of the class is equal to the target type GUID, then we have found the base class, and should return a cast
                    if (class.identifier.?.contents.guid_literal == target_fish_type.script.?.guid) {
                        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                        const cast_target_expression = try allocator.create(Parser.Node.Expression);
                        cast_target_expression.* = expression.*;

                        return .{
                            .contents = .{ .cast = cast_target_expression },
                            .type = intern_target_type,
                        };
                    }

                    switch (class.base_class) {
                        .resolved => |resolved_base_class| {
                            script_name = resolved_base_class.name;
                        },
                        .parsed => unreachable,
                        .none => break :base_check,
                    }
                }
            }

            // Integer literal -> safe_ptr coercion, only allowed in hard casts
            if (hard_cast and target_fish_type.machine_type == .safe_ptr and expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_safe_ptr = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            // Integer literal -> s32 coercion
            if (target_fish_type.machine_type == .s32 and expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_s32 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            // Int string literal -> s32 coercion
            if (target_fish_type.machine_type == .s32 and expression_type == .int_string_literal) {
                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .int_string_literal_to_s32 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            //Integer literal -> s64 coercion
            if (target_fish_type.machine_type == .s64 and expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_s64 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            // Integer literal -> f32 coercion
            if (target_fish_type.machine_type == .f32 and expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_f32 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            //Integer literal -> f64 coercion
            if (target_fish_type.machine_type == .f64 and expression_type == .integer_literal) {
                try self.resolveComptimeInteger(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .integer_literal_to_f64 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            // Float literal -> f32 coercion
            if (target_fish_type.machine_type == .f32 and expression_type == .float_literal) {
                try self.resolveComptimeFloat(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .float_literal_to_f32 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            //Float literal -> f64 coercion
            if (target_fish_type.machine_type == .f64 and expression_type == .float_literal) {
                try self.resolveComptimeFloat(expression);

                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .{ .float_literal_to_f64 = cast_target_expression },
                    .type = intern_target_type,
                };
            }

            if (expression_type == .fish) {
                // bool -> s32 conversion
                if (target_fish_type.machine_type == .s32 and expression_type.fish.machine_type == .bool) {
                    //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                    const cast_target_expression = try allocator.create(Parser.Node.Expression);
                    cast_target_expression.* = expression.*;

                    return .{
                        .contents = .{ .cast = cast_target_expression },
                        .type = intern_target_type,
                    };
                }
            }

            // Null -> safe_ptr conversion
            if (target_fish_type.machine_type == .safe_ptr and expression_type == .null_literal) {
                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .null_literal_to_safe_ptr,
                    .type = intern_target_type,
                };
            }

            // Null -> object_ref conversion
            if (target_fish_type.machine_type == .object_ref and expression_type == .null_literal) {
                //Dupe the source expression since this pointer will get overwritten later on with the value that we return
                const cast_target_expression = try allocator.create(Parser.Node.Expression);
                cast_target_expression.* = expression.*;

                return .{
                    .contents = .null_literal_to_object_ref,
                    .type = intern_target_type,
                };
            }
        },
        else => {},
    }

    return null;
}

fn findFunction(
    self: *Self,
    name: []const u8,
    calling_parameters: []const *Parser.Node.Expression,
    allocator: std.mem.Allocator,
    source_expression: ?*Parser.Node.Expression,
    calling_script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) !struct {
    found_function: *Parser.Node.Function,
    special_cased: bool,
    owning_type: MMTypes.TypeReference,
} {
    var candidates = std.ArrayList(struct { *Parser.Node.Function, MMTypes.TypeReference }).init(allocator);
    defer candidates.deinit();

    if (source_expression) |function_source_expression| {
        const function_source_expression_contents_type = function_source_expression.contents;
        const function_source_expression_type = self.type_intern_pool.get(function_source_expression.type);

        switch (function_source_expression_contents_type) {
            .class_access => |class| {
                const type_script = script_table.get(class).?;

                const type_class = getScriptClassNode(type_script.ast);

                //TODO: this needs to recurse the types, so you can call parent type static methods aswell
                for (type_class.functions) |function| {
                    // Skip non static functions, since we are calling directly on a class
                    if (!function.modifiers.static)
                        continue;

                    // If we found the function, return it
                    if (std.mem.eql(u8, function.name, name)) {
                        // Type resolve the function
                        try self.resolveFunctionHead(
                            function,
                            type_script,
                            script_table,
                            a_string_table,
                            parent_progress_node,
                        );

                        // Skip overloads which have the wrong count of parameters
                        if (function.parameters.len != calling_parameters.len)
                            continue;

                        try candidates.append(.{
                            function,
                            try typeReferenceFromScript(type_script, 0, a_string_table),
                        });
                    }
                }
            },
            else => {
                switch (function_source_expression_type.resolved) {
                    .fish => |fish| {
                        switch (fish.machine_type) {
                            .object_ref, .safe_ptr => {
                                const source_class_name = a_string_table.keys()[fish.type_name];

                                //Recursively go through all the parent classes, and try to find a matching function
                                var source_script = script_table.get(source_class_name).?;
                                var class = getScriptClassNode(source_script.ast);
                                while (true) {
                                    for (class.functions) |function| {
                                        //If the function name is wrong, continue
                                        if (!std.mem.eql(u8, function.name, name))
                                            continue;

                                        // If the parameter count is not equal to the calling parameter count, or
                                        // if its static and the calling parameter count is does not equal parameter count - 1
                                        // (eg. special cased static functions which act as member functions), then skip this overload
                                        if (function.parameters.len != calling_parameters.len and
                                            !(function.modifiers.static and
                                            function.parameters.len > 0 and
                                            function.parameters.len - 1 == calling_parameters.len))
                                        {
                                            // std.debug.print("skipping... cus param mismatch\n", .{});
                                            continue;
                                        }

                                        // Type resolve the function
                                        try self.resolveFunctionHead(
                                            function,
                                            source_script,
                                            script_table,
                                            a_string_table,
                                            parent_progress_node,
                                        );

                                        // You usually cant call static functions as member functions, aside from the case of
                                        if (function.modifiers.static) {
                                            //The first parameter being the class being used as the source can be called with this syntax
                                            //since these are special cased to be callable as member functions
                                            if (function.parameters.len > 0) blk: {
                                                const first_parameter = function.parameters[0];

                                                const first_parameter_type = self.type_intern_pool.get(first_parameter.type);

                                                //If the first parameter is not an object ref or safe ptr, then its definitely not special cased
                                                if (first_parameter_type.resolved.fish.machine_type != .safe_ptr and
                                                    first_parameter_type.resolved.fish.machine_type != .object_ref)
                                                    break :blk;

                                                //If its a safe_ptr or object_ref, the type name should always be known
                                                std.debug.assert(first_parameter_type.resolved.fish.type_name != 0xFFFFFFFF);

                                                const type_name = a_string_table.keys()[first_parameter_type.resolved.fish.type_name];

                                                // If the calling script derives the parameter's script, then this *can* be a member function, allow through
                                                if (scriptDerivesOtherScript(
                                                    calling_script,
                                                    script_table.get(type_name).?,
                                                    script_table,
                                                )) try candidates.append(.{
                                                    function,
                                                    try typeReferenceFromScript(source_script, 0, a_string_table),
                                                });

                                                continue;
                                            }

                                            std.debug.panic("you cant member call a static function {s}, sorry bub", .{function.name});
                                        }

                                        try candidates.append(.{
                                            function,
                                            try typeReferenceFromScript(source_script, 0, a_string_table),
                                        });
                                    }

                                    if (class.base_class == .none)
                                        break;

                                    source_script = script_table.get(class.base_class.resolved.name).?;
                                    class = getScriptClassNode(source_script.ast);
                                }
                            },
                            else => |machine_type| std.debug.panic(
                                "you cannot do field access on a variable with machine type {s}. must be object_ref or safe_ptr",
                                .{@tagName(machine_type)},
                            ),
                        }
                    },

                    else => |tag| std.debug.panic("cant call member function on {s}", .{@tagName(tag)}),
                }
            },
        }
    }

    if (candidates.items.len == 1) {
        const candidate = candidates.items[0];

        // Since the param count should *always* match unless its special cased, use that as the determining factor here
        return if (calling_parameters.len + 1 == candidate[0].parameters.len)
            .{ .found_function = candidate[0], .special_cased = true, .owning_type = candidate[1] }
        else
            .{ .found_function = candidate[0], .special_cased = false, .owning_type = candidate[1] };
    } else if (candidates.items.len > 1) {
        //Find which parameters differ between the overloads
        var checked_parameters = try allocator.alloc(?*Parser.Node.Function.Parameter, calling_parameters.len);

        for (candidates.items, 0..) |candidate, i| {
            // Since the param count should *always* match unless its special cased, use that as the determining factor here
            const is_special_cased = calling_parameters.len + 1 == candidate[0].parameters.len;

            const candidate_params = if (is_special_cased) candidate[0].parameters[1..] else candidate[0].parameters;

            if (i == 0) {
                for (candidate_params, 0..) |*param, j| {
                    checked_parameters[j] = param;
                }

                continue;
            }

            for (candidate_params, checked_parameters) |candidate_param, *current_param_ptr| {
                if (current_param_ptr.*) |curr_param| {
                    if (!curr_param.eql(candidate_param)) {
                        current_param_ptr.* = null;
                    }
                }
            }
        }

        // std.debug.print("{any}\n", .{checked_parameters});

        std.debug.panic("this is a massive TODO rn", .{});
    }

    // std.debug.print("calling params: {any}\n", .{calling_parameters});

    std.debug.panic("no function found with name {s} and param count {d}", .{ name, calling_parameters.len });
}

fn scriptDerivesOtherScript(script: *ParsedScript, other: *ParsedScript, script_table: *ParsedScriptTable) bool {
    if (std.mem.eql(u8, script.class_name, other.class_name))
        return true;

    var class = getScriptClassNode(script.ast);
    while (true) {
        if (class.base_class == .none)
            break;

        if (std.mem.eql(u8, class.base_class.resolved.name, other.class_name))
            return true;

        class = getScriptClassNode(script_table.get(class.base_class.resolved.name).?.ast);
    }

    return false;
}

fn resolveParsedType(
    self: *Self,
    type_index: Parser.TypeInternPool.Index,
    script: ?*ParsedScript,
    script_table: ?*ParsedScriptTable,
    a_string_table: ?*AStringTable,
    is_raw_ptr: bool,
    parent_progress_node: std.Progress.Node,
) Error!void {
    const intern_type = self.type_intern_pool.getMutable(type_index);

    if (intern_type.* == .resolved)
        return;

    const parsed_type = intern_type.parsed;

    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving parsed type {s}", .{parsed_type.name}), 0);
    defer progress_node.end();

    // TODO: raw_ptr class stubs? this hack shouldnt be here
    if (is_raw_ptr) {
        intern_type.* = .{
            .resolved = .{
                .fish = .{
                    .type_name = @intCast((try a_string_table.?.getOrPut(parsed_type.name)).index),
                    .script = null,
                    .machine_type = .raw_ptr,
                    .fish_type = .void,
                    .dimension_count = parsed_type.dimension_count,
                    .array_base_machine_type = .void,
                },
            },
        };

        return;
    }

    if (parsed_type.indirection_count > 0) {
        intern_type.* = .{
            .resolved = .{
                .pointer = .{
                    //TODO: arrays of pointers?
                    .indirection_count = parsed_type.indirection_count,
                    .type = .{ .fish = std.meta.stringToEnum(MMTypes.FishType, parsed_type.name).? },
                    .fish = .{
                        .array_base_machine_type = .void,
                        .dimension_count = 0,
                        .fish_type = .s32,
                        .machine_type = .s32,
                        .script = null,
                        .type_name = @intCast((try a_string_table.?.getOrPut("ptr")).index),
                    },
                },
            },
        };

        return;
    }

    if (std.meta.stringToEnum(MMTypes.FishType, parsed_type.name)) |fish_type| {
        intern_type.* = .{
            .resolved = .{
                .fish = if (parsed_type.dimension_count > 0)
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
                    },
            },
        };

        return;
    } else {
        if (parsed_type.base_type) |base_type| {
            if (script.?.imported_types.get(base_type) != null) {
                const referenced_script = script_table.?.get(base_type).?;

                const class = getScriptClassNode(referenced_script.ast);

                for (class.enums) |enumeration| {
                    if (std.mem.eql(u8, enumeration.name, parsed_type.name)) {
                        intern_type.* = .{ .resolved = .{ .fish = try self.typeReferenceFromScriptEnum(
                            referenced_script,
                            script_table.?,
                            enumeration,
                            parsed_type.dimension_count,
                            a_string_table.?,
                            progress_node,
                        ) } };

                        return;
                    }
                }
            }
        } else if (script.?.imported_types.get(parsed_type.name) != null) {
            const referenced_script = script_table.?.get(parsed_type.name).?;

            intern_type.* = .{ .resolved = .{ .fish = try typeReferenceFromScript(
                referenced_script,
                parsed_type.dimension_count,
                a_string_table.?,
            ) } };

            return;
        }

        std.debug.panic("no script {s} ({?s}) found in import table for script {s}", .{ parsed_type.name, parsed_type.base_type, script.?.class_name });
    }

    unreachable;
}

pub fn typeReferenceFromScriptEnum(
    self: *Self,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    enumeration: *Parser.Node.Enum,
    dimension_count: u8,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!MMTypes.TypeReference {
    //Qualify the name
    const qualified_name = try std.fmt.allocPrint(script.ast.allocator, "{s}.{s}", .{ script.class_name, enumeration.name });

    //Get the string table index of the name of this enum
    const name_idx = try a_string_table.getOrPut(qualified_name);

    const backing_type = self.type_intern_pool.get(enumeration.backing_type);

    // Resolve the enumeration's backing type, if needed
    try self.resolveParsedType(
        enumeration.backing_type,
        script,
        script_table,
        a_string_table,
        false,
        parent_progress_node,
    );

    const base_type = backing_type.resolved.fish;

    return if (dimension_count > 0)
        MMTypes.TypeReference{
            .array_base_machine_type = base_type.machine_type,
            .dimension_count = dimension_count,
            .fish_type = .void,
            .machine_type = .object_ref,
            .type_name = @intCast(name_idx.index),
            .script = null,
        }
    else
        MMTypes.TypeReference{
            .array_base_machine_type = .void,
            .dimension_count = 0,
            .fish_type = base_type.fish_type,
            .machine_type = base_type.machine_type,
            .type_name = @intCast(name_idx.index),
            .script = null,
        };
}

pub fn typeReferenceFromScript(
    script: *ParsedScript,
    dimension_count: u8,
    a_string_table: *AStringTable,
) Error!MMTypes.TypeReference {
    //Get the idx of the name of this script, or put into the string table
    const name_idx = try a_string_table.getOrPut(script.class_name);

    return if (dimension_count > 0)
        MMTypes.TypeReference{
            .array_base_machine_type = if (script.is_thing) .safe_ptr else .object_ref,
            .dimension_count = dimension_count,
            .fish_type = .void,
            .machine_type = .object_ref,
            .type_name = @intCast(name_idx.index),
            .script = script.resource_identifier,
        }
    else
        MMTypes.TypeReference{
            .array_base_machine_type = .void,
            .dimension_count = 0,
            .fish_type = .void,
            .machine_type = if (script.is_thing) .safe_ptr else .object_ref,
            .type_name = @intCast(name_idx.index),
            .script = script.resource_identifier,
        };
}

fn resolveField(
    self: *Self,
    field: *Parser.Node.Field,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    parent_progress_node: std.Progress.Node,
) Error!void {
    var progress_node_name_buf: [256]u8 = undefined;

    const progress_node = parent_progress_node.start(try std.fmt.bufPrint(&progress_node_name_buf, "Resolving field {s}", .{field.name}), 0);
    defer progress_node.end();

    // std.debug.print("type resolving field {s}\n", .{field.name});

    var field_type = self.type_intern_pool.get(field.type);

    //early return if its already resolved
    if (field_type.* == .resolved)
        return;

    switch (field_type.*) {
        .parsed => {
            try self.resolveParsedType(
                field.type,
                script,
                script_table,
                a_string_table,
                false,
                progress_node,
            );
        },
        .unknown => {
            if (field.default_value) |default_value| {
                //Resolve the type of the default value, with no specific target type in mind
                try self.resolveExpression(
                    default_value,
                    null,
                    script,
                    script_table,
                    a_string_table,
                    null,
                    progress_node,
                );

                //Set the type of the field to the resolved default value type
                field.type = default_value.type;
            } else unreachable;
        },
        else => unreachable,
    }

    field_type = self.type_intern_pool.get(field.type);

    // std.debug.print("resolved as {}\n", .{field_type});

    //Assert the type was actually resolved
    std.debug.assert(field_type.* == .resolved);

    //Panic if the resolved type is not knowable at runtime
    if (field_type.resolved != .fish)
        std.debug.panic(
            "field {s}'s default value's is unknown at runtime, currently is {s}",
            .{ field.name, @tagName(field_type.resolved) },
        );
}

pub fn mangleFunctionName(
    self: *Self,
    writer: anytype,
    a_string_table: *const AStringTable,
    name: []const u8,
    parameters: []const Parser.Node.Function.Parameter,
) !void {
    try writer.writeAll(name);
    try writer.writeAll("__");

    for (parameters) |parameter| {
        const parameter_type = self.type_intern_pool.get(parameter.type);

        const parameter_fish_type = switch (parameter_type.resolved) {
            .fish => |fish| fish,
            .pointer => |pointer| pointer.fish.?,
            else => @panic("cant parameter this type, sorry"),
        };

        for (0..parameter_fish_type.dimension_count) |_|
            try writer.writeByte('[');

        if (parameter_fish_type.type_name != 0xFFFFFFFF) {
            const type_name = a_string_table.keys()[parameter_fish_type.type_name];

            try writer.writeByte('Q');
            try std.fmt.formatInt(
                type_name.len,
                10,
                .lower,
                .{},
                writer,
            );

            try writer.writeAll(type_name);
        } else {
            try writer.writeByte(parameter_fish_type.fish_type.toMangledId());
        }
    }
}
