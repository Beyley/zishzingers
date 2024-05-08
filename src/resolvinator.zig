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

fn collectImportedTypes(class_name: []const u8, defined_libraries: Libraries, script_table: *ParsedScriptTable) Error!void {
    const script = script_table.get(class_name) orelse @panic("tsheointeonhsaoi");

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

                    const class = getScriptClassNode(ast);

                    if (script_table.get(class.name) == null)
                        try recursivelyResolveScript(ast, defined_libraries, script_table, null);

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

    std.debug.print("resolving script {s}\n", .{script.class_name});

    //Collect all the libraries which are imported by the script
    try collectImportedLibraries(script, defined_libraries);

    //Collect all the script types which are imported
    try collectImportedTypes(script.class_name, defined_libraries, script_table);

    script.is_thing = isScriptThing(script, script_table);

    if (class.base_class == .parsed) {
        const parsed_base_class = class.base_class.parsed;

        _ = script.imported_types.get(parsed_base_class) orelse std.debug.panic("base class {s} not found\n", .{parsed_base_class});

        const base_script = script_table.get(parsed_base_class).?;

        class.base_class = .{ .resolved = .{
            .ident = base_script.resource_identifier,
            .name = parsed_base_class,
        } };
    }

    std.debug.print("script {s} is {} thing/notthing\n", .{ script.class_name, script.is_thing });
}

///Figures out if a script extends Thing, checks the full inheritance chain
fn isScriptThing(script: *const ParsedScript, script_table: *const ParsedScriptTable) bool {
    if (std.mem.eql(u8, script.class_name, "Thing"))
        return true;

    const class = getScriptClassNode(script.ast);

    if (class.base_class == .parsed) {
        return isScriptThing(script_table.get(class.base_class.parsed).?, script_table);
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
) Error!void {
    var script_table = ParsedScriptTable.init(tree.allocator);
    defer script_table.deinit();

    try recursivelyResolveScript(tree, defined_libraries, &script_table, script_identifier);

    //Get the class of the script
    const class = getScriptClassNode(tree);

    class.type_reference = (try typeReferenceFromScript(script_table.get(class.name).?, 0, a_string_table)).runtime_type;

    const script = script_table.get(class.name) orelse unreachable;

    std.debug.print("type resolving {s}\n", .{script.class_name});

    for (class.fields) |field| {
        try resolveField(field, script, &script_table, a_string_table);
    }

    for (class.functions) |function| {
        try resolveFunctionHead(function, script, &script_table, a_string_table);
        try resolveFunctionBody(function, script, &script_table, a_string_table);
    }
}

fn resolveFunctionBody(
    function: *Parser.Node.Function,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) Error!void {
    if (function.body) |body| {
        var variable_stack = FunctionVariableStack.init(script.ast.allocator);
        defer variable_stack.deinit();

        var stack_info: FunctionVariableStackInfo = .{
            .current_level = 0,
            .stack = &variable_stack,
            .function = function,
        };

        if (body.type != .resolved)
            //TODO: once i parse the `=>` syntax for function bodies, this `null` for target type needs to be made correct!!!
            //      should i make function_body a special expression type? im not sure yet.
            //      maybe this could be as simple as "if block, target type == void, if not block, target type is the function return type" that should work
            try resolveExpression(
                body,
                null,
                script,
                script_table,
                a_string_table,
                &stack_info,
            );
    }

    std.debug.print("resolved function body {s}\n", .{function.name});
}

fn resolveFunctionHead(
    function: *Parser.Node.Function,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) Error!void {
    std.debug.print("resolving function head {s}\n", .{function.name});

    if (function.return_type != .resolved)
        function.return_type = .{
            .resolved = try resolveParsedType(
                function.return_type.parsed,
                script,
                script_table,
                a_string_table,
            ),
        };

    std.debug.print("resolved function return type as {}\n", .{function.return_type.resolved});

    for (function.parameters) |*parameter| {
        if (parameter.type != .resolved)
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

    std.debug.print("resolved function head {s}\n", .{function.name});

    var mangled_name = std.ArrayList(u8).init(script_table.allocator);
    try mangleFunctionName(
        mangled_name.writer(),
        a_string_table,
        function.name,
        function.parameters,
    );
    function.mangled_name = mangled_name.items;
}

fn stringType(a_string_table: *AStringTable) Error!Parser.Type.Resolved {
    return .{ .runtime_type = .{
        .script = .{ .guid = 16491 },
        .type_name = @intCast((try a_string_table.getOrPut("String")).index),
        .machine_type = .object_ref,
        .fish_type = .void,
        .dimension_count = 0,
        .array_base_machine_type = .void,
    } };
}

const FunctionVariableStack = std.StringArrayHashMap(struct {
    level: u8,
    type: Parser.Type,
});

const FunctionVariableStackInfo = struct {
    const StackLevel = u8;

    function: *Parser.Node.Function,
    current_level: StackLevel,
    stack: *FunctionVariableStack,
};

fn resolveExpression(
    expression: *Parser.Node.Expression,
    target_type: ?Parser.Type,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
    function_variable_stack: ?*FunctionVariableStackInfo,
) Error!void {
    //If this expression has already been resolved, do nothing
    if (expression.type == .resolved) {
        if (target_type) |target_parsed_type|
            if (!target_parsed_type.resolved.eql(expression.type.resolved))
                std.debug.panic("wanted type {}, got type {}", .{ target_parsed_type.resolved, expression.type.resolved });

        return;
    }

    std.debug.print("resolving {}\n", .{expression.contents});

    switch (expression.contents) {
        .integer_literal => {
            expression.type = .{ .resolved = .integer_literal };
        },
        .float_literal => {
            expression.type = .{ .resolved = .float_literal };
        },
        .null_literal => {
            expression.type = .{ .resolved = .null_literal };
        },
        .assignment => |assignment| {
            //Resolve the type of the destination
            try resolveExpression(
                assignment.destination,
                target_type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            std.debug.assert(assignment.destination.type.resolved == .runtime_type);

            //Resolve the type of the value, which should be the same type as the destination
            try resolveExpression(
                assignment.value,
                assignment.destination.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            // The type of the expression is the type of the destination value
            // This is to allow constructs like `func(a = 2);`
            expression.type = assignment.destination.type;
        },
        .field_access => |field_access| {
            //Resolve the source expression
            try resolveExpression(
                field_access.source,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            const field = switch (field_access.source.type.resolved.runtime_type.machine_type) {
                .object_ref, .safe_ptr => field_resolution: {
                    const source_class_name = a_string_table.keys()[field_access.source.type.resolved.runtime_type.type_name];

                    const source_script = script_table.get(source_class_name).?;

                    const source_class = getScriptClassNode(source_script.ast);

                    for (source_class.fields) |field| {
                        //If the field name is wrong, continue
                        if (!std.mem.eql(u8, field.name, field_access.field))
                            continue;

                        // Type resolve the field
                        try resolveField(
                            field,
                            source_script,
                            script_table,
                            a_string_table,
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

                expression.* = .{
                    .contents = .{ .class_access = class.class_name },
                    .type = typeFromName(variable_or_class_access),
                };
            } else {
                if (!function_variable_stack.?.function.modifiers.static and std.mem.eql(u8, variable_or_class_access, "this")) {
                    expression.* = .{
                        .contents = .{ .variable_access = variable_or_class_access },
                        .type = .{ .resolved = try typeReferenceFromScript(
                            script,
                            0,
                            a_string_table,
                        ) },
                    };
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
            expression.type = .{
                .resolved = try resolveParsedType(
                    Parser.Type.Parsed.Bool,
                    script,
                    script_table,
                    a_string_table,
                ),
            };
        },
        .wide_string_literal => {
            expression.type = .{ .resolved = try stringType(a_string_table) };
        },
        .function_call => |*function_call| {
            if (function_call.source != null) {
                //Resolve the source expression
                try resolveExpression(
                    function_call.source.?,
                    null,
                    script,
                    script_table,
                    a_string_table,
                    function_variable_stack,
                );

                const function = try findFunction(
                    function_call.function.name,
                    function_call.parameters,
                    script.ast.allocator,
                    function_call.source,
                    script,
                    script_table,
                    a_string_table,
                );
                std.debug.print("found function {s}, special cased? {}\n", .{ function.found_function.name, function.special_cased });

                const function_parameters_to_check = if (function.special_cased)
                    function.found_function.parameters[1..]
                else
                    function.found_function.parameters;

                for (function_parameters_to_check, function_call.parameters) |function_parameter, call_parameter| {
                    try resolveExpression(
                        call_parameter,
                        function_parameter.type,
                        script,
                        script_table,
                        a_string_table,
                        function_variable_stack,
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
                std.debug.print("found func {s} for call {s}\n", .{ function.name, function_call.function.name });
                try resolveFunctionHead(
                    function,
                    function_script,
                    script_table,
                    a_string_table,
                );

                function_call.function = .{
                    .function = .{
                        .function = function,
                        .owning_type = (try typeReferenceFromScript(
                            function_script,
                            0,
                            a_string_table,
                        )).runtime_type,
                    },
                };

                // Resolve all the call parameter expressions to the types of the function parameters
                for (function.parameters, function_call.parameters) |parameter, call_parameter| {
                    try resolveExpression(
                        call_parameter,
                        parameter.type,
                        script,
                        script_table,
                        a_string_table,
                        function_variable_stack,
                    );
                }

                expression.type = function.return_type;
            }
        },
        .logical_negation => |logical_negation| {
            // Resolve the sub-expression of the negation
            try resolveExpression(
                logical_negation,
                boolType(),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = logical_negation.type;
        },
        .numeric_negation => |numeric_negation| {
            // Resolve the sub-expression of the negation
            try resolveExpression(
                numeric_negation,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            if (!isNumberLike(numeric_negation.type.resolved))
                std.debug.panic("numeric negation is not number-like", .{});

            expression.type = numeric_negation.type;
        },
        .block => |block| {
            expression.type = .{
                .resolved = try resolveParsedType(
                    Parser.Type.Parsed.Void,
                    script,
                    script_table,
                    a_string_table,
                ),
            };

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

            //Only add the function parameters on the first scope level (eg. function scope)
            if (function_variable_stack.?.current_level == 1)
                for (function_variable_stack.?.function.parameters) |parameter| {
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
                                try resolveExpression(
                                    value,
                                    null,
                                    script,
                                    script_table,
                                    a_string_table,
                                    function_variable_stack,
                                );

                                //Panic if the resolved type is not knowable at runtime
                                if (value.type.resolved != .runtime_type)
                                    std.debug.panic(
                                        "variable declaration {s}'s default value's is unknown at runtime, currently is {s}",
                                        .{ variable_declaration.name, @tagName(value.type.resolved) },
                                    );

                                //Then we can use the type of the value expression for the type of the variable declaration
                                variable_declaration.type = value.type;
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
                                    function_variable_stack,
                                );
                            }
                        }

                        try function_variable_stack.?.stack.put(variable_declaration.name, .{
                            .level = function_variable_stack.?.current_level,
                            .type = variable_declaration.type,
                        });
                    },
                    .expression => |node_expression| {
                        try resolveExpression(
                            node_expression,
                            target_type,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                        );
                    },
                    .return_statement => |return_statement| {
                        if (function_variable_stack.?.function.return_type.resolved.runtime_type.machine_type != .void and return_statement.expression == null)
                            std.debug.panic("you cant return nothing when the function wants {}", .{function_variable_stack.?.function.return_type.resolved});

                        //If this return statement has an expression, type resolve that to the return type of the function
                        if (return_statement.expression) |return_value| {
                            try resolveExpression(
                                return_value,
                                function_variable_stack.?.function.return_type,
                                script,
                                script_table,
                                a_string_table,
                                function_variable_stack,
                            );
                        }
                    },
                    .if_statement => |if_statement| {
                        std.debug.print("condition {}\n", .{if_statement.condition});

                        //Resolve the condition
                        try resolveExpression(
                            if_statement.condition,
                            boolType(),
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                        );

                        try resolveExpression(
                            if_statement.body,
                            null,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                        );

                        if (if_statement.else_body) |else_body| {
                            try resolveExpression(
                                else_body,
                                null,
                                script,
                                script_table,
                                a_string_table,
                                function_variable_stack,
                            );
                        }
                    },
                    .while_statement => |while_statement| {
                        try resolveExpression(
                            while_statement.condition,
                            boolType(),
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                        );

                        try resolveExpression(
                            while_statement.body,
                            null,
                            script,
                            script_table,
                            a_string_table,
                            function_variable_stack,
                        );
                    },
                    else => |node_type| std.debug.panic("TODO: resolution of expression type {s}", .{@tagName(node_type)}),
                }
            }
        },
        .vec2_construction => |vec2_construction| {
            //Resolve the expressions into f32s
            for (vec2_construction) |param| {
                try resolveExpression(
                    param,
                    f32Type(),
                    script,
                    script_table,
                    a_string_table,
                    function_variable_stack,
                );
            }

            expression.type = vec2Type();
        },
        .less_than, .less_than_or_equal, .greater_than, .greater_than_or_equal => |comparison| {
            try resolveExpression(
                comparison.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            try resolveExpression(
                comparison.righthand,
                comparison.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = boolType();
        },
        .bitwise_and => |bitwise_and| {
            //Resolve the lefthand expression to whatever type it naturally wants to be
            try resolveExpression(
                bitwise_and.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            switch (bitwise_and.lefthand.type.resolved.runtime_type.machine_type) {
                .s32, .bool, .s64 => {},
                else => |tag| std.debug.panic(
                    "lefthand side of bitwise type must be s32, bool, or s64, currently is {s}",
                    .{@tagName(tag)},
                ),
            }

            //Resolve the righthand expression to the same type as the lefthand expression
            try resolveExpression(
                bitwise_and.righthand,
                bitwise_and.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = .{ .resolved = bitwise_and.lefthand.type.resolved };
        },
        .not_equal => |not_equal| {
            //Resolve the lefthand expression to whatever type it naturally wants to be
            try resolveExpression(
                not_equal.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            switch (not_equal.lefthand.type.resolved.runtime_type.machine_type) {
                //NEb   NEc    NEi   NEf   NEs64 NErp      NEo          NEsp
                .bool, .char, .s32, .f32, .s64, .raw_ptr, .object_ref, .safe_ptr => {},
                else => |tag| std.debug.panic(
                    "lefthand side of not equal must be .bool, .char, .s32, .f32, .s64, .raw_ptr, .object_ref, currently is {s}",
                    .{@tagName(tag)},
                ),
            }

            //Resolve the righthand expression to the same type as the lefthand expression
            try resolveExpression(
                not_equal.righthand,
                not_equal.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = boolType();
        },
        .logical_and, .logical_or => |logical_op| {
            try resolveExpression(
                logical_op.lefthand,
                boolType(),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            try resolveExpression(
                logical_op.righthand,
                boolType(),
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = boolType();
        },
        .addition, .subtraction => |math_op| {
            try resolveExpression(
                math_op.lefthand,
                null,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            try resolveExpression(
                math_op.righthand,
                math_op.lefthand.type,
                script,
                script_table,
                a_string_table,
                function_variable_stack,
            );

            expression.type = math_op.lefthand.type;
        },
        else => |contents| std.debug.panic("TODO: resolution of expression type {s}", .{@tagName(contents)}),
    }

    std.debug.assert(expression.type == .resolved);

    if (target_type) |target_parsed_type| {
        switch (target_parsed_type) {
            .resolved => {
                //If we have a target type, try to coerce to that type
                if (try coerceExpression(
                    script.ast.allocator,
                    target_parsed_type.resolved,
                    expression,
                )) |coersion_expression|
                    expression.* = coersion_expression;
            },
            else => |tag| std.debug.panic("target type must be a resolved type, cannot be {s}", .{@tagName(tag)}),
        }

        //If we couldnt coerce, then we've hit an unresolvable situation
        if (!target_parsed_type.resolved.eql(expression.type.resolved))
            std.debug.panic("wanted type {}, got type {}", .{ target_parsed_type.resolved, expression.type.resolved });
    }

    std.debug.assert(expression.type == .resolved);
}

fn isNumberLike(resolved_type: Parser.Type.Resolved) bool {
    return switch (resolved_type) {
        .type, .null_literal => false,
        .float_literal, .integer_literal => true,
        .runtime_type => |runtime_type| switch (runtime_type.machine_type) {
            .s32, .s64, .f32, .f64 => true,
            else => false,
        },
    };
}

fn typeFromName(name: []const u8) Parser.Type {
    return .{ .resolved = .{ .type = name } };
}

fn boolType() Parser.Type {
    return comptime .{ .resolved = resolveParsedType(Parser.Type.Parsed.Bool, null, null, null) catch unreachable };
}

fn vec2Type() Parser.Type {
    return .{ .resolved = resolveParsedType(
        Parser.Type.Parsed.Vec2,
        null,
        null,
        null,
    ) catch unreachable };
}

fn f32Type() Parser.Type {
    return .{ .resolved = resolveParsedType(
        Parser.Type.Parsed.F32,
        null,
        null,
        null,
    ) catch unreachable };
}

fn resolveComptimeInteger(expression: *Parser.Node.Expression) !void {
    std.debug.assert(expression.type.resolved == .integer_literal);

    switch (expression.contents) {
        // Do nothing if its already just an integer literal
        .integer_literal => {},
        .numeric_negation => |negation| {
            try resolveComptimeInteger(negation);

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

fn resolveComptimeFloat(expression: *Parser.Node.Expression) !void {
    std.debug.assert(expression.type.resolved == .float_literal);

    switch (expression.contents) {
        // Do nothing if its already just a float literal
        .float_literal => {},
        //At compile time, resolve the numeric negation
        .numeric_negation => |numeric_negation| {
            try resolveComptimeFloat(numeric_negation);

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
    allocator: std.mem.Allocator,
    target_type: Parser.Type.Resolved,
    expression: *Parser.Node.Expression,
) !?Parser.Node.Expression {
    const expression_type = expression.type.resolved;

    //If the types already match, then do nothing
    if (expression_type.eql(target_type))
        return null;

    // Integer literal -> s32 coercion
    if (target_type.runtime_type.machine_type == .s32 and expression_type == .integer_literal) {
        try resolveComptimeInteger(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .integer_literal_to_s32 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    //Integer literal -> s64 coercion
    if (target_type.runtime_type.machine_type == .s64 and expression_type == .integer_literal) {
        try resolveComptimeInteger(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .integer_literal_to_s64 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    // Integer literal -> f32 coercion
    if (target_type.runtime_type.machine_type == .f32 and expression_type == .integer_literal) {
        try resolveComptimeInteger(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .integer_literal_to_f32 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    //Integer literal -> f64 coercion
    if (target_type.runtime_type.machine_type == .f64 and expression_type == .integer_literal) {
        try resolveComptimeInteger(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .integer_literal_to_f64 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    // Float literal -> f32 coercion
    if (target_type.runtime_type.machine_type == .f32 and expression_type == .float_literal) {
        try resolveComptimeFloat(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .float_literal_to_f32 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    //Float literal -> f64 coercion
    if (target_type.runtime_type.machine_type == .f64 and expression_type == .float_literal) {
        try resolveComptimeFloat(expression);

        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .float_literal_to_f64 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    // bool -> s32 conversion
    if (target_type.runtime_type.machine_type == .s32 and expression_type.runtime_type.machine_type == .bool) {
        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .{ .bool_to_s32 = cast_target_expression },
            .type = .{ .resolved = target_type },
        };
    }

    // Null -> safe_ptr conversion
    if (target_type.runtime_type.machine_type == .safe_ptr and expression_type == .null_literal) {
        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .null_literal_to_safe_ptr,
            .type = .{ .resolved = target_type },
        };
    }

    // Null -> object_ptr conversion
    if (target_type.runtime_type.machine_type == .object_ref and expression_type == .null_literal) {
        //Dupe the source expression since this pointer will get overwritten later on with the value that we return
        const cast_target_expression = try allocator.create(Parser.Node.Expression);
        cast_target_expression.* = expression.*;

        return .{
            .contents = .null_literal_to_object_ptr,
            .type = .{ .resolved = target_type },
        };
    }

    return null;
}

fn findFunction(
    name: []const u8,
    calling_parameters: []const *Parser.Node.Expression,
    allocator: std.mem.Allocator,
    source_expression: ?*Parser.Node.Expression,
    calling_script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) !struct {
    found_function: *Parser.Node.Function,
    special_cased: bool,
    owning_type: MMTypes.TypeReference,
} {
    var candidates = std.ArrayList(struct { *Parser.Node.Function, MMTypes.TypeReference }).init(allocator);
    defer candidates.deinit();

    if (source_expression) |function_source_expression| {
        switch (function_source_expression.type.resolved) {
            .runtime_type => |runtime_type| {
                switch (runtime_type.machine_type) {
                    .object_ref, .safe_ptr => {
                        const source_class_name = a_string_table.keys()[runtime_type.type_name];

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
                                    std.debug.print("skipping... cus param mismatch\n", .{});
                                    continue;
                                }

                                // Type resolve the function
                                try resolveFunctionHead(
                                    function,
                                    source_script,
                                    script_table,
                                    a_string_table,
                                );

                                // You usually cant call static functions as member functions, aside from the case of
                                if (function.modifiers.static) {
                                    //The first parameter being the class being used as the source can be called with this syntax
                                    //since these are special cased to be callable as member functions
                                    if (function.parameters.len > 0) blk: {
                                        const first_parameter = function.parameters[0];

                                        //If the first parameter is not an object ref or safe ptr, then its definitely not special cased
                                        if (first_parameter.type.resolved.runtime_type.machine_type != .safe_ptr and
                                            first_parameter.type.resolved.runtime_type.machine_type != .object_ref)
                                            break :blk;

                                        //If its a safe_ptr or object_ref, the type name should always be known
                                        std.debug.assert(first_parameter.type.resolved.runtime_type.type_name != 0xFFFFFFFF);

                                        const type_name = a_string_table.keys()[first_parameter.type.resolved.runtime_type.type_name];

                                        // If the calling script derives the parameter's script, then this *can* be a member function, allow through
                                        if (scriptDerivesOtherScript(
                                            calling_script,
                                            script_table.get(type_name).?,
                                            script_table,
                                        )) try candidates.append(.{
                                            function,
                                            (try typeReferenceFromScript(source_script, 0, a_string_table)).runtime_type,
                                        });

                                        continue;
                                    }

                                    std.debug.panic("you cant member call a static function {s}, sorry bub", .{function.name});
                                }

                                try candidates.append(.{
                                    function,
                                    (try typeReferenceFromScript(source_script, 0, a_string_table)).runtime_type,
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
            .type => |comptime_type| {
                const type_script = script_table.get(comptime_type).?;

                const type_class = getScriptClassNode(type_script.ast);

                //TODO: this needs to recurse the types, so you can call parent type static methods aswell
                for (type_class.functions) |function| {
                    // Skip non static functions, since we are calling directly on a class
                    if (!function.modifiers.static)
                        continue;

                    // If we found the function, return it
                    if (std.mem.eql(u8, function.name, name)) {
                        // Type resolve the function
                        try resolveFunctionHead(
                            function,
                            type_script,
                            script_table,
                            a_string_table,
                        );

                        // Skip overloads which have the wrong count of parameters
                        if (function.parameters.len != calling_parameters.len)
                            continue;

                        try candidates.append(.{
                            function,
                            (try typeReferenceFromScript(type_script, 0, a_string_table)).runtime_type,
                        });
                    }
                }
            },
            else => |tag| std.debug.panic("cant call member function on {s}", .{@tagName(tag)}),
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

        std.debug.print("{any}\n", .{checked_parameters});

        std.debug.panic("this is a massive TODO rn", .{});
    }

    std.debug.print("calling params: {any}\n", .{calling_parameters});

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
    parsed_type: Parser.Type.Parsed,
    script: ?*ParsedScript,
    script_table: ?*ParsedScriptTable,
    a_string_table: ?*AStringTable,
) Error!Parser.Type.Resolved {
    if (std.meta.stringToEnum(MMTypes.FishType, parsed_type.name)) |fish_type| {
        return .{
            .runtime_type = if (parsed_type.dimension_count > 0)
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
        };
    } else {
        var iter = script.?.imported_types.keyIterator();
        while (iter.next()) |imported_type| {
            if (std.mem.eql(u8, parsed_type.name, imported_type.*)) {
                const referenced_script = script_table.?.get(imported_type.*).?;

                return try typeReferenceFromScript(
                    referenced_script,
                    parsed_type.dimension_count,
                    a_string_table.?,
                );
            }
        }

        std.debug.panic("no script {s} found in import table for script {s}", .{ parsed_type.name, script.?.class_name });
    }
}

pub fn typeReferenceFromScript(script: *ParsedScript, dimension_count: u8, a_string_table: *AStringTable) Error!Parser.Type.Resolved {
    //Get the idx of the name of this script, or put into the string table
    const name_idx = try a_string_table.getOrPut(script.class_name);

    return .{ .runtime_type = if (dimension_count > 0)
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
        } };
}

fn resolveField(
    field: *Parser.Node.Field,
    script: *ParsedScript,
    script_table: *ParsedScriptTable,
    a_string_table: *AStringTable,
) Error!void {
    std.debug.print("type resolving field {s}\n", .{field.name});

    //early return if its already resolved
    if (field.type == .resolved)
        return;

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
                    null,
                );

                //Set the type of the field to the resolved default value type
                field.type = default_value.type;
            } else unreachable;
        },
        else => unreachable,
    }

    std.debug.print("resolved as {}\n", .{field.type});

    //Assert the type was actually resolved
    std.debug.assert(field.type == .resolved);

    //Panic if the resolved type is not knowable at runtime
    if (field.type.resolved != .runtime_type)
        std.debug.panic(
            "field {s}'s default value's is unknown at runtime, currently is {s}",
            .{ field.name, @tagName(field.type.resolved) },
        );
}

pub fn mangleFunctionName(
    writer: anytype,
    a_string_table: *const AStringTable,
    name: []const u8,
    parameters: []const Parser.Node.Function.Parameter,
) !void {
    try writer.writeAll(name);
    try writer.writeAll("__");

    for (parameters) |parameter| {
        const parameter_type = parameter.type.resolved.runtime_type;
        switch (parameter_type.fish_type) {
            .void => {
                const type_name = a_string_table.keys()[parameter_type.type_name];

                try writer.writeByte('Q');
                try std.fmt.formatInt(
                    type_name.len,
                    10,
                    .lower,
                    .{},
                    writer,
                );

                try writer.writeAll(type_name);
            },
            else => try writer.writeByte(parameter_type.fish_type.toMangledId()),
        }
    }
}
