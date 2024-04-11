const std = @import("std");
const MMTypes = @import("MMTypes.zig");

pub const UsingType = enum {
    library,
};

pub const NodeType = enum {
    using,
    import,
    from_import,
    class,
    field,
    property,
    expression,
    function,
    constructor,
    function_parameters,
    variable_declaration,
    return_statement,
    if_statement,
};

const FromImportWanted = union(enum) {
    all: void,
    single: []const u8,
    multiple: []const []const u8,

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .all => {
                return writer.print(".all", .{});
            },
            .single => |single| {
                return writer.print("{{ .single = {s} }}", .{single});
            },
            .multiple => |multiple| {
                try writer.print("{{ target = [ ", .{});
                for (multiple) |wanted| {
                    try writer.print("{s}, ", .{wanted});
                }
                return writer.print("] }}", .{});
            },
        }
    }
};

pub const Type = union(enum) {
    pub const ParsedType = struct {
        name: []const u8,
        dimension_count: u8,

        pub const Void: Type = .{ .parsed = .{
            .name = "void",
            .dimension_count = 0,
        } };
    };

    parsed: ParsedType,
    unknown: void,
    resolved: MMTypes.TypeReference,
};

pub const Node = union(NodeType) {
    pub const Using = struct {
        type: UsingType,
        target: []const u8,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("using{{ type = {s}, target = {s} }}", .{ @tagName(value.type), value.target });
        }
    };

    pub const Import = struct {
        target: []const u8,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("target{{ target = {s} }}", .{value.target});
        }
    };

    pub const FromImport = struct {
        target: []const u8,
        wanted: FromImportWanted,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("from_import{{ target = {s}, wanted = {} }}", .{ value.target, value.wanted });
        }
    };

    pub const Class = struct {
        class_name: []const u8,
        base_class: ?[]const u8,
        guid: ?*Expression,

        fields: []const *Field,
        properties: []const *Property,
        functions: []const *Function,
        constructors: ?[]const *const Constructor,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("class{{ class_name = {s}, base_class = {?s}, guid = {?} }}", .{ value.class_name, value.base_class, value.guid });
        }
    };

    pub const Constructor = struct {
        body: ?*const Node.Expression,
        parameters: *const FunctionParameters,
        modifiers: MMTypes.Modifiers,
    };

    pub const Field = struct {
        modifiers: MMTypes.Modifiers,
        name: []const u8,
        type: Type,
        default_value: ?*Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("field{{ name = {s}, type = {?}, default_value = {?}, modifiers: {} }}", .{ value.name, value.type, value.default_value, value.modifiers });
        }
    };

    pub const Property = struct {
        pub const FunctionState = union(enum) {
            missing: void,
            forward_declaration: void,
            expression: *Node.Expression,
        };

        set_body: FunctionState,
        get_body: FunctionState,
        type: Type,
        name: []const u8,
        modifiers: MMTypes.Modifiers,
    };

    pub const Function = struct {
        return_type: Type,
        parameters: *FunctionParameters,
        body: ?*Expression,
        name: []const u8,
        modifiers: MMTypes.Modifiers,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("function{{ return_type = {}, modifiers = {}, parameters = {}, name = {s} }}", .{ value.return_type, value.modifiers, value.parameters, value.name });
        }
    };

    pub const Expression = struct {
        pub const ExpressionContents = union(enum) {
            s32_literal: i32,
            s64_literal: i64,
            f32_literal: f32,
            f64_literal: f32,
            guid_literal: u32,
            bool_literal: bool,
            ascii_string_literal: []const u8,
            wide_string_literal: []const u8,
            field_access: struct {
                source: *Expression,
                field: []const u8,
            },
            member_function_call: struct {
                source: *Expression,
                name: []const u8,
                parameters: []const *Expression,
            },
            class_name: []const u8,
            variable: []const u8,
            this: void,
            negate: *Expression,
            function_call: struct {
                name: []const u8,
                parameters: []const *Expression,
            },
            assignment: struct {
                destination: *Expression,
                value: *Expression,
            },
            block: []const Node,
            bitwise_and: struct {
                lefthand: *Expression,
                righthand: *Expression,
            },

            pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                return switch (value) {
                    .s32_literal => |literal| writer.print("expression_contents{{ .s32_literal = {d} }}", .{literal}),
                    .s64_literal => |literal| writer.print("expression_contents{{ .s64_literal = {d} }}", .{literal}),
                    .f32_literal => |literal| writer.print("expression_contents{{ .f32_literal = {d} }}", .{literal}),
                    .f64_literal => |literal| writer.print("expression_contents{{ .f64_literal = {d} }}", .{literal}),
                    .guid_literal => |literal| writer.print("expression_contents{{ .guid_literal = {d} }}", .{literal}),
                    .bool_literal => |literal| writer.print("expression_contents{{ .bool_literal = {} }}", .{literal}),
                    .ascii_string_literal => |literal| writer.print("expression_contents{{ .ascii_string_literal = {s} }}", .{literal}),
                    .wide_string_literal => |literal| writer.print("expression_contents{{ .wide_string_literal = {s} }}", .{literal}),
                    .class_name => |literal| writer.print("expression_contents{{ .class_name = {s} }}", .{literal}),
                    .field_access => |literal| writer.print("expression_contents{{ .field_access = .{{ .source = {}, .field = {s} }} }}", .{ literal.source, literal.field }),
                    .variable => |literal| writer.print("expression_contents{{ .variable = {s} }}", .{literal}),
                    .this => writer.print("expression_contents{{ .this }}", .{}),
                    .negate => |literal| writer.print("expression_contents{{ .negate = {} }}", .{literal}),
                    .function_call => |literal| writer.print("expression_contents{{ .function_call = .{{ .name = {s}, .parameters = {any} }} }}", .{ literal.name, literal.parameters }),
                    .member_function_call => |literal| writer.print("expression_contents{{ .member_function_call = .{{ .source = {}, .name = {s}, .parameters = {any} }} }}", .{ literal.source, literal.name, literal.parameters }),
                    .assignment => |literal| writer.print("expression_contents{{ .assignment = .{{ .destination = {}, .value = .{} }} }}", .{ literal.destination, literal.value }),
                    .block => |literal| writer.print("expression_contents{{ .block = {{ .body = {any} }} }}", .{literal}),
                    .bitwise_and => |literal| writer.print("expression_contents{{ .bitwise_and = {{ .lefthand = {}, .lefthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                };
            }
        };

        contents: ExpressionContents,
        type: Type,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("expression{{.contents = {}, .type = {}}}", .{ value.contents, value.type });
        }
    };

    pub const FunctionParameters = struct {
        pub const Parameter = struct {
            name: []const u8,
            type: Type,
        };

        parameters: []const Parameter,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.writeAll("[ ");

            for (value.parameters) |parameter| {
                try writer.print("{s}: {}", .{ parameter.name, parameter.type });
            }

            try writer.writeAll(" ]");
        }
    };

    pub const VariableDeclaration = struct {
        name: []const u8,
        type: Type,
        value: ?*Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("variable_declaration{{ name = {s}, type = {?}, value = {?} }}", .{ value.name, value.type, value.value });
        }
    };

    pub const ReturnStatement = struct {
        expression: ?*Expression,
    };

    pub const IfStatement = struct {
        condition: *Expression,
        body: *Expression,
        else_body: ?*Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("if_statement{{ condition = {}, body = {}, else_body = {?} }}", .{ value.condition, value.body, value.else_body });
        }
    };

    using: *Using,
    import: *Import,
    from_import: *FromImport,
    class: *Class,
    field: *Field,
    property: *Property,
    expression: *Expression,
    function: *Function,
    constructor: *Constructor,
    function_parameters: *FunctionParameters,
    variable_declaration: *VariableDeclaration,
    return_statement: *ReturnStatement,
    if_statement: *IfStatement,
};

pub const Tree = struct {
    allocator: std.mem.Allocator,
    root_elements: std.ArrayListUnmanaged(Node),
};

const Lexeme = []const u8;

pub fn SliceIterator(comptime T: type) type {
    return struct {
        slice: []const T,
        pos: usize,

        const Self = @This();

        pub fn next(self: *Self) ?T {
            if (self.pos >= self.slice.len)
                return null
            else {
                const item = self.slice[self.pos];

                self.pos += 1;

                return item;
            }
        }

        pub fn peek(self: Self) ?T {
            if (self.pos >= self.slice.len) {
                return null;
            }

            return self.slice[self.pos];
        }

        pub fn peekAt(self: Self, at: usize) ?T {
            if (self.pos + at >= self.slice.len) {
                return null;
            }

            return self.slice[self.pos + at];
        }

        pub fn prev(self: Self) ?T {
            if (self.pos == 0) {
                return null;
            }

            return self.slice[self.pos - 1];
        }
    };
}

///Parses a script, caller has to free the resulting `Tree` object
pub fn parse(allocator: std.mem.Allocator, lexemes: []const Lexeme) Error!Tree {
    var tree: Tree = .{
        .allocator = allocator,
        .root_elements = .{},
    };

    var lexeme_iter = SliceIterator(Lexeme){
        .pos = 0,
        .slice = lexemes,
    };

    try consumeTopLevel(&tree, &lexeme_iter);

    return tree;
}

const KeywordHash = u72;

fn maybeHashKeyword(keyword: []const u8) ?KeywordHash {
    if (keyword.len > (@bitSizeOf(u72) / 8)) {
        return null;
    }

    return hashKeyword(keyword);
}

fn hashKeyword(keyword: []const u8) KeywordHash {
    var val: KeywordHash = 0;
    @memcpy(std.mem.asBytes(&val)[0..keyword.len], keyword);
    return val;
}

fn consumeTopLevel(tree: *Tree, iter: *SliceIterator(Lexeme)) !void {
    while (iter.next()) |lexeme| {
        switch (hashKeyword(lexeme)) {
            hashKeyword("using") => try consumeUsingStatement(tree, iter),
            hashKeyword("import") => try consumeImportStatement(tree, iter),
            hashKeyword("from") => try consumeFromImportStatement(tree, iter),
            hashKeyword("class") => try consumeClassStatement(tree, iter),
            else => {
                std.debug.panic("Unexpected top level lexeme \"{s}\"", .{lexeme});
            },
        }
    }
}

fn consumeClassStatement(tree: *Tree, iter: *SliceIterator(Lexeme)) !void {
    const node = try tree.allocator.create(Node.Class);
    errdefer tree.allocator.destroy(node);

    if (consumeArbitraryLexemeIfAvailable(iter, "{")) {
        std.debug.panic("Unexpected {{, expecting class name", .{});
    }

    //Unreachable since we error out right above if theres EOF
    const class_name = iter.next() orelse unreachable;

    const guid: ?*Node.Expression = if (consumeArbitraryLexemeIfAvailable(iter, "(")) blk: {
        const expression = try consumeExpression(tree.allocator, iter);
        if (expression.contents != .guid_literal)
            @panic("aint a guid, buddy");

        consumeArbitraryLexeme(iter, ")");

        break :blk expression;
    } else null;

    //Consume the scope start if available, if not, parse the class property
    const base_class: ?[]const u8 = if (!consumeArbitraryLexemeIfAvailable(iter, "{")) blk: {
        const property = iter.next() orelse std.debug.panic("unexpected EOF when parsing class properties", .{});
        switch (hashKeyword(property)) {
            hashKeyword("extends") => {
                const base_class = iter.next() orelse std.debug.panic("unexpected EOF when reading base class name", .{});

                //Consume the scope start
                consumeArbitraryLexeme(iter, "{");

                break :blk base_class;
            },
            else => std.debug.panic("Unknown class property {s}", .{property}),
        }
    } else null;

    var functions = std.ArrayList(*Node.Function).init(tree.allocator);
    defer functions.deinit();
    var fields = std.ArrayList(*Node.Field).init(tree.allocator);
    defer fields.deinit();
    var properties = std.ArrayList(*Node.Property).init(tree.allocator);
    defer properties.deinit();

    var constructors = std.ArrayList(*Node.Constructor).init(tree.allocator);
    defer constructors.deinit();

    while (true) {
        const lexeme = iter.peek() orelse std.debug.panic("unexpected EOF when parsing class body", .{});

        // std.debug.print("vv {s}\n", .{lexeme});

        //If we hit a `}`, we have reached the end of scope
        if (lexeme[0] == '}') {
            consumeArbitraryLexeme(iter, "}");

            break;
        }

        const modifiers = consumeModifiers(iter);

        const next = iter.peek() orelse std.debug.panic("unexpected EOF when parsing declaration", .{});

        const next_keyword = maybeHashKeyword(next);

        //If the next keyword is a function, then we are consuming a function
        if (next_keyword == comptime hashKeyword("fn")) {
            //Get rid of the fn
            consumeArbitraryLexeme(iter, "fn");

            try functions.append(try consumeFunction(tree.allocator, iter, modifiers));
        }
        // Else, we are consuming a field, property, or constructor
        else blk: {
            if (iter.peekAt(1)) |ahead| {
                if (ahead[0] == '(') {
                    try constructors.append(try consumeConstructor(tree.allocator, iter, class_name, modifiers));

                    //Since we parsed it as a contructor, break out as we dont want to accidentally parse a property aswell
                    break :blk;
                }
            }

            switch (try consumeFieldOrProperty(tree.allocator, iter, modifiers)) {
                .field => |field| try fields.append(field),
                .property => |property| try properties.append(property),
            }
        }
    }

    node.* = .{
        .constructors = try constructors.toOwnedSlice(),
        .class_name = class_name,
        .base_class = base_class,
        .functions = try functions.toOwnedSlice(),
        .fields = try fields.toOwnedSlice(),
        .properties = try properties.toOwnedSlice(),
        .guid = guid,
    };

    try tree.root_elements.append(tree.allocator, .{ .class = node });
}

fn consumeConstructor(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme), class_name: []const u8, modifiers: MMTypes.Modifiers) !*Node.Constructor {
    const node = try allocator.create(Node.Constructor);
    errdefer allocator.destroy(node);

    const name = iter.next() orelse @panic("eof");

    std.debug.assert(std.mem.eql(u8, name, class_name));

    const parameters = try consumeFunctionParameters(allocator, iter);

    const body: ?*Node.Expression = if (consumeArbitraryLexemeIfAvailable(iter, ";"))
        null
    else
        try consumeBlockExpression(allocator, iter);

    node.* = .{
        .modifiers = modifiers,
        .parameters = parameters,
        .body = body,
    };

    return node;
}

fn consumeFieldOrProperty(
    allocator: std.mem.Allocator,
    iter: *SliceIterator(Lexeme),
    modifiers: MMTypes.Modifiers,
) !union(enum) {
    field: *Node.Field,
    property: *Node.Property,
} {
    const name = iter.next() orelse std.debug.panic("unexpected EOF when parsing field name", .{});

    const field_type: Type = if (consumeArbitraryLexemeIfAvailable(iter, ":"))
        consumeTypeName(iter)
    else
        .unknown;

    if (consumeArbitraryLexemeIfAvailable(iter, "{")) {
        if (field_type == .unknown) {
            std.debug.panic("Properties must specify type", .{});
        }

        const node = try allocator.create(Node.Property);
        errdefer allocator.destroy(node);

        const get_body, const set_body = blk: {
            var get_body: Node.Property.FunctionState = .missing;
            var set_body: Node.Property.FunctionState = .missing;

            while (true) {
                const next = iter.next() orelse @panic("EOF");
                switch (hashKeyword(next)) {
                    hashKeyword("get") => {
                        if (consumeArbitraryLexemeIfAvailable(iter, ";")) {
                            get_body = .forward_declaration;
                        } else {
                            get_body = .{ .expression = try consumeBlockExpression(allocator, iter) };
                        }
                    },
                    hashKeyword("set") => {
                        if (consumeArbitraryLexemeIfAvailable(iter, ";")) {
                            set_body = .forward_declaration;
                        } else {
                            set_body = .{ .expression = try consumeBlockExpression(allocator, iter) };
                        }
                    },
                    hashKeyword("}") => break :blk .{ get_body, set_body },
                    else => std.debug.panic("what are you doing {s}", .{next}),
                }
            }
        };

        node.* = .{
            .name = name,
            .type = field_type,
            .modifiers = modifiers,
            .get_body = get_body,
            .set_body = set_body,
        };

        return .{ .property = node };
    } else {
        const default_value: ?*Node.Expression = if (consumeArbitraryLexemeIfAvailable(iter, "="))
            try consumeExpression(allocator, iter)
        else
            null;

        if (field_type == .unknown and default_value == null) {
            std.debug.panic("Field {s} has no type and no default value", .{name});
        }

        const node = try allocator.create(Node.Field);
        errdefer allocator.destroy(node);

        node.* = .{
            .modifiers = modifiers,
            .name = name,
            .type = field_type,
            .default_value = default_value,
        };

        consumeSemicolon(iter);

        return .{ .field = node };
    }
}

pub const Error = std.mem.Allocator.Error || std.fmt.ParseIntError;

fn consumeBlockExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    const body_node = try allocator.create(Node.Expression);
    errdefer allocator.destroy(body_node);

    var body = std.ArrayList(Node).init(allocator);
    defer body.deinit();

    // std.debug.print("nn {s}\n", .{iter.peek() orelse unreachable});

    const is_block = consumeArbitraryLexemeIfAvailable(iter, "{");

    while (true) {
        const next = iter.peek() orelse std.debug.panic("unexpected EOF when parsing function body", .{});

        if (is_block and next[0] == '}') {
            consumeArbitraryLexeme(iter, "}");
            break;
        }

        const was_keyword: bool = if (maybeHashKeyword(next)) |maybe_keyword| blk: {
            switch (maybe_keyword) {
                hashKeyword("let") => {
                    const variable_declaration = try consumeVariableDeclaration(allocator, iter);

                    // std.debug.print("cc {}\n", .{variable_declaration});

                    try body.append(variable_declaration);
                    break :blk true;
                },
                hashKeyword("return") => {
                    const return_statement = try consumeReturnStatement(allocator, iter);

                    // std.debug.print("ee {}\n", .{return_statement});

                    try body.append(return_statement);
                    break :blk true;
                },
                hashKeyword("if") => {
                    const if_statement = try consumeIfStatement(allocator, iter);

                    // std.debug.print("ff {}\n", .{if_statement});

                    try body.append(if_statement);
                    break :blk true;
                },
                else => {
                    break :blk false;
                },
            }
        } else false;

        //If it was not parsed as a special keyword, then its an expression, and we need to parse it as one
        if (!was_keyword) {
            const node: Node = .{ .expression = try consumeExpression(allocator, iter) };
            consumeSemicolon(iter);

            // std.debug.print("dd {}\n", .{node});

            try body.append(node);
        }

        //If we are not parsing a block, we are already done
        if (!is_block) {
            break;
        }
    }

    body_node.* = .{ .contents = .{ .block = try body.toOwnedSlice() }, .type = .unknown };

    return body_node;
}

fn isInt(comptime T: type, str: []const u8) !?T {
    return std.fmt.parseInt(T, str, 0) catch |err| {
        if (err == error.InvalidCharacter) {
            return null;
        }

        return err;
    };
}

fn consumeExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    const node = try allocator.create(Node.Expression);
    errdefer allocator.destroy(node);

    const first = iter.next() orelse @panic("eof");

    node.* = .{
        .contents = if (first[0] == '!') blk: {
            break :blk .{ .negate = try consumeExpression(allocator, iter) };
        } else if (try isInt(i64, first)) |s64| blk: {
            // If its within the range of an i32, then make this be a s32 literal instead of an s64 literal,
            // since s32 will coerce to s64 in the type resolution stage
            if (s64 >= std.math.minInt(i32) and s64 <= std.math.maxInt(i32)) {
                break :blk .{ .s32_literal = @intCast(s64) };
            } else {
                break :blk .{ .s64_literal = s64 };
            }
        } else blk: {
            const next = iter.peek() orelse @panic("EOF");

            // We are parsing a function
            if (next[0] == '(') {
                break :blk .{
                    .function_call = .{
                        .name = first,
                        .parameters = try consumeFunctionCallParameters(allocator, iter),
                    },
                };
            }
            // We are parsing a field access/member call
            else if (next[0] == '.') {
                const source = try allocator.create(Node.Expression);
                errdefer allocator.destroy(source);

                //If the source of the access is `this`, then we want to special case that and emit the `this` expression
                source.* = .{
                    .contents = if (std.mem.eql(u8, first, "this"))
                        .this
                    else
                        .{ .variable = first },
                    .type = .unknown,
                };

                consumeArbitraryLexeme(iter, ".");

                const name = iter.next() orelse @panic("field name EOF");

                //If this member call, parse as that, else parse as field access
                if ((iter.peek() orelse @panic("EOF"))[0] == '(') {
                    const parameters = try consumeFunctionCallParameters(allocator, iter);

                    break :blk .{
                        .member_function_call = .{
                            .name = name,
                            .parameters = parameters,
                            .source = source,
                        },
                    };
                } else break :blk .{
                    .field_access = .{
                        .source = source,
                        .field = name,
                    },
                };
            }
            // We are parsing some other misc expression, like a string literal or boolean literal
            else {
                //We *may* be parsing a GUID literal
                if (first[0] == 'g') {
                    if (try isInt(u32, first[1..])) |guid| {
                        break :blk .{ .guid_literal = guid };
                    }
                }

                if (maybeHashKeyword(first)) |keyword| {
                    switch (keyword) {
                        hashKeyword("true") => break :blk .{ .bool_literal = true },
                        hashKeyword("false") => break :blk .{ .bool_literal = false },
                        else => {},
                    }
                }

                if (first.len >= 2 and std.mem.eql(u8, first[0..2], "L'")) {
                    break :blk .{ .wide_string_literal = unwrapStringLiteral(first[1..]) };
                }

                if (first[0] == '\'') {
                    break :blk .{ .ascii_string_literal = unwrapStringLiteral(first[1..]) };
                }

                break :blk .{ .variable = first };
            }
        },
        .type = .unknown,
    };

    if (iter.peek()) |next| {
        if (maybeHashKeyword(next)) |keyword| {
            switch (keyword) {
                // If the next char is `=`, then we know this is an assignment,
                // where the current value in `node` is the source for the assignment
                hashKeyword("=") => {
                    //consume the =
                    _ = iter.next();

                    const value = try consumeExpression(allocator, iter);

                    const destination = try allocator.create(Node.Expression);
                    errdefer allocator.free(destination);

                    destination.* = node.*;

                    node.* = .{
                        .contents = .{
                            .assignment = .{
                                .destination = destination,
                                .value = value,
                            },
                        },
                        .type = .unknown,
                    };
                },
                hashKeyword("&") => {
                    //consume the &
                    _ = iter.next();

                    const righthand = try consumeExpression(allocator, iter);

                    const lefthand = try allocator.create(Node.Expression);
                    errdefer allocator.free(lefthand);

                    //Copy the current expression into the left hand side
                    lefthand.* = node.*;

                    node.* = .{
                        .contents = .{
                            .bitwise_and = .{
                                .lefthand = lefthand,
                                .righthand = righthand,
                            },
                        },
                        .type = .unknown,
                    };
                },
                else => {},
            }
        }
    } else @panic("EOF");

    // std.debug.print("zz {}\n", .{node.*});

    return node;
}

fn consumeFunctionCallParameters(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) ![]const *Node.Expression {
    var parameters = std.ArrayList(*Node.Expression).init(allocator);
    defer parameters.deinit();

    consumeArbitraryLexeme(iter, "(");

    while (true) {
        const next = iter.peek() orelse @panic("EOF");

        if (next[0] == ')') {
            _ = iter.next();
            break;
        }

        try parameters.append(try consumeExpression(allocator, iter));

        //Consume a comma, if available
        _ = consumeArbitraryLexemeIfAvailable(iter, ",");
    }

    return parameters.toOwnedSlice();
}

fn consumeTypeName(iter: *SliceIterator(Lexeme)) Type {
    const name = iter.next() orelse std.debug.panic("unexpected EOF when reading type name", .{});

    var dimension_count: u8 = 0;

    blk: while (maybeHashKeyword(iter.peek() orelse @panic("EOF"))) |keyword| {
        switch (keyword) {
            hashKeyword("[") => {
                consumeArbitraryLexeme(iter, "[");
                consumeArbitraryLexeme(iter, "]");

                dimension_count += 1;
            },
            else => break :blk,
        }
    }

    return .{ .parsed = .{ .name = name, .dimension_count = dimension_count } };
}

fn consumeFunctionParameters(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !*Node.FunctionParameters {
    const node = try allocator.create(Node.FunctionParameters);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "(");

    var parameters = std.ArrayListUnmanaged(Node.FunctionParameters.Parameter){};
    defer parameters.deinit(allocator);

    while (true) {
        const name = iter.next() orelse std.debug.panic("unexpected EOF when parsing function definition parameters", .{});

        // We have reached the end of the parameters
        if (name[0] == ')')
            break;

        consumeArbitraryLexeme(iter, ":");
        const param_type = consumeTypeName(iter);

        try parameters.append(allocator, .{ .name = name, .type = param_type });

        _ = consumeArbitraryLexemeIfAvailable(iter, ",");
    }

    node.* = .{
        .parameters = try parameters.toOwnedSlice(allocator),
    };

    return node;
}

fn consumeFunction(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme), modifiers: MMTypes.Modifiers) !*Node.Function {
    const node = try allocator.create(Node.Function);
    errdefer allocator.destroy(node);

    const name = iter.next() orelse std.debug.panic("unexpected EOF when parsing function name", .{});

    const parameters = try consumeFunctionParameters(allocator, iter);

    const return_type = blk: {
        if (std.mem.eql(u8, iter.peek() orelse std.debug.panic("EOF", .{}), "->")) {
            consumeArbitraryLexeme(iter, "->");
            break :blk consumeTypeName(iter);
        }

        break :blk Type.ParsedType.Void;
    };

    const body: ?*Node.Expression = if (!consumeArbitraryLexemeIfAvailable(iter, ";"))
        try consumeBlockExpression(allocator, iter)
    else
        null;

    node.* = .{
        .modifiers = modifiers,
        .body = body,
        .parameters = parameters,
        .name = name,
        .return_type = return_type,
    };

    return node;
}

fn consumeIfStatement(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !Node {
    const node = try allocator.create(Node.IfStatement);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "if");

    consumeArbitraryLexeme(iter, "(");
    const condition = try consumeExpression(allocator, iter);
    consumeArbitraryLexeme(iter, ")");

    const body = try consumeBlockExpression(allocator, iter);

    const else_body: ?*Node.Expression = if (std.mem.eql(u8, "else", iter.peek() orelse @panic("EOF"))) blk: {
        consumeArbitraryLexeme(iter, "else");

        break :blk try consumeBlockExpression(allocator, iter);
    } else null;

    node.* = .{
        .condition = condition,
        .body = body,
        .else_body = else_body,
    };

    return .{ .if_statement = node };
}

fn consumeReturnStatement(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !Node {
    const node = try allocator.create(Node.ReturnStatement);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "return");

    //If the next lexeme is not a semicolon, then theres an expression after, parse it
    const expression: ?*Node.Expression = if (!consumeArbitraryLexemeIfAvailable(iter, ";")) blk: {
        const expression = try consumeExpression(allocator, iter);

        consumeSemicolon(iter);

        break :blk expression;
    } else null;

    node.* = .{ .expression = expression };

    return .{ .return_statement = node };
}

fn consumeVariableDeclaration(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !Node {
    const node = try allocator.create(Node.VariableDeclaration);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "let");

    const name = iter.next() orelse std.debug.panic("unexpected EOF", .{});

    const variable_type: Type = blk: {
        if (consumeArbitraryLexemeIfAvailable(iter, ":")) {
            break :blk consumeTypeName(iter);
        }

        break :blk .unknown;
    };

    const value: ?*Node.Expression = blk: {
        if (consumeArbitraryLexemeIfAvailable(iter, "=")) {
            break :blk try consumeExpression(allocator, iter);
        }

        break :blk null;
    };

    if (value == null and variable_type == .unknown) {
        std.debug.panic("variable has no expression or type, what?", .{});
    }

    consumeSemicolon(iter);

    node.* = .{
        .name = name,
        .type = variable_type,
        .value = value,
    };

    return .{ .variable_declaration = node };
}

fn consumeModifiers(iter: *SliceIterator(Lexeme)) MMTypes.Modifiers {
    var current_modifiers: MMTypes.Modifiers = .{};

    while (true) {
        const lexeme = iter.peek() orelse std.debug.panic("unexpected EOF when parsing class body", .{});

        const modifiers_type_info: std.builtin.Type = @typeInfo(MMTypes.Modifiers);

        //special case pub
        if (std.mem.eql(u8, lexeme, "pub")) {
            current_modifiers.public = true;

            _ = iter.next() orelse unreachable;
        } else {
            var found = false;

            inline for (modifiers_type_info.Struct.fields) |field| {
                if (comptime std.mem.eql(u8, field.name, "_unused"))
                    continue;

                if (std.mem.eql(u8, field.name, lexeme)) {
                    @field(current_modifiers, field.name) = true;
                    found = true;
                    break;
                }
            }

            //If its not a valid modifier keyword, immediately break out, we are done
            if (!found)
                break;

            _ = iter.next() orelse unreachable;
        }
    }

    return current_modifiers;
}

fn consumeFromImportStatement(tree: *Tree, iter: *SliceIterator(Lexeme)) !void {
    const node = try tree.allocator.create(Node.FromImport);
    errdefer tree.allocator.destroy(node);

    const target = iter.next() orelse std.debug.panic("unexpected EOF after from statement", .{});

    //Semantically unimportant, should always be there
    consumeArbitraryLexeme(iter, "import");

    const first_import_lexeme = iter.next() orelse std.debug.panic("unexpected EOF in from/import statement after import lexeme", .{});

    var was_multi_import = false;

    const wanted: FromImportWanted = switch (hashKeyword(first_import_lexeme)) {
        hashKeyword("{") => .{
            .multiple = blk: {
                var wanted_imports = std.ArrayListUnmanaged([]const u8){};

                was_multi_import = true;

                //If the next token is a `}`, consume it and break out
                if (consumeArbitraryLexemeIfAvailable(iter, "}")) {
                    break :blk &.{};
                }

                while (true) {
                    const curr = iter.next() orelse std.debug.panic("unexpected EOF in multi import block", .{});

                    //Append the new import
                    try wanted_imports.append(tree.allocator, unwrapStringLiteral(curr));

                    const next = iter.peek() orelse std.debug.panic("unexpected EOF in multi import block", .{});

                    //If the next token is a `}`, break out
                    if (next[0] == '}')
                        break;
                    //If the next token is a `,`, consume it
                    if (next[0] == ',')
                        _ = iter.next() orelse unreachable;
                }

                break :blk try wanted_imports.toOwnedSlice(tree.allocator);
            },
        },
        hashKeyword("*") => .{ .all = {} },
        else => .{ .single = first_import_lexeme },
    };

    node.* = .{
        .target = target,
        .wanted = wanted,
    };

    try tree.root_elements.append(tree.allocator, .{ .from_import = node });

    //If we were importing multiple things, the semicolon is not required
    if (!was_multi_import)
        consumeSemicolon(iter);
}

fn consumeImportStatement(tree: *Tree, iter: *SliceIterator(Lexeme)) !void {
    const node = try tree.allocator.create(Node.Import);
    errdefer tree.allocator.destroy(node);

    node.* = .{
        .target = unwrapStringLiteral(iter.next() orelse std.debug.panic("unexpected EOF after import statement", .{})),
    };

    try tree.root_elements.append(tree.allocator, .{ .import = node });

    consumeSemicolon(iter);
}

fn consumeUsingStatement(tree: *Tree, iter: *SliceIterator(Lexeme)) !void {
    const node = try tree.allocator.create(Node.Using);
    errdefer tree.allocator.destroy(node);

    const using_type_str = iter.next() orelse std.debug.panic("unexpected EOF after using statement", .{});

    const using_type = std.meta.stringToEnum(UsingType, using_type_str) orelse std.debug.panic("unknown using type {s}", .{using_type_str});

    const library_name = unwrapStringLiteral(iter.next() orelse std.debug.panic("unexpected EOF after using statement type", .{}));

    node.* = .{
        .type = using_type,
        .target = library_name,
    };

    try tree.root_elements.append(tree.allocator, .{ .using = node });

    consumeSemicolon(iter);
}

fn consumeSemicolon(iter: *SliceIterator(Lexeme)) void {
    consumeArbitraryLexeme(iter, ";");
}

fn consumeArbitraryLexeme(iter: *SliceIterator(Lexeme), intended: []const u8) void {
    // std.debug.print("consuming {s}\n", .{intended});

    if (iter.next()) |next| {
        if (!std.mem.eql(u8, next, intended)) {
            std.debug.panic("unexpected lexeme {s}, expected {s}", .{ next, intended });
        }
    } else {
        std.debug.panic("unexpected EOF when expecting {s}", .{intended});
    }
}

fn consumeArbitraryLexemeIfAvailable(iter: *SliceIterator(Lexeme), intended: []const u8) bool {
    //Peek the next lexeme
    if (iter.peek()) |next| {
        //If its the intended one
        if (std.mem.eql(u8, next, intended)) {
            //Consume it
            _ = iter.next() orelse unreachable;

            return true;
        }
    }

    return false;
}

fn unwrapStringLiteral(literal: []const u8) []const u8 {
    if (literal[0] != '\'' or literal[literal.len - 1] != '\'')
        std.debug.panic("bad string \"{s}\"", .{literal});

    return literal[1 .. literal.len - 1];
}

/// Turns a source into a stream of lexemes
pub const Lexemeizer = struct {
    source: []const u8,
    pos: usize = 0,

    const single_char_lexemes: []const u8 = "()[]{}!*,:;+.'<>+-";
    const special_double_lexemes: []const u16 = &.{
        @intCast(hashKeyword("->")),
        @intCast(hashKeyword(">>")),
        @intCast(hashKeyword("<<")),
    };

    pub fn next(self: *Lexemeizer) !?Lexeme {
        const iter = self.source[self.pos..];

        var lexeme_start: ?usize = null;
        var i: usize = 0;
        while (i < iter.len) {
            const char = iter[i];

            // If we hit a comment,
            if (char == '#') {
                // Skip characters until we hit a newline
                while (iter[i] != '\n') {
                    i += 1;
                }

                // Skip past the newline
                i += 1;

                // Continue past
                continue;
            }

            //If we havent started a lexeme and we are at whitespace, do nothing
            if (lexeme_start == null and std.ascii.isWhitespace(char)) {
                i += 1;
                continue;
            }

            const just_started_lexeme = lexeme_start == null;
            if (just_started_lexeme) {
                // Now that we've skipped all the whitespace, mark the start of the new lexeme
                lexeme_start = i;
            }

            const is_long_string = just_started_lexeme and char == 'L' and iter[i + 1] == '\'';

            //If this is the start of a lexeme and we hit a ' (the start of a string)
            if ((just_started_lexeme and char == '\'') or is_long_string) {
                //Increment to the next char
                i += 1;

                if (is_long_string)
                    i += 1;

                //Skip over all non ' characters
                while (iter[i] != '\'') {
                    i += 1;
                }

                //Mark to go to the next character
                i += 1;

                //Finish the lexeme
                break;
            }

            //If we hit an always single char lexeme, break out immediately
            if (std.mem.indexOf(u8, single_char_lexemes, &.{char}) != null) {
                if (i < iter.len - 1) {
                    if (std.mem.indexOf(u16, special_double_lexemes, &.{@intCast(hashKeyword(iter[i .. i + 2]))}) != null) {
                        i += 2;
                        break;
                    }
                }

                // If we just started the lexeme (aka this *is* the single char lexeme), mark to go to the next char
                if (just_started_lexeme)
                    i += 1;

                break;
            }

            //If we've hit whitespace, this is the end of a lexeme
            if (std.ascii.isWhitespace(char)) {
                break;
            }

            i += 1;
        }

        //If theres no lexemes left, return null
        if (lexeme_start == null) {
            return null;
        }

        self.pos += i;

        return iter[lexeme_start.?..i];
    }
};
