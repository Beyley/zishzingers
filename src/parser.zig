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
    expression,
    function,
    function_parameters,
    variable_declaration,
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

        fields: []const *Field,
        functions: []const *Function,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("class{{ class_name = {s}, base_class: {?s} }}", .{ value.class_name, value.base_class });
        }
    };

    pub const Field = struct {
        modifiers: MMTypes.Modifiers,
        name: []const u8,
        type: ?[]const u8,
        default_value: ?*Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("field{{ name = {s}, type = {?s}, default_value = {?}, modifiers: {} }}", .{ value.name, value.type, value.default_value, value.modifiers });
        }
    };

    pub const Function = struct {
        return_type: []const u8,
        parameters: *FunctionParameters,
        body: []const Node,
        name: []const u8,
        modifiers: MMTypes.Modifiers,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("function{{ return_type = {s}, modifiers = {}, parameters = {}, name = {s} }}", .{ value.return_type, value.modifiers, value.parameters, value.name });
        }
    };

    pub const Expression = union(enum) {
        s32_literal: i32,
        s64_literal: i64,
        f32_literal: f32,
        f64_literal: f32,
        bool_literal: bool,
        ascii_string_literal: []const u8,
        wide_string_literal: []const u8,
        field_access: struct {
            source: *Expression,
            field: []const u8,
        },
        variable: []const u8,
        this: void,
        negate: *Expression,
        function_call: struct {
            name: []const u8,
            parameters: []const *Expression,
        },

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return switch (value) {
                .s32_literal => |literal| writer.print("expression{{ .s32_literal = {d} }}", .{literal}),
                .s64_literal => |literal| writer.print("expression{{ .s64_literal = {d} }}", .{literal}),
                .f32_literal => |literal| writer.print("expression{{ .f32_literal = {d} }}", .{literal}),
                .f64_literal => |literal| writer.print("expression{{ .f64_literal = {d} }}", .{literal}),
                .bool_literal => |literal| writer.print("expression{{ .bool_literal = {} }}", .{literal}),
                .ascii_string_literal => |literal| writer.print("expression{{ .ascii_string_literal = {s} }}", .{literal}),
                .wide_string_literal => |literal| writer.print("expression{{ .wide_string_literal = {s} }}", .{literal}),
                .field_access => |literal| writer.print("expression{{ .field_access = .{{ .source = {}, .field = {s} }} }}", .{ literal.source, literal.field }),
                .variable => |literal| writer.print("expression{{ .variable = {s} }}", .{literal}),
                .this => writer.print("expression{{ .this }}", .{}),
                .negate => |literal| writer.print("expression{{ .negate = {} }}", .{literal}),
                .function_call => |literal| writer.print("expression{{ .function_call = .{{ .name = {s}, .parameters = {any} }} }}", .{ literal.name, literal.parameters }),
            };
        }
    };

    pub const FunctionParameters = struct {
        pub const Parameter = struct {
            name: []const u8,
            type: []const u8,
        };

        parameters: []const Parameter,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.writeAll("[ ");

            for (value.parameters) |parameter| {
                try writer.print("{s}: {s}", .{ parameter.name, parameter.type });
            }

            try writer.writeAll(" ]");
        }
    };

    pub const VariableDeclaration = struct {
        name: []const u8,
        type: ?[]const u8,
        value: ?*Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("variable_declaration{{ name = {s}, type = {?s}, value = {?} }}", .{ value.name, value.type, value.value });
        }
    };

    using: *Using,
    import: *Import,
    from_import: *FromImport,
    class: *Class,
    field: *Field,
    expression: *Expression,
    function: *Function,
    function_parameters: *FunctionParameters,
    variable_declaration: *VariableDeclaration,
};

pub const Tree = struct {
    arena: std.heap.ArenaAllocator,
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
            if (self.pos >= self.slice.len - 1) {
                return null;
            }

            return self.slice[self.pos];
        }

        pub fn peekAt(self: Self, at: usize) ?T {
            if (self.pos + at >= self.slice.len - 1) {
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

pub fn parse(allocator: std.mem.Allocator, lexemes: []const Lexeme) !Tree {
    var tree: Tree = .{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .allocator = undefined,
        .root_elements = .{},
    };
    tree.allocator = tree.arena.allocator();
    errdefer tree.arena.deinit();

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
                for (tree.root_elements.items) |item| {
                    switch (item) {
                        inline else => |ptr| {
                            std.debug.print("{}\n", .{ptr.*});
                        },
                    }
                }

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

    node.* = .{
        .class_name = class_name,
        .base_class = null,
        .functions = undefined,
        .fields = undefined,
    };

    //Consume the scope start if available, if not, parse the class property
    if (!consumeArbitraryLexemeIfAvailable(iter, "{")) {
        const property = iter.next() orelse std.debug.panic("unexpected EOF when parsing class properties", .{});
        switch (hashKeyword(property)) {
            hashKeyword("extends") => {
                const base_class = iter.next() orelse std.debug.panic("unexpected EOF when reading base class name", .{});

                node.base_class = base_class;
            },
            else => std.debug.panic("Unknown class property {s}", .{property}),
        }

        //Consume the scope start
        consumeArbitraryLexeme(iter, "{");
    }

    //TODO: parse class scope body

    while (true) {
        const lexeme = iter.peek() orelse std.debug.panic("unexpected EOF when parsing class body", .{});

        //If we hit a `}`, we have reached the end of scope
        if (lexeme[0] == '}') {
            break;
        }

        const modifiers = consumeModifiers(iter);

        const next = iter.peek() orelse std.debug.panic("unexpected EOF when parsing declaration", .{});

        //If the next keyword is a function, then we are consuming a function
        if (hashKeyword(next) == comptime hashKeyword("fn")) {
            //Get rid of the fn
            consumeArbitraryLexeme(iter, "fn");

            std.debug.print("aa {}\n", .{(try consumeFunction(tree.allocator, iter, modifiers)).*});
        }
        // Else, we are consuming a field
        else {
            std.debug.print("aa {}\n", .{(try consumeField(tree.allocator, iter, modifiers)).*});
        }
    }

    consumeArbitraryLexeme(iter, "}");

    try tree.root_elements.append(tree.allocator, .{ .class = node });
}

fn consumeField(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme), modifiers: MMTypes.Modifiers) !*Node.Field {
    const node = try allocator.create(Node.Field);
    errdefer allocator.destroy(node);

    const name = iter.next() orelse std.debug.panic("unexpected EOF when parsing field name", .{});

    const field_type: ?[]const u8 = if (consumeArbitraryLexemeIfAvailable(iter, ":")) blk: {
        break :blk consumeTypeName(iter);
    } else null;

    const default_value: ?*Node.Expression = if (consumeArbitraryLexemeIfAvailable(iter, "=")) blk: {
        break :blk try consumeExpression(allocator, iter);
    } else null;

    if (field_type == null and default_value == null) {
        std.debug.panic("Field has no type and no default value", .{});
    }

    node.* = .{
        .modifiers = modifiers,
        .name = name,
        .type = field_type,
        .default_value = default_value,
    };

    consumeSemicolon(iter);

    return node;
}

const Error = std.mem.Allocator.Error || std.fmt.ParseIntError;

fn consumeExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    const node = try allocator.create(Node.Expression);
    errdefer allocator.destroy(node);

    const first = iter.next() orelse @panic("eof");

    var isNumber = true;
    for (first) |c| {
        //TODO: hex literals
        if (!std.ascii.isDigit(c))
            isNumber = false;
    }

    node.* = if (isNumber) blk: {
        const s64 = try std.fmt.parseInt(i64, first, 0);

        // If its within the range of an i32, then make this be a s32 literal instead of an s64 literal,
        // since s32 will coerce to s64 in the type resolution stage
        if (s64 >= std.math.minInt(i32) and s64 <= std.math.maxInt(i32)) {
            break :blk .{ .s32_literal = @intCast(s64) };
        } else {
            break :blk .{ .s64_literal = s64 };
        }
    } else blk: {
        const next = iter.peek() orelse @panic("EOF");

        if (next[0] == '(') {
            break :blk .{
                .function_call = .{
                    .name = first,
                    .parameters = try consumeFunctionCallParameters(allocator, iter),
                },
            };
        } else {
            if (maybeHashKeyword(first)) |keyword| {
                switch (keyword) {
                    hashKeyword("true") => break :blk .{ .bool_literal = true },
                    hashKeyword("false") => break :blk .{ .bool_literal = false },
                    else => {},
                }
            }

            break :blk .{ .variable = first };
        }
    };

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

fn consumeTypeName(iter: *SliceIterator(Lexeme)) []const u8 {
    return iter.next() orelse std.debug.panic("unexpected EOF when reading type name", .{});
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
            break :blk iter.next() orelse std.debug.panic("EOF", .{});
        }

        break :blk "void";
    };

    const body = try consumeFunctionBody(allocator, iter);

    node.* = .{
        .modifiers = modifiers,
        .body = body,
        .parameters = parameters,
        .name = name,
        .return_type = return_type,
    };

    return node;
}

fn consumeFunctionBody(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) ![]const Node {
    var body = std.ArrayList(Node).init(allocator);
    defer body.deinit();

    consumeArbitraryLexeme(iter, "{");
    while (true) {
        const next = iter.peek() orelse std.debug.panic("unexpected EOF when parsing function body", .{});

        if (next[0] == '}') {
            _ = iter.next();
            break;
        }

        const was_keyword: bool = if (maybeHashKeyword(next)) |maybe_keyword| blk: {
            switch (maybe_keyword) {
                hashKeyword("let") => {
                    try body.append(try consumeVariableDeclaration(allocator, iter));
                    break :blk true;
                },
                hashKeyword("return") => {
                    break :blk true;
                },
                else => {
                    break :blk false;
                },
            }
        } else false;

        //If it was not parsed as a special keyword, then its an expression, and we need to parse it as one
        if (!was_keyword) {}
    }

    return try body.toOwnedSlice();
}

fn consumeVariableDeclaration(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !Node {
    const node = try allocator.create(Node.VariableDeclaration);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "let");

    const name = iter.next() orelse std.debug.panic("unexpected EOF", .{});

    const variable_type: ?[]const u8 = blk: {
        if (consumeArbitraryLexemeIfAvailable(iter, ":")) {
            break :blk iter.next() orelse std.debug.panic("unexpected EOF", .{});
        }

        break :blk null;
    };

    const value: ?*Node.Expression = blk: {
        if (consumeArbitraryLexemeIfAvailable(iter, "=")) {
            break :blk try consumeExpression(allocator, iter);
        }

        break :blk null;
    };

    if (value == null and variable_type == null) {
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
                    try wanted_imports.append(tree.allocator, curr);

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
        .target = iter.next() orelse std.debug.panic("unexpected EOF after import statement", .{}),
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

            //If this is the start of a lexeme and we hit a ' (the start of a string)
            if (just_started_lexeme and char == '\'') {
                //Increment to the next char
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
