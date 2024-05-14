const std = @import("std");
const MMTypes = @import("MMTypes.zig");

pub const UsingType = enum {
    library,
};

pub const FromImportWanted = union(enum) {
    pub const ImportedFunction = struct {
        name: []const u8,
        original_name: []const u8,
    };

    all: void,
    single: ImportedFunction,
    multiple: []const ImportedFunction,

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .all => {
                return writer.print(".all", .{});
            },
            .single => |single| {
                return writer.print("{{ .single = {} }}", .{single});
            },
            .multiple => |multiple| {
                try writer.print("{{ target = [ ", .{});
                for (multiple) |wanted| {
                    try writer.print("{}, ", .{wanted});
                }
                return writer.print("] }}", .{});
            },
        }
    }
};

pub const Type = union(enum) {
    pub const Parsed = struct {
        base_type: ?[]const u8,
        name: []const u8,
        dimension_count: u8,
        indirection_count: u8,

        pub fn fromFishType(fish_type: MMTypes.FishType) Parsed {
            return .{ .name = @tagName(fish_type), .dimension_count = 0, .base_type = null, .indirection_count = 0 };
        }

        pub fn eql(self: Parsed, other: Parsed) bool {
            if (self.base_type) |base_type| {
                if (other.base_type == null)
                    return false;

                if (!std.mem.eql(u8, base_type, other.base_type.?))
                    return false;
            } else if (other.base_type) |base_type| {
                if (self.base_type == null)
                    return false;

                if (!std.mem.eql(u8, base_type, self.base_type.?))
                    return false;
            }

            return self.dimension_count == other.dimension_count and
                self.indirection_count == other.indirection_count and
                std.mem.eql(u8, self.name, other.name);
        }
    };

    pub const Resolved = union(enum) {
        pub const Pointer = struct {
            indirection_count: u8,
            type: union(enum) {
                fish: MMTypes.FishType,

                pub fn eql(self: @This(), other: @This()) bool {
                    if (std.meta.activeTag(self) != std.meta.activeTag(other))
                        return false;

                    return switch (self) {
                        .fish => self.fish == other.fish,
                    };
                }
            },
            fish: ?MMTypes.TypeReference,

            pub fn eql(self: Pointer, other: Pointer) bool {
                return self.indirection_count == other.indirection_count and self.type.eql(other.type);
            }
        };

        /// This is a normal fish type which exists at runtime
        fish: MMTypes.TypeReference,
        pointer: Pointer,
        /// This is a type which only exists at compile time, and represents a type, eg. the expression is pointing to a Type
        type: []const u8,
        integer_literal: void,
        float_literal: void,
        null_literal: void,

        pub fn eql(self: Resolved, other: Resolved) bool {
            if (std.meta.activeTag(self) != std.meta.activeTag(other))
                return false;

            return switch (self) {
                .fish => |fish| fish.eql(other.fish),
                .pointer => |pointer| pointer.eql(other.pointer),
                .type => |comptime_type| std.mem.eql(u8, comptime_type, other.type),
                .integer_literal, .float_literal, .null_literal => true,
            };
        }
    };

    /// An unresolved type
    parsed: Parsed,
    /// An unknown type, waiting to be resolved
    unknown: void,
    /// A resolved type
    resolved: Resolved,
};

pub const Node = union(enum) {
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
        name: []const u8,
        base_class: union(enum) {
            none: void,
            parsed: []const u8,
            resolved: struct {
                name: []const u8,
                ident: MMTypes.ResourceIdentifier,
            },
        },
        identifier: ?*Expression,

        fields: []const *Field,
        properties: []const *Property,
        enums: []const *Enum,
        functions: []const *Function,
        constructors: ?[]const *const Constructor,
        modifiers: MMTypes.Modifiers,

        type_reference: ?MMTypes.TypeReference,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("class{{ class_name = {s}, base_class = {?}, guid = {?} }}", .{ value.name, value.base_class, value.identifier });
        }
    };

    pub const Constructor = struct {
        body: ?*const Node.Expression,
        parameters: []Function.Parameter,
        modifiers: MMTypes.Modifiers,
    };

    pub const Enum = struct {
        pub const Member = struct {
            name: []const u8,
            value: *Node.Expression,
        };

        name: []const u8,
        modifiers: MMTypes.Modifiers,
        backing_type: Type,
        members: []const Member,
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
        parameters: []Parameter,
        body: ?*Expression,
        name: []const u8,
        mangled_name: ?[]const u8,
        modifiers: MMTypes.Modifiers,
        attributes: []const *Node.Attribute,

        pub const Parameter = struct {
            name: []const u8,
            type: Type,

            pub fn eql(self: Parameter, other: Parameter) bool {
                if (!std.mem.eql(u8, self.name, other.name))
                    return false;

                if (std.meta.activeTag(self.type) != std.meta.activeTag(other.type))
                    return false;

                return switch (self.type) {
                    .parsed => self.type.parsed.eql(other.type.parsed),
                    .resolved => self.type.resolved.eql(other.type.resolved),
                    .unknown => return true,
                };
            }

            pub fn sliceEql(self: []const *const Parameter, other: []const *const Parameter) bool {
                if (self.len != other.len)
                    return false;

                for (self, other) |param, other_param| {
                    if (!param.eql(other_param))
                        return false;
                }

                return true;
            }
        };

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("function{{ return_type = {}, modifiers = {}, parameters = {any}, name = {s} }}", .{ value.return_type, value.modifiers, value.parameters, value.name });
        }
    };

    pub const Expression = struct {
        pub const Contents = union(enum) {
            pub const LiteralBase = enum {
                binary,
                octal,
                decimal,
                hex,
            };

            pub const UnaryExpression = *Node.Expression;
            pub const BinaryExpression = struct {
                lefthand: *Expression,
                righthand: *Expression,
            };

            integer_literal: struct { base: LiteralBase, value: i64 },
            integer_literal_to_s32: UnaryExpression,
            integer_literal_to_safe_ptr: UnaryExpression,
            integer_literal_to_ptr: UnaryExpression,
            integer_literal_to_f32: UnaryExpression,
            integer_literal_to_s64: UnaryExpression,
            integer_literal_to_f64: UnaryExpression,
            null_literal_to_safe_ptr: void,
            null_literal_to_object_ptr: void,
            float_literal: struct { base: LiteralBase, value: f64 },
            float_literal_to_f32: UnaryExpression,
            float_literal_to_f64: UnaryExpression,
            cast: *Expression,
            guid_literal: u32,
            bool_literal: bool,
            null_literal: void,
            ascii_string_literal: []const u8,
            wide_string_literal: []const u8,
            field_access: struct {
                source: *Expression,
                field: []const u8,
            },
            dereference: UnaryExpression,
            class_name: []const u8,
            /// An access on a variable of some kind (eg `var.Field`), or a class type (eg `Thing.Func()`)
            variable_or_class_access: []const u8,
            /// An access of a class
            class_access: []const u8,
            variable_access: []const u8,
            numeric_negation: UnaryExpression,
            logical_negation: UnaryExpression,
            function_call: struct {
                source: ?*Expression,
                function: union(enum) {
                    name: []const u8,
                    function: struct {
                        function: *Function,
                        owning_type: MMTypes.TypeReference,
                    },
                },
                parameters: []const *Expression,
            },
            assignment: struct {
                destination: *Expression,
                value: *Expression,
            },
            block: []const Node,
            bitwise_and: BinaryExpression,
            bitwise_or: BinaryExpression,
            bitwise_xor: BinaryExpression,
            equal: BinaryExpression,
            not_equal: BinaryExpression,
            less_than: BinaryExpression,
            less_than_or_equal: BinaryExpression,
            greater_than: BinaryExpression,
            greater_than_or_equal: BinaryExpression,
            addition: BinaryExpression,
            subtraction: BinaryExpression,
            multiplication: BinaryExpression,
            division: BinaryExpression,
            logical_or: BinaryExpression,
            logical_and: BinaryExpression,
            vec2_construction: [2]*Expression,
            vec3_construction: [3]*Expression,
            vec4_construction: [4]*Expression,

            pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                return switch (value) {
                    .integer_literal => |literal| writer.print("expression_contents{{ .integer_literal = {} }}", .{literal}),
                    .integer_literal_to_s32 => |literal| writer.print("expression_contents {{ .integer_literal_to_s32 = {d} }}", .{literal}),
                    .integer_literal_to_safe_ptr => |literal| writer.print("expression_contents {{ .integer_literal_to_safe_ptr = {d} }}", .{literal}),
                    .integer_literal_to_ptr => |literal| writer.print("expression_contents {{ .integer_literal_to_ptr = {d} }}", .{literal}),
                    .cast => |literal| writer.print("expression_contents {{ .cast = {} }}", .{literal}),
                    .integer_literal_to_f32 => |literal| writer.print("expression_contents {{ .integer_literal_to_f32 = {d} }}", .{literal}),
                    .integer_literal_to_s64 => |literal| writer.print("expression_contents {{ .integer_literal_to_s64 = {d} }}", .{literal}),
                    .integer_literal_to_f64 => |literal| writer.print("expression_contents {{ .integer_literal_to_f64 = {d} }}", .{literal}),
                    .null_literal_to_safe_ptr => writer.print("expression_contents {{ .null_literal_to_safe_ptr }}", .{}),
                    .null_literal_to_object_ptr => writer.print("expression_contents {{ .null_literal_to_object_ptr }}", .{}),
                    .float_literal => |literal| writer.print("expression_contents{{ .float_literal = {} }}", .{literal}),
                    .float_literal_to_f32 => |literal| writer.print("expression_contents {{ .float_literal_to_f32 = {d} }}", .{literal}),
                    .float_literal_to_f64 => |literal| writer.print("expression_contents {{ .float_literal_to_f64 = {d} }}", .{literal}),
                    .null_literal => writer.print("expression_contents {{ .null_literal }}", .{}),
                    .guid_literal => |literal| writer.print("expression_contents{{ .guid_literal = {d} }}", .{literal}),
                    .bool_literal => |literal| writer.print("expression_contents{{ .bool_literal = {} }}", .{literal}),
                    .ascii_string_literal => |literal| writer.print("expression_contents{{ .ascii_string_literal = {s} }}", .{literal}),
                    .wide_string_literal => |literal| writer.print("expression_contents{{ .wide_string_literal = {s} }}", .{literal}),
                    .class_name => |literal| writer.print("expression_contents{{ .class_name = {s} }}", .{literal}),
                    .field_access => |literal| writer.print("expression_contents{{ .field_access = .{{ .source = {}, .field = {s} }} }}", .{ literal.source, literal.field }),
                    .dereference => |literal| writer.print("expression_contents{{ .dereference = .{{ .source = {} }} }}", .{literal}),
                    .variable_or_class_access => |literal| writer.print("expression_contents{{ .variable_or_class_access = {s} }}", .{literal}),
                    .variable_access => |literal| writer.print("expression_contents{{ .variable_access = {s} }}", .{literal}),
                    .class_access => |literal| writer.print("expression_contents{{ .class_access = {s} }}", .{literal}),
                    .numeric_negation => |literal| writer.print("expression_contents{{ .numeric_negation = {} }}", .{literal}),
                    .logical_negation => |literal| writer.print("expression_contents{{ .logical_negation = {} }}", .{literal}),
                    .function_call => |literal| writer.print("expression_contents{{ .function_call = .{{ .function = {}, .parameters = {any} }} }}", .{ literal.function, literal.parameters }),
                    .assignment => |literal| writer.print("expression_contents{{ .assignment = .{{ .destination = {}, .value = .{} }} }}", .{ literal.destination, literal.value }),
                    .block => |literal| writer.print("expression_contents{{ .block = {{ .body = {any} }} }}", .{literal}),
                    .bitwise_and => |literal| writer.print("expression_contents{{ .bitwise_and = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .bitwise_or => |literal| writer.print("expression_contents{{ .bitwise_or = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .bitwise_xor => |literal| writer.print("expression_contents{{ .bitwise_xor = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .not_equal => |literal| writer.print("expression_contents{{ .not_equal = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .equal => |literal| writer.print("expression_contents{{ .equal = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .less_than => |literal| writer.print("expression_contents{{ .less_than = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .less_than_or_equal => |literal| writer.print("expression_contents{{ .less_than_or_equal = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .greater_than => |literal| writer.print("expression_contents{{ .greater_than = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .greater_than_or_equal => |literal| writer.print("expression_contents{{ .greater_than_or_equal = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .addition => |literal| writer.print("expression_contents{{ .addition = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .subtraction => |literal| writer.print("expression_contents{{ .subtraction = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .multiplication => |literal| writer.print("expression_contents{{ .multiplication = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .division => |literal| writer.print("expression_contents{{ .division = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .logical_and => |literal| writer.print("expression_contents{{ .logical_and = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .logical_or => |literal| writer.print("expression_contents{{ .logical_or = {{ .lefthand = {}, .righthand = {} }} }}", .{ literal.lefthand, literal.righthand }),
                    .vec2_construction => |literal| writer.print("expression_contents {{ .vec2_construction = {d} }}", .{literal}),
                    .vec3_construction => |literal| writer.print("expression_contents {{ .vec3_construction = {d} }}", .{literal}),
                    .vec4_construction => |literal| writer.print("expression_contents {{ .vec4_construction = {d} }}", .{literal}),
                };
            }
        };

        contents: Contents,
        type: Type,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("expression{{.contents = {}, .type = {}}}", .{ value.contents, value.type });
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

    pub const WhileStatement = struct {
        condition: *Expression,
        body: *Expression,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("while_statement{{ condition = {}, body = {} }}", .{ value.condition, value.body });
        }
    };

    pub const Attribute = union(enum) {
        pub const NativeInvoke = struct {
            address: u24,
            toc_switch: bool,
        };

        native_invoke: NativeInvoke,
    };

    using: *Using,
    import: *Import,
    from_import: *FromImport,
    class: *Class,
    field: *Field,
    enumeration: *Enum,
    property: *Property,
    expression: *Expression,
    function: *Function,
    constructor: *Constructor,
    variable_declaration: *VariableDeclaration,
    return_statement: *ReturnStatement,
    if_statement: *IfStatement,
    while_statement: *WhileStatement,
    attribute: *Attribute,
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
    while (iter.peek() != null) {
        const modifiers = consumeModifiers(iter);

        const lexeme = iter.next().?;

        switch (hashKeyword(lexeme)) {
            hashKeyword("using") => try consumeUsingStatement(tree, iter),
            hashKeyword("import") => try consumeImportStatement(tree, iter),
            hashKeyword("from") => try consumeFromImportStatement(tree, iter),
            hashKeyword("class") => try consumeClassStatement(tree, iter, modifiers),
            else => {
                std.debug.panic("Unexpected top level lexeme \"{s}\"", .{lexeme});
            },
        }
    }
}

fn consumeClassStatement(tree: *Tree, iter: *SliceIterator(Lexeme), class_modifiers: MMTypes.Modifiers) !void {
    const node = try tree.allocator.create(Node.Class);
    errdefer tree.allocator.destroy(node);

    if (consumeArbitraryLexemeIfAvailable(iter, "{")) {
        std.debug.panic("Unexpected {{, expecting class name", .{});
    }

    //Unreachable since we error out right above if theres EOF
    const class_name = iter.next() orelse unreachable;

    const identifier: ?*Node.Expression = if (consumeArbitraryLexemeIfAvailable(iter, "(")) blk: {
        const expression = try consumeExpression(tree.allocator, iter);
        if (expression.contents != .guid_literal)
            std.debug.panic("needs to be a guid, is {s}", .{@tagName(expression.contents)});

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
    var enums = std.ArrayList(*Node.Enum).init(tree.allocator);
    defer enums.deinit();

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

        var attributes = std.ArrayList(*Node.Attribute).init(tree.allocator);
        while (iter.peek().?[0] == '@') {
            consumeArbitraryLexeme(iter, "@");

            const name = iter.next() orelse @panic("unexpected EOF when parsing attribute");

            const parameters = try consumeFunctionCallParameters(tree.allocator, iter);

            const attribute = try tree.allocator.create(Node.Attribute);

            if (std.mem.eql(u8, "NativeInvoke", name)) {
                std.debug.assert(parameters.len == 2);

                attribute.* = .{
                    .native_invoke = .{
                        .address = @intCast(parameters[0].contents.integer_literal.value),
                        .toc_switch = parameters[1].contents.bool_literal,
                    },
                };
            } else {
                std.debug.panic("Unknown attribute {s}", .{name});
            }

            try attributes.append(attribute);
        }

        const modifiers = consumeModifiers(iter);

        const next = iter.peek() orelse std.debug.panic("unexpected EOF when parsing declaration", .{});

        const next_keyword = maybeHashKeyword(next);

        //If the next keyword is a function, then we are consuming a function
        if (next_keyword == comptime hashKeyword("fn")) {
            //Get rid of the fn
            consumeArbitraryLexeme(iter, "fn");

            try functions.append(try consumeFunction(tree.allocator, iter, modifiers, attributes.items));
        } else if (next_keyword == comptime hashKeyword("enum")) {
            try enums.append(try consumeEnum(tree.allocator, iter, modifiers));
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
        .name = class_name,
        .base_class = if (base_class == null)
            .none
        else
            .{ .parsed = base_class.? },
        .functions = try functions.toOwnedSlice(),
        .fields = try fields.toOwnedSlice(),
        .properties = try properties.toOwnedSlice(),
        .enums = try enums.toOwnedSlice(),
        .identifier = identifier,
        .type_reference = null,
        .modifiers = class_modifiers,
    };

    try tree.root_elements.append(tree.allocator, .{ .class = node });
}

fn consumeEnum(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme), modifiers: MMTypes.Modifiers) !*Node.Enum {
    const node = try allocator.create(Node.Enum);
    errdefer allocator.destroy(node);

    consumeArbitraryLexeme(iter, "enum");

    const name = iter.next() orelse @panic("EOF");

    const backing_type = if (consumeArbitraryLexemeIfAvailable(iter, ":"))
        consumeTypeName(iter)
    else
        Type{ .parsed = .{ .name = "s32", .dimension_count = 0, .base_type = null, .indirection_count = 0 } };

    consumeArbitraryLexeme(iter, "{");

    consumeArbitraryLexeme(iter, "}");

    node.* = .{
        .modifiers = modifiers,
        .backing_type = backing_type,
        .name = name,
        //TODO: enum members
        .members = &.{},
    };

    return node;
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
                hashKeyword("while") => {
                    const while_statement = try consumeWhileStatement(allocator, iter);

                    try body.append(while_statement);
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
        if (err == std.fmt.ParseIntError.InvalidCharacter)
            return null;

        return err;
    };
}

fn isFloatLiteral(str: []const u8) !?f64 {
    //Strip the `f` suffix from the literal, if its there
    const literal = if (str[str.len - 1] == 'f')
        str[0 .. str.len - 1]
    else
        str;

    return std.fmt.parseFloat(f64, literal) catch |err| {
        if (err == std.fmt.ParseFloatError.InvalidCharacter)
            return null;

        return err;
    };
}

fn consumePrimaryExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    // Try to match bool/null literals
    if (consumeAnyMatches(iter, &.{ "true", "false", "null" })) |match| {
        switch (match) {
            .true, .false => {
                const expression = try allocator.create(Node.Expression);

                expression.* = .{
                    .contents = .{ .bool_literal = match == .true },
                    .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
                };

                return expression;
            },
            .null => {
                const expression = try allocator.create(Node.Expression);

                expression.* = .{
                    .contents = .null_literal,
                    .type = .{ .parsed = .{ .name = "null", .dimension_count = 0, .base_type = null, .indirection_count = 0 } },
                };

                return expression;
            },
        }
    }

    const first = iter.next() orelse @panic("eof");

    if (first[0] == '(') {
        const expression = consumeExpression(allocator, iter);

        consumeArbitraryLexeme(iter, ")");

        return expression;
    }

    if (try isInt(i65, first)) |s65| {
        const base: Node.Expression.Contents.LiteralBase = if (first.len >= 2) switch (hashKeyword(first[0..2])) {
            hashKeyword("0x") => .hex,
            hashKeyword("0o") => .octal,
            hashKeyword("0b") => .binary,
            else => .decimal,
        } else .decimal;

        const expression = try allocator.create(Node.Expression);

        expression.* = .{
            .contents = .{ .integer_literal = .{
                .base = base,
                .value = if (base == .decimal)
                    @intCast(s65)
                else
                    @bitCast(@as(u64, @intCast(s65))),
            } },
            .type = .unknown,
        };

        return expression;
    }

    if (try isFloatLiteral(first)) |float| {
        const expression = try allocator.create(Node.Expression);

        //TODO: non-base-10 float literals
        expression.* = .{
            .contents = .{ .float_literal = .{
                .base = .decimal,
                .value = float,
            } },
            .type = .unknown,
        };

        return expression;
    }

    const next = iter.peek() orelse @panic("EOF");

    //If the next expression is a ( then its a function call
    if (next[0] == '(') {
        const expression = try allocator.create(Node.Expression);

        const parameters = try consumeFunctionCallParameters(allocator, iter);

        //Builtin functions
        if (maybeHashKeyword(first)) |keyword| {
            switch (keyword) {
                hashKeyword("float2") => {
                    if (parameters.len != 2)
                        @panic("wrong parameter length for vec2 construction");

                    expression.* = .{
                        .contents = .{ .vec2_construction = parameters[0..2].* },
                        .type = .{ .parsed = Type.Parsed.fromFishType(.vec2) },
                    };

                    return expression;
                },
                hashKeyword("float3") => {
                    if (parameters.len != 3)
                        @panic("wrong parameter length for vec3 construction");

                    expression.* = .{
                        .contents = .{ .vec3_construction = parameters[0..3].* },
                        .type = .{ .parsed = Type.Parsed.fromFishType(.vec3) },
                    };

                    return expression;
                },
                hashKeyword("float4") => {
                    if (parameters.len != 4)
                        @panic("wrong parameter length for vec4 construction");

                    expression.* = .{
                        .contents = .{ .vec4_construction = parameters[0..4].* },
                        .type = .{ .parsed = Type.Parsed.fromFishType(.vec4) },
                    };

                    return expression;
                },
                else => {},
            }
        }

        expression.* = .{
            .contents = .{ .function_call = .{
                .source = null,
                .function = .{ .name = first },
                .parameters = parameters,
            } },
            .type = .unknown,
        };

        return expression;
    }

    //If the first char is a g
    if (first[0] == 'g') {
        // And the rest of the lexeme is a number that fits into a u32, this is a GUID literal
        if (try isInt(u32, first[1..])) |guid| {
            const expression = try allocator.create(Node.Expression);
            expression.* = .{
                .contents = .{ .guid_literal = guid },
                .type = .unknown,
            };
            return expression;
        }
    }

    //TODO: hash literals here
    if (first[0] == 'h') {}

    if (isWideStringLiteral(first)) |wide_string_literal| {
        const expression = try allocator.create(Node.Expression);
        expression.* = .{
            .contents = .{ .wide_string_literal = wide_string_literal },
            .type = .unknown,
        };
        return expression;
    }

    if (isAsciiStringLiteral(first)) |string_literal| {
        const expression = try allocator.create(Node.Expression);
        expression.* = .{
            .contents = .{ .ascii_string_literal = string_literal },
            .type = .unknown,
        };
        return expression;
    }

    //If none of the other checks matched, this is a variable access
    const expression = try allocator.create(Node.Expression);
    expression.* = .{
        .contents = .{ .variable_or_class_access = first },
        .type = .unknown,
    };
    return expression;
}

fn consumeDotExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumePrimaryExpression(allocator, iter);

    while (consumeArbitraryLexemeIfAvailable(iter, ".")) {
        const field_access = try allocator.create(Node.Expression);

        const name = iter.next() orelse @panic("EOF");

        const next = iter.peek() orelse @panic("EOF");

        // `ptr.*` dereferencing
        field_access.* = if (name[0] == '*') .{
            .contents = .{ .dereference = node },
            .type = .unknown,
        } else .{
            .contents = if (next[0] == '(')
                .{
                    .function_call = .{
                        .source = node,
                        .function = .{ .name = name },
                        .parameters = try consumeFunctionCallParameters(allocator, iter),
                    },
                }
            else
                .{
                    .field_access = .{
                        .field = name,
                        .source = node,
                    },
                },
            .type = .unknown,
        };

        node = field_access;
    }

    return node;
}

fn consumeUnaryExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    if (consumeAnyMatches(iter, &.{ "!", "-" })) |match| {
        const unary = try allocator.create(Node.Expression);

        unary.* = switch (match) {
            .@"!" => .{
                .contents = .{
                    .logical_negation = try consumeUnaryExpression(allocator, iter),
                },
                .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
            },
            .@"-" => .{
                .contents = .{
                    .numeric_negation = try consumeUnaryExpression(allocator, iter),
                },
                .type = .unknown,
            },
        };

        return unary;
    } else {
        return consumeDotExpression(allocator, iter);
    }
}

fn consumeFactorExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeUnaryExpression(allocator, iter);

    while (consumeAnyMatches(iter, &.{ "/", "*" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const factor = try allocator.create(Node.Expression);

                factor.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@"/" => @tagName(Node.Expression.Contents.division),
                            .@"*" => @tagName(Node.Expression.Contents.multiplication),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try consumeUnaryExpression(allocator, iter),
                        },
                    ),
                    .type = .unknown,
                };

                break :blk factor;
            },
        };
    }

    return node;
}

fn consumeTermExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeFactorExpression(allocator, iter);

    while (consumeAnyMatches(iter, &.{ "-", "+" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const term = try allocator.create(Node.Expression);

                term.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@"+" => @tagName(Node.Expression.Contents.addition),
                            .@"-" => @tagName(Node.Expression.Contents.subtraction),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try consumeFactorExpression(allocator, iter),
                        },
                    ),
                    .type = .unknown,
                };

                break :blk term;
            },
        };
    }

    return node;
}

fn consumeBitwiseExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeTermExpression(allocator, iter);

    while (consumeAnyMatches(iter, &.{ "&", "^", "|" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const bitwise = try allocator.create(Node.Expression);

                bitwise.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@"&" => @tagName(Node.Expression.Contents.bitwise_and),
                            .@"^" => @tagName(Node.Expression.Contents.bitwise_xor),
                            .@"|" => @tagName(Node.Expression.Contents.bitwise_or),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try consumeTermExpression(allocator, iter),
                        },
                    ),
                    .type = .unknown,
                };

                break :blk bitwise;
            },
        };
    }

    return node;
}

fn consumeCastExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeBitwiseExpression(allocator, iter);

    while (consumeArbitraryLexemeIfAvailable(iter, "as")) {
        const cast_expression = try allocator.create(Node.Expression);

        cast_expression.* = .{
            .contents = .{ .cast = node },
            .type = consumeTypeName(iter),
        };

        node = cast_expression;
    }

    return node;
}

fn consumeComparisonExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeCastExpression(allocator, iter);

    while (consumeAnyMatches(iter, &.{ ">", ">=", "<", "<=" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const comparison = try allocator.create(Node.Expression);

                comparison.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@">" => @tagName(Node.Expression.Contents.greater_than),
                            .@">=" => @tagName(Node.Expression.Contents.greater_than_or_equal),
                            .@"<" => @tagName(Node.Expression.Contents.less_than),
                            .@"<=" => @tagName(Node.Expression.Contents.less_than_or_equal),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try consumeCastExpression(allocator, iter),
                        },
                    ),
                    .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
                };

                break :blk comparison;
            },
        };
    }

    return node;
}

fn consumeEqualityExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeComparisonExpression(allocator, iter);

    while (consumeAnyMatches(iter, &.{ "==", "!=" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const equality = try allocator.create(Node.Expression);

                equality.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        if (keyword == .@"==") @tagName(Node.Expression.Contents.equal) else @tagName(Node.Expression.Contents.not_equal),
                        .{
                            .lefthand = node,
                            .righthand = try consumeComparisonExpression(allocator, iter),
                        },
                    ),
                    .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
                };

                break :blk equality;
            },
        };
    }

    return node;
}

fn consumeLogicalAndExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeEqualityExpression(allocator, iter);

    while (consumeArbitraryLexemeIfAvailable(iter, "&&")) {
        const logical_and = try allocator.create(Node.Expression);

        logical_and.* = .{
            .contents = .{
                .logical_and = .{
                    .lefthand = node,
                    .righthand = try consumeEqualityExpression(allocator, iter),
                },
            },
            .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
        };

        node = logical_and;
    }

    return node;
}

fn consumeLogicalOrExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    var node = try consumeLogicalAndExpression(allocator, iter);

    while (consumeArbitraryLexemeIfAvailable(iter, "||")) {
        const logical_or = try allocator.create(Node.Expression);

        logical_or.* = .{
            .contents = .{
                .logical_or = .{
                    .lefthand = node,
                    .righthand = try consumeLogicalAndExpression(allocator, iter),
                },
            },
            .type = .{ .parsed = Type.Parsed.fromFishType(.bool) },
        };

        node = logical_or;
    }

    return node;
}

fn consumeAssignmentExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    const destination = try consumeLogicalOrExpression(allocator, iter);

    if (consumeArbitraryLexemeIfAvailable(iter, "=")) {
        const value = try consumeAssignmentExpression(allocator, iter);

        const assignment = try allocator.create(Node.Expression);

        assignment.* = .{
            .contents = .{ .assignment = .{
                .destination = destination,
                .value = value,
            } },
            .type = .unknown,
        };

        return assignment;
    }

    return destination;
}

fn consumeExpression(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) Error!*Node.Expression {
    return consumeAssignmentExpression(allocator, iter);
}

fn MatchEnum(comptime matches: []const [:0]const u8) type {
    const Enum = @Type(.{
        .Enum = .{
            .tag_type = std.math.IntFittingRange(0, matches.len),
            .is_exhaustive = true,
            .decls = &.{},
            .fields = comptime blk: {
                var fields: [matches.len]std.builtin.Type.EnumField = undefined;

                for (matches, &fields, 0..) |match, *field, i| {
                    field.* = .{
                        .name = match,
                        .value = i,
                    };
                }

                break :blk &fields;
            },
        },
    });

    return Enum;
}

fn consumeAnyMatches(iter: *SliceIterator(Lexeme), comptime matches: []const [:0]const u8) ?MatchEnum(matches) {
    const matches_keywords = comptime blk: {
        var keywords: [matches.len]u72 = undefined;

        for (matches, 0..) |match, i| {
            keywords[i] = hashKeyword(match);
        }

        break :blk keywords;
    };

    const Enum = MatchEnum(matches);

    //If the next lexeme is a valid keyword
    if (maybeHashKeyword(iter.peek() orelse @panic("EOF"))) |keyword| {
        inline for (matches_keywords, matches) |match, match_str| {
            if (keyword == match) {
                _ = iter.next() orelse unreachable;

                return @field(Enum, match_str);
            }
        }
    }

    return null;
}

fn consumeFunctionCallParameters(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) ![]const *Node.Expression {
    var parameters = std.ArrayList(*Node.Expression).init(allocator);
    defer parameters.deinit();

    consumeArbitraryLexeme(iter, "(");

    var i: usize = 0;
    while (true) {
        const next = iter.peek() orelse @panic("EOF");

        if (next[0] == ')') {
            _ = iter.next();
            break;
        }

        if (i > 0)
            consumeArbitraryLexeme(iter, ",");

        try parameters.append(try consumeExpression(allocator, iter));

        i += 1;
    }

    return parameters.toOwnedSlice();
}

fn consumeTypeName(iter: *SliceIterator(Lexeme)) Type {
    const name, const base_type: ?[]const u8 = blk: {
        const name = iter.next() orelse std.debug.panic("unexpected EOF when reading type name", .{});

        if (consumeArbitraryLexemeIfAvailable(iter, ".")) {
            break :blk .{ iter.next().?, name };
        }

        break :blk .{ name, null };
    };

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

    var indirection_count: u8 = 0;
    while (consumeArbitraryLexemeIfAvailable(iter, "*"))
        indirection_count += 1;

    return .{ .parsed = .{
        .name = name,
        .dimension_count = dimension_count,
        .base_type = base_type,
        .indirection_count = indirection_count,
    } };
}

fn consumeFunctionParameters(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) ![]Node.Function.Parameter {
    consumeArbitraryLexeme(iter, "(");

    var parameters = std.ArrayListUnmanaged(Node.Function.Parameter){};

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

    return parameters.items;
}

fn consumeFunction(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme), modifiers: MMTypes.Modifiers, attributes: []const *Node.Attribute) !*Node.Function {
    const node = try allocator.create(Node.Function);
    errdefer allocator.destroy(node);

    const name = iter.next() orelse std.debug.panic("unexpected EOF when parsing function name", .{});

    const parameters = try consumeFunctionParameters(allocator, iter);

    const return_type: Type = blk: {
        if (std.mem.eql(u8, iter.peek() orelse std.debug.panic("EOF", .{}), "->")) {
            consumeArbitraryLexeme(iter, "->");
            break :blk consumeTypeName(iter);
        }

        break :blk .{ .parsed = Type.Parsed.fromFishType(.void) };
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
        .mangled_name = null,
        .attributes = attributes,
    };

    return node;
}

fn isWideStringLiteral(lexeme: Lexeme) ?[]const u8 {
    if (lexeme.len < 3)
        return null;

    if (lexeme[0] != 'L')
        return null;

    if (lexeme[1] != '\'' or lexeme[lexeme.len - 1] != '\'')
        return null;

    //Strip to just the contents
    return lexeme[2 .. lexeme.len - 1];
}

fn isAsciiStringLiteral(lexeme: Lexeme) ?[]const u8 {
    if (lexeme.len < 2)
        return null;

    if (lexeme[0] != '\'' or lexeme[lexeme.len - 1] != '\'')
        return null;

    //Strip to just the contents
    return lexeme[1 .. lexeme.len - 1];
}

fn consumeWhileStatement(allocator: std.mem.Allocator, iter: *SliceIterator(Lexeme)) !Node {
    const node = try allocator.create(Node.WhileStatement);

    consumeArbitraryLexeme(iter, "while");

    consumeArbitraryLexeme(iter, "(");
    const condition = try consumeExpression(allocator, iter);
    consumeArbitraryLexeme(iter, ")");

    const body = try consumeBlockExpression(allocator, iter);

    node.* = .{
        .condition = condition,
        .body = body,
    };

    return .{ .while_statement = node };
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
        const lexeme = iter.peek() orelse std.debug.panic("unexpected EOF when parsing modifiers", .{});

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
                var wanted_imports = std.ArrayListUnmanaged(FromImportWanted.ImportedFunction){};

                was_multi_import = true;

                //If the next token is a `}`, consume it and break out
                if (consumeArbitraryLexemeIfAvailable(iter, "}")) {
                    break :blk &.{};
                }

                while (true) {
                    const curr = iter.next() orelse std.debug.panic("unexpected EOF in multi import block", .{});

                    //TODO: support renamed function imports eg. `import { Messaging_PoppetInfoMessage: ShowNotification }`

                    const name = unwrapStringLiteral(curr);

                    //Append the new import
                    try wanted_imports.append(tree.allocator, .{
                        .name = name,
                        .original_name = name,
                    });

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
        else => .{
            .single = .{
                //TODO: do we need to support the rename syntax for these?
                .name = first_import_lexeme,
                .original_name = first_import_lexeme,
            },
        },
    };

    node.* = .{
        .target = unwrapStringLiteral(target),
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
            const prev_lexeme = iter.slice[iter.pos - 2];
            const next_lexeme = iter.peek() orelse @panic("eof");

            std.debug.panic(
                "unexpected lexeme {s}, expected {s}, prev {s}, next {s}",
                .{ next, intended, prev_lexeme, next_lexeme },
            );
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

    const single_char_lexemes: []const u8 = "()[]{}!*,:;+.'<>+-@";
    const special_double_lexemes: []const u16 = &.{
        @intCast(hashKeyword("->")),
        @intCast(hashKeyword(">>")),
        @intCast(hashKeyword("<<")),
        @intCast(hashKeyword("==")),
        @intCast(hashKeyword("!=")),
        @intCast(hashKeyword(">=")),
        @intCast(hashKeyword("<=")),
    };

    pub fn next(self: *Lexemeizer) !?Lexeme {
        const iter = self.source[self.pos..];

        var is_number = true;
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

            if (!std.ascii.isDigit(char) and char != '.')
                is_number = false;

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

                // If this isnt the start of a lexeme, and its been a number so far,
                // and the found single char lexeme is a `.`, then we dont want to exit out,
                // and should continue parsing as float literal
                if (!just_started_lexeme and is_number and char == '.') {
                    i += 1;
                    continue;
                }

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
