//! Parses an A# source file into an abstract syntax tree

const std = @import("std");

const MMTypes = @import("MMTypes.zig");
const SliceIterator = @import("slice_iterator.zig").SliceIterator;

pub const Self = @This();

pub const UsingType = enum {
    library,
};

tree: Tree,
allocator: std.mem.Allocator,
iter: SliceIterator(Lexeme),
type_intern_pool: *TypeInternPool,
root_progress_node: std.Progress.Node,

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

pub const TypeInternPool = struct {
    const Key = union(enum(u8)) {
        parsed: Type.Parsed = 0,
        integer_literal: void = 1,
        float_literal: void = 2,
        null_literal: void = 3,
        int_string_literal: void = 4,
    };

    pub const HashMap = std.ArrayHashMap(Key, Type, struct {
        pub fn hash(self: @This(), s: Key) u32 {
            _ = self;

            var h = std.hash.Wyhash.init(0);

            h.update(std.mem.asBytes(&@intFromEnum(s)));
            switch (s) {
                .integer_literal, .float_literal, .null_literal, .int_string_literal => {},
                .parsed => |parsed| {
                    h.update(&.{ @intFromBool(parsed.base_type == null), parsed.indirection_count, parsed.dimension_count });
                    h.update(parsed.base_type orelse "");
                    h.update(parsed.name);
                },
            }

            return @truncate(h.final());
        }

        pub fn eql(self: @This(), a: Key, b: Key, b_index: usize) bool {
            _ = self; // autofix
            _ = b_index; // autofix

            return switch (a) {
                .parsed => |parsed| b == .parsed and parsed.eql(b.parsed),
                .integer_literal => b == .integer_literal,
                .float_literal => b == .float_literal,
                .null_literal => b == .null_literal,
                .int_string_literal => b == .int_string_literal,
            };
        }
    }, true);

    hash_map: HashMap,

    pub const Index = enum(u32) {
        unknown = std.math.maxInt(u32),
        _,
    };

    pub fn deinit(self: *TypeInternPool) void {
        self.hash_map.deinit();
    }

    pub fn get(self: *const TypeInternPool, index: Index) *const Type {
        std.debug.assert(index != .unknown);

        return &self.hash_map.values()[@intFromEnum(index)];
    }

    pub fn getMutable(self: *const TypeInternPool, index: Index) *Type {
        std.debug.assert(index != .unknown);

        return &self.hash_map.values()[@intFromEnum(index)];
    }

    pub fn getKey(self: *const TypeInternPool, index: Index) *const Key {
        std.debug.assert(index != .unknown);

        return &self.hash_map.keys()[@intFromEnum(index)];
    }

    pub fn getIndex(self: *const TypeInternPool, parsed: Key) ?Index {
        const index = self.hash_map.getIndex(parsed);

        if (index) |idx|
            return @enumFromInt(idx)
        else
            return null;
    }

    pub fn putParsed(self: *TypeInternPool, value: Type) !Index {
        try self.hash_map.putNoClobber(.{ .parsed = value.parsed }, value);

        return self.getIndex(.{ .parsed = value.parsed }).?;
    }

    pub fn getOrPutParsed(self: *TypeInternPool, value: Type) !Index {
        return self.getIndex(.{ .parsed = value.parsed }) orelse self.putParsed(value);
    }

    pub fn integerLiteral(self: *TypeInternPool) !Index {
        return @enumFromInt((try self.hash_map.getOrPutValue(.integer_literal, .{ .resolved = .integer_literal })).index);
    }

    pub fn intStringLiteral(self: *TypeInternPool) !Index {
        return @enumFromInt((try self.hash_map.getOrPutValue(.int_string_literal, .{ .resolved = .int_string_literal })).index);
    }

    pub fn floatLiteral(self: *TypeInternPool) !Index {
        return @enumFromInt((try self.hash_map.getOrPutValue(.float_literal, .{ .resolved = .float_literal })).index);
    }

    pub fn nullLiteral(self: *TypeInternPool) !Index {
        return @enumFromInt((try self.hash_map.getOrPutValue(.null_literal, .{ .resolved = .null_literal })).index);
    }

    pub fn fishTypePtr(self: *TypeInternPool, fish_type: MMTypes.FishType, indirection_count: u8) !Index {
        const value: Type = .{
            .resolved = .{
                .pointer = .{
                    .indirection_count = indirection_count,
                    .type = .{ .fish = fish_type },
                    .fish = .{
                        .array_base_machine_type = .void,
                        .dimension_count = 0,
                        .fish_type = .s32,
                        .machine_type = .s32,
                        .script = null,
                        .type_name = 0xFFFFFFFF,
                    },
                },
            },
        };

        const parsed: Type.Parsed = .{
            .base_type = null,
            .name = @tagName(fish_type),
            .indirection_count = indirection_count,
            .dimension_count = 0,
        };

        if (self.getIndex(.{ .parsed = parsed })) |index|
            return index;

        return @enumFromInt((try self.hash_map.getOrPutValue(.{ .parsed = parsed }, value)).index);
    }

    pub fn fromFishType(self: *TypeInternPool, fish_type: MMTypes.FishType) !Index {
        const value: Type = .{
            .parsed = .{
                .base_type = null,
                .name = @tagName(fish_type),
                .dimension_count = 0,
                .indirection_count = 0,
            },
        };

        if (self.getIndex(.{ .parsed = value.parsed })) |index|
            return index;

        return self.putParsed(value);
    }

    pub fn wideStringType(self: *TypeInternPool) !Index {
        return self.getOrPutParsed(.{ .parsed = .{
            .name = "String",
            .base_type = null,
            .indirection_count = 0,
            .dimension_count = 0,
        } });
    }

    pub fn asciiStringType(self: *TypeInternPool) !Index {
        return self.getOrPutParsed(.{ .parsed = .{
            .name = "StringA",
            .base_type = null,
            .indirection_count = 0,
            .dimension_count = 0,
        } });
    }

    pub const Type = union(enum) {
        pub const Parsed = struct {
            base_type: ?[]const u8,
            name: []const u8,
            dimension_count: u8,
            indirection_count: u8,

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

            fish: MMTypes.TypeReference,
            pointer: Pointer,
            integer_literal: void,
            float_literal: void,
            null_literal: void,
            int_string_literal: void,

            pub fn eql(self: Resolved, other: Resolved) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other))
                    return false;

                return switch (self) {
                    .fish => |fish| fish.eql(other.fish),
                    .pointer => |pointer| pointer.eql(other.pointer),
                    .integer_literal, .float_literal, .null_literal, .int_string_literal => true,
                };
            }

            pub fn machineType(self: Resolved) MMTypes.MachineType {
                return switch (self) {
                    .fish => |fish| fish.machine_type,
                    .pointer => .s32,
                    else => unreachable,
                };
            }

            pub fn valueTypeReference(self: Resolved) MMTypes.TypeReference {
                return switch (self) {
                    .fish => |fish| fish,
                    .pointer => |pointer| pointer.fish.?,
                    else => unreachable,
                };
            }
        };

        parsed: Parsed,
        unknown: void,
        resolved: Resolved,
    };
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
                has_parameterless_constructor: bool,
                type_reference: ?MMTypes.TypeReference,
            },
        },
        identifier: ?*Expression,

        fields: []const *Field,
        properties: []const *Property,
        enums: []const *Enum,
        functions: []const *Function,
        constructors: []const *Constructor,
        modifiers: MMTypes.Modifiers,

        type_reference: ?MMTypes.TypeReference,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("class{{ class_name = {s}, base_class = {?}, guid = {?} }}", .{ value.name, value.base_class, value.identifier });
        }
    };

    pub const Constructor = struct {
        body: ?*Node.Expression,
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
        backing_type: TypeInternPool.Index,
        members: []const Member,
    };

    pub const Field = struct {
        modifiers: MMTypes.Modifiers,
        name: []const u8,
        type: TypeInternPool.Index,
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
        type: TypeInternPool.Index,
        name: []const u8,
        modifiers: MMTypes.Modifiers,
    };

    pub const Function = struct {
        return_type: TypeInternPool.Index,
        parameters: []Parameter,
        body: ?*Expression,
        name: []const u8,
        mangled_name: ?[]const u8,
        modifiers: MMTypes.Modifiers,
        attributes: []const *Node.Attribute,

        pub const Parameter = struct {
            name: []const u8,
            type: TypeInternPool.Index,

            pub fn eql(self: Parameter, other: Parameter) bool {
                if (!std.mem.eql(u8, self.name, other.name))
                    return false;

                return self.type == other.type;
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
            int_string_literal_to_s32: UnaryExpression,
            integer_literal_to_safe_ptr: UnaryExpression,
            integer_literal_to_ptr: UnaryExpression,
            integer_literal_to_f32: UnaryExpression,
            integer_literal_to_s64: UnaryExpression,
            integer_literal_to_f64: UnaryExpression,
            null_literal_to_safe_ptr: void,
            null_literal_to_ptr: void,
            null_literal_to_object_ref: void,
            float_literal: struct { base: LiteralBase, value: f64 },
            float_literal_to_f32: UnaryExpression,
            float_literal_to_f64: UnaryExpression,
            cast: *Expression,
            guid_literal: u32,
            bool_literal: bool,
            null_literal: void,
            ascii_string_literal: []const u8,
            wide_string_literal: []const u8,
            int_string_literal: []const u8,
            field_access: struct {
                source: *Expression,
                field: []const u8,
            },
            dereference: UnaryExpression,
            class_name: []const u8,
            variable_or_class_access: []const u8,
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
            native_strcpy: struct {
                src: *Expression,
                dst: *Expression,
                fill_byte: ?u8,
                length: ?u32,
            },
            new_array: struct {
                size: *Expression,
                child: TypeInternPool.Index,
            },
            array_access: BinaryExpression,

            pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                return switch (value) {
                    .integer_literal => |literal| writer.print("expression_contents{{ .integer_literal = {} }}", .{literal}),
                    .integer_literal_to_s32 => |literal| writer.print("expression_contents {{ .integer_literal_to_s32 = {d} }}", .{literal}),
                    .int_string_literal_to_s32 => |literal| writer.print("expression_contents {{ .integer_literal_to_s32 = {d} }}", .{literal}),
                    .integer_literal_to_safe_ptr => |literal| writer.print("expression_contents {{ .integer_literal_to_safe_ptr = {d} }}", .{literal}),
                    .integer_literal_to_ptr => |literal| writer.print("expression_contents {{ .integer_literal_to_ptr = {d} }}", .{literal}),
                    .cast => |literal| writer.print("expression_contents {{ .cast = {} }}", .{literal}),
                    .integer_literal_to_f32 => |literal| writer.print("expression_contents {{ .integer_literal_to_f32 = {d} }}", .{literal}),
                    .integer_literal_to_s64 => |literal| writer.print("expression_contents {{ .integer_literal_to_s64 = {d} }}", .{literal}),
                    .integer_literal_to_f64 => |literal| writer.print("expression_contents {{ .integer_literal_to_f64 = {d} }}", .{literal}),
                    .null_literal_to_safe_ptr => writer.print("expression_contents {{ .null_literal_to_safe_ptr }}", .{}),
                    .null_literal_to_ptr => writer.print("expression_contents {{ .null_literal_to_ptr }}", .{}),
                    .null_literal_to_object_ref => writer.print("expression_contents {{ .null_literal_to_object_ref }}", .{}),
                    .float_literal => |literal| writer.print("expression_contents{{ .float_literal = {} }}", .{literal}),
                    .float_literal_to_f32 => |literal| writer.print("expression_contents {{ .float_literal_to_f32 = {d} }}", .{literal}),
                    .float_literal_to_f64 => |literal| writer.print("expression_contents {{ .float_literal_to_f64 = {d} }}", .{literal}),
                    .null_literal => writer.print("expression_contents {{ .null_literal }}", .{}),
                    .guid_literal => |literal| writer.print("expression_contents{{ .guid_literal = {d} }}", .{literal}),
                    .bool_literal => |literal| writer.print("expression_contents{{ .bool_literal = {} }}", .{literal}),
                    .ascii_string_literal => |literal| writer.print("expression_contents{{ .ascii_string_literal = {s} }}", .{literal}),
                    .wide_string_literal => |literal| writer.print("expression_contents{{ .wide_string_literal = {s} }}", .{literal}),
                    .int_string_literal => |literal| writer.print("expression_contents{{ .int_string_literal = {s} }}", .{literal}),
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
                    .native_strcpy => |literal| writer.print("expression_contents {{ .native_strcpy = {{ .dst = {}, .fill_byte = {?d}, .length = {?d}, .src = {} }} }}", .{ literal.dst, literal.fill_byte, literal.length, literal.src }),
                    .new_array => |new_array| writer.print("new_array {{ .new_array = {} }}", .{new_array.size}),
                    .array_access => |array_access| writer.print("array_access {{ .array_access = .{{ .arr = {}, .idx = {} }} }}", .{ array_access.lefthand, array_access.righthand }),
                };
            }
        };

        contents: Contents,
        type: TypeInternPool.Index,

        pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            return writer.print("expression{{.contents = {}, .type = {}}}", .{ value.contents, value.type });
        }
    };

    pub const VariableDeclaration = struct {
        name: []const u8,
        type: TypeInternPool.Index,
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
            address: u32,
            toc_index: u8,
        };

        native_invoke: NativeInvoke,
        initializer: void,
    };

    pub const InlineAsmStatement = struct {
        bytecode: []Bytecode,
        jump_targets: std.StringHashMap(u32),
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
    inline_asm_statement: *InlineAsmStatement,
    @"unreachable": void,
};

pub const Tree = struct {
    allocator: std.mem.Allocator,
    root_elements: std.ArrayListUnmanaged(Node),
};

const Lexeme = []const u8;

pub fn parse(
    allocator: std.mem.Allocator,
    lexemes: []const Lexeme,
    type_intern_pool: *TypeInternPool,
    parent_progress_node: std.Progress.Node,
) Error!Self {
    var self = Self{
        .tree = .{
            .allocator = allocator,
            .root_elements = .{},
        },
        .allocator = allocator,
        .iter = .{
            .pos = 0,
            .slice = lexemes,
        },
        .type_intern_pool = type_intern_pool,
        .root_progress_node = parent_progress_node.start("Parsing", 1),
    };

    try self.consumeTopLevel();

    self.root_progress_node.end();

    return self;
}

const KeywordHash = u88;

fn maybeHashKeyword(keyword: []const u8) ?KeywordHash {
    if (keyword.len > (@bitSizeOf(KeywordHash) / 8)) {
        return null;
    }

    return hashKeyword(keyword);
}

fn hashKeyword(keyword: []const u8) KeywordHash {
    var val: KeywordHash = 0;
    @memcpy(std.mem.asBytes(&val)[0..keyword.len], keyword);
    return val;
}

fn consumeTopLevel(self: *Self) !void {
    const progress_node = self.root_progress_node.start("Parsing top level", 0);
    defer progress_node.end();

    while (self.iter.peek() != null) {
        const modifiers = self.consumeModifiers(progress_node);

        const lexeme = self.iter.next().?;

        switch (hashKeyword(lexeme)) {
            hashKeyword("using") => try self.consumeUsingStatement(progress_node),
            hashKeyword("import") => try self.consumeImportStatement(progress_node),
            hashKeyword("from") => try self.consumeFromImportStatement(progress_node),
            hashKeyword("class") => try self.consumeClassStatement(progress_node, modifiers),
            else => {
                std.debug.panic("Unexpected top level lexeme \"{s}\"", .{lexeme});
            },
        }
    }
}

fn consumeClassStatement(self: *Self, parent_progress_node: std.Progress.Node, class_modifiers: MMTypes.Modifiers) !void {
    const progress_node = parent_progress_node.start("Parsing class", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Class);
    errdefer self.allocator.destroy(node);

    if (self.consumeArbitraryLexemeIfAvailable("{")) {
        std.debug.panic("Unexpected {{, expecting class name", .{});
    }

    //Unreachable since we error out right above if theres EOF
    const class_name = self.iter.next() orelse unreachable;

    const identifier: ?*Node.Expression = if (self.consumeArbitraryLexemeIfAvailable("(")) blk: {
        const expression = try self.consumeExpression(progress_node);
        if (expression.contents != .guid_literal)
            std.debug.panic("needs to be a guid, is {s}", .{@tagName(expression.contents)});

        self.consumeArbitraryLexeme(")");

        break :blk expression;
    } else null;

    //Consume the scope start if available, if not, parse the class property
    const base_class: ?[]const u8 = if (!self.consumeArbitraryLexemeIfAvailable("{")) blk: {
        const property = self.iter.next() orelse std.debug.panic("unexpected EOF when parsing class properties", .{});
        switch (hashKeyword(property)) {
            hashKeyword("extends") => {
                const base_class = self.iter.next() orelse std.debug.panic("unexpected EOF when reading base class name", .{});

                //Consume the scope start
                self.consumeArbitraryLexeme("{");

                break :blk base_class;
            },
            else => std.debug.panic("Unknown class property {s}", .{property}),
        }
    } else null;

    var functions = std.ArrayList(*Node.Function).init(self.allocator);
    defer functions.deinit();
    var fields = std.ArrayList(*Node.Field).init(self.allocator);
    defer fields.deinit();
    var properties = std.ArrayList(*Node.Property).init(self.allocator);
    defer properties.deinit();
    var enums = std.ArrayList(*Node.Enum).init(self.allocator);
    defer enums.deinit();

    var constructors = std.ArrayList(*Node.Constructor).init(self.allocator);
    defer constructors.deinit();

    while (true) {
        const lexeme = self.iter.peek() orelse std.debug.panic("unexpected EOF when parsing class body", .{});

        // std.debug.print("vv {s}\n", .{lexeme});

        //If we hit a `}`, we have reached the end of scope
        if (lexeme[0] == '}') {
            self.consumeArbitraryLexeme("}");

            break;
        }

        const attributes_progress_node = progress_node.start("Parsing attributes", 0);

        var attributes = std.ArrayList(*Node.Attribute).init(self.allocator);
        while (self.iter.peek().?[0] == '@') {
            const name = (self.iter.next() orelse @panic("unexpected EOF when parsing attribute"))[1..];

            const parameters = try self.consumeFunctionCallParameters(attributes_progress_node);

            const attribute = try self.allocator.create(Node.Attribute);

            if (std.mem.eql(u8, "NativeInvoke", name)) {
                std.debug.assert(parameters.len == 2);

                attribute.* = .{
                    .native_invoke = .{
                        .address = @intCast(parameters[0].contents.integer_literal.value),
                        .toc_index = @intCast(parameters[1].contents.integer_literal.value),
                    },
                };
            } else if (std.mem.eql(u8, "Initialize", name)) {
                std.debug.assert(parameters.len == 0);

                attribute.* = .{
                    .initializer = {},
                };
            } else {
                std.debug.panic("Unknown attribute {s}", .{name});
            }

            try attributes.append(attribute);
        }

        attributes_progress_node.end();

        const modifiers = self.consumeModifiers(parent_progress_node);

        const next = self.iter.peek() orelse std.debug.panic("unexpected EOF when parsing declaration", .{});

        const next_keyword = maybeHashKeyword(next);

        //If the next keyword is a function, then we are consuming a function
        if (next_keyword == comptime hashKeyword("fn")) {
            //Get rid of the fn
            self.consumeArbitraryLexeme("fn");

            try functions.append(try self.consumeFunction(modifiers, attributes.items, progress_node));
        } else if (next_keyword == comptime hashKeyword("enum")) {
            try enums.append(try self.consumeEnum(modifiers, progress_node));
        }
        // Else, we are consuming a field, property, or constructor
        else blk: {
            if (self.iter.peekAt(1)) |ahead| {
                if (ahead[0] == '(') {
                    try constructors.append(try self.consumeConstructor(class_name, modifiers, progress_node));

                    //Since we parsed it as a contructor, break out as we dont want to accidentally parse a property aswell
                    break :blk;
                }
            }

            switch (try self.consumeFieldOrProperty(modifiers, progress_node)) {
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

    try self.tree.root_elements.append(self.allocator, .{ .class = node });
}

fn consumeEnum(self: *Self, modifiers: MMTypes.Modifiers, parent_progress_node: std.Progress.Node) !*Node.Enum {
    const progress_node = parent_progress_node.start("Parsing enum", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Enum);
    errdefer self.allocator.destroy(node);

    self.consumeArbitraryLexeme("enum");

    const name = self.iter.next() orelse @panic("EOF");

    const backing_type = if (self.consumeArbitraryLexemeIfAvailable(":"))
        try self.consumeTypeName(progress_node, false)
    else
        try self.type_intern_pool.fromFishType(.s32);

    self.consumeArbitraryLexeme("{");

    self.consumeArbitraryLexeme("}");

    node.* = .{
        .modifiers = modifiers,
        .backing_type = backing_type,
        .name = name,
        //TODO: enum members
        .members = &.{},
    };

    return node;
}

fn consumeConstructor(self: *Self, class_name: []const u8, modifiers: MMTypes.Modifiers, parent_progress_node: std.Progress.Node) !*Node.Constructor {
    const progress_node = parent_progress_node.start("Parsing constructor", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Constructor);
    errdefer self.allocator.destroy(node);

    const name = self.iter.next() orelse @panic("eof");

    std.debug.assert(std.mem.eql(u8, name, class_name));

    const parameters = try self.consumeFunctionParameters(progress_node);

    const body: ?*Node.Expression = if (self.consumeArbitraryLexemeIfAvailable(";"))
        null
    else
        try self.consumeBlockExpression(progress_node);

    node.* = .{
        .modifiers = modifiers,
        .parameters = parameters,
        .body = body,
    };

    return node;
}

fn consumeFieldOrProperty(self: *Self, modifiers: MMTypes.Modifiers, parent_progress_node: std.Progress.Node) !union(enum) {
    field: *Node.Field,
    property: *Node.Property,
} {
    const progress_node = parent_progress_node.start("Parsing field/property", 0);
    defer progress_node.end();

    const name = self.iter.next() orelse std.debug.panic("unexpected EOF when parsing field name", .{});

    const field_type: TypeInternPool.Index = if (self.consumeArbitraryLexemeIfAvailable(":"))
        try self.consumeTypeName(progress_node, false)
    else
        .unknown;

    if (self.consumeArbitraryLexemeIfAvailable("{")) {
        if (field_type == .unknown) {
            std.debug.panic("Properties must specify type", .{});
        }

        const node = try self.allocator.create(Node.Property);
        errdefer self.allocator.destroy(node);

        const get_body, const set_body = blk: {
            var get_body: Node.Property.FunctionState = .missing;
            var set_body: Node.Property.FunctionState = .missing;

            while (true) {
                const next = self.iter.next() orelse @panic("EOF");
                switch (hashKeyword(next)) {
                    hashKeyword("get") => {
                        if (self.consumeArbitraryLexemeIfAvailable(";")) {
                            get_body = .forward_declaration;
                        } else {
                            get_body = .{ .expression = try self.consumeBlockExpression(progress_node) };
                        }
                    },
                    hashKeyword("set") => {
                        if (self.consumeArbitraryLexemeIfAvailable(";")) {
                            set_body = .forward_declaration;
                        } else {
                            set_body = .{ .expression = try self.consumeBlockExpression(progress_node) };
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
        const default_value: ?*Node.Expression = if (self.consumeArbitraryLexemeIfAvailable("="))
            try self.consumeExpression(progress_node)
        else
            null;

        if (field_type == .unknown and default_value == null) {
            std.debug.panic("Field {s} has no type and no default value", .{name});
        }

        const node = try self.allocator.create(Node.Field);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .modifiers = modifiers,
            .name = name,
            .type = field_type,
            .default_value = default_value,
        };

        self.consumeSemicolon();

        return .{ .field = node };
    }
}

pub const Error = std.mem.Allocator.Error || std.fmt.ParseIntError || error{InvalidUtf8};

fn consumeBlockExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    const progress_node = parent_progress_node.start("Parsing block expression", 0);
    defer progress_node.end();

    const body_node = try self.allocator.create(Node.Expression);
    errdefer self.allocator.destroy(body_node);

    var body = std.ArrayList(Node).init(self.allocator);
    defer body.deinit();

    // std.debug.print("nn {s}\n", .{iter.peek() orelse unreachable});

    const is_block = self.consumeArbitraryLexemeIfAvailable("{");

    while (true) {
        const next = self.iter.peek() orelse std.debug.panic("unexpected EOF when parsing function body", .{});

        if (is_block and next[0] == '}') {
            self.consumeArbitraryLexeme("}");
            break;
        }

        const was_keyword: bool = if (maybeHashKeyword(next)) |maybe_keyword| blk: {
            switch (maybe_keyword) {
                hashKeyword("let") => {
                    const variable_declaration = try self.consumeVariableDeclaration(progress_node);

                    // std.debug.print("cc {}\n", .{variable_declaration});

                    try body.append(variable_declaration);
                    break :blk true;
                },
                hashKeyword("return") => {
                    const return_statement = try self.consumeReturnStatement(progress_node);

                    // std.debug.print("ee {}\n", .{return_statement});

                    try body.append(return_statement);
                    break :blk true;
                },
                hashKeyword("if") => {
                    const if_statement = try self.consumeIfStatement(progress_node);

                    // std.debug.print("ff {}\n", .{if_statement});

                    try body.append(if_statement);
                    break :blk true;
                },
                hashKeyword("while") => {
                    const while_statement = try self.consumeWhileStatement(progress_node);

                    try body.append(while_statement);
                    break :blk true;
                },
                hashKeyword("inline_asm") => {
                    const inline_asm_statement = try self.consumeInlineAsmStatement(progress_node);

                    try body.append(inline_asm_statement);
                    break :blk true;
                },
                hashKeyword("unreachable") => {
                    try body.append(.@"unreachable");
                    self.consumeArbitraryLexeme("unreachable");
                    self.consumeSemicolon();

                    break :blk true;
                },
                else => {
                    break :blk false;
                },
            }
        } else false;

        //If it was not parsed as a special keyword, then its an expression, and we need to parse it as one
        if (!was_keyword) {
            const node: Node = .{ .expression = try self.consumeExpression(progress_node) };
            self.consumeSemicolon();

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

fn consumeInlineAsmStatement(self: *Self, parent_progress_node: std.Progress.Node) !Node {
    const progress_node = parent_progress_node.start("Parsing inline asm statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.InlineAsmStatement);

    self.consumeArbitraryLexeme("inline_asm");

    var bytecode = std.ArrayList(Bytecode).init(self.allocator);
    var targets = std.StringHashMap(u32).init(self.allocator);

    self.consumeArbitraryLexeme("{");

    while (!self.consumeArbitraryLexemeIfAvailable("}")) {
        defer progress_node.completeOne();

        const name = self.iter.next() orelse @panic("EOF parsing op");

        const parsed_op = std.meta.stringToEnum(MMTypes.InstructionType, name);

        if (parsed_op == null) {
            if (self.consumeArbitraryLexemeIfAvailable(":")) {
                try targets.putNoClobber(name, @intCast(bytecode.items.len));
            } else std.debug.panic("wat {s} {s} {s}", .{ self.iter.slice[self.iter.pos - 2], name, self.iter.peek().? });
        } else switch (parsed_op.?) {
            inline else => |op| {
                const ParamType = @TypeOf(@field(@as(MMTypes.InstructionParams, undefined), @tagName(op)));

                switch (ParamType) {
                    MMTypes.NopClass => {
                        try bytecode.append(Bytecode{
                            .op = .{ .NOP = .{} },
                            .machine_type = .void,
                        });
                    },
                    MMTypes.LoadConstClass => try bytecode.append(.{
                        .op = switch (op) {
                            .LCb => .{
                                .LCb = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = switch (hashKeyword(self.iter.next().?)) {
                                        hashKeyword("true") => true,
                                        hashKeyword("false") => false,
                                        else => @panic("thnseoa"),
                                    },
                                },
                            },
                            .LCc => .{
                                .LCc = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        self.consumeArbitraryLexeme("'");
                                        const char = self.iter.next().?;
                                        self.consumeArbitraryLexeme("'");

                                        std.debug.assert(char.len == 1);

                                        // TODO: make this handle unicode
                                        break :blk char[0];
                                    },
                                },
                            },
                            .LCi => .{
                                .LCi = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        const is_negative = self.consumeArbitraryLexemeIfAvailable("-");

                                        const int = try std.fmt.parseInt(i33, self.iter.next().?, 0);

                                        break :blk if (is_negative) @intCast(-int) else @intCast(int);
                                    },
                                },
                            },
                            .LCf => .{
                                .LCf = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        const is_negative = self.consumeArbitraryLexemeIfAvailable("-");
                                        const int = try std.fmt.parseFloat(f32, self.iter.next().?);

                                        break :blk if (is_negative) -int else int;
                                    },
                                },
                            },
                            .LCsa => .{
                                .LCsa = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        break :blk try unwrapStringLiteral(self.allocator, self.iter.next().?);
                                    },
                                },
                            },
                            .LCsw => .{
                                .LCsw = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        break :blk try std.unicode.utf8ToUtf16LeAlloc(self.allocator, try unwrapStringLiteral(self.allocator, self.iter.next().?));
                                    },
                                },
                            },
                            inline .LC_NULLsp, .LC_NULLo => |tag| @unionInit(Bytecode.Params, @tagName(tag), .{
                                .dst_idx = try self.consumeRegister(false),
                            }),
                            .LCv4 => .{
                                .LCv4 = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        const x_neg = self.consumeArbitraryLexemeIfAvailable("-");
                                        const x = try std.fmt.parseFloat(f32, self.iter.next().?);
                                        self.consumeArbitraryLexeme(",");

                                        const y_neg = self.consumeArbitraryLexemeIfAvailable("-");
                                        const y = try std.fmt.parseFloat(f32, self.iter.next().?);
                                        self.consumeArbitraryLexeme(",");

                                        const z_neg = self.consumeArbitraryLexemeIfAvailable("-");
                                        const z = try std.fmt.parseFloat(f32, self.iter.next().?);
                                        self.consumeArbitraryLexeme(",");

                                        const w_neg = self.consumeArbitraryLexemeIfAvailable("-");
                                        const w = try std.fmt.parseFloat(f32, self.iter.next().?);

                                        break :blk .{
                                            if (x_neg) -x else x,
                                            if (y_neg) -y else y,
                                            if (z_neg) -z else z,
                                            if (w_neg) -w else w,
                                        };
                                    },
                                },
                            },
                            .LCs64 => .{
                                .LCs64 = .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .value = blk: {
                                        const is_negative = self.consumeArbitraryLexemeIfAvailable("-");
                                        const int = try std.fmt.parseInt(i64, self.iter.next().?, 0);

                                        break :blk if (is_negative) -int else int;
                                    },
                                },
                            },
                            else => @compileError("wrong op " ++ @tagName(op)),
                        },
                        .machine_type = .void,
                    }),
                    MMTypes.UnaryClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.UnaryClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .src_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.BinaryClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.BinaryClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .src_a_idx = try self.consumeRegister(true),
                                    .src_b_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.GetBuiltinMemberClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.GetBuiltinMemberClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .base_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.SetBuiltinMemberClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.SetBuiltinMemberClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .base_idx = try self.consumeRegister(true),
                                    .src_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.GetMemberClass => {
                        const dst_idx = try self.consumeRegister(true);
                        const base_idx = try self.consumeRegister(true);

                        const full_name = self.iter.next().?;

                        var split_iter = std.mem.splitScalar(u8, full_name, '.');

                        const type_name = split_iter.next().?;
                        const field_name = full_name[split_iter.index.?..];

                        try bytecode.append(Bytecode{
                            .op = switch (op) {
                                inline else => |tag| blk: {
                                    if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.GetMemberClass)
                                        unreachable;

                                    break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                        .dst_idx = dst_idx,
                                        .base_idx = base_idx,
                                        .type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                                            .name = type_name,
                                            .base_type = null,
                                            .dimension_count = 0,
                                            .indirection_count = 0,
                                        } }),
                                        .field = field_name,
                                    });
                                },
                            },
                            .machine_type = self.consumeMachineType().?,
                        });
                    },
                    MMTypes.SetMemberClass => {
                        const base_idx = try self.consumeRegister(true);

                        const full_name = self.iter.next().?;

                        var split_iter = std.mem.splitScalar(u8, full_name, '.');

                        const type_name = split_iter.next().?;
                        const field_name = full_name[split_iter.index.?..];

                        self.consumeArbitraryLexeme(",");

                        const src_idx = try self.consumeRegister(false);

                        try bytecode.append(Bytecode{
                            .op = switch (op) {
                                inline else => |tag| blk: {
                                    if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.SetMemberClass)
                                        unreachable;

                                    break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                        .base_idx = base_idx,
                                        .type = try self.type_intern_pool.getOrPutParsed(.{
                                            .parsed = .{
                                                .name = type_name,
                                                .base_type = null,
                                                .dimension_count = 0,
                                                .indirection_count = 0,
                                            },
                                        }),
                                        .field = field_name,
                                        .src_idx = src_idx,
                                    });
                                },
                            },
                            .machine_type = self.consumeMachineType().?,
                        });
                    },
                    MMTypes.GetElementClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.GetElementClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .base_idx = try self.consumeRegister(true),
                                    .src_or_index_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.SetElementClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.SetElementClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .base_idx = try self.consumeRegister(true),
                                    .src_idx = try self.consumeRegister(true),
                                    .index_idx = try self.consumeRegister(false),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.NewArrayClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.NewArrayClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                                        .base_type = null,
                                        .dimension_count = 0,
                                        .indirection_count = 0,
                                        .name = self.iter.next().?,
                                    } }),
                                    .size_idx = size_idx: {
                                        self.consumeArbitraryLexeme(",");

                                        break :size_idx try self.consumeRegister(false);
                                    },
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.WriteClass => try bytecode.append(Bytecode.init(.{ .WRITE = .{
                        .src_idx = try self.consumeRegister(false),
                    } }, self.consumeMachineType().?)),
                    MMTypes.ArgClass => {
                        try bytecode.append(Bytecode{
                            .op = .{
                                .ARG = .{
                                    .arg_idx = try self.consumeRegister(true),
                                    .src_idx = try self.consumeRegister(false),
                                },
                            },
                            .machine_type = self.consumeMachineType().?,
                        });
                    },
                    MMTypes.CallClass => {
                        try bytecode.append(.{
                            .op = switch (op) {
                                inline else => |tag| blk: {
                                    if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.CallClass)
                                        unreachable;

                                    const dst_idx = try self.consumeRegister(true);

                                    const full_name = self.iter.next().?;

                                    var split_iter = std.mem.splitScalar(u8, full_name, '.');

                                    const type_name = split_iter.next().?;
                                    const function_name = full_name[split_iter.index.?..];

                                    break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                        .dst_idx = dst_idx,
                                        .type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                                            .name = type_name,
                                            .base_type = null,
                                            .dimension_count = 0,
                                            .indirection_count = 0,
                                        } }),
                                        .function = .{ .name = function_name },
                                    });
                                },
                            },
                            .machine_type = self.consumeMachineType() orelse .void,
                        });
                    },
                    MMTypes.ReturnClass => {
                        try bytecode.append(Bytecode{
                            .op = .{ .RET = .{ .src_idx = try self.consumeRegister(false) } },
                            .machine_type = self.consumeMachineType() orelse .void,
                        });
                    },
                    MMTypes.BranchClass => {
                        try bytecode.append(.{
                            .op = switch (op) {
                                inline else => |tag| blk: {
                                    if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.BranchClass)
                                        unreachable;

                                    break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                        .target = self.iter.next().?,
                                        .src_idx = if (op == .B) std.math.maxInt(u16) else src_idx: {
                                            self.consumeArbitraryLexeme(",");

                                            break :src_idx try self.consumeRegister(false);
                                        },
                                    });
                                },
                            },
                            .machine_type = .void,
                        });
                    },
                    MMTypes.CastClass => try bytecode.append(.{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.CastClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .type = try self.type_intern_pool.getOrPutParsed(.{
                                        .parsed = .{
                                            .name = self.iter.next().?,
                                            .base_type = null,
                                            .indirection_count = 0,
                                            .dimension_count = 0,
                                        },
                                    }),
                                    .src_idx = src_idx: {
                                        self.consumeArbitraryLexeme(",");

                                        break :src_idx try self.consumeRegister(false);
                                    },
                                });
                            },
                        },
                        .machine_type = .void,
                    }),
                    MMTypes.NewObjectClass => try bytecode.append(Bytecode{
                        .op = switch (op) {
                            inline else => |tag| blk: {
                                if (@TypeOf(@field(@as(MMTypes.TaggedInstruction, undefined), @tagName(op))) != MMTypes.NewObjectClass)
                                    unreachable;

                                break :blk @unionInit(Bytecode.Params, @tagName(tag), .{
                                    .dst_idx = try self.consumeRegister(true),
                                    .type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                                        .base_type = null,
                                        .dimension_count = 0,
                                        .indirection_count = 0,
                                        .name = self.iter.next().?,
                                    } }),
                                });
                            },
                        },
                        .machine_type = self.consumeMachineType() orelse .void,
                    }),
                    MMTypes.ExternalInvokeClass => @panic("TODO"),
                    else => |unhandled_type| @compileError("Unhandled type " ++ @typeName(unhandled_type)),
                }
            },
        }
    }

    node.* = .{
        .bytecode = bytecode.items,
        .jump_targets = targets,
    };

    return .{ .inline_asm_statement = node };
}

fn consumeRegister(self: *Self, want_comma: bool) !u16 {
    const register = self.iter.next().?;

    if (want_comma)
        self.consumeArbitraryLexeme(",");

    return try std.fmt.parseInt(u16, register[1..], 10);
}

// Consumes a machine type wrapped in parentheses
fn consumeMachineType(self: *Self) ?MMTypes.MachineType {
    if (self.consumeArbitraryLexemeIfAvailable("(")) {
        const name = self.iter.next().?;
        self.consumeArbitraryLexeme(")");

        return std.meta.stringToEnum(MMTypes.MachineType, name).?;
    } else return null;
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

fn consumePrimaryExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {

    // Try to match bool/null literals
    if (self.consumeAnyMatches(&.{ "true", "false", "null" })) |match| {
        switch (match) {
            .true, .false => {
                const expression = try self.allocator.create(Node.Expression);

                expression.* = .{
                    .contents = .{ .bool_literal = match == .true },
                    .type = try self.type_intern_pool.fromFishType(.bool),
                };

                return expression;
            },
            .null => {
                const expression = try self.allocator.create(Node.Expression);

                expression.* = .{
                    .contents = .null_literal,
                    .type = try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
                        .name = "null",
                        .dimension_count = 0,
                        .base_type = null,
                        .indirection_count = 0,
                    } }),
                };

                return expression;
            },
        }
    }

    const peek = self.iter.peek() orelse @panic("EOF");

    if (peek[0] == '{') {
        const expression = try self.consumeBlockExpression(parent_progress_node);

        return expression;
    }

    const first = self.iter.next() orelse @panic("eof");

    if (first[0] == '(') {
        const expression = try self.consumeExpression(parent_progress_node);

        self.consumeArbitraryLexeme(")");

        return expression;
    }

    if (try isInt(i65, first)) |s65| {
        const base: Node.Expression.Contents.LiteralBase = if (first.len >= 2) switch (hashKeyword(first[0..2])) {
            hashKeyword("0x") => .hex,
            hashKeyword("0o") => .octal,
            hashKeyword("0b") => .binary,
            else => .decimal,
        } else .decimal;

        const expression = try self.allocator.create(Node.Expression);

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
        const expression = try self.allocator.create(Node.Expression);

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

    const next = self.iter.peek() orelse @panic("EOF");

    //If the next expression is a ( then its a function call
    if (next[0] == '(') {
        const expression = try self.allocator.create(Node.Expression);

        const parameters = try self.consumeFunctionCallParameters(parent_progress_node);

        //Builtin functions
        if (maybeHashKeyword(first)) |keyword| {
            switch (keyword) {
                hashKeyword("@strcpy") => {
                    if (parameters.len != 2 and parameters.len != 4)
                        @panic("wrong parameter length for strcpy builtin");

                    expression.* = .{
                        .contents = .{ .native_strcpy = .{
                            .dst = parameters[0],
                            .src = parameters[1],
                            .fill_byte = if (parameters.len > 2) parameters[2].contents.ascii_string_literal[0] else null,
                            .length = if (parameters.len > 2) @intCast(parameters[3].contents.integer_literal.value) else null,
                        } },
                        .type = try self.type_intern_pool.fromFishType(.void),
                    };

                    return expression;
                },
                hashKeyword("@float2") => {
                    if (parameters.len != 2)
                        @panic("wrong parameter length for vec2 construction");

                    expression.* = .{
                        .contents = .{ .vec2_construction = parameters[0..2].* },
                        .type = try self.type_intern_pool.fromFishType(.vec2),
                    };

                    return expression;
                },
                hashKeyword("@float3") => {
                    if (parameters.len != 3)
                        @panic("wrong parameter length for vec3 construction");

                    expression.* = .{
                        .contents = .{ .vec3_construction = parameters[0..3].* },
                        .type = try self.type_intern_pool.fromFishType(.vec3),
                    };

                    return expression;
                },
                hashKeyword("@float4") => {
                    if (parameters.len != 4)
                        @panic("wrong parameter length for vec4 construction");

                    expression.* = .{
                        .contents = .{ .vec4_construction = parameters[0..4].* },
                        .type = try self.type_intern_pool.fromFishType(.vec4),
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
            const expression = try self.allocator.create(Node.Expression);
            expression.* = .{
                .contents = .{ .guid_literal = guid },
                .type = .unknown,
            };
            return expression;
        }
    }

    //TODO: hash literals here
    if (first[0] == 'h') {}

    if (try isIntStringLiteral(self.allocator, first)) |int_string_literal| {
        const expression = try self.allocator.create(Node.Expression);
        expression.* = .{
            .contents = .{ .int_string_literal = int_string_literal },
            .type = .unknown,
        };
        return expression;
    }

    if (try isWideStringLiteral(self.allocator, first)) |wide_string_literal| {
        const expression = try self.allocator.create(Node.Expression);
        expression.* = .{
            .contents = .{ .wide_string_literal = wide_string_literal },
            .type = .unknown,
        };
        return expression;
    }

    if (try isAsciiStringLiteral(self.allocator, first)) |string_literal| {
        const expression = try self.allocator.create(Node.Expression);
        expression.* = .{
            .contents = .{ .ascii_string_literal = string_literal },
            .type = .unknown,
        };
        return expression;
    }

    //If none of the other checks matched, this is a variable access
    const expression = try self.allocator.create(Node.Expression);
    expression.* = .{
        .contents = .{ .variable_or_class_access = first },
        .type = .unknown,
    };
    return expression;
}

fn consumeArrayAccessExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumePrimaryExpression(parent_progress_node);

    while (self.consumeArbitraryLexemeIfAvailable("[")) {
        const array_access = try self.allocator.create(Node.Expression);

        const index = try self.consumeExpression(parent_progress_node);

        self.consumeArbitraryLexeme("]");

        array_access.* = .{
            .contents = .{ .array_access = .{ .lefthand = node, .righthand = index } },
            .type = .unknown,
        };

        node = array_access;
    }

    return node;
}

fn consumeDotExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeArrayAccessExpression(parent_progress_node);

    while (self.consumeArbitraryLexemeIfAvailable(".")) {
        const field_access = try self.allocator.create(Node.Expression);

        const name = self.iter.next() orelse @panic("EOF");

        const next = self.iter.peek() orelse @panic("EOF");

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
                        .parameters = try self.consumeFunctionCallParameters(parent_progress_node),
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

fn consumeArrayInitExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    if (self.consumeArbitraryLexemeIfAvailable("new")) {
        const array_init = try self.allocator.create(Node.Expression);

        const return_pos = self.iter.pos;
        const array_type = try self.consumeTypeName(parent_progress_node, true);
        self.iter.pos = return_pos;
        const child_type = try self.consumeTypeName(parent_progress_node, false);

        self.consumeArbitraryLexeme("[");
        const size = try self.consumeExpression(parent_progress_node);
        self.consumeArbitraryLexeme("]");

        array_init.* = .{
            .contents = .{
                .new_array = .{
                    .size = size,
                    .child = child_type,
                },
            },
            .type = array_type,
        };

        return array_init;
    }

    return self.consumeDotExpression(parent_progress_node);
}

fn consumeUnaryExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    if (self.consumeAnyMatches(&.{ "!", "-" })) |match| {
        const unary = try self.allocator.create(Node.Expression);

        unary.* = switch (match) {
            .@"!" => .{
                .contents = .{
                    .logical_negation = try self.consumeUnaryExpression(parent_progress_node),
                },
                .type = try self.type_intern_pool.fromFishType(.bool),
            },
            .@"-" => .{
                .contents = .{
                    .numeric_negation = try self.consumeUnaryExpression(parent_progress_node),
                },
                .type = .unknown,
            },
        };

        return unary;
    } else {
        return self.consumeArrayInitExpression(parent_progress_node);
    }
}

fn consumeFactorExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeUnaryExpression(parent_progress_node);

    while (self.consumeAnyMatches(&.{ "/", "*" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const factor = try self.allocator.create(Node.Expression);

                factor.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@"/" => @tagName(Node.Expression.Contents.division),
                            .@"*" => @tagName(Node.Expression.Contents.multiplication),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try self.consumeUnaryExpression(parent_progress_node),
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

fn consumeTermExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeFactorExpression(parent_progress_node);

    while (self.consumeAnyMatches(&.{ "-", "+" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const term = try self.allocator.create(Node.Expression);

                term.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        switch (keyword) {
                            .@"+" => @tagName(Node.Expression.Contents.addition),
                            .@"-" => @tagName(Node.Expression.Contents.subtraction),
                        },
                        .{
                            .lefthand = node,
                            .righthand = try self.consumeFactorExpression(parent_progress_node),
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

fn consumeBitwiseExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeTermExpression(parent_progress_node);

    while (self.consumeAnyMatches(&.{ "&", "^", "|" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const bitwise = try self.allocator.create(Node.Expression);

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
                            .righthand = try self.consumeTermExpression(parent_progress_node),
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

fn consumeCastExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeBitwiseExpression(parent_progress_node);

    while (self.consumeArbitraryLexemeIfAvailable("as")) {
        const cast_expression = try self.allocator.create(Node.Expression);

        cast_expression.* = .{
            .contents = .{ .cast = node },
            .type = try self.consumeTypeName(parent_progress_node, false),
        };

        node = cast_expression;
    }

    return node;
}

fn consumeComparisonExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeCastExpression(parent_progress_node);

    while (self.consumeAnyMatches(&.{ ">", ">=", "<", "<=" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const comparison = try self.allocator.create(Node.Expression);

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
                            .righthand = try self.consumeCastExpression(parent_progress_node),
                        },
                    ),
                    .type = try self.type_intern_pool.fromFishType(.bool),
                };

                break :blk comparison;
            },
        };
    }

    return node;
}

fn consumeEqualityExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeComparisonExpression(parent_progress_node);

    while (self.consumeAnyMatches(&.{ "==", "!=" })) |match| {
        node = switch (match) {
            inline else => |keyword| blk: {
                const equality = try self.allocator.create(Node.Expression);

                equality.* = .{
                    .contents = @unionInit(
                        Node.Expression.Contents,
                        if (keyword == .@"==") @tagName(Node.Expression.Contents.equal) else @tagName(Node.Expression.Contents.not_equal),
                        .{
                            .lefthand = node,
                            .righthand = try self.consumeComparisonExpression(parent_progress_node),
                        },
                    ),
                    .type = try self.type_intern_pool.fromFishType(.bool),
                };

                break :blk equality;
            },
        };
    }

    return node;
}

fn consumeLogicalAndExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeEqualityExpression(parent_progress_node);

    while (self.consumeArbitraryLexemeIfAvailable("&&")) {
        const logical_and = try self.allocator.create(Node.Expression);

        logical_and.* = .{
            .contents = .{
                .logical_and = .{
                    .lefthand = node,
                    .righthand = try self.consumeEqualityExpression(parent_progress_node),
                },
            },
            .type = try self.type_intern_pool.fromFishType(.bool),
        };

        node = logical_and;
    }

    return node;
}

fn consumeLogicalOrExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    var node = try self.consumeLogicalAndExpression(parent_progress_node);

    while (self.consumeArbitraryLexemeIfAvailable("||")) {
        const logical_or = try self.allocator.create(Node.Expression);

        logical_or.* = .{
            .contents = .{
                .logical_or = .{
                    .lefthand = node,
                    .righthand = try self.consumeLogicalAndExpression(parent_progress_node),
                },
            },
            .type = try self.type_intern_pool.fromFishType(.bool),
        };

        node = logical_or;
    }

    return node;
}

fn consumeAssignmentExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    const destination = try self.consumeLogicalOrExpression(parent_progress_node);

    if (self.consumeArbitraryLexemeIfAvailable("=")) {
        const value = try self.consumeAssignmentExpression(parent_progress_node);

        const assignment = try self.allocator.create(Node.Expression);

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

fn consumeExpression(self: *Self, parent_progress_node: std.Progress.Node) Error!*Node.Expression {
    const progress_node = parent_progress_node.start("Parsing expression", 0);
    defer progress_node.end();

    return self.consumeAssignmentExpression(progress_node);
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

fn consumeAnyMatches(self: *Self, comptime matches: []const [:0]const u8) ?MatchEnum(matches) {
    const matches_keywords = comptime blk: {
        var keywords: [matches.len]KeywordHash = undefined;

        for (matches, 0..) |match, i| {
            keywords[i] = hashKeyword(match);
        }

        break :blk keywords;
    };

    const Enum = MatchEnum(matches);

    //If the next lexeme is a valid keyword
    if (maybeHashKeyword(self.iter.peek() orelse @panic("EOF"))) |keyword| {
        inline for (matches_keywords, matches) |match, match_str| {
            if (keyword == match) {
                _ = self.iter.next() orelse unreachable;

                return @field(Enum, match_str);
            }
        }
    }

    return null;
}

fn consumeFunctionCallParameters(self: *Self, parent_progress_node: std.Progress.Node) ![]const *Node.Expression {
    const progress_node = parent_progress_node.start("Parsing function call parameters", 0);
    defer progress_node.end();

    var parameters = std.ArrayList(*Node.Expression).init(self.allocator);
    defer parameters.deinit();

    self.consumeArbitraryLexeme("(");

    var i: usize = 0;
    while (true) {
        const next = self.iter.peek() orelse @panic("EOF");

        if (next[0] == ')') {
            _ = self.iter.next();
            break;
        }

        if (i > 0)
            self.consumeArbitraryLexeme(",");

        try parameters.append(try self.consumeExpression(progress_node));

        i += 1;
    }

    return parameters.toOwnedSlice();
}

fn consumeTypeName(self: *Self, parent_progress_node: std.Progress.Node, is_array_type: bool) !TypeInternPool.Index {
    const progress_node = parent_progress_node.start("Parsing type name", 0);
    defer progress_node.end();

    const name, const base_type: ?[]const u8 = blk: {
        const name = self.iter.next() orelse std.debug.panic("unexpected EOF when reading type name", .{});

        if (self.consumeArbitraryLexemeIfAvailable(".")) {
            break :blk .{ self.iter.next().?, name };
        }

        break :blk .{ name, null };
    };

    var dimension_count: u8 = if (is_array_type) 1 else 0;

    blk: while (maybeHashKeyword(self.iter.peek() orelse @panic("EOF"))) |keyword| {
        switch (keyword) {
            hashKeyword("[") => {
                // If the next isnt a closing, then its not part of the type name
                if (self.iter.peekAt(1).?[0] != ']')
                    break :blk;

                self.consumeArbitraryLexeme("[");
                self.consumeArbitraryLexeme("]");

                dimension_count += 1;
            },
            else => break :blk,
        }
    }

    var indirection_count: u8 = 0;
    while (self.consumeArbitraryLexemeIfAvailable("*"))
        indirection_count += 1;

    return try self.type_intern_pool.getOrPutParsed(.{ .parsed = .{
        .name = name,
        .dimension_count = dimension_count,
        .base_type = base_type,
        .indirection_count = indirection_count,
    } });
}

fn consumeFunctionParameters(self: *Self, parent_progress_node: std.Progress.Node) ![]Node.Function.Parameter {
    const progress_node = parent_progress_node.start("Parsing function parameters", 0);
    defer progress_node.end();

    self.consumeArbitraryLexeme("(");

    var parameters = std.ArrayListUnmanaged(Node.Function.Parameter){};

    while (true) {
        const name = self.iter.next() orelse std.debug.panic("unexpected EOF when parsing function definition parameters", .{});

        // We have reached the end of the parameters
        if (name[0] == ')')
            break;

        self.consumeArbitraryLexeme(":");
        const param_type = try self.consumeTypeName(progress_node, false);

        try parameters.append(self.allocator, .{ .name = name, .type = param_type });

        _ = self.consumeArbitraryLexemeIfAvailable(",");
    }

    return parameters.items;
}

fn consumeFunction(self: *Self, modifiers: MMTypes.Modifiers, attributes: []const *Node.Attribute, parent_progress_node: std.Progress.Node) !*Node.Function {
    const progress_node = parent_progress_node.start("Parsing function", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Function);
    errdefer self.allocator.destroy(node);

    const name = self.iter.next() orelse std.debug.panic("unexpected EOF when parsing function name", .{});

    const parameters = try self.consumeFunctionParameters(progress_node);

    const return_type: TypeInternPool.Index = blk: {
        if (std.mem.eql(u8, self.iter.peek() orelse std.debug.panic("EOF", .{}), "->")) {
            self.consumeArbitraryLexeme("->");
            break :blk try self.consumeTypeName(progress_node, false);
        }

        break :blk try self.type_intern_pool.fromFishType(.void);
    };

    const body: ?*Node.Expression = if (!self.consumeArbitraryLexemeIfAvailable(";"))
        try self.consumeBlockExpression(progress_node)
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

fn isWideStringLiteral(allocator: std.mem.Allocator, lexeme: Lexeme) !?[]const u8 {
    if (lexeme.len < 3)
        return null;

    if (lexeme[0] != 'L')
        return null;

    if (lexeme[1] != '\'' or lexeme[lexeme.len - 1] != '\'')
        return null;

    //Strip to just the contents
    return try unwrapStringLiteral(allocator, lexeme[1..]);
}

fn isIntStringLiteral(allocator: std.mem.Allocator, lexeme: Lexeme) !?[]const u8 {
    if (lexeme.len < 3)
        return null;

    if (lexeme[0] != 'u')
        return null;

    if (lexeme[1] != '\'' or lexeme[lexeme.len - 1] != '\'')
        return null;

    //Strip to just the contents
    return try unwrapStringLiteral(allocator, lexeme[1..]);
}

fn isAsciiStringLiteral(allocator: std.mem.Allocator, lexeme: Lexeme) !?[]const u8 {
    if (lexeme.len < 2)
        return null;

    if (lexeme[0] != '\'' or lexeme[lexeme.len - 1] != '\'')
        return null;

    //Strip to just the contents
    return try unwrapStringLiteral(allocator, lexeme);
}

fn consumeWhileStatement(self: *Self, parent_progress_node: std.Progress.Node) !Node {
    const progress_node = parent_progress_node.start("Parsing while statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.WhileStatement);

    self.consumeArbitraryLexeme("while");

    self.consumeArbitraryLexeme("(");
    const condition = try self.consumeExpression(progress_node);
    self.consumeArbitraryLexeme(")");

    const body = try self.consumeBlockExpression(progress_node);

    node.* = .{
        .condition = condition,
        .body = body,
    };

    return .{ .while_statement = node };
}

fn consumeIfStatement(self: *Self, parent_progress_node: std.Progress.Node) !Node {
    const progress_node = parent_progress_node.start("Parsing if statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.IfStatement);
    errdefer self.allocator.destroy(node);

    self.consumeArbitraryLexeme("if");

    self.consumeArbitraryLexeme("(");
    const condition = try self.consumeExpression(progress_node);
    self.consumeArbitraryLexeme(")");

    const body = try self.consumeBlockExpression(progress_node);

    const else_body: ?*Node.Expression = if (std.mem.eql(u8, "else", self.iter.peek() orelse @panic("EOF"))) blk: {
        self.consumeArbitraryLexeme("else");

        break :blk try self.consumeBlockExpression(progress_node);
    } else null;

    node.* = .{
        .condition = condition,
        .body = body,
        .else_body = else_body,
    };

    return .{ .if_statement = node };
}

fn consumeReturnStatement(self: *Self, parent_progress_node: std.Progress.Node) !Node {
    const progress_node = parent_progress_node.start("Parsing return statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.ReturnStatement);
    errdefer self.allocator.destroy(node);

    self.consumeArbitraryLexeme("return");

    //If the next lexeme is not a semicolon, then theres an expression after, parse it
    const expression: ?*Node.Expression = if (!self.consumeArbitraryLexemeIfAvailable(";")) blk: {
        const expression = try self.consumeExpression(progress_node);

        self.consumeSemicolon();

        break :blk expression;
    } else null;

    node.* = .{ .expression = expression };

    return .{ .return_statement = node };
}

fn consumeVariableDeclaration(self: *Self, parent_progress_node: std.Progress.Node) !Node {
    const progress_node = parent_progress_node.start("Parsing variable declaration", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.VariableDeclaration);
    errdefer self.allocator.destroy(node);

    self.consumeArbitraryLexeme("let");

    const name = self.iter.next() orelse std.debug.panic("unexpected EOF", .{});

    const variable_type: TypeInternPool.Index = blk: {
        if (self.consumeArbitraryLexemeIfAvailable(":")) {
            break :blk try self.consumeTypeName(progress_node, false);
        }

        break :blk .unknown;
    };

    const value: ?*Node.Expression = blk: {
        if (self.consumeArbitraryLexemeIfAvailable("=")) {
            break :blk try self.consumeExpression(progress_node);
        }

        break :blk null;
    };

    if (value == null and variable_type == .unknown) {
        std.debug.panic("variable has no expression or type, what?", .{});
    }

    self.consumeSemicolon();

    node.* = .{
        .name = name,
        .type = variable_type,
        .value = value,
    };

    return .{ .variable_declaration = node };
}

fn consumeModifiers(self: *Self, parent_progress_node: std.Progress.Node) MMTypes.Modifiers {
    const progress_node = parent_progress_node.start("Parsing modifiers", 0);
    defer progress_node.end();

    var current_modifiers: MMTypes.Modifiers = .{};

    while (true) {
        defer progress_node.completeOne();

        const lexeme = self.iter.peek() orelse std.debug.panic("unexpected EOF when parsing modifiers", .{});

        const modifiers_type_info: std.builtin.Type = @typeInfo(MMTypes.Modifiers);

        //special case pub
        if (std.mem.eql(u8, lexeme, "pub")) {
            current_modifiers.public = true;

            _ = self.iter.next() orelse unreachable;
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

            _ = self.iter.next() orelse unreachable;
        }
    }

    return current_modifiers;
}

fn consumeFromImportStatement(self: *Self, parent_progress_node: std.Progress.Node) !void {
    const progress_node = parent_progress_node.start("Parsing from/import statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.FromImport);
    errdefer self.allocator.destroy(node);

    const target = self.iter.next() orelse std.debug.panic("unexpected EOF after from statement", .{});

    //Semantically unimportant, should always be there
    self.consumeArbitraryLexeme("import");

    const first_import_lexeme = self.iter.next() orelse std.debug.panic("unexpected EOF in from/import statement after import lexeme", .{});

    var was_multi_import = false;

    const wanted: FromImportWanted = switch (hashKeyword(first_import_lexeme)) {
        hashKeyword("{") => .{
            .multiple = blk: {
                var wanted_imports = std.ArrayListUnmanaged(FromImportWanted.ImportedFunction){};

                was_multi_import = true;

                //If the next token is a `}`, consume it and break out
                if (self.consumeArbitraryLexemeIfAvailable("}")) {
                    break :blk &.{};
                }

                while (true) {
                    const curr = self.iter.next() orelse std.debug.panic("unexpected EOF in multi import block", .{});

                    //TODO: support renamed function imports eg. `import { Messaging_PoppetInfoMessage: ShowNotification }`

                    const name = try unwrapStringLiteral(self.allocator, curr);

                    //Append the new import
                    try wanted_imports.append(self.allocator, .{
                        .name = name,
                        .original_name = name,
                    });

                    const next = self.iter.peek() orelse std.debug.panic("unexpected EOF in multi import block", .{});

                    //If the next token is a `}`, break out
                    if (next[0] == '}')
                        break;
                    //If the next token is a `,`, consume it
                    if (next[0] == ',')
                        _ = self.iter.next() orelse unreachable;
                }

                break :blk try wanted_imports.toOwnedSlice(self.allocator);
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
        .target = try unwrapStringLiteral(self.allocator, target),
        .wanted = wanted,
    };

    try self.tree.root_elements.append(self.allocator, .{ .from_import = node });

    //If we were importing multiple things, the semicolon is not required
    if (!was_multi_import)
        self.consumeSemicolon();
}

fn consumeImportStatement(self: *Self, parent_progress_node: std.Progress.Node) !void {
    const progress_node = parent_progress_node.start("Parsing import statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Import);
    errdefer self.allocator.destroy(node);

    node.* = .{
        .target = try unwrapStringLiteral(
            self.allocator,
            self.iter.next() orelse std.debug.panic("unexpected EOF after import statement", .{}),
        ),
    };

    try self.tree.root_elements.append(self.allocator, .{ .import = node });

    self.consumeSemicolon();
}

fn consumeUsingStatement(self: *Self, parent_progress_node: std.Progress.Node) !void {
    const progress_node = parent_progress_node.start("Parsing using statement", 0);
    defer progress_node.end();

    const node = try self.allocator.create(Node.Using);
    errdefer self.allocator.destroy(node);

    const using_type_str = self.iter.next() orelse std.debug.panic("unexpected EOF after using statement", .{});

    const using_type = std.meta.stringToEnum(UsingType, using_type_str) orelse std.debug.panic("unknown using type {s}", .{using_type_str});

    const library_name = try unwrapStringLiteral(
        self.allocator,
        self.iter.next() orelse std.debug.panic("unexpected EOF after using statement type", .{}),
    );

    node.* = .{
        .type = using_type,
        .target = library_name,
    };

    try self.tree.root_elements.append(self.allocator, .{ .using = node });

    self.consumeSemicolon();
}

fn consumeSemicolon(self: *Self) void {
    self.consumeArbitraryLexeme(";");
}

fn consumeArbitraryLexeme(self: *Self, intended: []const u8) void {
    // std.debug.print("consuming {s}\n", .{intended});

    if (self.iter.next()) |next| {
        if (!std.mem.eql(u8, next, intended)) {
            const prev_lexeme = self.iter.slice[self.iter.pos - 2];
            const next_lexeme = self.iter.peek() orelse @panic("eof");

            std.debug.panic(
                "unexpected lexeme {s}, expected {s}, prev {s}, next {s}",
                .{ next, intended, prev_lexeme, next_lexeme },
            );
        }
    } else {
        std.debug.panic("unexpected EOF when expecting {s}", .{intended});
    }
}

fn consumeArbitraryLexemeIfAvailable(self: *Self, intended: []const u8) bool {
    //Peek the next lexeme
    if (self.iter.peek()) |next| {
        //If its the intended one
        if (std.mem.eql(u8, next, intended)) {
            //Consume it
            _ = self.iter.next() orelse unreachable;

            return true;
        }
    }

    return false;
}

fn unwrapStringLiteral(allocator: std.mem.Allocator, literal: []const u8) ![]const u8 {
    if (literal[0] != '\'' or literal[literal.len - 1] != '\'')
        std.debug.panic("bad string \"{s}\"", .{literal});

    const raw = literal[1 .. literal.len - 1];

    var unescaped_literal = std.ArrayList(u8).init(allocator);

    var i: usize = 0;
    while (i < raw.len) : (i += 1) {
        const c = raw[i];

        if (c == '\\') {
            switch (raw[i + 1]) {
                'n' => try unescaped_literal.append('\n'),
                'r' => try unescaped_literal.append('\r'),
                '\'' => try unescaped_literal.append('\''),
                '0' => try unescaped_literal.append('\x00'),
                else => |escape_char| std.debug.panic("unknown escape character {c}", .{escape_char}),
            }
            i += 1;
            continue;
        }

        try unescaped_literal.append(c);
    }

    // std.debug.print("escaped string \"{s}\"\n", .{unescaped_literal.items});

    // for (unescaped_literal.items) |c| {
    //     if (c == '\n')
    //         std.debug.print("\"", .{});
    //     std.debug.print("{c}", .{c});
    // }
    // std.debug.print("\n", .{});

    return unescaped_literal.items;
}

pub const Lexemeizer = struct {
    source: []const u8,
    pos: usize = 0,
    is_asm: bool = false,

    const normal_single_char_lexemes: []const u8 = "()[]{}!*,:;+.'<>+-";
    const asm_single_char_lexemes: []const u8 = "(){}!*,:;+'<>+-";
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

        const single_char_lexemes = if (self.is_asm)
            asm_single_char_lexemes
        else
            normal_single_char_lexemes;

        var is_number = true;
        var lexeme_start: ?usize = null;
        var i: usize = 0;
        while (i < iter.len) {
            const char = iter[i];

            // If we hit a comment,
            if (char == '/' and iter[i + 1] == '/') {
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
            const is_int_string = just_started_lexeme and char == 'u' and iter[i + 1] == '\'';

            //If this is the start of a lexeme and we hit a ' (the start of a string)
            if ((just_started_lexeme and char == '\'') or is_long_string or is_int_string) {
                //Increment to the next char
                i += 1;

                if (is_long_string or is_int_string)
                    i += 1;

                //Skip over all non ' characters which do not start with \ (eg. an escaped apostrophe)
                while (iter[i] != '\'' or iter[i - 1] == '\\') {
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

        const lexeme = iter[lexeme_start.?..i];

        // When we hit an inline ASM block, we need to change some parameters of the parser (like that `.` is no longer a single char lexeme) due to function names containing `.`
        if (std.mem.eql(u8, lexeme, "inline_asm")) {
            self.is_asm = true;
        }

        if (self.is_asm and lexeme[0] == '}') {
            self.is_asm = false;
        }

        return lexeme;
    }
};

pub const Bytecode = struct {
    pub const Params = union(MMTypes.InstructionType) {
        pub const NopClass = struct {};
        pub const CastClass = struct {
            dst_idx: u16,
            src_idx: u16,
            type: TypeInternPool.Index,
        };
        pub const UnaryClass = struct {
            dst_idx: u16,
            src_idx: u16,
        };
        pub const BinaryClass = struct {
            dst_idx: u16,
            src_a_idx: u16,
            src_b_idx: u16,
        };
        pub const GetBuiltinMemberClass = struct {
            dst_idx: u16,
            base_idx: u16,
        };
        pub const SetBuiltinMemberClass = struct {
            src_idx: u16,
            base_idx: u16,
        };
        pub const GetMemberClass = struct {
            dst_idx: u16,
            base_idx: u16,
            type: TypeInternPool.Index,
            field: []const u8,
        };
        pub const SetMemberClass = struct {
            src_idx: u16,
            base_idx: u16,
            type: TypeInternPool.Index,
            field: []const u8,
        };
        pub const GetElementClass = struct {
            dst_idx: u16,
            base_idx: u16,
            src_or_index_idx: u16,
        };
        pub const SetElementClass = struct {
            src_idx: u16,
            base_idx: u16,
            index_idx: u16,
        };
        pub const NewObjectClass = struct {
            dst_idx: u16,
            type: TypeInternPool.Index,
        };
        pub const NewArrayClass = struct {
            dst_idx: u16,
            type: TypeInternPool.Index,
            size_idx: u16,
        };
        pub const WriteClass = struct {
            src_idx: u16,
        };
        pub const ArgClass = struct {
            src_idx: u16,
            arg_idx: u16,
        };
        pub const CallClass = struct {
            dst_idx: u16,
            function: union(enum) {
                name: []const u8,
                function: *Node.Function,
                initializer: void,
                constructor: []const u8,
            },
            type: TypeInternPool.Index,
        };
        pub const ReturnClass = struct {
            src_idx: u16,
        };
        pub const BranchClass = struct {
            src_idx: u16,
            target: []const u8,
        };
        pub const ExternalInvokeClass = struct {
            dst_idx: u16,
        };

        pub const LoadBoolConst = struct {
            dst_idx: u16,
            value: bool,
        };
        pub const LoadCharConst = struct {
            dst_idx: u16,
            value: u16,
        };
        pub const LoadIntConst = struct {
            dst_idx: u16,
            value: i32,
        };
        pub const LoadFloatConst = struct {
            dst_idx: u16,
            value: f32,
        };
        pub const LoadAsciiStringConst = struct {
            dst_idx: u16,
            value: []const u8,
        };
        pub const LoadWideStringConst = struct {
            dst_idx: u16,
            value: []const u16,
        };
        pub const LoadNullConst = struct {
            dst_idx: u16,
        };
        pub const LoadVectorConst = struct {
            dst_idx: u16,
            value: [4]f32,
        };
        pub const LoadLongConst = struct {
            dst_idx: u16,
            value: i64,
        };

        NOP: NopClass,
        LCb: LoadBoolConst,
        LCc: LoadCharConst,
        LCi: LoadIntConst,
        LCf: LoadFloatConst,
        LCsw: LoadWideStringConst,
        LC_NULLsp: LoadNullConst,
        MOVb: UnaryClass,
        LOG_NEGb: UnaryClass,
        MOVc: UnaryClass,
        MOVi: UnaryClass,
        INCi: UnaryClass,
        DECi: UnaryClass,
        NEGi: UnaryClass,
        BIT_NEGi: UnaryClass,
        LOG_NEGi: UnaryClass,
        ABSi: UnaryClass,
        MOVf: UnaryClass,
        NEGf: UnaryClass,
        ABSf: UnaryClass,
        SQRTf: UnaryClass,
        SINf: UnaryClass,
        COSf: UnaryClass,
        TANf: UnaryClass,
        MOVv4: UnaryClass,
        NEGv4: UnaryClass,
        MOVm44: UnaryClass,
        IT_MOV_S_DEPRECATED: NopClass,
        MOVrp: UnaryClass,
        MOVcp: UnaryClass,
        MOVsp: UnaryClass,
        MOVo: UnaryClass,
        EQb: BinaryClass,
        NEb: BinaryClass,
        IT_RESERVED0_C: BinaryClass,
        IT_RESERVED1_C: BinaryClass,
        LTc: BinaryClass,
        LTEc: BinaryClass,
        GTc: BinaryClass,
        GTEc: BinaryClass,
        EQc: BinaryClass,
        NEc: BinaryClass,
        ADDi: BinaryClass,
        SUBi: BinaryClass,
        MULi: BinaryClass,
        DIVi: BinaryClass,
        MODi: BinaryClass,
        MINi: BinaryClass,
        MAXi: BinaryClass,
        SLAi: BinaryClass,
        SRAi: BinaryClass,
        SRLi: BinaryClass,
        BIT_ORi: BinaryClass,
        BIT_ANDi: BinaryClass,
        BIT_XORi: BinaryClass,
        LTi: BinaryClass,
        LTEi: BinaryClass,
        GTi: BinaryClass,
        GTEi: BinaryClass,
        EQi: BinaryClass,
        NEi: BinaryClass,
        ADDf: BinaryClass,
        SUBf: BinaryClass,
        MULf: BinaryClass,
        DIVf: BinaryClass,
        MINf: BinaryClass,
        MAXf: BinaryClass,
        LTf: BinaryClass,
        LTEf: BinaryClass,
        GTf: BinaryClass,
        GTEf: BinaryClass,
        EQf: BinaryClass,
        NEf: BinaryClass,
        ADDv4: BinaryClass,
        SUBv4: BinaryClass,
        MULSv4: BinaryClass,
        DIVSv4: BinaryClass,
        DOT4v4: BinaryClass,
        DOT3v4: BinaryClass,
        DOT2v4: BinaryClass,
        CROSS3v4: BinaryClass,
        MULm44: BinaryClass,
        IT_EQ_S_DEPRECATED: NopClass,
        IT_NE_S_DEPRECATED: NopClass,
        EQrp: BinaryClass,
        NErp: BinaryClass,
        EQo: BinaryClass,
        NEo: BinaryClass,
        EQsp: BinaryClass,
        NEsp: BinaryClass,
        GET_V4_X: GetBuiltinMemberClass,
        GET_V4_Y: GetBuiltinMemberClass,
        GET_V4_Z: GetBuiltinMemberClass,
        GET_V4_W: GetBuiltinMemberClass,
        GET_V4_LEN2: GetBuiltinMemberClass,
        GET_V4_LEN3: GetBuiltinMemberClass,
        GET_V4_LEN4: GetBuiltinMemberClass,
        GET_M44_XX: GetBuiltinMemberClass,
        GET_M44_XY: GetBuiltinMemberClass,
        GET_M44_XZ: GetBuiltinMemberClass,
        GET_M44_XW: GetBuiltinMemberClass,
        GET_M44_YX: GetBuiltinMemberClass,
        GET_M44_YY: GetBuiltinMemberClass,
        GET_M44_YZ: GetBuiltinMemberClass,
        GET_M44_YW: GetBuiltinMemberClass,
        GET_M44_ZX: GetBuiltinMemberClass,
        GET_M44_ZY: GetBuiltinMemberClass,
        GET_M44_ZZ: GetBuiltinMemberClass,
        GET_M44_ZW: GetBuiltinMemberClass,
        GET_M44_WX: GetBuiltinMemberClass,
        GET_M44_WY: GetBuiltinMemberClass,
        GET_M44_WZ: GetBuiltinMemberClass,
        GET_M44_WW: GetBuiltinMemberClass,
        GET_M44_rX: GetBuiltinMemberClass,
        GET_M44_rY: GetBuiltinMemberClass,
        GET_M44_rZ: GetBuiltinMemberClass,
        GET_M44_rW: GetBuiltinMemberClass,
        GET_M44_cX: GetBuiltinMemberClass,
        GET_M44_cY: GetBuiltinMemberClass,
        GET_M44_cZ: GetBuiltinMemberClass,
        GET_M44_cW: GetBuiltinMemberClass,
        SET_V4_X: SetBuiltinMemberClass,
        SET_V4_Y: SetBuiltinMemberClass,
        SET_V4_Z: SetBuiltinMemberClass,
        SET_V4_W: SetBuiltinMemberClass,
        SET_M44_XX: SetBuiltinMemberClass,
        SET_M44_XY: SetBuiltinMemberClass,
        SET_M44_XZ: SetBuiltinMemberClass,
        SET_M44_XW: SetBuiltinMemberClass,
        SET_M44_YX: SetBuiltinMemberClass,
        SET_M44_YY: SetBuiltinMemberClass,
        SET_M44_YZ: SetBuiltinMemberClass,
        SET_M44_YW: SetBuiltinMemberClass,
        SET_M44_ZX: SetBuiltinMemberClass,
        SET_M44_ZY: SetBuiltinMemberClass,
        SET_M44_ZZ: SetBuiltinMemberClass,
        SET_M44_ZW: SetBuiltinMemberClass,
        SET_M44_WX: SetBuiltinMemberClass,
        SET_M44_WY: SetBuiltinMemberClass,
        SET_M44_WZ: SetBuiltinMemberClass,
        SET_M44_WW: SetBuiltinMemberClass,
        SET_M44_rX: SetBuiltinMemberClass,
        SET_M44_rY: SetBuiltinMemberClass,
        SET_M44_rZ: SetBuiltinMemberClass,
        SET_M44_rW: SetBuiltinMemberClass,
        SET_M44_cX: SetBuiltinMemberClass,
        SET_M44_cY: SetBuiltinMemberClass,
        SET_M44_cZ: SetBuiltinMemberClass,
        SET_M44_cW: SetBuiltinMemberClass,
        GET_SP_MEMBER: GetMemberClass,
        GET_RP_MEMBER: GetMemberClass,
        SET_SP_MEMBER: SetMemberClass,
        SET_RP_MEMBER: SetMemberClass,
        GET_ELEMENT: GetElementClass,
        SET_ELEMENT: SetElementClass,
        GET_ARRAY_LEN: GetBuiltinMemberClass,
        NEW_ARRAY: NewArrayClass,
        ARRAY_INSERT: SetElementClass,
        ARRAY_APPEND: SetElementClass,
        ARRAY_ERASE: SetElementClass,
        ARRAY_FIND: GetElementClass,
        ARRAY_CLEAR: SetElementClass,
        WRITE: WriteClass,
        ARG: ArgClass,
        CALL: CallClass,
        RET: ReturnClass,
        B: BranchClass,
        BEZ: BranchClass,
        BNEZ: BranchClass,
        CASTsp: CastClass,
        INTb: UnaryClass,
        INTc: UnaryClass,
        INTf: UnaryClass,
        FLOATb: UnaryClass,
        FLOATc: UnaryClass,
        FLOATi: UnaryClass,
        BOOLc: UnaryClass,
        BOOLi: UnaryClass,
        BOOLf: UnaryClass,
        GET_OBJ_MEMBER: GetMemberClass,
        SET_OBJ_MEMBER: SetMemberClass,
        NEW_OBJECT: NewObjectClass,
        ARRAY_RESIZE: SetElementClass,
        ARRAY_RESERVE: SetElementClass,
        LCv4: LoadVectorConst,
        LC_NULLo: LoadNullConst,
        CASTo: CastClass,
        GET_SP_NATIVE_MEMBER: GetMemberClass,
        LCsa: LoadAsciiStringConst,
        BIT_ORb: BinaryClass,
        BIT_ANDb: BinaryClass,
        BIT_XORb: BinaryClass,
        CALLVo: CallClass,
        CALLVsp: CallClass,
        ASSERT: WriteClass,
        LCs64: LoadLongConst,
        MOVs64: UnaryClass,
        ADDs64: BinaryClass,
        EQs64: BinaryClass,
        NEs64: BinaryClass,
        BIT_ORs64: BinaryClass,
        BIT_ANDs64: BinaryClass,
        BIT_XORs64: BinaryClass,
        EXT_ADDRESS: UnaryClass,
        EXT_LOAD: UnaryClass,
        EXT_STORE: UnaryClass,
        EXT_INVOKE: ExternalInvokeClass,
    };

    machine_type: MMTypes.MachineType,
    op: Params,

    pub fn init(params: Params, machine_type: MMTypes.MachineType) Bytecode {
        return .{
            .op = params,
            .machine_type = machine_type,
        };
    }
};
