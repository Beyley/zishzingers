const std = @import("std");

const Parser = @import("parser.zig");
const Resolvinator = @import("resolvinator.zig");
const Node = Parser.Node;

const Error = std.io.AnyWriter.Error;

indent: usize,
writer: std.io.AnyWriter,
a_string_table: *Resolvinator.AStringTable,
type_intern_pool: *Parser.TypeInternPool,

const Self = @This();

pub fn dumpAst(self: *Self, ast: Parser.Tree) Error!void {
    for (ast.root_elements.items) |root_node| {
        switch (root_node) {
            .using => |using| try self.printUsing(using),
            .import => |import| try self.printImport(import),
            .from_import => |from_import| try self.printFromImport(from_import),
            .class => |class| try self.printClass(class),
            else => |tag| std.debug.panic("todo: {s}", .{@tagName(tag)}),
        }
    }
}

fn printImport(self: *Self, import: *Node.Import) Error!void {
    try self.printIndent();
    try self.writer.print("Import: {s}\n", .{import.target});
}

fn printFromImport(self: *Self, from_import: *Node.FromImport) Error!void {
    try self.printIndent();
    try self.writer.print("From Import: {s}, Wanted {}\n", .{ from_import.target, from_import.wanted });
}

fn printUsing(self: *Self, using: *Node.Using) Error!void {
    try self.printIndent();

    try self.writer.print("Using: {s}, Target {s}\n", .{ @tagName(using.type), using.target });
}

fn printClass(self: *Self, class: *Node.Class) Error!void {
    try self.printIndent();

    try self.writer.print("Class: {s}, Base Class: {?}\n", .{ class.name, class.base_class });

    self.indent += 1;
    defer self.indent -= 1;

    if (class.identifier) |identifier| {
        try self.printIndent();
        try self.writer.print("Identifier:\n", .{});

        self.indent += 1;
        defer self.indent -= 1;

        try self.printIndent();
        try self.printExpression(identifier);
    }

    for (class.fields) |field| {
        try self.printIndent();
        try self.writer.print(
            "Field: {s}, Modifiers: {}, Type: ",
            .{ field.name, field.modifiers },
        );
        try self.printType(field.type);
        try self.writer.writeByte('\n');

        self.indent += 1;
        defer self.indent -= 1;

        if (field.default_value) |default_value| {
            try self.printIndent();
            try self.writer.writeAll("Default Value:\n");

            self.indent += 1;
            defer self.indent -= 1;

            try self.printIndent();
            try self.printExpression(default_value);
        }
    }

    for (class.properties) |property| {
        try self.printIndent();
        try self.writer.print(
            "Property: {s}, Modifiers: {}, Type: ",
            .{ property.name, property.modifiers },
        );
        try self.printType(property.type);
        try self.writer.writeByte('\n');

        try self.printIndent();
        switch (property.get_body) {
            .missing, .forward_declaration => {},
            .expression => |expression| {
                try self.writer.writeAll("Get body:\n");

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();

                try self.printExpression(expression);
            },
        }
    }

    for (class.functions) |function| {
        try self.printIndent();
        try self.writer.print(
            "Function: {s}, Modifiers {}, Type: ",
            .{ function.name, function.modifiers },
        );
        try self.printType(function.return_type);
        try self.writer.writeAll(", Parameters: [ ");
        for (function.parameters) |parameter| {
            try self.writer.print("{{ Name: {s}, Type: ", .{parameter.name});
            try self.printType(parameter.type);
            try self.writer.writeAll("}, ");
        }
        try self.writer.writeAll("]\n");

        if (function.body) |body| {
            self.indent += 1;
            defer self.indent -= 1;

            try self.printIndent();
            try self.printExpression(body);
        }
    }
}

fn printType(self: *Self, tree_type_index: Parser.TypeInternPool.Index) Error!void {
    const tree_type = self.type_intern_pool.get(tree_type_index).*;

    switch (tree_type) {
        .parsed => |parsed| {
            if (parsed.dimension_count > 0) {
                try self.writer.print(
                    "{s} ({d} dimensions)",
                    .{ parsed.name, parsed.dimension_count },
                );
            } else {
                try self.writer.print("{s}", .{parsed.name});
            }
        },
        .resolved => |resolved| {
            switch (resolved) {
                .fish => |fish| {
                    if (fish.dimension_count > 0) try self.writer.print(
                        "Name: {s}, Machine Type: {s}, Fish Type: {s}, Dimension Count: {d}, Array Base Machine Type: {s}, Script: {?}",
                        .{
                            if (fish.type_name == 0xFFFFFFFF)
                                "(unknown)"
                            else
                                self.a_string_table.keys()[fish.type_name],
                            @tagName(fish.machine_type),
                            @tagName(fish.fish_type),
                            fish.dimension_count,
                            @tagName(fish.array_base_machine_type),
                            fish.script,
                        },
                    ) else try self.writer.print(
                        "Name: {s}, Machine Type: {s}, Fish Type: {s}, Script: {?}",
                        .{
                            if (fish.type_name == 0xFFFFFFFF)
                                "(unknown)"
                            else
                                self.a_string_table.keys()[fish.type_name],
                            @tagName(fish.machine_type),
                            @tagName(fish.fish_type),
                            fish.script,
                        },
                    );
                },
                .pointer => |pointer| try self.writer.print("Base Type: {s}, Indirection Count: {d}", .{ @tagName(pointer.type), pointer.indirection_count }),
                .integer_literal => try self.writer.print("Integer Literal", .{}),
                .float_literal => try self.writer.print("Float Literal", .{}),
                .null_literal => try self.writer.print("Null Literal", .{}),
            }
        },
        .unknown => try self.writer.writeAll("unknown"),
    }
}

fn printExpression(self: *Self, expression: *Node.Expression) Error!void {
    switch (expression.contents) {
        .guid_literal => |guid| try self.writer.print("GUID literal: {d}", .{guid}),
        .integer_literal => |literal| try self.writer.print(
            "Integer literal: {d} ({s})",
            .{ literal.value, @tagName(literal.base) },
        ),
        .bool_literal => |literal| try self.writer.print("Boolean literal: {}", .{literal}),
        inline .integer_literal_to_s32, .integer_literal_to_ptr => |integer_literal_cast, tag| {
            try self.writer.print("{s}:\n", .{@tagName(tag)});

            self.indent += 1;
            defer self.indent -= 1;

            try self.printIndent();
            try self.writer.writeAll("Contents:\n");

            self.indent += 1;
            defer self.indent -= 1;

            try self.printIndent();
            try self.printExpression(integer_literal_cast);
        },
        .block => |block| {
            try self.writer.writeAll("Block:\n");

            self.indent += 1;
            defer self.indent -= 1;

            try self.printBlock(block);
        },
        .variable_or_class_access => |variable_or_class_access| {
            try self.writer.print("Variable or class access: {s}", .{variable_or_class_access});
        },
        .variable_access => |variable_access| {
            try self.writer.print("Variable access: {s}", .{variable_access});
        },
        .function_call => |function_call| {
            try self.writer.print(
                "Function call: {s}\n",
                .{switch (function_call.function) {
                    .name => |name| name,
                    .function => |function| function.function.name,
                }},
            );

            self.indent += 1;
            defer self.indent -= 1;

            for (function_call.parameters) |parameter| {
                try self.printIndent();
                try self.writer.writeAll("Parameter:\n");

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.writer.writeAll("Type: ");
                try self.printType(parameter.type);
                try self.writer.writeByte('\n');

                try self.printIndent();
                try self.writer.writeAll("Value: ");
                try self.printExpression(parameter);
            }
        },
        .ascii_string_literal => |ascii_string_literal| {
            try self.writer.print("ASCII string literal: {s}", .{ascii_string_literal});
        },
        .wide_string_literal => |wide_string_literal| {
            try self.writer.print("UTF-16 string literal: {s}", .{wide_string_literal});
        },
        .bitwise_and, .not_equal => |bitwise_and| {
            try self.writer.print("{s}:\n", .{@tagName(expression.contents)});

            self.indent += 1;
            defer self.indent -= 1;

            try self.printIndent();
            try self.writer.print("Lefthand:\n", .{});

            {
                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.printExpression(bitwise_and.lefthand);
            }

            try self.printIndent();
            try self.writer.print("Righthand:\n", .{});

            {
                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.printExpression(bitwise_and.righthand);
            }
        },
        .assignment => |assignment| {
            try self.writer.writeAll("Assignment:\n");

            {
                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.writer.writeAll("Destination:\n");

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.printExpression(assignment.destination);
            }

            {
                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.writer.writeAll("Value:\n");

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.printExpression(assignment.value);
            }
        },
        else => |tag| try self.writer.print("todo {s}", .{@tagName(tag)}),
    }

    try self.writer.writeByte('\n');
}

fn printBlock(self: *Self, nodes: []const Node) Error!void {
    for (nodes) |node| {
        try self.printIndent();
        switch (node) {
            .variable_declaration => |variable_declaration| {
                try self.writer.print(
                    "Variable Declaration: {s}, Type: ",
                    .{variable_declaration.name},
                );
                try self.printType(variable_declaration.type);
                try self.writer.writeByte('\n');
                if (variable_declaration.value) |value| {
                    self.indent += 1;
                    defer self.indent -= 1;

                    try self.printIndent();
                    try self.writer.writeAll("Value:\n");

                    self.indent += 1;
                    defer self.indent -= 1;

                    try self.printIndent();
                    try self.printExpression(value);
                }
            },
            .expression => |expression| {
                try self.writer.print(
                    "Expression, Type: ",
                    .{},
                );
                try self.printType(expression.type);
                try self.writer.writeByte('\n');

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.printExpression(expression);
            },
            .if_statement => |if_statement| {
                try self.writer.print(
                    "If Statement:\n",
                    .{},
                );

                self.indent += 1;
                defer self.indent -= 1;

                try self.printIndent();
                try self.writer.writeAll("Condition:\n");

                {
                    self.indent += 1;
                    defer self.indent -= 1;

                    try self.printIndent();
                    try self.printExpression(if_statement.condition);
                }
            },
            else => |tag| try self.writer.print("todo: {s}\n", .{@tagName(tag)}),
        }
    }
}

fn printIndent(self: *Self) Error!void {
    for (0..self.indent) |_| {
        try self.writer.writeAll("    ");
    }
}
