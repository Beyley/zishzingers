const std = @import("std");

const Parser = @import("parser.zig");
const Node = Parser.Node;

const Error = std.fs.File.WriteError;

pub fn dumpAst(writer: anytype, ast: Parser.Tree) Error!void {
    var indent: usize = 0;
    for (ast.root_elements.items) |root_node| {
        switch (root_node) {
            .using => |using| try printUsing(writer, &indent, using),
            .import => |import| try printImport(writer, &indent, import),
            .from_import => |from_import| try printFromImport(writer, &indent, from_import),
            .class => |class| try printClass(writer, &indent, class),
            else => |tag| std.debug.panic("todo: {s}", .{@tagName(tag)}),
        }
    }
}

fn printImport(writer: anytype, indent: *usize, import: *Node.Import) Error!void {
    try printIndent(writer, indent);

    try writer.print("Import: {s}\n", .{import.target});
}

fn printFromImport(writer: anytype, indent: *usize, from_import: *Node.FromImport) Error!void {
    try printIndent(writer, indent);

    try writer.print("From Import: {s}, Wanted {}\n", .{ from_import.target, from_import.wanted });
}

fn printUsing(writer: anytype, indent: *usize, using: *Node.Using) Error!void {
    try printIndent(writer, indent);

    try writer.print("Using: {s}, Target {s}\n", .{ @tagName(using.type), using.target });
}

fn printClass(writer: anytype, indent: *usize, class: *Node.Class) Error!void {
    try printIndent(writer, indent);

    try writer.print("Class: {s}, Base Class: {?s}\n", .{ class.name, class.base_class });

    indent.* += 1;
    defer indent.* -= 1;

    if (class.identifier) |identifier| {
        try printIndent(writer, indent);
        try writer.print("Identifier:\n", .{});

        indent.* += 1;
        defer indent.* -= 1;

        try printIndent(writer, indent);
        try printExpression(writer, indent, identifier);
    }

    for (class.fields) |field| {
        try printIndent(writer, indent);
        try writer.print(
            "Field: {s}, Modifiers: {}, Type: ",
            .{ field.name, field.modifiers },
        );
        try printType(writer, indent, field.type);
        try writer.writeByte('\n');

        indent.* += 1;
        defer indent.* -= 1;

        if (field.default_value) |default_value| {
            try printIndent(writer, indent);
            try writer.writeAll("Default Value:\n");

            indent.* += 1;
            defer indent.* -= 1;

            try printIndent(writer, indent);
            try printExpression(writer, indent, default_value);
        }
    }

    for (class.properties) |property| {
        try printIndent(writer, indent);
        try writer.print(
            "Property: {s}, Modifiers: {}, Type: ",
            .{ property.name, property.modifiers },
        );
        try printType(writer, indent, property.type);
        try writer.writeByte('\n');

        try printIndent(writer, indent);
        switch (property.get_body) {
            .missing, .forward_declaration => {},
            .expression => |expression| {
                try writer.writeAll("Get body:\n");

                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);

                try printExpression(writer, indent, expression);
            },
        }
    }

    for (class.functions) |function| {
        try printIndent(writer, indent);
        try writer.print(
            "Function: {s}, Modifiers {}, Type: ",
            .{ function.name, function.modifiers },
        );
        try printType(writer, indent, function.return_type);
        try writer.writeAll(", Parameters: [ ");
        for (function.parameters) |parameter| {
            try writer.print("{{ Name: {s}, Type: ", .{parameter.name});
            try printType(writer, indent, parameter.type);
            try writer.writeAll("}, ");
        }
        try writer.writeAll("]\n");

        if (function.body) |body| {
            indent.* += 1;
            defer indent.* -= 1;

            try printIndent(writer, indent);
            try printExpression(writer, indent, body);
        }
    }
}

fn printType(writer: anytype, indent: *usize, tree_type: Parser.Type) Error!void {
    _ = indent; // autofix

    switch (tree_type) {
        .parsed => |parsed| {
            if (parsed.dimension_count > 0) {
                try writer.print(
                    "{s} ({d} dimensions)",
                    .{ parsed.name, parsed.dimension_count },
                );
            } else {
                try writer.print("{s}", .{parsed.name});
            }
        },
        .resolved => try writer.writeAll("todo"),
        .unknown => try writer.writeAll("unknown"),
    }
}

fn printExpression(writer: anytype, indent: *usize, expression: *Node.Expression) Error!void {
    switch (expression.contents) {
        .guid_literal => |guid| try writer.print("GUID literal: {d}", .{guid}),
        .integer_literal => |literal| try writer.print(
            "Integer literal: {d} ({s})",
            .{ literal.value, @tagName(literal.base) },
        ),
        .block => |block| {
            try writer.writeAll("Block:\n");

            indent.* += 1;
            defer indent.* -= 1;

            try printBlock(writer, indent, block);
        },
        .variable_or_class_access => |variable_or_class_access| {
            try writer.print("Variable or class access: {s}", .{variable_or_class_access});
        },
        .function_call => |function_call| {
            try writer.print(
                "Function call: {s}, Type: ",
                .{switch (function_call.function) {
                    .name => |name| name,
                    .function => |function| function.name,
                }},
            );
            try printType(writer, indent, function_call.type);
            try writer.writeByte('\n');

            indent.* += 1;
            defer indent.* -= 1;

            for (function_call.parameters) |parameter| {
                try printIndent(writer, indent);
                try writer.writeAll("Parameter:\n");

                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try printExpression(writer, indent, parameter);
            }
        },
        .ascii_string_literal => |ascii_string_literal| {
            try writer.print("ASCII string literal: {s}", .{ascii_string_literal});
        },
        .wide_string_literal => |wide_string_literal| {
            try writer.print("UTF-16 string literal: {s}", .{wide_string_literal});
        },
        .assignment => |assignment| {
            try writer.writeAll("Assignment:\n");

            {
                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try writer.writeAll("Destination:\n");

                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try printExpression(writer, indent, assignment.destination);
            }

            {
                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try writer.writeAll("Value:\n");

                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try printExpression(writer, indent, assignment.value);
            }
        },
        else => |tag| try writer.print("todo {s}", .{@tagName(tag)}),
    }

    try writer.writeByte('\n');
}

fn printBlock(writer: anytype, indent: *usize, nodes: []const Node) Error!void {
    for (nodes) |node| {
        try printIndent(writer, indent);
        switch (node) {
            .variable_declaration => |variable_declaration| {
                try writer.print(
                    "Variable Declaration: {s}, Type: ",
                    .{variable_declaration.name},
                );
                try printType(writer, indent, variable_declaration.type);
                try writer.writeByte('\n');
                if (variable_declaration.value) |value| {
                    indent.* += 1;
                    defer indent.* -= 1;

                    try printIndent(writer, indent);
                    try writer.writeAll("Value:\n");

                    indent.* += 1;
                    defer indent.* -= 1;

                    try printIndent(writer, indent);
                    try printExpression(writer, indent, value);
                }
            },
            .expression => |expression| {
                try writer.print(
                    "Expression, Type: ",
                    .{},
                );
                try printType(writer, indent, expression.type);
                try writer.writeByte('\n');

                indent.* += 1;
                defer indent.* -= 1;

                try printIndent(writer, indent);
                try printExpression(writer, indent, expression);
            },
            else => |tag| try writer.print("todo: {s}\n", .{@tagName(tag)}),
        }
    }
}

fn printIndent(writer: anytype, indent: *usize) Error!void {
    for (0..indent.*) |_| {
        try writer.writeAll("    ");
    }
}
