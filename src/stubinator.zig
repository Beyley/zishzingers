const std = @import("std");

const MMTypes = @import("MMTypes.zig");

pub fn generateStubs(
    writer: anytype,
    allocator: std.mem.Allocator,
    script: MMTypes.Script,
    script_guid: u32,
    asset_lookup: MMTypes.FileDB.GuidLookupMap,
    script_lookup: std.AutoHashMap(u32, MMTypes.Script),
    namespace: ?[]const u8,
    library: []const u8,
) !void {
    try writer.print("using library '{s}';", .{library});
    try writer.writeByte('\n');
    try writer.writeByte('\n');

    var script_references = std.AutoHashMap(u32, []const u8).init(allocator);
    defer script_references.deinit();

    if (script.super_class_script) |super_script| {
        try script_references.put(super_script.guid, std.fs.path.stem(asset_lookup.get(super_script.guid).?.path));
    }

    for (script.type_references) |type_references| {
        if (type_references.script) |referenced_script| {
            switch (referenced_script) {
                .guid => |reference_guid| try script_references.put(reference_guid, std.fs.path.stem(asset_lookup.get(reference_guid).?.path)),
                .hash => std.debug.panic("Unable to resolve reference to hashed script.", .{}),
            }
        }
    }

    var referenced_script_iter = script_references.iterator();
    while (referenced_script_iter.next()) |referenced_script| {
        //Dont try to import self
        if (referenced_script.key_ptr.* == script_guid)
            continue;

        if (namespace != null)
            try writer.print("import '{s}:{s}'", .{ namespace.?, referenced_script.value_ptr.* })
        else
            try writer.print("import '{s}'", .{referenced_script.value_ptr.*});

        try writer.writeByte('\n');
    }

    try writer.writeByte('\n');

    try writer.print("class {s}(g{d})", .{ script.class_name, script_guid });
    if (script.super_class_script) |super_script| {
        try writer.print(" extends {s}", .{script_lookup.get(super_script.guid).?.class_name});
    }
    try writer.writeByte('\n');

    try writer.writeAll("{\n");
    for (script.field_definitions) |field| {
        try writer.print("    {} {s}: ", .{
            field.modifiers,
            script.a_string_table.strings[field.name],
        });

        try formatType(writer, script_lookup, script, script.type_references[field.type_reference]);

        try writer.writeAll(";\n");
    }

    try writer.writeAll("\n");

    for (script.functions) |function| {
        const demangled_name = try MMTypes.demangleFunctionName(script.a_string_table.strings[function.name], false, allocator);
        defer allocator.free(demangled_name);

        try writer.print("    {} fn {s}", .{ function.modifiers, demangled_name });
        try writer.writeByte('(');
        for (function.arguments.slice(script.arguments), 0..) |argument, i| {
            // Try to resolve a name for this argument using the local variables,
            // since arguments themselves dont actually store the names.
            // This is generally safe since the original compiler isnt really good at re-using registers
            const name: ?[]const u8 = blk: {
                for (function.local_variables.slice(script.local_variables)) |local_variable| {
                    if (local_variable.offset == argument.offset) {
                        if (local_variable.name != 0xFFFFFFFF) {
                            break :blk script.a_string_table.strings[local_variable.name];
                        }
                    }
                }

                break :blk null;
            };

            if (i > 0)
                try writer.writeAll(", ");

            if (name) |resolved_name|
                try writer.print("{s}: ", .{resolved_name})
            else
                try writer.print("arg{d}: ", .{i});

            try formatType(
                writer,
                script_lookup,
                script,
                script.type_references[argument.type_reference],
            );
        }
        try writer.writeAll(") -> ");
        try formatType(
            writer,
            script_lookup,
            script,
            script.type_references[function.type_reference],
        );
        try writer.writeAll(";\n");
    }
    try writer.writeAll("}\n");
}

fn formatType(writer: anytype, script_lookup: std.AutoHashMap(u32, MMTypes.Script), script: MMTypes.Script, type_reference: MMTypes.TypeReference) !void {
    const base_name = typeBaseName(script_lookup, script, type_reference);

    if (type_reference.dimension_count > 0) {
        switch (type_reference.array_base_machine_type) {
            .object_ref => try writer.writeAll("object"),
            .safe_ptr => try writer.writeAll("safeptr"),
            else => {
                const guessed_fish_type = MMTypes.FishType.guessFromMachineType(type_reference.array_base_machine_type);

                if (guessed_fish_type == .void)
                    std.debug.panic("bad array of type {s}", .{@tagName(type_reference.array_base_machine_type)});

                try writer.writeAll(guessed_fish_type.scriptName());
            },
        }
        for (0..type_reference.dimension_count) |_| {
            try writer.writeAll("[]");
        }
    } else {
        try writer.writeAll(base_name);
    }
}

fn typeBaseName(script_lookup: std.AutoHashMap(u32, MMTypes.Script), script: MMTypes.Script, type_reference: MMTypes.TypeReference) []const u8 {
    if (type_reference.machine_type == .void) {
        return "void";
    }

    if (type_reference.fish_type == .void) {
        if (type_reference.dimension_count > 0 and type_reference.machine_type == .object_ref) {
            return "object";
        }

        // Lets try to resolve this ourself... Some scripts like TweakPrimitive have script references but dont include type name,
        // maybe an older/newer version of the compiler excludes this information?
        if (type_reference.type_name == 0xFFFFFFFF) {
            if (type_reference.script) |type_script| {
                return script_lookup.get(type_script.guid).?.class_name;
            } else {
                @panic("what the fuck is happening here?");
            }
        }

        return script.a_string_table.strings[type_reference.type_name];
    }

    return type_reference.fish_type.scriptName();
}
