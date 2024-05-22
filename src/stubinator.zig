//! Generates library stubs from a MAP and a folder of script files

const std = @import("std");

const MMTypes = @import("MMTypes.zig");
const Disasm = @import("disasm.zig");

pub fn generateStubs(
    writer: anytype,
    allocator: std.mem.Allocator,
    script: MMTypes.Script,
    script_guid: u32,
    script_lookup: std.AutoHashMap(u32, MMTypes.Script),
    namespace: ?[]const u8,
    library: []const u8,
) !void {
    std.debug.print("dumping script {s}\n", .{script.class_name});

    try writer.print("using library '{s}';", .{library});
    try writer.writeByte('\n');
    try writer.writeByte('\n');

    var script_references = std.AutoHashMap(u32, []const u8).init(allocator);
    defer script_references.deinit();

    if (script.super_class_script) |super_script| {
        try script_references.putNoClobber(super_script.guid, script_lookup.get(super_script.guid).?.class_name);
    }

    for (script.type_references) |type_reference| {
        if (type_reference.script) |referenced_script| {
            switch (referenced_script) {
                .guid => |reference_guid| try script_references.put(reference_guid, script_lookup.get(reference_guid).?.class_name),
                .hash => std.debug.panic("Unable to resolve reference to hashed script.", .{}),
            }
        }
    }

    var referenced_script_iter = script_references.iterator();
    while (referenced_script_iter.next()) |referenced_script| {
        //Dont try to import self
        if (referenced_script.key_ptr.* == script_guid)
            continue;

        try writer.writeAll("import '");
        if (namespace) |script_namespace| {
            try writer.print("{s}:", .{script_namespace});
        }
        for (referenced_script.value_ptr.*) |c| {
            try writer.writeByte(std.ascii.toLower(c));
        }
        try writer.writeAll("';\n");
    }

    try writer.writeByte('\n');

    try writer.print("{} class {s}(g{d})", .{ script.modifiers orelse MMTypes.Modifiers{}, script.class_name, script_guid });
    if (script.super_class_script) |super_script| {
        try writer.print(" extends {s}", .{script_lookup.get(super_script.guid).?.class_name});
    }
    try writer.writeByte('\n');

    try writer.writeAll("{\n");

    for (script.type_references) |type_reference|
        // If it has a name and is not an object ref or safe ptr, then its probably an enum
        if (type_reference.type_name != 0xFFFFFFFF and
            type_reference.fish_type != .void)
        {
            const type_name = script.a_string_table.strings[type_reference.type_name];

            // If the type name starts with `.`
            if (std.mem.startsWith(u8, type_name, script.class_name) and
                type_name[script.class_name.len] == '.')
            {
                const enum_name = type_name[script.class_name.len + 1 ..];

                // We cant really guess at the enum *contents*, but we can just emit an empty enum and im sure its fine
                try writer.print("    pub enum {s} : {s} {{}}\n", .{ enum_name, @tagName(type_reference.fish_type) });
            }
        };
    try writer.writeByte('\n');

    for (script.field_definitions) |field| {
        try writer.print("    {} {s}: ", .{
            field.modifiers,
            script.a_string_table.strings[field.name],
        });

        try formatType(
            writer,
            script_lookup,
            script,
            script.type_references[field.type_reference],
        );

        try writer.writeAll(";\n");
    }

    try writer.writeAll("\n");

    for (script.property_definitions) |property| {
        try writer.print("    {} {s}: ", .{
            property.modifiers,
            script.a_string_table.strings[property.name],
        });

        try formatType(
            writer,
            script_lookup,
            script,
            script.type_references[property.type_reference],
        );

        try writer.writeAll(" {\n");

        if (property.get_function != 0xFFFFFFFF) {
            try writer.writeAll("        get;\n");
        }

        if (property.set_function != 0xFFFFFFFF) {
            try writer.writeAll("        set;\n");
        }

        try writer.writeAll("    }\n");
    }

    try writer.writeAll("\n");

    func: for (script.functions, 0..) |function, func_idx| {
        //Search for any properties that use this function, if they do, skip this func
        for (script.property_definitions) |property|
            if (property.get_function == func_idx or property.set_function == func_idx)
                continue :func;

        const mangled_name = script.a_string_table.strings[function.name];

        const demangled_name, const parameters = try MMTypes.demangleFunctionName(mangled_name, true, allocator);
        defer {
            for (parameters.?) |parameter|
                allocator.free(parameter);

            allocator.free(parameters.?);
        }

        //Skip init function
        if (std.mem.eql(u8, demangled_name, ".init")) {
            try writer.writeAll("    fn __init()");
            continue;
        } else if (std.mem.eql(u8, demangled_name, ".ctor")) {
            try writer.print("    {} {s}", .{ function.modifiers, script.class_name });
            try writer.writeByte('(');
            try formatParameters(writer, script_lookup, function, script, parameters.?);
            try writer.writeAll(")");

            // constructor should always be void
            std.debug.assert(script.type_references[function.type_reference].machine_type == .void);
        } else if (demangled_name[0] == '.') {
            std.debug.panic("Bad special-cased func {s}, needs special handling above.", .{demangled_name});
        } else {
            try writer.print("    {} fn {s}", .{ function.modifiers, demangled_name });
            try writer.writeByte('(');
            try formatParameters(writer, script_lookup, function, script, parameters.?);
            try writer.writeAll(") -> ");
            try formatType(
                writer,
                script_lookup,
                script,
                script.type_references[function.type_reference],
            );
        }

        const bytecode = function.bytecode.slice(script.bytecode);

        if (bytecode.len > 0) {
            try writer.writeAll(
                \\ {
                \\        inline_asm 
                \\        {
                \\
            );

            try Disasm.disassembleBytecode(
                writer,
                allocator,
                script,
                &function,
                2,
                true,
            );

            try writer.writeAll(
                \\        }
                \\
                \\        unreachable;
                \\    }
                \\
            );
        } else {
            try writer.writeAll(";\n");
        }
    }
    try writer.writeAll("}\n");
}

fn formatParameters(
    writer: anytype,
    script_lookup: std.AutoHashMap(u32, MMTypes.Script),
    function: MMTypes.FunctionDefinition,
    script: MMTypes.Script,
    parameter_type_names: []const []const u8,
) !void {
    _ = script_lookup; // autofix

    for (function.arguments.slice(script.arguments), parameter_type_names, 0..) |argument, parameter_type_name, i| {

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

        try writer.writeAll(parameter_type_name);
        // try formatType(
        //     writer,
        //     script_lookup,
        //     script,
        //     script.type_references[argument.type_reference],
        // );
    }
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

                try writer.writeAll(@tagName(guessed_fish_type));
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

    // i dont know what this will break, but this will resolve the names for enums
    if (type_reference.type_name != 0xFFFFFFFF) {
        return script.a_string_table.strings[type_reference.type_name];
    }

    return @tagName(type_reference.fish_type);
}
