const std = @import("std");

pub const Script = struct {
    up_to_date_script: ?ResourceIdentifier,
    class_name: []const u8,
    super_class_script: ?ResourceIdentifier,
    modifiers: u32,
    type_references: []const TypeReference,
    field_references: []const FieldReference,
    function_references: []const FunctionReference,
    field_definitions: []const FieldDefinition,
    property_definitions: []const PropertyDefinition,
};

const ResolvableTypeReference = union(enum) {
    type_reference: *const TypeReference,
    idx: u32,
};

const ResolvableString = union(enum) {
    string: *const []const u8,
    idx: u32,
};

const PropertyDefinition = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    get_function_idx: u32,
    set_function_idx: u32,
};

const FieldDefinition = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
};

const FunctionReference = struct {
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
};

pub const FieldReference = struct {
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
};

pub const TypeReference = struct {
    machine_type: MachineType,
    fish_type: FishType,
    dimension_count: u8,
    array_base_machine_type: u8,
    script: ?ResourceIdentifier,
    type_name: ResolvableString,
};

pub const Revision = struct {
    head: u32,
    branch_id: u16,
    branch_revision: u16,
};

pub const ResourceType = enum(u8) {
    invalid = 0,
    texture = 1,
    // gtf_texture = 1,
    // gxt_texture = 1,
    mesh = 2,
    pixel_shader = 3,
    vertex_shader = 4,
    animation = 5,
    guid_substitution = 6,
    gfx_material = 7,
    spu_elf = 8,
    level = 9,
    filename = 10,
    script = 11,
    settings_character = 12,
    file_of_bytes = 13,
    settings_soft_phys = 14,
    fontface = 15,
    material = 16,
    downloadable_content = 17,
    editor_settings = 18,
    joint = 19,
    game_constants = 20,
    poppet_settings = 21,
    cached_level_data = 22,
    synced_profile = 23,
    bevel = 24,
    game = 25,
    settings_network = 26,
    packs = 27,
    big_profile = 28,
    slot_list = 29,
    translation = 30,
    adventure_create_profile = 31,
    local_profile = 32,
    limits_settings = 33,
    tutorials = 34,
    guid_list = 35,
    audio_materials = 36,
    settings_fluid = 37,
    plan = 38,
    texture_list = 39,
    music_settings = 40,
    mixer_settings = 41,
    replay_config = 42,
    palette = 43,
    static_mesh = 44,
    animated_texture = 45,
    voip_recording = 46,
    pins = 47,
    instrument = 48,
    sample = 49,
    outfit_list = 50,
    paint_brush = 51,
    thing_recording = 52,
    painting = 53,
    quest = 54,
    animation_bank = 55,
    animation_set = 56,
    skeleton_map = 57,
    skeleton_registry = 58,
    skeleton_anim_styles = 59,
    crossplay_vita = 60,
    streaming_chunk = 61,
    shared_adventure_data = 62,
    adventure_play_profile = 63,
    animation_map = 64,
    cached_costume_data = 65,
    data_labels = 66,
    adventure_maps = 67,
};

pub const MachineType = enum(u8) {
    void = 0x0,
    bool = 0x1,
    char = 0x2,
    s32 = 0x3,
    f32 = 0x4,
    v4 = 0x5,
    m44 = 0x6,
    deprecated = 0x7,
    raw_ptr = 0x8,
    ref_ptr = 0x9,
    safe_ptr = 0xa,
    object_ref = 0xb,
    s64 = 0xc,
    f64 = 0xd,
};

pub const FishType = enum(u8) {
    void = 0x0,
    bool = 0x1,
    char = 0x2,
    s32 = 0x3,
    f32 = 0x4,
    v2 = 0x5,
    v3 = 0x6,
    v4 = 0x7,
    m44 = 0x8,
    guid = 0x9,
    s64 = 0xa,
    f64 = 0xb,
};

const ResourceIdentifier = union(enum) {
    guid: u32,
    hash: [std.crypto.hash.Sha1.digest_length]u8,
};

pub const ResourceDescriptor = struct {
    type: ResourceType,
    ident: ResourceIdentifier,
    flags: u32,
};

pub const CompressionFlags = packed struct(u8) {
    compressed_integers: bool,
    compressed_vectors: bool,
    compressed_matrices: bool,
    _padding: u5 = undefined,
};

pub fn MMReader(comptime Reader: type) type {
    _ = Reader; // autofix
    return struct {
        reader: std.fs.File.Reader,
        compression_flags: CompressionFlags,
        revision: Revision,

        const Self = @This();

        pub fn readInt(self: Self, comptime T: type) !T {
            if (@typeInfo(T).Int.bits <= 8) @compileError("You probably want .readByte");

            //If the file has compressed integers, read the varint
            if (self.compression_flags.compressed_integers)
                return try self.readVarInt(T);

            //Else read the int as normal
            return try self.reader.readInt(T, .big);
        }

        pub fn readByte(self: Self) !u8 {
            return try self.reader.readByte();
        }

        pub fn readSha1(self: Self) ![std.crypto.hash.Sha1.digest_length]u8 {
            var ret: [std.crypto.hash.Sha1.digest_length]u8 = undefined;

            _ = try self.reader.readAtLeast(&ret, std.crypto.hash.Sha1.digest_length);

            return ret;
        }

        fn readVarInt(self: Self, comptime IntType: type) !IntType {
            //Do all the work in unsigned space to prevent signed-ness shenanigans
            var result: std.meta.Int(.unsigned, @bitSizeOf(IntType)) = 0;
            var i: std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(IntType))) = 0;

            while (true) {
                const b: u8 = try self.reader.readByte();

                result |= @as(@TypeOf(result), b & 0b01111111) << i;

                //If the MSB is not set, break out
                if (b & 0b10000000 == 0) {
                    break;
                }

                //Mark that we are now filling out the next 7 bits
                i += 7;
            }

            return @bitCast(result);
        }

        fn readString(self: Self, allocator: std.mem.Allocator) ![]const u8 {
            const len =
                if (self.compression_flags.compressed_integers)
                @divExact(try self.readInt(u32), 2)
            else
                try self.readInt(u32);

            //Allocate the space for the new string
            const str = try allocator.alloc(u8, len);
            errdefer allocator.free(str);

            //Make sure that we read the whole string
            if (try self.reader.readAll(str) < len)
                return error.EndOfStream;

            return str;
        }

        pub fn readScript(self: Self, allocator: std.mem.Allocator) !Script {
            const up_to_date_script: ?ResourceIdentifier = if (self.revision.head <= 0x33a)
                try self.readResource(false)
            else
                null;

            std.debug.print("up_to_date_script: {?}\n", .{up_to_date_script});

            const class_name = try self.readString(allocator);
            std.debug.print("class name: {s}\n", .{class_name});

            const super_class_script = try self.readResource(false);

            std.debug.print("super class script: {?}\n", .{super_class_script});

            const read_type_is_u16 = self.revision.head >= 0x3d9;

            switch (read_type_is_u16) {
                inline else => |val| {
                    const ScriptReadType = if (val) u16 else u32;

                    const modifiers = if (self.revision.head >= 0x1e5)
                        try self.readInt(ScriptReadType)
                    else
                        0;

                    std.debug.print("modifiers: {d}\n", .{modifiers});

                    const type_references = try self.readArray(TypeReference, allocator, null, ScriptReadType);
                    for (type_references) |type_reference| {
                        std.debug.print("type_reference: {}\n", .{type_reference});
                    }

                    const field_references = try self.readArray(FieldReference, allocator, null, ScriptReadType);
                    for (field_references) |field_reference| {
                        std.debug.print("field_reference: {}\n", .{field_reference});
                    }

                    const function_references = try self.readArray(FunctionReference, allocator, null, ScriptReadType);
                    for (function_references) |function_reference| {
                        std.debug.print("function_reference: {}\n", .{function_reference});
                    }

                    const field_definitions = try self.readArray(FieldDefinition, allocator, null, ScriptReadType);
                    for (field_definitions) |field_definition| {
                        std.debug.print("field_definition: {}\n", .{field_definition});
                    }

                    const property_definitions = try self.readArray(PropertyDefinition, allocator, null, ScriptReadType);
                    for (property_definitions) |property_definition| {
                        std.debug.print("property_definition: {}\n", .{property_definition});
                    }

                    return Script{
                        .up_to_date_script = up_to_date_script,
                        .class_name = class_name,
                        .super_class_script = super_class_script,
                        .modifiers = modifiers,
                        .type_references = type_references,
                        .field_references = field_references,
                        .function_references = function_references,
                        .field_definitions = field_definitions,
                        .property_definitions = property_definitions,
                    };
                },
            }
        }

        pub fn readResource(self: Self, skip_flags: bool) !?ResourceIdentifier {
            const hash: u32, const guid: u32 = if (self.revision.head <= 0x18b) .{ 2, 1 } else .{ 1, 2 };

            const flags: u32 = if (self.revision.head > 0x22e and !skip_flags) try self.readInt(u32) else 0;
            _ = flags; // idk what flags does, seems to just be ignored

            const ident_type = try self.readByte();

            if (ident_type == hash) {
                return .{
                    .hash = try self.readSha1(),
                };
            } else if (ident_type == guid) {
                return .{
                    .guid = try self.readInt(u32),
                };
            }

            //TODO: should this be an error?
            return null;
        }

        pub fn readArray(self: Self, comptime T: type, allocator: std.mem.Allocator, length: ?usize, comptime ScriptReadType: type) ![]const T {
            const len: usize = length orelse try self.readInt(u32);

            const arr = try allocator.alloc(T, len);
            errdefer allocator.free(arr);

            for (arr) |*item| {
                item.* = switch (T) {
                    TypeReference => .{
                        .machine_type = try self.reader.readEnum(MachineType, .big),
                        .fish_type = try self.reader.readEnum(FishType, .big),
                        .dimension_count = try self.readByte(),
                        .array_base_machine_type = try self.readByte(),
                        .script = try self.readResource(false),
                        .type_name = .{ .idx = try self.readInt(u32) },
                    },
                    FieldReference, FunctionReference => .{
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                    },
                    FieldDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                    },
                    PropertyDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .get_function_idx = try self.readInt(u32),
                        .set_function_idx = try self.readInt(u32),
                    },
                    else => @compileError("Unknown type " ++ @typeName(T)),
                };
            }

            return arr;
        }
    };
}
