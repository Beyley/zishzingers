const std = @import("std");
const builtin = @import("builtin");

pub const Script = struct {
    up_to_date_script: ?ResourceIdentifier,
    class_name: []const u8,
    super_class_script: ?ResourceIdentifier,
    modifiers: ?u32,
    type_references: []const TypeReference,
    field_references: []const FieldReference,
    function_references: []const FunctionReference,
    field_definitions: []const FieldDefinition,
    property_definitions: []const PropertyDefinition,
    functions: []const FunctionDefinition,
    shared_arguments: []const Argument,
    shared_bytecode: []const u64,
    shared_line_numbers: []const u16,
    shared_local_variables: []const LocalVariable,
    a_string_table: AStringTable,
    w_string_table: WStringTable,
    constant_table_s64: ?[]const u32, //why is this called s64 but its a u32 data type
    constant_table_float: []const f32,
    depending_guids: ?[]const u32,

    pub fn deinit(self: *const Script, allocator: std.mem.Allocator) void {
        allocator.free(self.class_name);
        allocator.free(self.type_references);
        allocator.free(self.field_references);
        allocator.free(self.function_references);
        allocator.free(self.field_definitions);
        allocator.free(self.property_definitions);
        allocator.free(self.functions);
        allocator.free(self.shared_arguments);
        allocator.free(self.shared_bytecode);
        allocator.free(self.shared_line_numbers);
        allocator.free(self.shared_local_variables);
        self.a_string_table.deinit(allocator);
        self.w_string_table.deinit(allocator);
        if (self.constant_table_s64) |constant_table_s64|
            allocator.free(constant_table_s64);
        allocator.free(self.constant_table_float);
        if (self.depending_guids) |depending_guids|
            allocator.free(depending_guids);
    }
};

const AStringTable = struct {
    buf: []const u8,
    strings: []const []const u8,

    pub fn deinit(self: AStringTable, allocator: std.mem.Allocator) void {
        allocator.free(self.strings);
        allocator.free(self.buf);
    }
};

const WStringTable = struct {
    buf: []const u16,
    strings: []const []const u16,

    pub fn deinit(self: WStringTable, allocator: std.mem.Allocator) void {
        allocator.free(self.strings);
        allocator.free(self.buf);
    }
};

const ResolvableTypeReference = union(enum) {
    type_reference: *const TypeReference,
    idx: u32,
};

const ResolvableString = union(enum) {
    string: ?*const []const u8,
    idx: u32,
};

const ResolvableFunction = union(enum) {
    function: *const FunctionDefinition,
    idx: u32,
};

const ResolvableArgumentSlice = union(enum) {
    slice: []const Argument,
    idx: struct {
        begin: u32,
        end: u32,
    },
};

const ResolvableBytecodeSlice = union(enum) {
    slice: []const u64,
    idx: struct {
        begin: u32,
        end: u32,
    },
};

const ResolvableLineNumberSlice = union(enum) {
    slice: []const u16,
    idx: struct {
        begin: u32,
        end: u32,
    },
};

const ResolvableLocalVariableSlice = union(enum) {
    slice: []const LocalVariable,
    idx: struct {
        begin: u32,
        end: u32,
    },
};

const FunctionDefinition = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    arguments: ResolvableArgumentSlice,
    bytecode: ResolvableBytecodeSlice,
    line_numbers: ResolvableLineNumberSlice,
    local_variables: ResolvableLocalVariableSlice,
    stack_size: u32,
};

const Function = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    arguments: []const Argument,
    bytecode: []const u64,
    line_numbers: []const u16,
    local_variables: []const LocalVariable,
    stack_size: u32,
};

const LocalVariable = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    offset: u32,
};

const Argument = struct {
    type_reference: ResolvableTypeReference,
    offset: u32,
};

const PropertyDefinition = struct {
    modifiers: u32,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    get_function: ResolvableFunction,
    set_function: ResolvableFunction,
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

pub const InstructionType = enum(u8) {
    NOP = 0x0,
    LCb = 0x1,
    LCc = 0x2,
    LCi = 0x3,
    LCf = 0x4,
    LCsw = 0x5,
    LC_NULLsp = 0x6,
    MOVb = 0x7,
    LOG_NEGb = 0x8,
    MOVc = 0x9,
    MOVi = 0xa,
    INCi = 0xb,
    DECi = 0xc,
    NEGi = 0xd,
    BIT_NEGi = 0xe,
    LOG_NEGi = 0xf,
    ABSi = 0x10,
    MOVf = 0x11,
    NEGf = 0x12,
    ABSf = 0x13,
    SQRTf = 0x14,
    SINf = 0x15,
    COSf = 0x16,
    TANf = 0x17,
    MOVv4 = 0x18,
    NEGv4 = 0x19,
    MOVm44 = 0x1a,
    IT_MOV_S_DEPRECATED = 0x1b,
    MOVrp = 0x1c,
    MOVcp = 0x1d,
    MOVsp = 0x1e,
    MOVo = 0x1f,
    EQb = 0x20,
    NEb = 0x21,
    IT_RESERVED0_C = 0x22,
    IT_RESERVED1_C = 0x23,
    LTc = 0x24,
    LTEc = 0x25,
    GTc = 0x26,
    GTEc = 0x27,
    EQc = 0x28,
    NEc = 0x29,
    ADDi = 0x2a,
    SUBi = 0x2b,
    MULi = 0x2c,
    DIVi = 0x2d,
    MODi = 0x2e,
    MINi = 0x2f,
    MAXi = 0x30,
    SLAi = 0x31,
    SRAi = 0x32,
    SRLi = 0x33,
    BIT_ORi = 0x34,
    BIT_ANDi = 0x35,
    BIT_XORi = 0x36,
    LTi = 0x37,
    LTEi = 0x38,
    GTi = 0x39,
    GTEi = 0x3a,
    EQi = 0x3b,
    NEi = 0x3c,
    ADDf = 0x3d,
    SUBf = 0x3e,
    MULf = 0x3f,
    DIVf = 0x40,
    MINf = 0x41,
    MAXf = 0x42,
    LTf = 0x43,
    LTEf = 0x44,
    GTf = 0x45,
    GTEf = 0x46,
    EQf = 0x47,
    NEf = 0x48,
    ADDv4 = 0x49,
    SUBv4 = 0x4a,
    MULSv4 = 0x4b,
    DIVSv4 = 0x4c,
    DOT4v4 = 0x4d,
    DOT3v4 = 0x4e,
    DOT2v4 = 0x4f,
    CROSS3v4 = 0x50,
    MULm44 = 0x51,
    IT_EQ_S_DEPRECATED = 0x52,
    IT_NE_S_DEPRECATED = 0x53,
    EQrp = 0x54,
    NErp = 0x55,
    EQo = 0x56,
    NEo = 0x57,
    EQsp = 0x58,
    NEsp = 0x59,
    GET_V4_X = 0x5a,
    GET_V4_Y = 0x5b,
    GET_V4_Z = 0x5c,
    GET_V4_W = 0x5d,
    GET_V4_LEN2 = 0x5e,
    GET_V4_LEN3 = 0x5f,
    GET_V4_LEN4 = 0x60,
    GET_M44_XX = 0x61,
    GET_M44_XY = 0x62,
    GET_M44_XZ = 0x63,
    GET_M44_XW = 0x64,
    GET_M44_YX = 0x65,
    GET_M44_YY = 0x66,
    GET_M44_YZ = 0x67,
    GET_M44_YW = 0x68,
    GET_M44_ZX = 0x69,
    GET_M44_ZY = 0x6a,
    GET_M44_ZZ = 0x6b,
    GET_M44_ZW = 0x6c,
    GET_M44_WX = 0x6d,
    GET_M44_WY = 0x6e,
    GET_M44_WZ = 0x6f,
    GET_M44_WW = 0x70,
    GET_M44_rX = 0x71,
    GET_M44_rY = 0x72,
    GET_M44_rZ = 0x73,
    GET_M44_rW = 0x74,
    GET_M44_cX = 0x75,
    GET_M44_cY = 0x76,
    GET_M44_cZ = 0x77,
    GET_M44_cW = 0x78,
    SET_V4_X = 0x79,
    SET_V4_Y = 0x7a,
    SET_V4_Z = 0x7b,
    SET_V4_W = 0x7c,
    SET_M44_XX = 0x7d,
    SET_M44_XY = 0x7e,
    SET_M44_XZ = 0x7f,
    SET_M44_XW = 0x80,
    SET_M44_YX = 0x81,
    SET_M44_YY = 0x82,
    SET_M44_YZ = 0x83,
    SET_M44_YW = 0x84,
    SET_M44_ZX = 0x85,
    SET_M44_ZY = 0x86,
    SET_M44_ZZ = 0x87,
    SET_M44_ZW = 0x88,
    SET_M44_WX = 0x89,
    SET_M44_WY = 0x8a,
    SET_M44_WZ = 0x8b,
    SET_M44_WW = 0x8c,
    SET_M44_rX = 0x8d,
    SET_M44_rY = 0x8e,
    SET_M44_rZ = 0x8f,
    SET_M44_rW = 0x90,
    SET_M44_cX = 0x91,
    SET_M44_cY = 0x92,
    SET_M44_cZ = 0x93,
    SET_M44_cW = 0x94,
    GET_SP_MEMBER = 0x95,
    GET_RP_MEMBER = 0x96,
    SET_SP_MEMBER = 0x97,
    SET_RP_MEMBER = 0x98,
    GET_ELEMENT = 0x99,
    SET_ELEMENT = 0x9a,
    GET_ARRAY_LEN = 0x9b,
    NEW_ARRAY = 0x9c,
    ARRAY_INSERT = 0x9d,
    ARRAY_APPEND = 0x9e,
    ARRAY_ERASE = 0x9f,
    ARRAY_FIND = 0xa0,
    ARRAY_CLEAR = 0xa1,
    WRITE = 0xa2,
    ARG = 0xa3,
    CALL = 0xa4,
    RET = 0xa5,
    B = 0xa6,
    BEZ = 0xa7,
    BNEZ = 0xa8,
    CASTsp = 0xa9,
    INTb = 0xaa,
    INTc = 0xab,
    INTf = 0xac,
    FLOATb = 0xad,
    FLOATc = 0xae,
    FLOATi = 0xaf,
    BOOLc = 0xb0,
    BOOLi = 0xb1,
    BOOLf = 0xb2,
    GET_OBJ_MEMBER = 0xb3,
    SET_OBJ_MEMBER = 0xb4,
    NEW_OBJECT = 0xb5,
    ARRAY_RESIZE = 0xb6,
    ARRAY_RESERVE = 0xb7,
    LCv4 = 0xb8,
    LC_NULLo = 0xb9,
    CASTo = 0xba,
    GET_SP_NATIVE_MEMBER = 0xbb,
    LCsa = 0xbc,
    BIT_ORb = 0xbd,
    BIT_ANDb = 0xbe,
    BIT_XORb = 0xbf,
    CALLVo = 0xc0,
    CALLVsp = 0xc1,
    ASSERT = 0xc2,
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
            //If the file has compressed integers, read the varint
            if (self.compression_flags.compressed_integers and @typeInfo(T).Int.bits > 16)
                return try self.readVarInt(T);

            //Else read the int as normal
            return try self.reader.readInt(T, .big);
        }

        pub fn readFloat(self: Self, comptime T: type) !T {
            return @bitCast(try self.reader.readInt(std.meta.Int(.unsigned, @typeInfo(T).Float.bits), .big));
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

        fn readBytes(self: Self, buf: []u8) !usize {
            return try self.reader.readAll(buf);
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

            const class_name = try self.readString(allocator);
            const super_class_script = try self.readResource(false);

            const read_type_is_u16 = self.revision.head >= 0x3d9;

            switch (read_type_is_u16) {
                inline else => |val| {
                    const ScriptReadType = if (val) u16 else u32;

                    const modifiers: ?u32 = if (self.revision.head >= 0x1e5)
                        try self.readInt(ScriptReadType)
                    else
                        null;

                    const type_references = try self.readArray(TypeReference, allocator, null, ScriptReadType);
                    const field_references = try self.readArray(FieldReference, allocator, null, ScriptReadType);
                    const function_references = try self.readArray(FunctionReference, allocator, null, ScriptReadType);
                    const field_definitions = try self.readArray(FieldDefinition, allocator, null, ScriptReadType);
                    const property_definitions = try self.readArray(PropertyDefinition, allocator, null, ScriptReadType);

                    if (self.revision.head < 0x1ec) @panic("AAAA");

                    const functions = try self.readArray(FunctionDefinition, allocator, null, ScriptReadType);
                    const shared_arguments = try self.readArray(Argument, allocator, null, ScriptReadType);
                    const shared_bytecode = try self.readArray(u64, allocator, null, ScriptReadType);
                    const shared_line_numbers = try self.readArray(u16, allocator, null, ScriptReadType);
                    const shared_local_variables = try self.readArray(LocalVariable, allocator, null, ScriptReadType);

                    // In revision 0x3d9, range ends were made to be relative, not absolute, this is probably to make save storage on large scripts
                    if (self.revision.head >= 0x3d9) {
                        for (functions) |*function_definition| {
                            function_definition.arguments.idx.end += function_definition.arguments.idx.begin;
                            function_definition.bytecode.idx.end += function_definition.bytecode.idx.begin;
                            function_definition.line_numbers.idx.end += function_definition.line_numbers.idx.begin;
                            function_definition.local_variables.idx.end += function_definition.local_variables.idx.begin;
                        }
                    }

                    const a_str_table: AStringTable = blk: {
                        const indices = try self.readTable(allocator);
                        defer allocator.free(indices);

                        const str_buf_len = try self.readInt(u32);

                        //Read the full string buffer
                        const str_buf = try allocator.alloc(u8, str_buf_len);
                        if (try self.readBytes(str_buf) < str_buf_len)
                            return error.EndOfStream;

                        const str_count = std.mem.count(u8, str_buf, &.{0});

                        const strings: [][]const u8 = try allocator.alloc([]const u8, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u8, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end];

                            i = end + 1;
                        }

                        break :blk AStringTable{
                            .buf = str_buf,
                            .strings = strings,
                        };
                    };

                    const w_str_table: WStringTable = blk: {
                        const indices = try self.readTable(allocator);
                        defer allocator.free(indices);

                        const str_buf_len = try self.readInt(u32);

                        //Read the full string buffer
                        const str_buf = try allocator.alloc(u16, str_buf_len);
                        if (try self.readBytes(std.mem.sliceAsBytes(str_buf)) < str_buf_len)
                            return error.EndOfStream;

                        const str_count = std.mem.count(u16, str_buf, &.{0});

                        const strings: [][]u16 = try allocator.alloc([]u16, str_count);

                        var i: usize = 0;
                        //Iterate over all strings
                        for (strings) |*str| {
                            const end = std.mem.indexOfPos(u16, str_buf, i, &.{0}).?;

                            //Slice to the next 0 byte after the start
                            str.* = str_buf[i..end];

                            // If we are on a LE machine, byte swap the data, since LBP uses BE UTF-16 here.
                            if (builtin.cpu.arch.endian() == .little)
                                for (str.*) |*c| {
                                    c.* = @byteSwap(c.*);
                                };

                            i = end + 1;
                        }

                        break :blk WStringTable{
                            .buf = str_buf,
                            .strings = strings,
                        };
                    };

                    const constant_table_s64: ?[]u32 =
                        if (self.revision.head >= 0x3e2)
                        try self.readArray(u32, allocator, null, ScriptReadType)
                    else
                        null;

                    const constant_table_float: []f32 = try self.readArray(f32, allocator, null, ScriptReadType);

                    const depending_guids: ?[]u32 =
                        if (self.revision.head >= 0x1ec)
                        try self.readArray(u32, allocator, null, ScriptReadType)
                    else
                        null;

                    { //start resolving shit
                        for (type_references) |*type_reference| {
                            if (type_reference.type_name.idx != 0xFFFFFFFF)
                                type_reference.type_name = .{ .string = &a_str_table.strings[type_reference.type_name.idx] }
                            else
                                type_reference.type_name = .{ .string = null };
                        }

                        for (field_references) |*field_reference| {
                            if (field_reference.name.idx != 0xFFFFFFFF)
                                field_reference.name = .{ .string = &a_str_table.strings[field_reference.name.idx] }
                            else
                                field_reference.name = .{ .string = null };

                            if (field_reference.type_reference.idx != 0xFFFFFFFF)
                                field_reference.type_reference = .{ .type_reference = &type_references[field_reference.type_reference.idx] };
                        }

                        for (function_references) |*function_reference| {
                            if (function_reference.name.idx != 0xFFFFFFFF)
                                function_reference.name = .{ .string = &a_str_table.strings[function_reference.name.idx] }
                            else
                                function_reference.name = .{ .string = null };

                            if (function_reference.type_reference.idx != 0xFFFFFFFF)
                                function_reference.type_reference = .{ .type_reference = &type_references[function_reference.type_reference.idx] };
                        }

                        for (field_definitions) |*field_definition| {
                            if (field_definition.name.idx != 0xFFFFFFFF)
                                field_definition.name = .{ .string = &a_str_table.strings[field_definition.name.idx] }
                            else
                                field_definition.name = .{ .string = null };

                            if (field_definition.type_reference.idx != 0xFFFFFFFF)
                                field_definition.type_reference = .{ .type_reference = &type_references[field_definition.type_reference.idx] };
                        }

                        for (property_definitions) |*property_definition| {
                            if (property_definition.name.idx != 0xFFFFFFFF)
                                property_definition.name = .{ .string = &a_str_table.strings[property_definition.name.idx] }
                            else
                                property_definition.name = .{ .string = null };

                            if (property_definition.type_reference.idx != 0xFFFFFFFF)
                                property_definition.type_reference = .{ .type_reference = &type_references[property_definition.type_reference.idx] };

                            if (property_definition.set_function.idx != 0xFFFFFFFF)
                                property_definition.set_function = .{ .function = &functions[property_definition.set_function.idx] };

                            if (property_definition.get_function.idx != 0xFFFFFFFF)
                                property_definition.get_function = .{ .function = &functions[property_definition.get_function.idx] };
                        }

                        for (functions) |*function| {
                            if (function.name.idx != 0xFFFFFFFF)
                                function.name = .{ .string = &a_str_table.strings[function.name.idx] }
                            else
                                function.name = .{ .string = null };

                            if (function.type_reference.idx != 0xFFFFFFFF)
                                function.type_reference = .{ .type_reference = &type_references[function.type_reference.idx] };

                            function.arguments = .{ .slice = shared_arguments[function.arguments.idx.begin..function.arguments.idx.end] };
                            function.bytecode = .{ .slice = shared_bytecode[function.bytecode.idx.begin..function.bytecode.idx.end] };
                            function.line_numbers = .{ .slice = shared_line_numbers[function.line_numbers.idx.begin..function.line_numbers.idx.end] };
                            function.local_variables = .{ .slice = shared_local_variables[function.local_variables.idx.begin..function.local_variables.idx.end] };
                        }

                        for (shared_arguments) |*argument| {
                            if (argument.type_reference.idx != 0xFFFFFFFF)
                                argument.type_reference = .{ .type_reference = &type_references[argument.type_reference.idx] };
                        }

                        for (shared_local_variables) |*local_variable| {
                            if (local_variable.type_reference.idx != 0xFFFFFFFF)
                                local_variable.type_reference = .{ .type_reference = &type_references[local_variable.type_reference.idx] };

                            if (local_variable.name.idx != 0xFFFFFFFF)
                                local_variable.name = .{ .string = &a_str_table.strings[local_variable.name.idx] }
                            else
                                local_variable.name = .{ .string = null };
                        }
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
                        .functions = functions,
                        .shared_arguments = shared_arguments,
                        .shared_bytecode = shared_bytecode,
                        .shared_line_numbers = shared_line_numbers,
                        .shared_local_variables = shared_local_variables,
                        .a_string_table = a_str_table,
                        .w_string_table = w_str_table,
                        .constant_table_s64 = constant_table_s64,
                        .constant_table_float = constant_table_float,
                        .depending_guids = depending_guids,
                    };
                },
            }
        }

        pub fn readResource(self: Self, skip_flags: bool) !?ResourceIdentifier {
            const hash: u32, const guid: u32 = if (self.revision.head <= 0x18b) .{ 2, 1 } else .{ 1, 2 };

            const flags: u32 = if (self.revision.head > 0x22e and !skip_flags) try self.readInt(u32) else 0;
            _ = flags; // idk what flags does, seems to just be ignored

            const ident_type = try self.readInt(u8);

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

        pub fn readArray(self: Self, comptime T: type, allocator: std.mem.Allocator, length: ?usize, comptime ScriptReadType: type) ![]T {
            const len: usize = length orelse try self.readInt(u32);

            const arr = try allocator.alloc(T, len);
            errdefer allocator.free(arr);

            for (arr) |*item| {
                if (@typeInfo(T) == .Int) {
                    item.* = try self.readInt(T);
                } else if (@typeInfo(T) == .Float) {
                    item.* = try self.readFloat(T);
                } else item.* = switch (T) {
                    TypeReference => .{
                        .machine_type = try self.reader.readEnum(MachineType, .big),
                        .fish_type = try self.reader.readEnum(FishType, .big),
                        .dimension_count = try self.readInt(u8),
                        .array_base_machine_type = try self.readInt(u8),
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
                        .get_function = .{ .idx = try self.readInt(u32) },
                        .set_function = .{ .idx = try self.readInt(u32) },
                    },
                    FunctionDefinition => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .arguments = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .bytecode = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .line_numbers = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .local_variables = .{
                            .idx = .{
                                .begin = try self.readInt(ScriptReadType),
                                .end = try self.readInt(ScriptReadType),
                            },
                        },
                        .stack_size = try self.readInt(u32),
                    },
                    Argument => .{
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .offset = try self.readInt(u32),
                    },
                    LocalVariable => .{
                        .modifiers = try self.readInt(ScriptReadType),
                        .type_reference = .{ .idx = try self.readInt(u32) },
                        .name = .{ .idx = try self.readInt(u32) },
                        .offset = try self.readInt(u32),
                    },
                    else => @compileError("Unknown type " ++ @typeName(T)),
                };
            }

            return arr;
        }

        pub fn readTable(self: Self, allocator: std.mem.Allocator) ![]u32 {
            if (!self.compression_flags.compressed_vectors) {
                return self.readArray(u32, allocator, null, undefined);
            }

            const index_count = try self.readInt(u32);

            //If theres no indices, return nothing
            if (index_count == 0) return &.{};

            const table_count = try self.readInt(u32);

            //If theres no tables, return a single 0 item
            if (table_count == 0) return try allocator.dupe(u32, &.{0});

            const values = try allocator.alloc(u32, index_count);

            for (0..index_count) |i|
                values[i] = try self.readInt(u8);

            for (1..table_count) |_|
                for (0..index_count) |j| {
                    values[j] += @as(u32, try self.readInt(u8)) * 0x100;
                };

            return values;
        }
    };
}
