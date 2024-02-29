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
        for (self.functions) |function|
            function.deinit(allocator);
        allocator.free(self.functions);
        self.a_string_table.deinit(allocator);
        self.w_string_table.deinit(allocator);
        if (self.constant_table_s64) |constant_table_s64|
            allocator.free(constant_table_s64);
        allocator.free(self.constant_table_float);
        if (self.depending_guids) |depending_guids|
            allocator.free(depending_guids);
    }
};

pub const NopClass = packed struct(u48) {
    _unused: u48 = undefined,
};

pub const LoadConstClass = packed struct(u48) {
    dst_idx: u16,
    constant_idx: u32,
};

pub const CastClass = packed struct(u48) {
    dst_idx: u16,
    src_idx: u16,
    type_idx: u16,
};

pub const UnaryClass = packed struct(u48) {
    dst_idx: u16,
    src_idx: u16,
    _unused: u16 = undefined,
};

pub const BinaryClass = packed struct(u48) {
    dst_idx: u16,
    src_a_idx: u16,
    src_b_idx: u16,
};

pub const GetBuiltinMemberClass = packed struct(u48) {
    dst_idx: u16,
    base_idx: u16,
    _unused: u16 = undefined,
};

pub const SetBuiltinMemberClass = packed struct(u48) {
    src_idx: u16,
    base_idx: u16,
    _unused: u16 = undefined,
};

pub const GetMemberClass = packed struct(u48) {
    dst_idx: u16,
    base_idx: u16,
    field_ref: u16,
};

pub const SetMemberClass = packed struct(u48) {
    src_idx: u16,
    base_idx: u16,
    field_ref: u16,
};

pub const GetElementClass = packed struct(u48) {
    dst_idx: u16,
    base_idx: u16,
    src_or_index_idx: u16,
};

pub const SetElementClass = packed struct(u48) {
    src_idx: u16,
    base_idx: u16,
    index_idx: u16,
};

pub const NewObjectClass = packed struct(u48) {
    dst_idx: u16,
    _unused: u16 = undefined,
    type_idx: u16,
};

pub const NewArrayClass = packed struct(u48) {
    dst_idx: u16,
    size_idx: u16,
    type_idx: u16,
};

pub const WriteClass = packed struct(u48) {
    _unused1: u16 = undefined,
    src_idx: u16,
    _unused2: u16 = undefined,
};

pub const ArgClass = packed struct(u48) {
    src_idx: u16,
    arg_idx: u16,
    _unused: u16 = undefined,
};

pub const CallClass = packed struct(u48) {
    dst_idx: u16,
    call_idx: u16,
    _unused: u16 = undefined,
};

pub const ReturnClass = packed struct(u48) {
    _unused1: u16 = undefined,
    src_idx: u16,
    _unused2: u16 = undefined,
};

pub const BranchClass = packed struct(u48) {
    src_idx: u16,
    branch_offset: i32,
};

pub const Bytecode = packed struct(u64) {
    op: InstructionType,
    type: MachineType,
    params: InstructionParams,

    pub inline fn init(instr: TaggedInstruction, machine_type: MachineType) Bytecode {
        const op, const params = instr.destructure();
        return .{
            .op = op,
            .type = machine_type,
            .params = params,
        };
    }
};

pub const InstructionType = @typeInfo(TaggedInstruction).Union.tag_type.?;
pub const InstructionParams = @Type(.{ .Union = blk: {
    var info = @typeInfo(TaggedInstruction).Union;
    info.tag_type = null;
    info.layout = .Packed;
    info.decls = &.{};
    break :blk info;
} });
pub const TaggedInstruction = union(enum(u8)) {
    NOP: NopClass = 0x0,
    LCb: LoadConstClass = 0x1,
    LCc: LoadConstClass = 0x2,
    LCi: LoadConstClass = 0x3,
    LCf: LoadConstClass = 0x4,
    LCsw: LoadConstClass = 0x5,
    LC_NULLsp: LoadConstClass = 0x6,
    MOVb: UnaryClass = 0x7,
    LOG_NEGb: UnaryClass = 0x8,
    MOVc: UnaryClass = 0x9,
    MOVi: UnaryClass = 0xa,
    INCi: UnaryClass = 0xb,
    DECi: UnaryClass = 0xc,
    NEGi: UnaryClass = 0xd,
    BIT_NEGi: UnaryClass = 0xe,
    LOG_NEGi: UnaryClass = 0xf,
    ABSi: UnaryClass = 0x10,
    MOVf: UnaryClass = 0x11,
    NEGf: UnaryClass = 0x12,
    ABSf: UnaryClass = 0x13,
    SQRTf: UnaryClass = 0x14,
    SINf: UnaryClass = 0x15,
    COSf: UnaryClass = 0x16,
    TANf: UnaryClass = 0x17,
    MOVv4: UnaryClass = 0x18,
    NEGv4: UnaryClass = 0x19,
    MOVm44: UnaryClass = 0x1a,
    IT_MOV_S_DEPRECATED: NopClass = 0x1b,
    MOVrp: UnaryClass = 0x1c,
    MOVcp: UnaryClass = 0x1d,
    MOVsp: UnaryClass = 0x1e,
    MOVo: UnaryClass = 0x1f,
    EQb: BinaryClass = 0x20,
    NEb: BinaryClass = 0x21,
    IT_RESERVED0_C: BinaryClass = 0x22,
    IT_RESERVED1_C: BinaryClass = 0x23,
    LTc: BinaryClass = 0x24,
    LTEc: BinaryClass = 0x25,
    GTc: BinaryClass = 0x26,
    GTEc: BinaryClass = 0x27,
    EQc: BinaryClass = 0x28,
    NEc: BinaryClass = 0x29,
    ADDi: BinaryClass = 0x2a,
    SUBi: BinaryClass = 0x2b,
    MULi: BinaryClass = 0x2c,
    DIVi: BinaryClass = 0x2d,
    MODi: BinaryClass = 0x2e,
    MINi: BinaryClass = 0x2f,
    MAXi: BinaryClass = 0x30,
    SLAi: BinaryClass = 0x31,
    SRAi: BinaryClass = 0x32,
    SRLi: BinaryClass = 0x33,
    BIT_ORi: BinaryClass = 0x34,
    BIT_ANDi: BinaryClass = 0x35,
    BIT_XORi: BinaryClass = 0x36,
    LTi: BinaryClass = 0x37,
    LTEi: BinaryClass = 0x38,
    GTi: BinaryClass = 0x39,
    GTEi: BinaryClass = 0x3a,
    EQi: BinaryClass = 0x3b,
    NEi: BinaryClass = 0x3c,
    ADDf: BinaryClass = 0x3d,
    SUBf: BinaryClass = 0x3e,
    MULf: BinaryClass = 0x3f,
    DIVf: BinaryClass = 0x40,
    MINf: BinaryClass = 0x41,
    MAXf: BinaryClass = 0x42,
    LTf: BinaryClass = 0x43,
    LTEf: BinaryClass = 0x44,
    GTf: BinaryClass = 0x45,
    GTEf: BinaryClass = 0x46,
    EQf: BinaryClass = 0x47,
    NEf: BinaryClass = 0x48,
    ADDv4: BinaryClass = 0x49,
    SUBv4: BinaryClass = 0x4a,
    MULSv4: BinaryClass = 0x4b,
    DIVSv4: BinaryClass = 0x4c,
    DOT4v4: BinaryClass = 0x4d,
    DOT3v4: BinaryClass = 0x4e,
    DOT2v4: BinaryClass = 0x4f,
    CROSS3v4: BinaryClass = 0x50,
    MULm44: BinaryClass = 0x51,
    IT_EQ_S_DEPRECATED: NopClass = 0x52,
    IT_NE_S_DEPRECATED: NopClass = 0x53,
    EQrp: BinaryClass = 0x54,
    NErp: BinaryClass = 0x55,
    EQo: BinaryClass = 0x56,
    NEo: BinaryClass = 0x57,
    EQsp: BinaryClass = 0x58,
    NEsp: BinaryClass = 0x59,
    GET_V4_X: GetBuiltinMemberClass = 0x5a,
    GET_V4_Y: GetBuiltinMemberClass = 0x5b,
    GET_V4_Z: GetBuiltinMemberClass = 0x5c,
    GET_V4_W: GetBuiltinMemberClass = 0x5d,
    GET_V4_LEN2: GetBuiltinMemberClass = 0x5e,
    GET_V4_LEN3: GetBuiltinMemberClass = 0x5f,
    GET_V4_LEN4: GetBuiltinMemberClass = 0x60,
    GET_M44_XX: GetBuiltinMemberClass = 0x61,
    GET_M44_XY: GetBuiltinMemberClass = 0x62,
    GET_M44_XZ: GetBuiltinMemberClass = 0x63,
    GET_M44_XW: GetBuiltinMemberClass = 0x64,
    GET_M44_YX: GetBuiltinMemberClass = 0x65,
    GET_M44_YY: GetBuiltinMemberClass = 0x66,
    GET_M44_YZ: GetBuiltinMemberClass = 0x67,
    GET_M44_YW: GetBuiltinMemberClass = 0x68,
    GET_M44_ZX: GetBuiltinMemberClass = 0x69,
    GET_M44_ZY: GetBuiltinMemberClass = 0x6a,
    GET_M44_ZZ: GetBuiltinMemberClass = 0x6b,
    GET_M44_ZW: GetBuiltinMemberClass = 0x6c,
    GET_M44_WX: GetBuiltinMemberClass = 0x6d,
    GET_M44_WY: GetBuiltinMemberClass = 0x6e,
    GET_M44_WZ: GetBuiltinMemberClass = 0x6f,
    GET_M44_WW: GetBuiltinMemberClass = 0x70,
    GET_M44_rX: GetBuiltinMemberClass = 0x71,
    GET_M44_rY: GetBuiltinMemberClass = 0x72,
    GET_M44_rZ: GetBuiltinMemberClass = 0x73,
    GET_M44_rW: GetBuiltinMemberClass = 0x74,
    GET_M44_cX: GetBuiltinMemberClass = 0x75,
    GET_M44_cY: GetBuiltinMemberClass = 0x76,
    GET_M44_cZ: GetBuiltinMemberClass = 0x77,
    GET_M44_cW: GetBuiltinMemberClass = 0x78,
    SET_V4_X: SetBuiltinMemberClass = 0x79,
    SET_V4_Y: SetBuiltinMemberClass = 0x7a,
    SET_V4_Z: SetBuiltinMemberClass = 0x7b,
    SET_V4_W: SetBuiltinMemberClass = 0x7c,
    SET_M44_XX: SetBuiltinMemberClass = 0x7d,
    SET_M44_XY: SetBuiltinMemberClass = 0x7e,
    SET_M44_XZ: SetBuiltinMemberClass = 0x7f,
    SET_M44_XW: SetBuiltinMemberClass = 0x80,
    SET_M44_YX: SetBuiltinMemberClass = 0x81,
    SET_M44_YY: SetBuiltinMemberClass = 0x82,
    SET_M44_YZ: SetBuiltinMemberClass = 0x83,
    SET_M44_YW: SetBuiltinMemberClass = 0x84,
    SET_M44_ZX: SetBuiltinMemberClass = 0x85,
    SET_M44_ZY: SetBuiltinMemberClass = 0x86,
    SET_M44_ZZ: SetBuiltinMemberClass = 0x87,
    SET_M44_ZW: SetBuiltinMemberClass = 0x88,
    SET_M44_WX: SetBuiltinMemberClass = 0x89,
    SET_M44_WY: SetBuiltinMemberClass = 0x8a,
    SET_M44_WZ: SetBuiltinMemberClass = 0x8b,
    SET_M44_WW: SetBuiltinMemberClass = 0x8c,
    SET_M44_rX: SetBuiltinMemberClass = 0x8d,
    SET_M44_rY: SetBuiltinMemberClass = 0x8e,
    SET_M44_rZ: SetBuiltinMemberClass = 0x8f,
    SET_M44_rW: SetBuiltinMemberClass = 0x90,
    SET_M44_cX: SetBuiltinMemberClass = 0x91,
    SET_M44_cY: SetBuiltinMemberClass = 0x92,
    SET_M44_cZ: SetBuiltinMemberClass = 0x93,
    SET_M44_cW: SetBuiltinMemberClass = 0x94,
    GET_SP_MEMBER: GetMemberClass = 0x95,
    GET_RP_MEMBER: GetMemberClass = 0x96,
    SET_SP_MEMBER: SetMemberClass = 0x97,
    SET_RP_MEMBER: SetMemberClass = 0x98,
    GET_ELEMENT: GetElementClass = 0x99,
    SET_ELEMENT: SetElementClass = 0x9a,
    GET_ARRAY_LEN: GetBuiltinMemberClass = 0x9b,
    NEW_ARRAY: NewArrayClass = 0x9c,
    ARRAY_INSERT: SetElementClass = 0x9d,
    ARRAY_APPEND: SetElementClass = 0x9e,
    ARRAY_ERASE: SetElementClass = 0x9f,
    ARRAY_FIND: GetElementClass = 0xa0,
    ARRAY_CLEAR: SetElementClass = 0xa1,
    WRITE: WriteClass = 0xa2,
    ARG: ArgClass = 0xa3,
    CALL: CallClass = 0xa4,
    RET: ReturnClass = 0xa5,
    B: BranchClass = 0xa6,
    BEZ: BranchClass = 0xa7,
    BNEZ: BranchClass = 0xa8,
    CASTsp: CastClass = 0xa9,
    INTb: UnaryClass = 0xaa,
    INTc: UnaryClass = 0xab,
    INTf: UnaryClass = 0xac,
    FLOATb: UnaryClass = 0xad,
    FLOATc: UnaryClass = 0xae,
    FLOATi: UnaryClass = 0xaf,
    BOOLc: UnaryClass = 0xb0,
    BOOLi: UnaryClass = 0xb1,
    BOOLf: UnaryClass = 0xb2,
    GET_OBJ_MEMBER: GetMemberClass = 0xb3,
    SET_OBJ_MEMBER: SetMemberClass = 0xb4,
    NEW_OBJECT: NewObjectClass = 0xb5,
    ARRAY_RESIZE: SetElementClass = 0xb6,
    ARRAY_RESERVE: SetElementClass = 0xb7,
    LCv4: LoadConstClass = 0xb8,
    LC_NULLo: LoadConstClass = 0xb9,
    CASTo: CastClass = 0xba,
    GET_SP_NATIVE_MEMBER: GetMemberClass = 0xbb,
    LCsa: LoadConstClass = 0xbc,
    BIT_ORb: BinaryClass = 0xbd,
    BIT_ANDb: BinaryClass = 0xbe,
    BIT_XORb: BinaryClass = 0xbf,
    CALLVo: CallClass = 0xc0,
    CALLVsp: CallClass = 0xc1,
    ASSERT: WriteClass = 0xc2,

    pub inline fn destructure(tagged: TaggedInstruction) struct { InstructionType, InstructionParams } {
        return .{
            tagged,
            switch (tagged) {
                inline else => |payload, itag| @unionInit(InstructionParams, @tagName(itag), payload),
            },
        };
    }

    pub inline fn from(tag: InstructionType, payload: InstructionParams) TaggedInstruction {
        return switch (tag) {
            inline else => |itag| @unionInit(TaggedInstruction, @tagName(itag), @field(payload, @tagName(itag))),
        };
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
    slice: []const Bytecode,
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

    pub fn deinit(self: FunctionDefinition, allocator: std.mem.Allocator) void {
        allocator.free(self.arguments.slice);
        allocator.free(self.bytecode.slice);
        allocator.free(self.line_numbers.slice);
        allocator.free(self.local_variables.slice);
    }
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
    array_base_machine_type: MachineType,
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
                    defer allocator.free(shared_arguments);
                    const shared_bytecode = try self.readArray(Bytecode, allocator, null, ScriptReadType);
                    defer allocator.free(shared_bytecode);
                    const shared_line_numbers = try self.readArray(u16, allocator, null, ScriptReadType);
                    defer allocator.free(shared_line_numbers);
                    const shared_local_variables = try self.readArray(LocalVariable, allocator, null, ScriptReadType);
                    defer allocator.free(shared_local_variables);

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

                        for (functions) |*function| {
                            if (function.name.idx != 0xFFFFFFFF)
                                function.name = .{ .string = &a_str_table.strings[function.name.idx] }
                            else
                                function.name = .{ .string = null };

                            if (function.type_reference.idx != 0xFFFFFFFF)
                                function.type_reference = .{ .type_reference = &type_references[function.type_reference.idx] };

                            function.arguments = .{ .slice = try allocator.dupe(Argument, shared_arguments[function.arguments.idx.begin..function.arguments.idx.end]) };
                            function.bytecode = .{ .slice = try allocator.dupe(Bytecode, shared_bytecode[function.bytecode.idx.begin..function.bytecode.idx.end]) };
                            function.line_numbers = .{ .slice = try allocator.dupe(u16, shared_line_numbers[function.line_numbers.idx.begin..function.line_numbers.idx.end]) };
                            function.local_variables = .{ .slice = try allocator.dupe(LocalVariable, shared_local_variables[function.local_variables.idx.begin..function.local_variables.idx.end]) };
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
                    Bytecode => @bitCast(try self.readInt(u64)),
                    // Bytecode => byteSwapFromBe(Bytecode, @bitCast(try self.readInt(u64))),
                    TypeReference => .{
                        .machine_type = try self.reader.readEnum(MachineType, .big),
                        .fish_type = try self.reader.readEnum(FishType, .big),
                        .dimension_count = try self.readInt(u8),
                        .array_base_machine_type = try self.reader.readEnum(MachineType, .big),
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

/// Demangles a function name
pub fn demangleFunctionName(orig: []const u8, extract_args: bool, allocator: std.mem.Allocator) ![]const u8 {
    const sep_index = std.mem.indexOf(u8, orig, "__").?;

    const name = orig[0..sep_index];

    if (!extract_args)
        return try allocator.dupe(u8, name);

    const args_str = orig[sep_index + 2 ..];

    var demangled = std.ArrayList(u8).init(allocator);
    defer demangled.deinit();

    try demangled.appendSlice(name);
    try demangled.appendSlice("(");

    var i: usize = 0;
    while (i < args_str.len) : (i += 1) {
        if (i > 0) try demangled.appendSlice(", ");

        // std.debug.print("i: {d}", .{i});

        const c = args_str[i];
        if (c == 'Q') {
            var digit_count: usize = 0;

            //count the amount of numeric chars
            for (args_str[i + 1 ..]) |d| {
                if (std.ascii.isDigit(d))
                    digit_count += 1
                else
                    break;
            }

            const count = try std.fmt.parseInt(usize, args_str[i + 1 .. i + digit_count + 1], 10);

            const type_name = args_str[i + 1 + digit_count .. i + 1 + digit_count + count];

            try demangled.appendSlice(type_name);

            //Skip the whole parameter
            i += digit_count + count;
            continue;
        }

        const fish_type = fishTypeFromMangledId(c);

        try demangled.appendSlice(@tagName(fish_type));
    }

    try demangled.appendSlice(")");

    return demangled.toOwnedSlice();
}

fn fishTypeFromMangledId(id: u8) FishType {
    return switch (id) {
        'v' => .void,
        'b' => .bool,
        'w' => .char,
        'i' => .s32,
        'f' => .f32,
        'p' => .v2,
        'q' => .v3,
        'r' => .v4,
        'm' => .m44,
        'g' => .guid,
        'j' => .s64,
        'd' => .f64,
        else => std.debug.panic("Unknown mangling ID {c}", .{id}),
    };
}

// /// Byteswap's all the fields of a struct individually from BE to LE
// fn byteSwapFromBe(comptime T: type, val: T) T {
//     //Short circuit the function on big endian, since no byte swap needs to take place
//     if (builtin.cpu.arch.endian() == .big)
//         return val;

//     const type_info: std.builtin.Type = @typeInfo(T);

//     var ret: T = val;

//     //we do the byteswap earlier :)
//     if (T == Bytecode) {
//         return ret;
//     }

//     switch (type_info) {
//         .Struct => |data| {
//             inline for (data.fields) |field| {
//                 @field(ret, field.name) = byteSwapFromBe(field.type, @field(ret, field.name));
//             }
//         },
//         .Int => {
//             ret = @byteSwap(ret);
//         },
//         .Enum => |data| {
//             ret = @enumFromInt(byteSwapFromBe(data.tag_type, @intFromEnum(ret)));
//         },
//         .Float => |float| {
//             const IntermediaryIntType = std.meta.Int(.unsigned, float.bits);

//             ret = @bitCast(@byteSwap(@as(IntermediaryIntType, ret)));
//         },
//         else => @compileError("Unsupported type " ++ @typeName(T)),
//     }

//     return ret;
// }
