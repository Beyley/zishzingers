const std = @import("std");
const builtin = @import("builtin");

pub const Script = struct {
    up_to_date_script: ?ResourceIdentifier,
    class_name: []const u8,
    super_class_script: ?ResourceIdentifier,
    modifiers: ?Modifiers,
    type_references: []const TypeReference,
    field_references: []const FieldReference,
    function_references: []const FunctionReference,
    field_definitions: []const FieldDefinition,
    property_definitions: []const PropertyDefinition,
    functions: []const FunctionDefinition,
    a_string_table: AStringTable,
    w_string_table: WStringTable,
    constant_table_s64: ?[]const i64,
    constant_table_float: []const f32,
    depending_guids: ?[]const u32,
    bytecode: []const Bytecode,
    arguments: []const Argument,
    line_numbers: []const u16,
    local_variables: []const LocalVariable,

    pub fn deinit(self: *const Script, allocator: std.mem.Allocator) void {
        allocator.free(self.class_name);
        allocator.free(self.type_references);
        allocator.free(self.field_references);
        allocator.free(self.function_references);
        allocator.free(self.field_definitions);
        allocator.free(self.property_definitions);
        allocator.free(self.functions);
        self.a_string_table.deinit(allocator);
        self.w_string_table.deinit(allocator);
        if (self.constant_table_s64) |constant_table_s64|
            allocator.free(constant_table_s64);
        allocator.free(self.constant_table_float);
        if (self.depending_guids) |depending_guids|
            allocator.free(depending_guids);
        allocator.free(self.bytecode);
        allocator.free(self.arguments);
        allocator.free(self.line_numbers);
        allocator.free(self.local_variables);
    }
};

/// Demangles a function name
pub fn demangleFunctionName(orig: []const u8, extract_args: bool, allocator: std.mem.Allocator) ![]const u8 {
    const sep_index = std.mem.lastIndexOf(u8, orig, "__").?;

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

        const fish_type = FishType.fromMangledId(c);

        try demangled.appendSlice(@tagName(fish_type));
    }

    try demangled.appendSlice(")");

    return demangled.toOwnedSlice();
}

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

/// Used for instructions with a single source and a single destination register
pub const UnaryClass = packed struct(u48) {
    dst_idx: u16,
    src_idx: u16,
    _unused: u16 = undefined,
};

/// Used for instructions with two source registers and one destination register
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

pub const ExternalInvokeClass = packed struct(u48) {
    dst_idx: u16,
    call_address: u24,
    toc_switch: bool,
    _unused: u7 = undefined,
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
    info.layout = .@"packed";
    info.decls = &.{};
    break :blk info;
} });
pub const TaggedInstruction = union(enum(u8)) {
    /// Does nothing, progresses to the next instruction
    NOP: NopClass = 0x0,
    /// Loads a boolean into the destination register,
    /// storing a 1 for true, and 0 for false.
    /// The result is a single byte
    LCb: LoadConstClass = 0x1,
    /// Loads a 16-bit integer into the destination register,
    /// This directly uses the upper 16-bits of the constant_idx as the loaded constant
    LCc: LoadConstClass = 0x2,
    /// Loads a 32-bit integer into a the destination register,
    /// This directly uses the constant_idx as the loaded constant
    LCi: LoadConstClass = 0x3,
    /// Loads a 32-bit floating point number into the destination register
    /// This directly uses the constant_idx as the loaded constant
    LCf: LoadConstClass = 0x4,
    /// Loads the 32-bit pointer to a string into the destination register
    /// This pulls from the wide-string table
    LCsw: LoadConstClass = 0x5,
    /// Loads a 32-bit null pointer into the destination register
    LC_NULLsp: LoadConstClass = 0x6,
    /// Moves a single byte from the source register into the destination register
    MOVb: UnaryClass = 0x7,
    /// Negates the 1-byte boolean at the source register,
    /// storing the result in the destination register as a 1-byte boolean
    LOG_NEGb: UnaryClass = 0x8,
    /// Moves a two byte integer from the source register into the destination register
    MOVc: UnaryClass = 0x9,
    /// Moves a four-byte integer from the source register into the destination register
    MOVi: UnaryClass = 0xa,
    /// Increments the signed 32-bit integer at the source register,
    /// storing the result as a signed 32-bit integer into the destination register
    INCi: UnaryClass = 0xb,
    /// Decrements the signed 32-bit integer at the source register,
    /// storing the result as a signed 32-bit integer into the destination register
    DECi: UnaryClass = 0xc,
    /// Negates the signed 32-bit integer at the source register (eg `-val`),
    /// storing the result as a signed 32-bit integer into the destination register
    NEGi: UnaryClass = 0xd,
    /// Does a bitwise negation of the 4-byte region of data stored at the source register
    /// storing the result into the destination register
    BIT_NEGi: UnaryClass = 0xe,
    /// Does a logical negation of the 32-bit integer stored at the source register,
    /// eg. turning a 1 into a 0, and a 0 into a 1
    LOG_NEGi: UnaryClass = 0xf,
    /// Gets the absolute value of the 32-bit integer stored at the source register,
    /// storing the result in the destination register
    ABSi: UnaryClass = 0x10,
    /// Moves a 32-bit float from the source register into the destination register
    MOVf: UnaryClass = 0x11,
    /// Negates the 32-bit float stored at the source register (eg. `-val`),
    /// storing the result in the destination register
    NEGf: UnaryClass = 0x12,
    /// Gets the absolute value of the 32-bit float stored at the source register
    /// storing the result in the destination register
    ABSf: UnaryClass = 0x13,
    /// Gets the square root of the 32-bit float stored at the source register
    /// storing the result in the destination register
    SQRTf: UnaryClass = 0x14,
    /// Does a sine operation on the 32-bit float stored at the source register,
    /// first, the value is cast to a double before the operation takes place,
    /// then the result of the operation is cast back to a 32-bit integer,
    /// and stored in the destination register as a 32-bit float
    SINf: UnaryClass = 0x15,
    /// Does a cosine operation on the 32-bit float stored at the source register,
    /// first, the value is cast to a double before the operation takes place,
    /// then the result of the operationis cast back to a 32-bit integer,
    /// and stored in the destination register as a 32-bit float
    COSf: UnaryClass = 0x16,
    /// Does a tangent operation on the 32-bit float stored at the source register,
    /// storing the result in the destination register as a 32-bit float
    TANf: UnaryClass = 0x17,
    /// Moves a v4 from the source register to the destination register
    /// Both registers are rounded *down* to the nearest multiple of 16, due to AltiVec requirements
    MOVv4: UnaryClass = 0x18,
    /// Negates the four f32 elements in the vector (eg `-val`)
    /// Storing the result in the destination register
    NEGv4: UnaryClass = 0x19,
    /// Moves the m44 matrix from the source register into the destination register
    MOVm44: UnaryClass = 0x1a,
    /// Treated as a NOP
    IT_MOV_S_DEPRECATED: NopClass = 0x1b,
    /// Moves a 4-byte raw_ptr from the source register into the destination register
    MOVrp: UnaryClass = 0x1c,
    /// Treated as a NOP
    MOVcp: UnaryClass = 0x1d,
    /// Moves a 4-byte safe_ptr from the source register into the destination register
    MOVsp: UnaryClass = 0x1e,
    /// Moves a 4-byte object_ref from the source register into the destination register
    MOVo: UnaryClass = 0x1f,
    /// Compares if the booleans stored at the source registers are equal,
    /// storing into the destination register as a boolean
    EQb: BinaryClass = 0x20,
    /// Compares if the booleans stored at the source registers are not equal,
    /// storing into the destination register as a boolean
    NEb: BinaryClass = 0x21,
    /// Seems to trigger a non-fatal "invalid instruction" message
    IT_RESERVED0_C: BinaryClass = 0x22,
    /// Seems to trigger a non-fatal "invalid instruction" message
    IT_RESERVED1_C: BinaryClass = 0x23,
    /// Compares two chars, checking if the value stored in src_a
    /// is less than the value stored in src_b
    ///
    /// Stores the result into the destination register as a boolean
    LTc: BinaryClass = 0x24,
    /// Compares two chars, checking if the value stored in src_a
    /// is less than or equal to the value stored in src_b
    ///
    /// Stores the result into the destination register as a boolean
    LTEc: BinaryClass = 0x25,
    /// Compares two chars, checking if the value stored in src_a
    /// is greater than the value stored in src_b
    ///
    /// Stores the result into the destination register as a boolean
    GTc: BinaryClass = 0x26,
    /// Compares two chars, checking if the value stored in src_a
    /// is greater than or equal to the value stored in src_b
    ///
    /// Stores the result into the destination register as a boolean
    GTEc: BinaryClass = 0x27,
    /// Compares if the chars stored at the source registers are equal,
    /// storing into the destination register as a boolean
    EQc: BinaryClass = 0x28,
    /// Compares if the chars stored at the source registers are not equal,
    /// storing into the destination register as a boolean
    NEc: BinaryClass = 0x29,
    /// Adds two signed 32-bit integers together, storing the result in the destionation register
    ADDi: BinaryClass = 0x2a,
    /// Subtracts two 32-bit integers from each other, storing a 32-bit integer result.
    /// Subtracting src_b_idx from src_a_idx, and storing the result into dst_idx
    SUBi: BinaryClass = 0x2b,
    /// Multiplies two 32-bit ingeters with eachother, storing a 32-bit integer result in dst_idx
    MULi: BinaryClass = 0x2c,
    /// Divides two 32-bit ingeters, storing the result as a 32-bit integer
    /// Divides src_a by src_b
    DIVi: BinaryClass = 0x2d,
    /// Performs modulo on two signed 32-bit integers, storing the result as a signed 32-bit integer
    /// Operation is as follows: dst = src_a % src_b
    MODi: BinaryClass = 0x2e,
    /// Reads two signed 32-bit integers from the source registers,
    /// storing the smaller value in the destination register as a signed 32-bit integer
    MINi: BinaryClass = 0x2f,
    /// Reads two signed 32-bit integers from the source registers,
    /// storing the larger value in the destination register as a signed 32-bit integer
    MAXi: BinaryClass = 0x30,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// masking src_b with 0x11111, then shifts src_a left by the masked value,
    /// storing the result in dst_idx
    ///
    /// Zeroes are shifted in.
    SLAi: BinaryClass = 0x31,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// masking src_b with 0x11111, then shifts src_a right by the masked value,
    /// storing the result as a signed 32-bit integer  in dst_idx
    ///
    /// The highest bit is shifted in.
    SRAi: BinaryClass = 0x32,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// masking src_b with 0x11111, then shifts src_a right by the masked value,
    /// storing the result as a signed 32-bit integer in dst_idx
    ///
    /// Zeroes are shifted in.
    SRLi: BinaryClass = 0x33,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then does a bitwise OR on the values, storing the result in dst_idx
    BIT_ORi: BinaryClass = 0x34,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then does a bitwise AND on the values, storing the result in dst_idx
    BIT_ANDi: BinaryClass = 0x35,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then does a bitwise XOR on the values, storing the result in dst_idx
    BIT_XORi: BinaryClass = 0x36,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a < src_b,
    /// then storing the result into dst_idx as a 32-bit signed integer
    ///
    /// The result is 1 if true, 0 if false
    LTi: BinaryClass = 0x37,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a <= src_b,
    /// then storing the result into dst_idx as a 32-bit signed integer
    ///
    /// The result is 1 if true, 0 if false
    LTEi: BinaryClass = 0x38,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a > src_b,
    /// then storing the result into dst_idx as a 32-bit signed integer
    ///
    /// The result is 1 if true, 0 if false
    GTi: BinaryClass = 0x39,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a >= src_b,
    /// then storing the result into dst_idx as a 32-bit signed integer
    ///
    /// The result is 1 if true, 0 if false
    GTEi: BinaryClass = 0x3a,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a == src_b,
    /// then storing the result into dst_idx as a single byte (?)
    ///
    /// The result is 1 if true, 0 if false
    EQi: BinaryClass = 0x3b,
    /// Loads a signed 32-bit integer from src_a and src_b,
    /// then compares whether src_a != src_b,
    /// then storing the result into dst_idx as a single byte (?)
    ///
    /// The result is 1 if true, 0 if false
    NEi: BinaryClass = 0x3c,
    /// Adds the two 32-bit floats stored at the source registers together,
    /// then stores the result in the destination register as a 32-bit float
    ADDf: BinaryClass = 0x3d,
    /// Subtracts the 32-bit float from the src_b register from the src_a register (eg. `*src_a - *src_b`),
    /// then stores the result in the destination register as a 32-bit float
    SUBf: BinaryClass = 0x3e,
    /// Multiplies the two 32-bit floats stored in the source registers,
    /// then stores the result in the destination register
    MULf: BinaryClass = 0x3f,
    /// Divides the 32-bit float stored in src_a by the 32-bit float stored in src_b (eg. *src_a / *src_b),
    /// then stores the result in the destination register
    DIVf: BinaryClass = 0x40,
    /// Stores the lower 32-bit float of the two source registers into the destination register
    MINf: BinaryClass = 0x41,
    /// Stores the higher 32-bit float of the two source registers into the destination register
    MAXf: BinaryClass = 0x42,
    /// Compares whether the 32-bit float stored at src_a is less than the 32-bit float stored at src_b
    /// Stores the result as a boolean into the destination register
    LTf: BinaryClass = 0x43,
    /// Compares whether the 32-bit float stored at src_a is less than or equal to the 32-bit float stored at src_b
    /// Stores the result as a boolean into the destination register
    LTEf: BinaryClass = 0x44,
    /// Compares whether the 32-bit float stored at src_a is greater than the 32-bit float stored at src_b
    /// Stores the result as a boolean into the destination register
    GTf: BinaryClass = 0x45,
    /// Compares whether the 32-bit float stored at src_a is greater than or equal to the 32-bit float stored at src_b
    /// Stores the result as a boolean into the destination register
    GTEf: BinaryClass = 0x46,
    /// Compares whether the two 32-bit floats in the source registers are equal,
    /// storing the result as a boolean in the destination register
    EQf: BinaryClass = 0x47,
    /// Compares whether the two 32-bit floats in the source registers are not equal,
    /// storing the result as a boolean in the destination register
    NEf: BinaryClass = 0x48,
    /// Adds the four 32-bit floating point numbers in the src_a register with the ones in the src_b register,
    /// storing the result in the destination register
    ///
    /// The source and destination registers are rounded *down* to the nearest multiple of 16, due to Altivec requirements
    ADDv4: BinaryClass = 0x49,
    /// Subtracts the four 32-bit floating point numbers in the src_b register from the ones in the src_b register,
    /// storing the result in the destination register (eg. *src_a - *src_b)
    ///
    /// The source and destination registers are rounded *down* to the nearest multiple of 16, due to Altivec requirements
    SUBv4: BinaryClass = 0x4a,
    /// TODO: 0x00217b98
    MULSv4: BinaryClass = 0x4b,
    /// TODO: 0x00217b1c
    DIVSv4: BinaryClass = 0x4c,
    /// Does a dot-product of two 4-element vectors stored at src_a and src_b,
    /// and stores the result as a 32-bit floating point number in the destination register
    ///
    /// Due to AltiVec requirements, the source registers are rounded *down* to the nearest multiple of 16,
    /// and the destination register is rounded *down* to the nearest multiple of 4
    DOT4v4: BinaryClass = 0x4d,
    /// Does a dot product of the rightmost three elements stored in both vectors,
    /// storing the result as a 32-bit float in the destination register
    ///
    /// Due to AltiVec requirements, the source registers are rounded *down* to the nearest multiple of 16,
    /// and the destination register is rounded *down* to the nearest multiple of 4
    DOT3v4: BinaryClass = 0x4e,
    /// Does a dot product on the rightmost two elements stored in both vectors,
    /// storing the result as a 32-bit float in the destination register
    ///
    /// Due to AltiVec requirements, the source registers are rounded *down* to the nearest multiple of 16,
    /// and the destination register is rounded *down* to the nearest multiple of 4
    DOT2v4: BinaryClass = 0x4f,
    /// Does a cross product on the vectors stored in the two input registers,
    /// storing the result vector in the destination register
    ///
    /// Due to AltiVec requirements, the source and destination registers
    /// are rounded *down* to the nearest multiple of 16
    CROSS3v4: BinaryClass = 0x50,
    /// Multiplies two 4x4 matrices of f32 together.
    /// Stores the result in the destination register.
    ///
    /// Due to AltiVec requirements, the source and destination registers
    /// are rounded *down* to the nearest multiple of 16
    MULm44: BinaryClass = 0x51,
    /// Treated as a NOP
    IT_EQ_S_DEPRECATED: NopClass = 0x52,
    /// Treated as a NOP
    IT_NE_S_DEPRECATED: NopClass = 0x53,
    /// Compares if the two 32-bit values stored in the source registers are equal,
    /// storing the result in the destination register as a boolean
    EQrp: BinaryClass = 0x54,
    /// Compares if the two 32-bit values stored in the source registers are not equal,
    /// storing the result in the destination register as a boolean
    NErp: BinaryClass = 0x55,
    /// Compares if the two 32-bit values stored in the source registers are equal,
    /// storing the result in the destination register as a boolean
    EQo: BinaryClass = 0x56,
    /// Compares if the two 32-bit values stored in the source registers are not equal,
    /// storing the result in the destination register as a boolean
    NEo: BinaryClass = 0x57,
    /// Gets the Things that are located at the two safe_ptrs in the source registers,
    /// and checks if they are equal
    /// Stores the result in the destination register as a boolean
    EQsp: BinaryClass = 0x58,
    /// Gets the Things that are located at the two safe_ptrs in the source registers,
    /// and checks if they are not equal
    /// Stores the result in the destination register as a boolean
    NEsp: BinaryClass = 0x59,
    /// Gets the X [0] member of the v4 specified in the base register,
    /// stores the result in the destination register as an f32
    GET_V4_X: GetBuiltinMemberClass = 0x5a,
    /// Gets the Y [1] member of the v4 specified in the base register,
    /// stores the result in the destination register as an f32
    GET_V4_Y: GetBuiltinMemberClass = 0x5b,
    /// Gets the Z [2] member of the v4 specified in the base register,
    /// stores the result in the destination register as an f32
    GET_V4_Z: GetBuiltinMemberClass = 0x5c,
    /// Gets the W [3] member of the v4 specified in the base register,
    /// stores the result in the destination register as an f32
    GET_V4_W: GetBuiltinMemberClass = 0x5d,
    /// TODO
    GET_V4_LEN2: GetBuiltinMemberClass = 0x5e,
    /// TODO
    GET_V4_LEN3: GetBuiltinMemberClass = 0x5f,
    /// TODO
    GET_V4_LEN4: GetBuiltinMemberClass = 0x60,
    /// Loads the X, X element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x00
    GET_M44_XX: GetBuiltinMemberClass = 0x61,
    /// Loads the X, Y element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x10
    GET_M44_XY: GetBuiltinMemberClass = 0x62,
    /// Loads the X, Z element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x20
    GET_M44_XZ: GetBuiltinMemberClass = 0x63,
    /// Loads the X, W element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x30
    GET_M44_XW: GetBuiltinMemberClass = 0x64,
    /// Loads the Y, X element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x04
    GET_M44_YX: GetBuiltinMemberClass = 0x65,
    /// Loads the Y, Y element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x14
    GET_M44_YY: GetBuiltinMemberClass = 0x66,
    /// Loads the Y, Z element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x24
    GET_M44_YZ: GetBuiltinMemberClass = 0x67,
    /// Loads the Y, W element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x34
    GET_M44_YW: GetBuiltinMemberClass = 0x68,
    /// Loads the Z, X element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x08
    GET_M44_ZX: GetBuiltinMemberClass = 0x69,
    /// Loads the Z, Y element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x18
    GET_M44_ZY: GetBuiltinMemberClass = 0x6a,
    /// Loads the Z, Z element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x28
    GET_M44_ZZ: GetBuiltinMemberClass = 0x6b,
    /// Loads the Z, W element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x38
    GET_M44_ZW: GetBuiltinMemberClass = 0x6c,
    /// Loads the W, X element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x0c
    GET_M44_WX: GetBuiltinMemberClass = 0x6d,
    /// Loads the W, Y element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x1c
    GET_M44_WY: GetBuiltinMemberClass = 0x6e,
    /// Loads the W, Z element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x2c
    GET_M44_WZ: GetBuiltinMemberClass = 0x6f,
    /// Loads the W, W element of the 4x4 matrix,
    /// storing the result as a 32-bit float inside of the destination register
    ///
    /// The load offset is 0x3c
    GET_M44_WW: GetBuiltinMemberClass = 0x70,
    /// Gets the X row of the matrix,
    /// storing the result in the destination register
    GET_M44_rX: GetBuiltinMemberClass = 0x71,
    /// Gets the Y row of the matrix,
    /// storing the result in the destination register
    GET_M44_rY: GetBuiltinMemberClass = 0x72,
    /// Gets the Z row of the matrix,
    /// storing the result in the destination register
    GET_M44_rZ: GetBuiltinMemberClass = 0x73,
    /// Gets the W row of the matrix,
    /// storing the result in the destination register
    GET_M44_rW: GetBuiltinMemberClass = 0x74,
    /// Gets the X column of the matrix,
    /// storing the result in the destination register
    GET_M44_cX: GetBuiltinMemberClass = 0x75,
    /// Gets the Y column of the matrix,
    /// storing the result in the destination register
    GET_M44_cY: GetBuiltinMemberClass = 0x76,
    /// Gets the Z column of the matrix,
    /// storing the result in the destination register
    GET_M44_cZ: GetBuiltinMemberClass = 0x77,
    /// Gets the W column of the matrix,
    /// storing the result in the destination register
    GET_M44_cW: GetBuiltinMemberClass = 0x78,
    /// Sets the X [0] element of the vector to the f32 stored in the source register
    SET_V4_X: SetBuiltinMemberClass = 0x79,
    /// Sets the Y [1] element of the vector to the f32 stored in the source register
    SET_V4_Y: SetBuiltinMemberClass = 0x7a,
    /// Sets the Z [2] element of the vector to the f32 stored in the source register
    SET_V4_Z: SetBuiltinMemberClass = 0x7b,
    /// Sets the W [3] element of the vector to the f32 stored in the source register
    SET_V4_W: SetBuiltinMemberClass = 0x7c,
    /// Sets the X, X element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x00
    SET_M44_XX: SetBuiltinMemberClass = 0x7d,
    /// Sets the X, Y element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x10
    SET_M44_XY: SetBuiltinMemberClass = 0x7e,
    /// Sets the X, Z element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x20
    SET_M44_XZ: SetBuiltinMemberClass = 0x7f,
    /// Sets the X, W element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x30
    SET_M44_XW: SetBuiltinMemberClass = 0x80,
    /// Sets the Y, X element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x04
    SET_M44_YX: SetBuiltinMemberClass = 0x81,
    /// Sets the Y, Y element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x14
    SET_M44_YY: SetBuiltinMemberClass = 0x82,
    /// Sets the Y, Z element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x24
    SET_M44_YZ: SetBuiltinMemberClass = 0x83,
    /// Sets the Y, W element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x34
    SET_M44_YW: SetBuiltinMemberClass = 0x84,
    /// Sets the Z, X element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x08
    SET_M44_ZX: SetBuiltinMemberClass = 0x85,
    /// Sets the Z, Y element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x18
    SET_M44_ZY: SetBuiltinMemberClass = 0x86,
    /// Sets the Z, Z element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x28
    SET_M44_ZZ: SetBuiltinMemberClass = 0x87,
    /// Sets the Z, W element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x38
    SET_M44_ZW: SetBuiltinMemberClass = 0x88,
    /// Sets the W, X element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x0c
    SET_M44_WX: SetBuiltinMemberClass = 0x89,
    /// Sets the W, Y element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x1c
    SET_M44_WY: SetBuiltinMemberClass = 0x8a,
    /// Sets the W, Z element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x2c
    SET_M44_WZ: SetBuiltinMemberClass = 0x8b,
    /// Sets the W, W element of the 4x4 matrix, to the f32 stored in the source register
    ///
    /// The store offset is 0x3c
    SET_M44_WW: SetBuiltinMemberClass = 0x8c,
    /// Sets the X row of the 4x4 matrix to the v4 specified in the source register
    SET_M44_rX: SetBuiltinMemberClass = 0x8d,
    /// Sets the Y row of the 4x4 matrix to the v4 specified in the source register
    SET_M44_rY: SetBuiltinMemberClass = 0x8e,
    /// Sets the Z row of the 4x4 matrix to the v4 specified in the source register
    SET_M44_rZ: SetBuiltinMemberClass = 0x8f,
    /// Sets the W row of the 4x4 matrix to the v4 specified in the source register
    SET_M44_rW: SetBuiltinMemberClass = 0x90,
    /// Sets the X column of the 4x4 matrix to the v4 specified in the source register
    SET_M44_cX: SetBuiltinMemberClass = 0x91,
    /// Sets the Y column of the 4x4 matrix to the v4 specified in the source register
    SET_M44_cY: SetBuiltinMemberClass = 0x92,
    /// Sets the Z column of the 4x4 matrix to the v4 specified in the source register
    SET_M44_cZ: SetBuiltinMemberClass = 0x93,
    /// Sets the W column of the 4x4 matrix to the v4 specified in the source register
    SET_M44_cW: SetBuiltinMemberClass = 0x94,
    /// Gets the specified member of a safe_ptr thing, storing the result in the destination register
    ///
    /// The amount of data moved depends on the machine type of the instruction
    ///
    /// void       - No load takes place
    /// bool       - Loads one byte
    /// char       - Loads two bytes
    /// s32        - Loads four bytes
    /// f32        - Loads four bytes
    /// v4         - Loads 16 bytes
    /// m44        - Loads 64 bytes
    /// deprecated - No load takes place
    /// raw_ptr    - Loads 4 bytes
    /// ref_ptr    - No load takes place
    /// safe_ptr   - Loads 4 bytes
    /// object_ref - Loads 4 bytes
    /// s64        - Loads 8 bytes
    /// f64        - Loads 8 bytes
    GET_SP_MEMBER: GetMemberClass = 0x95,
    /// Gets the specified member of a raw_ptr, storing the result in the destination register
    ///
    /// Throws a script exception if the pointer is equal to NULL
    ///
    /// probably does a very similar thing to GET_SP_MEMBER,
    /// but idk it does some function pointer shenanigans and im too lazy to figure it out
    GET_RP_MEMBER: GetMemberClass = 0x96,
    /// Sets the specified member of a safe_ptr thing, loading the value from the source regsiter
    ///
    /// The amount of data moved depends on the machine type of the instruction
    ///
    /// void       - No store takes place
    /// bool       - Stores one byte
    /// char       - Stores two bytes
    /// s32        - Stores four bytes
    /// f32        - Stores four bytes
    /// v4         - Stores 16 bytes
    /// m44        - Stores 64 bytes
    /// deprecated - No store takes place
    /// raw_ptr    - Stores 4 bytes
    /// ref_ptr    - No store takes place
    /// safe_ptr   - Stores 4 bytes
    /// object_ref - Stores 4 bytes
    /// s64        - Stores 8 bytes
    /// f64        - Stores 8 bytes
    SET_SP_MEMBER: SetMemberClass = 0x97,
    /// Sets the specified member of a raw_ptr, loading the value from the source regsiter
    ///
    /// Throws a script exception if the pointer is equal to NULL
    ///
    /// probably does a very similar thing to SET_SP_MEMBER,
    /// but idk it does some function pointer shenanigans and im too lazy to figure it out
    SET_RP_MEMBER: SetMemberClass = 0x98,
    /// Gets the Nth element of an array
    ///
    /// The array pointer is pulled from the base_idx register,
    /// the index is pulled from the src_or_index_idx register,
    /// and the result is stored in the dst_idx register
    ///
    /// theres vtable shenanigans going on here so im not 100% sure,
    /// but im assuming data is moved identically to GET_SP_MEMBER based on the machine type
    GET_ELEMENT: GetElementClass = 0x99,
    /// Sets the Nth element of an array
    ///
    /// The array pointer is pulled from the base_idx register,
    /// the index is pulled from the index_idx register,
    /// and the value is loaded from src_idx
    ///
    /// theres vtable shenanigans going on here so im not 100% sure,
    /// but im assuming data is moved identically to GET_SP_MEMBER based on the machine type
    SET_ELEMENT: SetElementClass = 0x9a,
    /// Gets the length of the array stored in the base_idx register,
    /// storing the result as a 32-bit integer inside of the destination register
    GET_ARRAY_LEN: GetBuiltinMemberClass = 0x9b,
    /// Creates a new array with the specified type, with the size coming from the size_idx register,
    /// stores the result in the destination register
    NEW_ARRAY: NewArrayClass = 0x9c,
    /// TODO: 0x211e08
    ///
    /// Assumption: Inserts the element from the source register
    ///             into the index stored at the index_idx register
    ///             at the array stored at the base_idx register
    ///
    ///             Data is copied in the same way as SET_SP_MEMBER
    ARRAY_INSERT: SetElementClass = 0x9d,
    /// TODO: 0x2166e8
    ///
    /// Assumption: Appends the element from the source register to
    ///             the the end of the array stored at the base_idx register
    ///
    ///             Data is copied in the same way as SET_SP_MEMBER
    ARRAY_APPEND: SetElementClass = 0x9e,
    /// TODO: 0x2166a8
    ///
    /// Assumption: Removes the element index (stored at the index_idx register)
    ///             from the array stored at the base_idx register
    ARRAY_ERASE: SetElementClass = 0x9f,
    /// TODO: 0x216660
    ///
    /// Assumption: Searches for the element stored in the src_or_index_idx register
    ///             in the array stored in the base_idx register
    ///             storing the result in the destination register
    ARRAY_FIND: GetElementClass = 0xa0,
    /// TODO: 0x216634
    ///
    /// Assumption: Clears the array stored at the base_idx register, eg. making it have only 0 elements
    ARRAY_CLEAR: SetElementClass = 0xa1,
    /// Formats the object specified in the source index, and writes it out
    ///
    /// This only works on debug copies of the game.
    WRITE: WriteClass = 0xa2,
    /// Loads the specified register into the specified argument register
    ///
    /// The data loaded depends on the machine type of the instruction.
    ///
    /// If the destination register is a0 and the machine type is safe_ptr
    /// it does something idk look at 218c14 in ghidra in deploy
    /// its maybe a special "pointer to self" if I had to guess?
    /// Its set to the CThing GUID when preparing a non-static function,
    /// so its *probably* the self ptr or the attached GUID of the function if I had to guess.
    ///
    /// void       - No load takes place
    /// bool       - Loads one byte
    /// char       - Loads two bytes
    /// s32        - Loads four bytes
    /// f32        - Loads four bytes
    /// v4         - Loads sixteen bytes, rounding *down* the src and dst registers
    ///              to the nearest multiple of 16, due to Altivec requirements.
    /// m44        - Loads four packed v4 (total of 64 bytes), rounding *down* the src and dst registers
    ///              to the nearest multiple of 16, due to Altivec requirements.
    /// deprecated - No load takes place
    /// raw_ptr    - Loaded as s32
    /// ref_ptr    - No load takes place
    /// safe_ptr   - Loaded as s32
    /// object_ref - Loaded as s32
    /// s64        - Loads eight bytes
    /// f64        - Loads eight bytes
    ARG: ArgClass = 0xa3,
    /// Calls the function specified by call_idx, storing the return value inside of dst_idx
    CALL: CallClass = 0xa4,
    /// Returns from the function, with the return value being pulled from the source register
    RET: ReturnClass = 0xa5,
    /// Unconditional relative branch, with the offset specified
    B: BranchClass = 0xa6,
    /// Branches if the byte in the specified register is equal to zero
    BEZ: BranchClass = 0xa7,
    /// Branches if the byte in the specified register is not equal to zero
    BNEZ: BranchClass = 0xa8,
    /// Casts the object contained at the source register to the type specified in type_idx, storing the result in the destination register
    ///
    /// This will place a NULL pointer into the destination pointer if the source register is null, or if the cast is invalid.
    CASTsp: CastClass = 0xa9,
    /// Casts a boolean stored in the source register into a 32-bit integer, storing the result in the destination register
    INTb: UnaryClass = 0xaa,
    /// Casts an unsigned 16-bit integer stored in the source register into a 32-bit integer in the destination register
    INTc: UnaryClass = 0xab,
    /// Casts a 32-bit float stored in the source register into a 32-bit integer in the destination register
    INTf: UnaryClass = 0xac,
    /// Casts a boolean stored in the source register into a 32-bit float, storing the result in the destination register
    FLOATb: UnaryClass = 0xad,
    /// Casts an unsigned 16-bit integer stored in the source register into a 32-bit float stored in the destination register
    FLOATc: UnaryClass = 0xae,
    /// Casts a signed 32-bit integer stored in the source register into a 32-bit float stored in the destination register
    FLOATi: UnaryClass = 0xaf,
    /// Casts an unsigned 16-bit integer stored in the source register into a boolean stored in the destination register
    ///
    /// Any non-zero value is converted to 0x01
    BOOLc: UnaryClass = 0xb0,
    /// Casts a signed 32-bit integer stored in the source register into a boolean stored in the destination register
    ///
    /// Any non-zero value is converted to 0xFF
    ///
    /// TODO: is this correct?
    BOOLi: UnaryClass = 0xb1,
    /// Casts a 32-bit floating point number stored in the source register into a boolean stored in the destination register.
    ///
    /// This is equivalent to a standard float -> byte conversion
    BOOLf: UnaryClass = 0xb2,
    /// Loads the field specified in field_ref on the object specified in the base_idx register,
    /// and stores the result in the destination register
    ///
    /// Data is copied in the same way as GET_SP_MEMBER
    GET_OBJ_MEMBER: GetMemberClass = 0xb3,
    /// Stores the data from the source register into the field specified by field_ref
    /// on the object specified by the base_idx register
    ///
    /// Data is copied in the same way as SET_SP_MEMBER
    SET_OBJ_MEMBER: SetMemberClass = 0xb4,
    /// Creates a new object specified by type_idx, storing the result in the destination register
    NEW_OBJECT: NewObjectClass = 0xb5,
    /// Resizes the array stored in the base_idx register to the size specified in the index_idx register.
    ARRAY_RESIZE: SetElementClass = 0xb6,
    /// TODO: 0x2161bc
    ARRAY_RESERVE: SetElementClass = 0xb7,
    /// Loads a v4 const into the destination register,
    /// the constant is pulled from the f32 constant array at the specified idx, uses 4 elements starting from that index
    LCv4: LoadConstClass = 0xb8,
    /// Loads a null pointer into the destination register
    LC_NULLo: LoadConstClass = 0xb9,
    /// Casts the object from the source register into the specified type, storing the result in the destination register
    ///
    /// Stores zero into the destination register if the source pointer is null or the cast is invalid
    CASTo: CastClass = 0xba,
    /// TODO: 0x2160cc
    ///
    /// Gets the native member (specified in field_ref) of the Thing stored in the base_idx register,
    /// storing the result in the destination register
    ///
    /// This instruction throws a script exception if the part is invalid (whatever that means)
    /// or if the instructions machine type is not equal to raw_ptr
    GET_SP_NATIVE_MEMBER: GetMemberClass = 0xbb,
    /// Loads the AString (UTF-8/ASCII) specified in constant_idx into the destination register.
    LCsa: LoadConstClass = 0xbc,
    /// Does a bitwise OR on the booleans stored in the source registers,
    /// storing the result as a boolean in the destination register
    BIT_ORb: BinaryClass = 0xbd,
    /// Does a bitwise AND on the booleans stored in the source registers,
    /// storing the result as a boolean in the destination register
    BIT_ANDb: BinaryClass = 0xbe,
    /// Does a bitwise XOR on the booleans stored in the source registers,
    /// storing the result as a boolean in the destination register
    BIT_XORb: BinaryClass = 0xbf,
    /// Calls the specified method on the object, storing the result in the destination register
    ///
    /// Throws an exception if the object is NULL or the script object is invalid (TODO: does this just mean "if its a script or not"?)
    CALLVo: CallClass = 0xc0,
    /// Calls the specified method on the safe ptr, storing the result in the destination register
    ///
    /// Throws an exception if the safe ptr lookup returns a NULL Thing
    CALLVsp: CallClass = 0xc1,
    /// Would trigger a script assertion, but it seems that the code was compiled out at least in builds like 1/2/3 and even Deploy.
    ASSERT: WriteClass = 0xc2,

    // LBP2+

    /// Loads an s64 constant from the s64 constant array into the destination register
    ///
    /// Only available in revision 0x30c or higher
    LCs64: LoadConstClass = 0xc3,
    /// Moves an s64 from the source register into the destination register
    ///
    /// Only available in revision 0x30c or higher
    MOVs64: UnaryClass = 0xc4,
    /// Adds the two s64 values from the source registers into the destination register
    ///
    /// Only available in revision 0x30c or higher
    ADDs64: BinaryClass = 0xc5,
    /// Compares whether the two s64 values are equal,
    /// storing the result as a boolean in the destination register
    ///
    /// Only available in revision 0x30c or higher
    EQs64: BinaryClass = 0xc6,
    /// Compares whether the two s64 values are not equal,
    /// storing the result as a boolean in the destination register
    ///
    /// Only available in revision 0x30c or higher
    NEs64: BinaryClass = 0xc7,
    /// Does a bitwise OR on the two source registers, storing the result in the destination register
    ///
    /// Only available in revision 0x30c or higher
    BIT_ORs64: BinaryClass = 0xc8,
    /// Does a bitwise AND on the two source registers, storing the result in the destination register
    ///
    /// Only available in revision 0x30c or higher
    BIT_ANDs64: BinaryClass = 0xc9,
    /// Does a bitwise XOR on the two source registers, storing the result in the destination register
    ///
    /// Only available in revision 0x30c or higher
    BIT_XORs64: BinaryClass = 0xca,

    // Aidan's modified runtime only

    /// Gets the native address to the specified register, storing the result in the destination register
    ///
    /// Only available in Aidan's modified script runtime
    EXT_ADDRESS: UnaryClass = 0xcb,
    /// TODO:
    ///
    /// Assumption: Loads 4 bytes from the specified native address into the destination register
    ///
    /// Only available in Aidan's modified script runtime
    EXT_LOAD: UnaryClass = 0xcc,
    /// TODO:
    ///
    /// Assumption: Stores 4 bytes from the specified native address into the destination register
    ///
    /// Only available in Aidan's modified script runtime
    EXT_STORE: UnaryClass = 0xcd,
    /// Invokes a native method at the specified address with the TOC switch set, storing the result in the destination pointer,
    /// the amount of data moved into the destination register depends on the machine type of the instruction.
    /// HOWEVER: this is *not* implemented right now, and it always assumes an s32 return type.
    ///
    /// a0  -> a32 are integer values (8 registers, r3 -> r10)
    /// a32 -> a48 are float values (4 registers f1 -> f4)
    /// a48 is a single vec4 that gets stored in the v2 register
    ///
    /// Only available in Aidan's modified script runtime
    EXT_INVOKE: ExternalInvokeClass = 0xce,

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

pub const AStringTable = struct {
    buf: []const u8,
    strings: []const [:0]const u8,

    pub fn deinit(self: AStringTable, allocator: std.mem.Allocator) void {
        allocator.free(self.strings);
        allocator.free(self.buf);
    }
};

pub const WStringTable = struct {
    buf: []const u16,
    strings: []const [:0]const u16,

    pub fn deinit(self: WStringTable, allocator: std.mem.Allocator) void {
        allocator.free(self.strings);
        allocator.free(self.buf);
    }
};

const ResolvableTypeReference = u32;

const ResolvableString = u32;

const ResolvableFunction = u32;

const ArgumentSlice = struct {
    begin: u32,
    end: u32,

    pub inline fn len(self: ArgumentSlice) u32 {
        return self.end - self.begin;
    }

    pub fn slice(self: ArgumentSlice, source: []const Argument) []const Argument {
        return source[self.begin..self.end];
    }
};

const ResolvableBytecodeSlice = struct {
    begin: u32,
    end: u32,

    pub inline fn len(self: ResolvableBytecodeSlice) u32 {
        return self.end - self.begin;
    }

    pub fn slice(self: ResolvableBytecodeSlice, source: []const Bytecode) []const Bytecode {
        return source[self.begin..self.end];
    }
};

const ResolvableLineNumberSlice = struct {
    begin: u32,
    end: u32,

    pub inline fn len(self: ResolvableLineNumberSlice) u32 {
        return self.end - self.begin;
    }

    pub fn slice(self: ResolvableLineNumberSlice, source: []const u16) []const u16 {
        return source[self.begin..self.end];
    }
};

const ResolvableLocalVariableSlice = struct {
    begin: u32,
    end: u32,

    pub inline fn len(self: ResolvableLocalVariableSlice) u32 {
        return self.end - self.begin;
    }

    pub fn slice(self: ResolvableLocalVariableSlice, source: []const LocalVariable) []const LocalVariable {
        return source[self.begin..self.end];
    }
};

pub const FunctionDefinition = struct {
    modifiers: Modifiers,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    arguments: ArgumentSlice,
    bytecode: ResolvableBytecodeSlice,
    line_numbers: ResolvableLineNumberSlice,
    local_variables: ResolvableLocalVariableSlice,
    stack_size: u32,
};

pub const Function = struct {
    modifiers: Modifiers,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    arguments: []const Argument,
    bytecode: []const Bytecode,
    line_numbers: []const u16,
    local_variables: []const LocalVariable,
    stack_size: u32,

    pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
        allocator.free(self.arguments);
        allocator.free(self.bytecode);
        allocator.free(self.line_numbers);
        allocator.free(self.local_variables);
    }
};

pub const LocalVariable = struct {
    modifiers: Modifiers,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    offset: u32,
};

pub const Argument = struct {
    type_reference: ResolvableTypeReference,
    offset: u32,
};

pub const PropertyDefinition = struct {
    modifiers: Modifiers,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
    get_function: ResolvableFunction,
    set_function: ResolvableFunction,
};

pub const Modifiers = packed struct(u32) {
    static: bool = false,
    native: bool = false,
    ephemeral: bool = false,
    pinned: bool = false,
    @"const": bool = false,
    public: bool = false,
    protected: bool = false,
    private: bool = false,
    property: bool = false,
    abstract: bool = false,
    virtual: bool = false,
    override: bool = false,
    divergent: bool = false,
    _unused: u19 = undefined,

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var first = true;

        inline for (@typeInfo(Modifiers).Struct.fields) |field| {
            if (comptime std.mem.eql(u8, "_unused", field.name))
                continue;

            if (@field(value, field.name)) {
                if (!first)
                    try writer.writeByte(' ');

                try writer.print("{s}", .{field.name});
                first = false;
            }
        }
    }
};

pub const FieldDefinition = struct {
    modifiers: Modifiers,
    type_reference: ResolvableTypeReference,
    name: ResolvableString,
};

pub const FunctionReference = struct {
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

    pub fn eql(self: TypeReference, other: TypeReference) bool {
        if ((self.script == null and other.script != null) or (self.script != null and other.script == null))
            return false;

        if (self.script) |self_script|
            if (other.script) |other_script|
                if (!self_script.eql(other_script))
                    return false;

        return self.machine_type == other.machine_type and
            self.fish_type == other.fish_type and
            self.dimension_count == other.dimension_count and
            self.array_base_machine_type == other.array_base_machine_type and
            self.type_name == other.type_name;
    }
};

pub const Revision = struct {
    head: u32,
    branch_id: u16,
    branch_revision: u16,
};

pub const ResourceType = GeneratedTypes.ResourceType;
pub const headerToResourceType = GeneratedTypes.headerToResourceType;
pub const resourceTypeToHeader = GeneratedTypes.resourceTypeToHeader;

const GeneratedTypes = GenerateResourceType(.{
    .{ "invalid", null, 0 },
    .{ "texture", "TEX", 1 },
    // .{ "gtf_texture", "GTF", 1 },
    // .{ "gxt_texture", "GXT", 1 },
    .{ "mesh", "MSH", 2 },
    .{ "pixel_shader", null, 3 },
    .{ "vertex_shader", null, 4 },
    .{ "animation", "ANM", 5 },
    .{ "guid_substitution", "GSB", 6 },
    .{ "gfx_material", "GMT", 7 },
    .{ "spu_elf", null, 8 },
    .{ "level", "LVL", 9 },
    .{ "filename", null, 10 },
    .{ "script", "FSH", 11 },
    .{ "settings_character", "CHA", 12 },
    .{ "file_of_bytes", null, 13 },
    .{ "settings_soft_phys", "SSP", 14 },
    .{ "fontface", "FNT", 15 },
    .{ "material", "MAT", 16 },
    .{ "downloadable_content", "DLC", 17 },
    .{ "editor_settings", null, 18 },
    .{ "joint", "JNT", 19 },
    .{ "game_constants", "CON", 20 },
    .{ "poppet_settings", "POP", 21 },
    .{ "cached_level_data", "CLD", 22 },
    .{ "synced_profile", "PRF", 23 },
    .{ "bevel", "BEV", 24 },
    .{ "game", "GAM", 25 },
    .{ "settings_network", "NWS", 26 },
    .{ "packs", "PCK", 27 },
    .{ "big_profile", "BPR", 28 },
    .{ "slot_list", "SLT", 29 },
    .{ "translation", null, 30 },
    .{ "adventure_create_profile", "ADC", 31 },
    .{ "local_profile", "IPR", 32 },
    .{ "limits_settings", "LMT", 33 },
    .{ "tutorials", "TUT", 34 },
    .{ "guid_list", "GLT", 35 },
    .{ "audio_materials", "AUM", 36 },
    .{ "settings_fluid", "SSF", 37 },
    .{ "plan", "PLN", 38 },
    .{ "texture_list", "TXL", 39 },
    .{ "music_settings", "MUS", 40 },
    .{ "mixer_settings", "MIX", 41 },
    .{ "replay_config", "REP", 42 },
    .{ "palette", "PAL", 43 },
    .{ "static_mesh", "SMH", 44 },
    .{ "animated_texture", "ATX", 45 },
    .{ "voip_recording", "VOP", 46 },
    .{ "pins", "PIN", 47 },
    .{ "instrument", "INS", 48 },
    .{ "sample", null, 49 },
    .{ "outfit_list", "OFT", 50 },
    .{ "paint_brush", "PBR", 51 },
    .{ "thing_recording", "REC", 52 },
    .{ "painting", "PTG", 53 },
    .{ "quest", "QST", 54 },
    .{ "animation_bank", "ABK", 55 },
    .{ "animation_set", "AST", 56 },
    .{ "skeleton_map", "SMP", 57 },
    .{ "skeleton_registry", "SRG", 58 },
    .{ "skeleton_anim_styles", "SAS", 59 },
    .{ "crossplay_vita", null, 60 },
    .{ "streaming_chunk", "CHK", 61 },
    .{ "shared_adventure_data", "ADS", 62 },
    .{ "adventure_play_profile", "ADP", 63 },
    .{ "animation_map", "AMP", 64 },
    .{ "cached_costume_data", "CCD", 65 },
    .{ "data_labels", "DLA", 66 },
    .{ "adventure_maps", "ADM", 67 },
});

pub const SerializationMethod = enum(u8) {
    binary = 'b',
    text = 't',
    encrypted_binary = 'e',
    texture = ' ',
    gxt_simple = 's',
    gxt_extended = 'S',
};

fn GenerateResourceType(items: anytype) type {
    comptime {
        const enum_fields = blk: {
            var enum_fields: [items.len]std.builtin.Type.EnumField = undefined;

            for (items, &enum_fields) |item, *field| {
                const name = item[0];
                // const header = item[1];
                const value = item[2];

                field.* = .{
                    .name = name,
                    .value = value,
                };
            }

            break :blk enum_fields;
        };

        return struct {
            pub const ResourceType = @Type(std.builtin.Type{
                .Enum = .{
                    .tag_type = u8,
                    .decls = &.{},
                    .is_exhaustive = false,
                    .fields = &enum_fields,
                },
            });

            pub fn headerToResourceType(header: [3]u8) ?@This().ResourceType {
                inline for (items) |item| {
                    if (@TypeOf(item[1]) == @TypeOf(null)) continue;

                    const header_u24: u24 = @bitCast(header);
                    const item_header_u24: u24 = @bitCast(@as([3]u8, item[1].*));

                    if (item_header_u24 == header_u24) {
                        return @enumFromInt(item[2]);
                    }
                }

                return null;
            }

            pub fn resourceTypeToHeader(resource_type: @This().ResourceType) ?[3]u8 {
                inline for (items) |item| {
                    if (@intFromEnum(resource_type) == item[2]) {
                        if (@TypeOf(item[1]) == @TypeOf(null))
                            return null;

                        return item[1].*;
                    }
                }

                return null;
            }
        };
    }
}

pub const MachineType = enum(u8) {
    void = 0x0,
    /// A boolean, one byte in size.
    bool = 0x1,
    /// A character, two bytes in size, presumably as a single big endian UTF-16 codepoint
    char = 0x2,
    /// A signed 32-bit integer
    s32 = 0x3,
    /// A 32-bit floating point number
    f32 = 0x4,
    /// A four element f32 vector, sixteen bytes in size, in the order of XYZW
    v4 = 0x5,
    /// A 4x4 matrix of f32 elements, total of 64 bytes.
    /// This is in-memory equivalent to four v4 next to eachother.
    /// This is stored in column-major order
    m44 = 0x6,
    deprecated = 0x7,
    raw_ptr = 0x8,
    ref_ptr = 0x9,
    safe_ptr = 0xa,
    object_ref = 0xb,
    /// A signed 64-bit integer
    s64 = 0xc,
    /// A 64-bit floating point number
    f64 = 0xd,

    pub fn size(self: MachineType) u16 {
        return switch (self) {
            .void => 0,
            .bool => 1,
            .char => 2,
            .s32 => 4,
            .f32 => 4,
            .v4 => 4 * 4,
            .m44 => 4 * 4 * 4,
            .raw_ptr => 4,
            .ref_ptr => 4,
            .safe_ptr => 4,
            .object_ref => 4,
            .s64 => 8,
            .f64 => 8,
            .deprecated => unreachable,
        };
    }
};

/// Also known as BuiltInType
///
/// A special type builtin to the compiler, this seems to have no effect on runtime.
/// These types represent all the types builtin to the compiler,
/// which do not require another file/script to be imported.
/// When the field/parameter type is a non-builtin type, this type is set to `void`
pub const FishType = enum(u8) {
    void = 0x0,
    bool = 0x1,
    char = 0x2,
    s32 = 0x3,
    f32 = 0x4,
    vec2 = 0x5,
    vec3 = 0x6,
    vec4 = 0x7,
    m44 = 0x8,
    guid = 0x9,
    s64 = 0xa,
    f64 = 0xb,

    pub fn fromMangledId(id: u8) FishType {
        return switch (id) {
            'v' => .void,
            'b' => .bool,
            'w' => .char,
            'i' => .s32,
            'f' => .f32,
            'p' => .vec2,
            'q' => .vec3,
            'r' => .vec4,
            'm' => .m44,
            'g' => .guid,
            'j' => .s64,
            'd' => .f64,
            else => std.debug.panic("Unknown mangling ID {c}", .{id}),
        };
    }

    pub fn toMangledId(fish_type: FishType) u8 {
        return switch (fish_type) {
            .void => 'v',
            .bool => 'b',
            .char => 'w',
            .s32 => 'i',
            .f32 => 'f',
            .vec2 => 'p',
            .vec3 => 'q',
            .vec4 => 'r',
            .m44 => 'm',
            .guid => 'g',
            .s64 => 'j',
            .f64 => 'd',
        };
    }

    pub fn guessFromMachineType(machine_type: MachineType) FishType {
        return switch (machine_type) {
            .void => .void,
            .bool => .bool,
            .char => .char,
            .s32 => .s32,
            .f32 => .f32,
            .v4 => .vec4,
            .m44 => .m44,
            .deprecated => .void,
            .raw_ptr => .void,
            .ref_ptr => .void,
            .safe_ptr => .void,
            .object_ref => .void,
            .s64 => .s64,
            .f64 => .f64,
        };
    }

    pub fn toMachineType(self: FishType) MachineType {
        return switch (self) {
            .void => .void,
            .bool => .bool,
            .char => .char,
            .s32 => .s32,
            .f32 => .f32,
            .vec2 => .v4,
            .vec3 => .v4,
            .vec4 => .v4,
            .m44 => .m44,
            .guid => .s32,
            .s64 => .s64,
            .f64 => .s64,
        };
    }
};

pub const ResourceIdentifier = union(enum(u8)) {
    guid: u32 = 2,
    hash: [std.crypto.hash.Sha1.digest_length]u8 = 1,

    pub fn eql(self: ResourceIdentifier, other: ResourceIdentifier) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other))
            return false;

        return switch (self) {
            .guid => self.guid == other.guid,
            .hash => std.mem.eql(u8, &self.hash, &other.hash),
        };
    }
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

pub const FileDB = struct {
    pub const HashLookupMap = std.AutoHashMap([std.crypto.hash.Sha1.digest_length]u8, FileDB.Entry);
    pub const GuidLookupMap = std.AutoHashMap(u32, FileDB.Entry);

    allocator: std.heap.ArenaAllocator,
    hash_lookup: HashLookupMap,
    guid_lookup: GuidLookupMap,

    pub const Type = enum {
        pre_lbp3,
        lbp3,
        vita,
        unknown,
    };

    pub const Entry = struct {
        path: []const u8,
        timestamp: i32,
        size: u32,
    };

    pub fn deinit(self: FileDB) void {
        self.allocator.deinit();
    }
};
