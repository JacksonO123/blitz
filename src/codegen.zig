const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const TempRegister = vmInfo.TempRegister;
const Writer = std.Io.Writer;
const Context = blitz.context.Context;
const MemoryPool = std.heap.MemoryPool;

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
    ExpectedLoopInfo,
    StackFrameSizeTooLarge,
    RegInteractionNotSupported,
    NoTrivialRegister,
    AccessTargetDoesNotHaveStructName,
    LabelDoesNotExist,
};
const GenBytecodeError = CodeGenError ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    ast.AstTypeError;

const LoopCondInfo = struct {
    prevCmpAsReg: bool,
    isCompExpr: bool,
};

const OpSizes = enum {
    U8,
    U16,
    U32,
    U64,
};

const sizeToOpSizeTuple = .{
    .{ std.math.maxInt(u8), OpSizes.U8 },
    .{ std.math.maxInt(u16), OpSizes.U16 },
    .{ std.math.maxInt(u32), OpSizes.U32 },
    .{ std.math.maxInt(u64), OpSizes.U64 },
};

pub const InstructionVariants = enum(u8) {
    const Self = @This();

    NoOp, // OB (not in output)
    Label, // 0B (not in output)

    SetReg64, // inst, reg, 8B data
    SetReg32, // inst, reg, 4B data
    SetReg16, // inst, reg, 2B data
    SetReg8, // inst, reg, 1B data

    Add, // inst, out reg, reg1, reg2
    Add8, // inst, out reg, reg1, 1B data
    Add16, // inst, out reg, reg1, 2B data
    Sub, // inst, out reg, reg1, reg2
    Sub8, // inst, out reg, reg1, 1B data
    Sub16, // inst, out reg, reg1, 2B data
    Mult, // inst, out reg, reg1, reg2

    Jump, // inst, 4B data
    JumpEQ, // inst, 4B data
    JumpNE, // inst, 4B data
    JumpGT, // inst, 4B data
    JumpLT, // inst, 4B data
    JumpGTE, // inst, 4B data
    JumpLTE, // inst, 4B data
    JumpBack, // inst, 4B data
    JumpBackEQ, // inst, 4B data
    JumpBackNE, // inst, 4B data
    JumpBackGT, // inst, 4B data
    JumpBackLT, // inst, 4B data
    JumpBackGTE, // inst, 4B data
    JumpBackLTE, // inst, 4B data

    Cmp, // inst, reg1, reg2  ;  sets to flags
    CmpSetRegEQ, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegNE, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegGT, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegLT, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegGTE, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegLTE, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpConst8, // inst, reg1, 1B data

    IncConst8, // inst, in/out reg, 1B data
    DecConst8, // inst, in/out reg, 1B data

    Mov, // inst, reg1, reg2
    MovSpNegOffsetAny, // inst, dest, offset (TBD by compiler)B
    MovSpNegOffset16, // inst, dest, offset 2B
    MovSpNegOffset32, // inst, dest, offset 4B
    MovSpNegOffset64, // inst, dest, offset 8B

    Xor, // inst, out reg, reg1, reg2
    XorConst8, // inst, out reg, reg1, 1B data

    AddSp8, // inst, 2B data
    SubSp8, // inst, 2B data
    AddSp16, // inst, 2B data
    SubSp16, // inst, 2B data
    AddSp32, // inst, 4B data
    SubSp32, // inst, 4B data
    AddSp64, // inst, 8B data
    SubSp64, // inst, 8B data

    Store64AtReg, // inst, reg, to reg (ptr)
    Store32AtReg, // inst, reg, to reg (ptr)
    Store16AtReg, // inst, reg, to reg (ptr)
    Store8AtReg, // inst, reg, to reg (ptr)

    Store64AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store32AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store16AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store8AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B

    Store64AtSpNegOffset16, // inst, reg, offset 2B
    Store32AtSpNegOffset16, // inst, reg, offset 2B
    Store16AtSpNegOffset16, // inst, reg, offset 2B
    Store8AtSpNegOffset16, // inst, reg, offset 2B

    Load64AtRegOffset16, // inst, dest reg, from reg (ptr), offset 2B
    Load32AtRegOffset16, // inst, dest reg, from reg (ptr), offset 2B
    Load16AtRegOffset16, // inst, dest reg, from reg (ptr), offset 2B
    Load8AtRegOffset16, // inst, dest reg, from reg (ptr), offset 2B

    Load64AtReg, // inst, dest reg, from reg (ptr)
    Load32AtReg, // inst, dest reg, from reg (ptr)
    Load16AtReg, // inst, dest reg, from reg (ptr)
    Load8AtReg, // inst, dest reg, from reg (ptr)

    MulReg16AddReg, // inst, dest, addReg, mulReg, data 2B ( dest = addReg + (mulReg1 * data) )

    DbgReg, // inst, reg

    BitAnd, // inst, dest reg, reg1, reg2
    BitOr, // inst, dest reg, reg1, reg2

    And, // inst, reg1, reg2  ;  sets to flags
    Or, // inst, reg1, reg2  ;  sets to flags

    AndSetReg, // inst, dest reg, reg1, reg2
    OrSetReg, // inst, dest reg, reg1, reg2

    PrePushRegNegOffsetAny, // inst, top reg, offset (TBD by compiler)B
    PrePushRegNegOffset8, // inst, top reg, offset 1B
    PrePushRegNegOffset16, // inst, top reg, offset 2B
    PrePushRegNegOffset32, // inst, top reg, offset 4B
    PrePushRegNegOffset64, // inst, top reg, offset 8B

    PostPopRegNegOffsetAny, // inst, top reg, offset (TBD by compiler)B
    PostPopRegNegOffset8, // inst, top reg, offset 1B
    PostPopRegNegOffset16, // inst, top reg, offset 2B
    PostPopRegNegOffset32, // inst, top reg, offset 4B
    PostPopRegNegOffset64, // inst, top reg, offset 8B

    Ret,

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn getInstrLen(self: Self) u8 {
        return switch (self) {
            .NoOp => 0,
            .Label => 0,

            .SetReg64 => 10,
            .SetReg32 => 6,
            .SetReg16 => 4,
            .SetReg8 => 3,

            .Add, .Sub, .Mult, .Add8, .Sub8 => 4,
            .Add16, .Sub16 => 5,

            .Jump,
            .JumpEQ,
            .JumpNE,
            .JumpGT,
            .JumpLT,
            .JumpGTE,
            .JumpLTE,
            .JumpBack,
            .JumpBackEQ,
            .JumpBackNE,
            .JumpBackGT,
            .JumpBackLT,
            .JumpBackGTE,
            .JumpBackLTE,
            => 5,

            .Cmp => 3,
            .CmpSetRegEQ,
            .CmpSetRegNE,
            .CmpSetRegGT,
            .CmpSetRegLT,
            .CmpSetRegGTE,
            .CmpSetRegLTE,
            => 4,

            .CmpConst8 => 3,

            .IncConst8, .DecConst8 => 3,

            .Mov => 3,
            .MovSpNegOffsetAny => unreachable, // SHOULD NOT EXIST FOR WRITER
            .MovSpNegOffset16 => 4,
            .MovSpNegOffset32 => 6,
            .MovSpNegOffset64 => 10,

            .Xor, .XorConst8 => 4,

            .AddSp8, .SubSp8 => 2,
            .AddSp16, .SubSp16 => 3,
            .AddSp32, .SubSp32 => 5,
            .AddSp64, .SubSp64 => 9,

            .Store64AtReg,
            .Store32AtReg,
            .Store16AtReg,
            .Store8AtReg,
            => 3,

            .Store64AtRegPostInc16,
            .Store32AtRegPostInc16,
            .Store16AtRegPostInc16,
            .Store8AtRegPostInc16,
            => 5,

            .Store64AtSpNegOffset16,
            .Store32AtSpNegOffset16,
            .Store16AtSpNegOffset16,
            .Store8AtSpNegOffset16,
            => 4,

            .Load64AtReg,
            .Load32AtReg,
            .Load16AtReg,
            .Load8AtReg,
            => 3,

            .Load64AtRegOffset16,
            .Load32AtRegOffset16,
            .Load16AtRegOffset16,
            .Load8AtRegOffset16,
            => 5,

            .MulReg16AddReg => 6,

            .DbgReg => 2,

            .BitAnd, .BitOr => 4,

            .And, .Or => 3,
            .AndSetReg, .OrSetReg => 4,

            .PrePushRegNegOffsetAny, .PostPopRegNegOffsetAny => unreachable, // SHOULD NOT EXIST FOR WRITER
            .PrePushRegNegOffset8, .PostPopRegNegOffset8 => 3,
            .PrePushRegNegOffset16, .PostPopRegNegOffset16 => 4,
            .PrePushRegNegOffset32, .PostPopRegNegOffset32 => 6,
            .PrePushRegNegOffset64, .PostPopRegNegOffset64 => 10,

            .Ret => 1,
        };
    }

    pub fn maxInstrSize() u8 {
        var max: u8 = 0;
        inline for (@typeInfo(Self).@"enum".fields) |field| {
            const val: Self = @enumFromInt(field.value);

            // place holders, not actual instructions
            switch (val) {
                .MovSpNegOffsetAny,
                .PrePushRegNegOffsetAny,
                .PostPopRegNegOffsetAny,
                => continue,
                else => {},
            }

            max = @max(val.getInstrLen(), max);
        }
        return max;
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .NoOp => "noop",
            .Label => "(label)",

            .SetReg64 => "set_reg_64",
            .SetReg32 => "set_reg_32",
            .SetReg16 => "set_reg_16",
            .SetReg8 => "set_reg_8",

            .Add => "add",
            .Sub => "sub",
            .Mult => "mult",
            .Add8 => "add_8",
            .Sub8 => "sub_8",
            .Add16 => "add_16",
            .Sub16 => "sub_16",

            .Jump => "jump",
            .JumpEQ => "jump_eq",
            .JumpNE => "jump_ne",
            .JumpGT => "jump_gt",
            .JumpLT => "jump_lt",
            .JumpGTE => "jump_gte",
            .JumpLTE => "jump_lte",
            .JumpBack => "jump_back",
            .JumpBackEQ => "jump_back_eq",
            .JumpBackNE => "jump_back_ne",
            .JumpBackGT => "jump_back_gt",
            .JumpBackLT => "jump_back_lt",
            .JumpBackGTE => "jump_back_gte",
            .JumpBackLTE => "jump_back_lte",

            .CmpConst8 => "cmp_const_8",
            .Cmp => "cmp",
            .CmpSetRegEQ => "cmp_set_reg_eq",
            .CmpSetRegNE => "cmp_set_reg_ne",
            .CmpSetRegGT => "cmp_set_reg_gt",
            .CmpSetRegLT => "cmp_set_reg_lt",
            .CmpSetRegGTE => "cmp_set_reg_gte",
            .CmpSetRegLTE => "cmp_set_reg_lte",

            .IncConst8 => "inc_const_8",
            .DecConst8 => "dec_const_8",

            .Mov => "mov",
            .MovSpNegOffsetAny => "mov_sp_neg_offset_ANY",
            .MovSpNegOffset16 => "mov_sp_neg_offset_16",
            .MovSpNegOffset32 => "mov_sp_neg_offset_32",
            .MovSpNegOffset64 => "mov_sp_neg_offset_64",

            .Xor => "xor",
            .XorConst8 => "xor_const_8",

            .AddSp8 => "add_sp_8",
            .SubSp8 => "sub_sp_8",
            .AddSp16 => "add_sp_16",
            .SubSp16 => "sub_sp_16",
            .AddSp32 => "add_sp_32",
            .SubSp32 => "sub_sp_32",
            .AddSp64 => "add_sp_64",
            .SubSp64 => "sub_sp_64",

            .Store64AtReg => "store_64_at_reg",
            .Store32AtReg => "store_32_at_reg",
            .Store16AtReg => "store_16_at_reg",
            .Store8AtReg => "store_8_at_reg",

            .Store64AtRegPostInc16 => "store_64_at_reg_post_inc_16",
            .Store32AtRegPostInc16 => "store_32_at_reg_post_inc_16",
            .Store16AtRegPostInc16 => "store_16_at_reg_post_inc_16",
            .Store8AtRegPostInc16 => "store_8_at_reg_post_inc_16",

            .Store64AtSpNegOffset16 => "store_64_at_sp_neg_offset_16",
            .Store32AtSpNegOffset16 => "store_32_at_sp_neg_offset_16",
            .Store16AtSpNegOffset16 => "store_16_at_sp_neg_offset_16",
            .Store8AtSpNegOffset16 => "store_8_at_sp_neg_offset_16",

            .Load64AtReg => "load_64_at_reg",
            .Load32AtReg => "load_32_at_reg",
            .Load16AtReg => "load_16_at_reg",
            .Load8AtReg => "load_8_at_reg",

            .Load64AtRegOffset16 => "load_64_at_reg_offset_16",
            .Load32AtRegOffset16 => "load_32_at_reg_offset_16",
            .Load16AtRegOffset16 => "load_16_at_reg_offset_16",
            .Load8AtRegOffset16 => "load_8_at_reg_offset_16",

            .MulReg16AddReg => "mul_reg_16_add_reg",

            .DbgReg => "dbg_reg",

            .BitAnd => "bit_and",
            .BitOr => "bit_or",

            .And => "and",
            .Or => "or",

            .AndSetReg => "and_set_reg",
            .OrSetReg => "or_set_reg",

            .PrePushRegNegOffsetAny => "push_reg_neg_offset_ANY",
            .PrePushRegNegOffset8 => "push_reg_neg_offset_8",
            .PrePushRegNegOffset16 => "push_reg_neg_offset_16",
            .PrePushRegNegOffset32 => "push_reg_neg_offset_32",
            .PrePushRegNegOffset64 => "push_reg_neg_offset_64",

            .PostPopRegNegOffsetAny => "pop_reg_neg_offset_ANY",
            .PostPopRegNegOffset8 => "pop_reg_neg_offset_8",
            .PostPopRegNegOffset16 => "pop_reg_neg_offset_16",
            .PostPopRegNegOffset32 => "pop_reg_neg_offset_32",
            .PostPopRegNegOffset64 => "pop_reg_neg_offset_64",

            .Ret => "ret",
        };
    }
};

const TwoOpResultInstr = struct {
    dest: TempRegister = 0,
    reg1: TempRegister = 0,
    reg2: TempRegister = 0,
};

fn OneOpResultInstr(comptime T: type) type {
    return struct {
        dest: TempRegister = 0,
        reg: TempRegister = 0,
        data: T,
    };
}

const RegBytePayloadInstr = struct {
    reg: TempRegister = 0,
    data: u8 = 0,
};

const MathInstr = TwoOpResultInstr;

const SpInstr = struct {
    amount: TempRegister = 0,
};

const SpRegInstr = struct {
    reg: TempRegister = 0,
};

fn SetRegInstr(comptime T: type) type {
    return struct {
        reg: TempRegister = 0,
        data: T = 0,
    };
}

const CmpInstr = struct {
    reg1: TempRegister = 0,
    reg2: TempRegister = 0,
};

const CmpSetRegInstr = TwoOpResultInstr;

fn StoreOffsetInstr(comptime T: type) type {
    return struct {
        fromReg: TempRegister = 0,
        toRegPtr: TempRegister = 0,
        offset: T = 0,
    };
}

fn StoreOffsetSpInstr(comptime T: type) type {
    return struct {
        reg: TempRegister = 0,
        offset: T = 0,
    };
}

fn StoreAtRegIncInstr(comptime T: type) type {
    return struct {
        fromReg: TempRegister = 0,
        toRegPtr: TempRegister = 0,
        inc: T = 0,
    };
}

const StoreAtRegInstr = struct {
    fromReg: TempRegister = 0,
    toRegPtr: TempRegister = 0,
};

fn StoreIncSpInstr(comptime T: type) type {
    return struct {
        reg: TempRegister = 0,
        inc: T = 0,
    };
}

const LoadAtReg = struct {
    dest: TempRegister = 0,
    fromRegPtr: TempRegister = 0,
};

const LoadAtRegOffset16 = struct {
    dest: TempRegister = 0,
    fromRegPtr: TempRegister = 0,
    offset: u16 = 0,
};

fn MulRegTAddReg(comptime T: type) type {
    return struct {
        dest: TempRegister = 0,
        addReg: TempRegister = 0,
        mulReg: TempRegister = 0,
        data: T,
    };
}

fn MovSpNegOffset(comptime T: type) type {
    return struct {
        reg: TempRegister = 0,
        offset: T = 0,
    };
}

fn PushOrPopRegNegOffset(comptime T: type) type {
    return struct {
        reg: TempRegister,
        offset: T = 0,
    };
}

pub const Instr = union(InstructionVariants) {
    const Self = @This();

    NoOp: void,
    Label: vmInfo.LabelType,

    SetReg64: SetRegInstr(u64),
    SetReg32: SetRegInstr(u32),
    SetReg16: SetRegInstr(u16),
    SetReg8: SetRegInstr(u8),

    Add: MathInstr,
    Add8: OneOpResultInstr(u8),
    Add16: OneOpResultInstr(u16),
    Sub: MathInstr,
    Sub8: OneOpResultInstr(u8),
    Sub16: OneOpResultInstr(u16),
    Mult: MathInstr,

    // must be u64 because stores absolute byte position
    Jump: u64,
    JumpEQ: u64,
    JumpNE: u64,
    JumpGT: u64,
    JumpLT: u64,
    JumpGTE: u64,
    JumpLTE: u64,
    JumpBack: u64,
    JumpBackEQ: u64,
    JumpBackNE: u64,
    JumpBackGT: u64,
    JumpBackLT: u64,
    JumpBackGTE: u64,
    JumpBackLTE: u64,

    Cmp: CmpInstr,
    CmpSetRegEQ: CmpSetRegInstr,
    CmpSetRegNE: CmpSetRegInstr,
    CmpSetRegGT: CmpSetRegInstr,
    CmpSetRegLT: CmpSetRegInstr,
    CmpSetRegGTE: CmpSetRegInstr,
    CmpSetRegLTE: CmpSetRegInstr,
    CmpConst8: RegBytePayloadInstr,

    IncConst8: RegBytePayloadInstr,
    DecConst8: RegBytePayloadInstr,

    Mov: struct {
        dest: TempRegister = 0,
        src: TempRegister = 0,
    },
    MovSpNegOffsetAny: MovSpNegOffset(u64),
    MovSpNegOffset16: MovSpNegOffset(u16),
    MovSpNegOffset32: MovSpNegOffset(u32),
    MovSpNegOffset64: MovSpNegOffset(u64),

    Xor: TwoOpResultInstr,
    XorConst8: struct {
        dest: TempRegister = 0,
        reg: TempRegister = 0,
        byte: u8 = 0,
    },

    AddSp8: u8,
    SubSp8: u8,
    AddSp16: u16,
    SubSp16: u16,
    AddSp32: u32,
    SubSp32: u32,
    AddSp64: u64,
    SubSp64: u64,

    Store64AtReg: StoreAtRegInstr,
    Store32AtReg: StoreAtRegInstr,
    Store16AtReg: StoreAtRegInstr,
    Store8AtReg: StoreAtRegInstr,

    Store64AtRegPostInc16: StoreAtRegIncInstr(u16),
    Store32AtRegPostInc16: StoreAtRegIncInstr(u16),
    Store16AtRegPostInc16: StoreAtRegIncInstr(u16),
    Store8AtRegPostInc16: StoreAtRegIncInstr(u16),

    Store64AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store32AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store16AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store8AtSpNegOffset16: StoreOffsetSpInstr(u16),

    Load64AtRegOffset16: LoadAtRegOffset16,
    Load32AtRegOffset16: LoadAtRegOffset16,
    Load16AtRegOffset16: LoadAtRegOffset16,
    Load8AtRegOffset16: LoadAtRegOffset16,

    Load64AtReg: LoadAtReg,
    Load32AtReg: LoadAtReg,
    Load16AtReg: LoadAtReg,
    Load8AtReg: LoadAtReg,

    MulReg16AddReg: MulRegTAddReg(u16),

    DbgReg: TempRegister,

    BitAnd: TwoOpResultInstr,
    BitOr: TwoOpResultInstr,

    And: CmpInstr,
    Or: CmpInstr,

    AndSetReg: TwoOpResultInstr,
    OrSetReg: TwoOpResultInstr,

    PrePushRegNegOffsetAny: PushOrPopRegNegOffset(u64),
    PrePushRegNegOffset8: PushOrPopRegNegOffset(u8),
    PrePushRegNegOffset16: PushOrPopRegNegOffset(u16),
    PrePushRegNegOffset32: PushOrPopRegNegOffset(u32),
    PrePushRegNegOffset64: PushOrPopRegNegOffset(u64),

    PostPopRegNegOffsetAny: PushOrPopRegNegOffset(u64),
    PostPopRegNegOffset8: PushOrPopRegNegOffset(u8),
    PostPopRegNegOffset16: PushOrPopRegNegOffset(u16),
    PostPopRegNegOffset32: PushOrPopRegNegOffset(u32),
    PostPopRegNegOffset64: PushOrPopRegNegOffset(u64),

    Ret: void,

    pub fn getInstrLen(self: Self) u8 {
        const active = std.meta.activeTag(self);
        return active.getInstrLen();
    }

    pub fn toString(self: Self) []const u8 {
        const active = std.meta.activeTag(self);
        return active.toString();
    }

    pub fn getInstrByte(self: Self) u8 {
        const active = std.meta.activeTag(self);
        return active.getInstrByte();
    }

    pub fn maxInstrSize() u8 {
        return InstructionVariants.maxInstrSize();
    }
};

const SliceBytecodeInfo = struct {
    sliceLocation: u64,
    reg: TempRegister,
};

const InstrInfo = struct {
    chunk: *Instr,
    label: vmInfo.LabelType,
};

const LoopInfo = struct {
    const Self = @This();

    continueLabel: vmInfo.LabelType,
    breaks: *ArrayList(InstrInfo),
    continues: *ArrayList(InstrInfo),

    pub fn init(allocator: Allocator) !Self {
        const breaksPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);
        const continuesPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);

        return .{
            .continueLabel = 0,
            .breaks = breaksPtr,
            .continues = continuesPtr,
        };
    }

    pub fn appendBreak(
        self: *Self,
        allocator: Allocator,
        instr: *Instr,
        label: vmInfo.LabelType,
    ) !void {
        try self.breaks.append(allocator, .{
            .chunk = instr,
            .label = label,
        });
    }

    pub fn appendContinue(
        self: *Self,
        allocator: Allocator,
        instr: *Instr,
        label: vmInfo.LabelType,
    ) !void {
        try self.continues.append(allocator, .{
            .chunk = instr,
            .label = label,
        });
    }

    pub fn setContinueLabel(self: *Self, label: vmInfo.LabelType) void {
        self.continueLabel = label;
    }
};

const RegInfo = struct {
    varInfo: ?*VarGenInfo = null,
    active: bool = false,
};

const GenInfoSettings = struct {
    // respected for one expr node, then set to default
    outputCmpAsRegister: bool = true,

    propertyAccessReturnsPointer: bool = false,
};

const VarGenInfo = struct {
    stackLocation: ?u64 = null,
    prevInfo: ?*VarGenInfo = null,
};

const VAR_GEN_INFO_POOL_SIZE = 1024;
const VarGenInfoPool = MemoryPool(VarGenInfo);

const LabelByteInfo = struct {
    const Self = @This();
    const WaitingLabels = std.AutoHashMap(vmInfo.LabelType, *ArrayList(*u64));

    labelBytes: []u64,
    labelExists: []bool,
    waitingLabels: *WaitingLabels,

    pub fn init(allocator: Allocator) !Self {
        const waitingLabels = WaitingLabels.init(allocator);
        const waitingLabelsPtr = try utils.createMut(WaitingLabels, allocator, waitingLabels);

        return .{
            .labelBytes = &[_]u64{},
            .labelExists = &[_]bool{},
            .waitingLabels = waitingLabelsPtr,
        };
    }

    pub fn reserveLabelCount(
        self: *Self,
        allocator: Allocator,
        lastLabel: vmInfo.LabelType,
    ) !void {
        // if label id is 0, no labels have been used
        if (lastLabel == 0) return;

        self.labelBytes = try allocator.alloc(u64, lastLabel);
        self.labelExists = try allocator.alloc(bool, lastLabel);
        @memset(self.labelExists, false);
    }

    pub fn setLabelLocation(
        self: *Self,
        label: vmInfo.LabelType,
        location: u64,
    ) void {
        self.labelExists[label] = true;
        self.labelBytes[label] = location;

        if (self.waitingLabels.get(label)) |arrList| {
            for (arrList.items) |ptr| {
                const newVal: u32 = @intCast(location - ptr.*);
                ptr.* = newVal;
            }

            _ = self.waitingLabels.remove(label);
        }
    }

    pub fn getLabelLocation(self: *Self, labelId: vmInfo.LabelType) !?u64 {
        if (labelId >= self.labelExists.len) {
            return CodeGenError.LabelDoesNotExist;
        }

        if (!self.labelExists[labelId]) return null;
        return self.labelBytes[labelId];
    }

    /// labelPtr must point to the label to watch for, and be the memory
    /// location to write the label byte location to
    pub fn waitForLabel(
        self: *Self,
        allocator: Allocator,
        labelId: vmInfo.LabelType,
        labelPtr: *u64,
    ) !void {
        if (self.waitingLabels.get(labelId)) |arrList| {
            try arrList.append(allocator, labelPtr);
        } else {
            const arrList = try utils.createMut(ArrayList(*u64), allocator, .empty);
            try arrList.append(allocator, labelPtr);
            try self.waitingLabels.put(labelId, arrList);
        }
    }
};

pub const WriteLocInfo = struct {
    reg: TempRegister,
    value: u64,
};

pub const Proc = struct {
    stackFrameSize: u64 = 0,
    startIndex: usize,
    maxPreserveReg: ?TempRegister = null,
};

pub const GenInfo = struct {
    const Self = @This();

    instrList: *ArrayList(Instr),
    currentProc: Proc,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    varGenInfoPool: *VarGenInfoPool,
    varNameToReg: *StringHashMap(TempRegister),
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),
    byteCounter: u64,
    currentLabelId: vmInfo.LabelType,
    registers: *ArrayList(?*VarGenInfo),
    labelByteInfo: *LabelByteInfo,

    pub fn init(allocator: Allocator) !Self {
        const varNameReg = try utils.initMutPtrT(StringHashMap(TempRegister), allocator);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, .empty);

        const tempPool = try VarGenInfoPool.initPreheated(allocator, VAR_GEN_INFO_POOL_SIZE);
        const varGenInfoPool = try utils.createMut(VarGenInfoPool, allocator, tempPool);

        const labelByteInfo = try LabelByteInfo.init(allocator);
        const labelByteInfoPtr = try utils.createMut(LabelByteInfo, allocator, labelByteInfo);

        const instrListPtr = try utils.createMut(ArrayList(Instr), allocator, .empty);

        const registersPtr = try utils.createMut(ArrayList(?*VarGenInfo), allocator, .empty);

        return .{
            .instrList = instrListPtr,
            .currentProc = .{
                .startIndex = 0,
            },
            .vmInfo = .{
                .stackStartSize = 0,
                .version = 0,
            },
            .varGenInfoPool = varGenInfoPool,
            .varNameToReg = varNameReg,
            .byteCounter = 0,
            .settings = .{},
            .loopInfo = loopInfoPtr,
            .currentLabelId = 0,
            .registers = registersPtr,
            .labelByteInfo = labelByteInfoPtr,
        };
    }

    pub fn writeChunks(self: *Self, writer: *Writer) !void {
        try writer.writeByte(self.vmInfo.version);
        var buf: [vmInfo.START_STACK_TYPE_SIZE]u8 = undefined;
        std.mem.writeInt(vmInfo.StartStackType, &buf, self.vmInfo.stackStartSize, .little);
        try writer.writeAll(&buf);

        for (self.instrList.items) |instr| {
            try writeChunk(instr, writer);
        }
    }

    pub fn appendChunk(self: *Self, allocator: Allocator, instr: Instr) !void {
        _ = try self.appendChunkAndReturn(allocator, instr);
    }

    pub fn appendChunkAndReturn(self: *Self, allocator: Allocator, instr: Instr) !*Instr {
        try self.instrList.append(allocator, instr);
        return &self.instrList.items[self.instrList.items.len - 1];
    }

    pub fn takeLabelId(self: *Self) vmInfo.LabelType {
        const res = self.currentLabelId;
        self.currentLabelId += 1;
        return res;
    }

    pub fn getNextRegister(self: *Self, allocator: Allocator) !TempRegister {
        try self.registers.append(allocator, null);
        return @intCast(self.registers.items.len - 1);
    }

    pub fn getVariableRegister(self: Self, name: []const u8) TempRegister {
        return self.varNameToReg.get(name).?;
    }

    pub fn setVariableRegister(
        self: *Self,
        name: []const u8,
        reg: TempRegister,
    ) !void {
        var varGenInfo = try self.varGenInfoPool.create();
        varGenInfo.* = .{};

        try self.varNameToReg.put(name, reg);
        if (self.registers.items[reg]) |*varInfo| {
            varGenInfo.prevInfo = varInfo.*;
            varInfo.* = varGenInfo;
        }
    }

    // deactivates register and removes variable info
    pub fn removeVariableRegister(self: *Self, name: []const u8) void {
        if (self.varNameToReg.get(name)) |reg| {
            const varInfo = self.registers.items[reg];

            if (varInfo) |info| {
                const prevInfo = info.prevInfo;
                self.registers.items[reg] = prevInfo;
                if (prevInfo == null) {
                    _ = self.varNameToReg.remove(name);
                }
            } else {
                _ = self.varNameToReg.remove(name);
            }
        }
    }

    pub fn isRegVariable(self: Self, reg: TempRegister) bool {
        return self.registers.items[reg] != null;
    }

    pub fn pushLoopInfo(self: *Self, allocator: Allocator) !void {
        const newLoop = try LoopInfo.init(allocator);
        const newLoopPtr = try utils.createMut(LoopInfo, allocator, newLoop);
        try self.loopInfo.append(allocator, newLoopPtr);
    }

    pub fn popLoopInfo(self: *Self) void {
        const last = self.loopInfo.pop();

        if (last) |info| {
            for (info.breaks.items) |chunk| {
                setJumpAmount(chunk.chunk, chunk.label);
            }

            for (info.continues.items) |chunk| {
                setJumpAmount(chunk.chunk, info.continueLabel);
            }
        }
    }

    pub fn currentLoopInfo(self: Self) ?*LoopInfo {
        return self.loopInfo.getLastOrNull();
    }

    pub fn setContinueLabel(self: *Self, label: vmInfo.LabelType) void {
        const loopInfo = self.currentLoopInfo();
        if (loopInfo) |info| {
            info.setContinueLabel(label);
        }
    }

    pub fn getRegInfo(self: Self, reg: TempRegister) ?*VarGenInfo {
        return self.registers.items[reg];
    }

    pub fn getVarGenInfoFromName(self: Self, name: []const u8) ?struct {
        varInfo: *VarGenInfo,
        reg: TempRegister,
    } {
        if (self.varNameToReg.get(name)) |reg| {
            const infoOrNull = self.registers.items[reg];
            if (infoOrNull) |info| {
                return .{
                    .varInfo = info,
                    .reg = reg,
                };
            }
        }

        return null;
    }

    pub fn newProc(self: *Self, allocator: Allocator) !void {
        // noop for possible sp add instr
        try self.instrList.append(allocator, .{ .NoOp = {} });
        const startIndex = self.instrList.items.len - 1;
        // noop or possible register push instr
        try self.instrList.append(allocator, .{ .NoOp = {} });

        self.currentProc = .{
            .startIndex = startIndex,
        };
    }

    pub fn finishProc(self: *Self, allocator: Allocator, context: *Context, root: bool) !void {
        try self.labelByteInfo.reserveLabelCount(allocator, self.currentLabelId);

        var stackOffset: u64 = 0;
        if (!root) {
            if (self.currentProc.maxPreserveReg) |maxPreserve| {
                const numRegs = maxPreserve - vmInfo.PRESERVE_REGISTER_START + 1;
                stackOffset = numRegs * vmInfo.POINTER_SIZE;
                self.currentProc.stackFrameSize += stackOffset;

                const pushRegInstr = Instr{
                    .PrePushRegNegOffsetAny = .{
                        .offset = 0,
                        .reg = maxPreserve,
                    },
                };
                self.instrList.items[self.currentProc.startIndex] = pushRegInstr;

                const popRegInstr = Instr{
                    .PostPopRegNegOffsetAny = .{
                        .offset = 0,
                        .reg = maxPreserve,
                    },
                };
                try self.instrList.append(allocator, popRegInstr);
            }
        }

        const spInstrs = try getSpIncInstructions(self.currentProc.stackFrameSize);
        const procInstrs = self.instrList.items[self.currentProc.startIndex..];

        self.byteCounter = vmInfo.VM_INFO_BYTECODE_LEN;
        try adjustInstructions(
            allocator,
            context,
            procInstrs,
            self.currentProc.stackFrameSize,
            stackOffset,
        );

        if (self.currentProc.stackFrameSize + stackOffset > 0) {
            self.byteCounter += spInstrs.add.getInstrLen() + spInstrs.sub.getInstrLen();

            self.instrList.items[self.currentProc.startIndex + 1] = spInstrs.add;
            try self.instrList.append(allocator, spInstrs.sub);
        }

        try self.instrList.append(allocator, .{ .Ret = {} });
    }
};

fn adjustInstructions(
    allocator: Allocator,
    context: *Context,
    instrs: []Instr,
    frameSize: u64,
    stackOffset: u64,
) !void {
    for (instrs) |*instr| {
        try adjustInstruction(allocator, context, instr, frameSize, stackOffset);
        context.genInfo.byteCounter += instr.getInstrLen();
    }
}

/// adjusts store instructions that are based on sp offsets
fn adjustInstruction(
    allocator: Allocator,
    context: *Context,
    instr: *Instr,
    frameSize: u64,
    stackOffset: u64,
) !void {
    switch (instr.*) {
        .Label => |label| {
            context.genInfo.labelByteInfo.setLabelLocation(
                label,
                context.genInfo.byteCounter,
            );
        },
        .Store8AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store64AtSpNegOffset16,
        => |*storeInstr| {
            storeInstr.offset = @intCast(stackOffset + frameSize - storeInstr.offset);
        },
        .MovSpNegOffsetAny => |movInstr| {
            const newInstr = try movSpNegOffset(
                movInstr.reg,
                stackOffset + frameSize - movInstr.offset,
            );
            instr.* = newInstr;
        },
        .Jump,
        .JumpEQ,
        .JumpNE,
        .JumpGT,
        .JumpLT,
        .JumpGTE,
        .JumpLTE,
        .JumpBack,
        .JumpBackEQ,
        .JumpBackNE,
        .JumpBackGT,
        .JumpBackLT,
        .JumpBackGTE,
        .JumpBackLTE,
        => |*data| {
            const labelId = data.*;

            if (try context.genInfo.labelByteInfo.getLabelLocation(@intCast(labelId))) |offset| {
                data.* = context.genInfo.byteCounter - offset;
            } else {
                const byteOffset = switch (instr.*) {
                    .JumpBack,
                    .JumpBackEQ,
                    .JumpBackNE,
                    .JumpBackGT,
                    .JumpBackLT,
                    .JumpBackGTE,
                    .JumpBackLTE,
                    => 0,
                    else => instr.getInstrLen(),
                };
                data.* = context.genInfo.byteCounter + byteOffset;

                try context.genInfo.labelByteInfo.waitForLabel(allocator, @intCast(labelId), data);
            }
        },
        // not changed by stackOffset
        .PrePushRegNegOffsetAny => |pushInstr| {
            const newInstr = try adjustPushRegNegOffsetAnyInstr(
                pushInstr.reg,
                frameSize - pushInstr.offset,
            );
            instr.* = newInstr;
        },
        // not changed by stackOffset
        .PostPopRegNegOffsetAny => |popInstr| {
            const newInstr = try adjustPopRegNegOffsetAnyInstr(
                popInstr.reg,
                frameSize - popInstr.offset,
            );
            instr.* = newInstr;
        },
        else => {},
    }
}

fn adjustPopRegNegOffsetAnyInstr(reg: TempRegister, offset: u64) !Instr {
    const spOpSize = try getOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U8 => Instr{
            .PostPopRegNegOffset8 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U16 => Instr{
            .PostPopRegNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U32 => Instr{
            .PostPopRegNegOffset32 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U64 => Instr{
            .PostPopRegNegOffset64 = .{
                .reg = reg,
                .offset = offset,
            },
        },
    };
}

fn adjustPushRegNegOffsetAnyInstr(reg: TempRegister, offset: u64) !Instr {
    const spOpSize = try getOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U8 => Instr{
            .PrePushRegNegOffset8 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U16 => Instr{
            .PrePushRegNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U32 => Instr{
            .PrePushRegNegOffset32 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U64 => Instr{
            .PrePushRegNegOffset64 = .{
                .reg = reg,
                .offset = offset,
            },
        },
    };
}

fn movSpNegOffset(reg: TempRegister, offset: u64) !Instr {
    const spOpSize = try getOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U8, .U16 => Instr{
            .MovSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U32 => Instr{
            .MovSpNegOffset32 = .{
                .reg = reg,
                .offset = @intCast(offset),
            },
        },
        .U64 => Instr{
            .MovSpNegOffset64 = .{
                .reg = reg,
                .offset = offset,
            },
        },
    };
}

pub fn codegenAst(allocator: Allocator, context: *Context, tree: ast.Ast) !void {
    context.genInfo.vmInfo.version = version.VERSION;
    try context.genInfo.newProc(allocator);
    _ = try genBytecode(allocator, context, tree.root);
    try context.genInfo.finishProc(allocator, context, true);
    try codegenFunctions(allocator, context);
}

fn writeIntSliceToInstr(
    comptime T: type,
    buf: []const u8,
    offset: usize,
    num: []const u8,
) !void {
    const size = @sizeOf(T);
    std.mem.writeInt(
        T,
        @ptrCast(buf[offset .. size + offset]),
        try std.fmt.parseInt(T, num, 10),
        .little,
    );
}

pub fn genBytecode(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
) GenBytecodeError!?TempRegister {
    return genBytecodeUtil(allocator, context, node, null);
}

pub fn genBytecodeUtil(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
    writeLoc: ?WriteLocInfo,
) GenBytecodeError!?TempRegister {
    switch (node.variant) {
        .StructPlaceholder, .StructDec, .UndefValue, .NoOp, .FuncDec => {},
        .Seq => |seq| {
            for (seq) |seqNode| {
                _ = try genBytecode(allocator, context, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, context, dec.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            if (node.typeInfo.lastVarUse) {
                return null;
            }

            const varReg = if (context.genInfo.isRegVariable(reg)) a: {
                const newReg = try context.genInfo.getNextRegister(allocator);

                const instr = Instr{
                    .Mov = .{
                        .dest = newReg,
                        .src = reg,
                    },
                };
                try context.genInfo.appendChunk(allocator, instr);

                break :a newReg;
            } else a: {
                var outReg = reg;

                outReg = try context.genInfo.getNextRegister(allocator);

                const movInstr = Instr{
                    .Mov = .{
                        .dest = outReg,
                        .src = reg,
                    },
                };
                try context.genInfo.appendChunk(allocator, movInstr);

                break :a outReg;
            };

            if (!node.typeInfo.lastVarUse) {
                try context.genInfo.setVariableRegister(
                    dec.name,
                    varReg,
                );
            }
        },
        .Value => |value| {
            switch (value) {
                .Null => {},
                .RawNumber => |num| {
                    const reg = try context.genInfo.getNextRegister(allocator);

                    const instr = switch (num.numType) {
                        .U64 => Instr{
                            .SetReg64 = .{
                                .reg = reg,
                                .data = try std.fmt.parseInt(u64, num.digits, 10),
                            },
                        },
                        .U32 => Instr{
                            .SetReg32 = .{
                                .reg = reg,
                                .data = try std.fmt.parseInt(u32, num.digits, 10),
                            },
                        },
                        .U16 => Instr{
                            .SetReg16 = .{
                                .reg = reg,
                                .data = try std.fmt.parseInt(u16, num.digits, 10),
                            },
                        },
                        .U8 => Instr{
                            .SetReg8 = .{
                                .reg = reg,
                                .data = try std.fmt.parseInt(u8, num.digits, 10),
                            },
                        },
                        .Char => Instr{
                            .SetReg8 = .{
                                .reg = reg,
                                .data = num.digits[0],
                            },
                        },
                        else => utils.unimplemented(),
                    };

                    try context.genInfo.appendChunk(allocator, instr);
                    return reg;
                },
                .Bool => |b| {
                    const reg = try context.genInfo.getNextRegister(allocator);

                    const instr = Instr{
                        .SetReg8 = .{
                            .reg = reg,
                            .data = @as(u8, @intFromBool(b)),
                        },
                    };

                    try context.genInfo.appendChunk(allocator, instr);
                    return reg;
                },
                .Char => |ch| {
                    const reg = try context.genInfo.getNextRegister(allocator);

                    const instr = Instr{
                        .SetReg8 = .{
                            .reg = reg,
                            .data = ch,
                        },
                    };

                    try context.genInfo.appendChunk(allocator, instr);
                    return reg;
                },
                .Number => |num| {
                    const reg = try context.genInfo.getNextRegister(allocator);

                    const instr = switch (num) {
                        .Char, .U8 => |byte| Instr{
                            .SetReg8 = .{
                                .reg = reg,
                                .data = byte,
                            },
                        },
                        .U64 => |data| Instr{
                            .SetReg64 = .{
                                .reg = reg,
                                .data = data,
                            },
                        },
                        .U32 => |data| Instr{
                            .SetReg32 = .{
                                .reg = reg,
                                .data = data,
                            },
                        },
                        .U16 => |data| Instr{
                            .SetReg16 = .{
                                .reg = reg,
                                .data = data,
                            },
                        },
                        else => utils.unimplemented(),
                    };

                    try context.genInfo.appendChunk(allocator, instr);
                    return reg;
                },
                .ArrayDec => |items| {
                    var movSpInstr: Instr = undefined;

                    var writeLocInfo = if (writeLoc) |wLoc|
                        wLoc
                    else a: {
                        const ptrReg = try context.genInfo.getNextRegister(allocator);

                        const padding = utils.calculatePadding(
                            context.genInfo.currentProc.stackFrameSize,
                            node.typeInfo.alignment,
                        );
                        context.genInfo.currentProc.stackFrameSize += padding;

                        movSpInstr = Instr{
                            .MovSpNegOffsetAny = .{
                                .reg = ptrReg,
                                .offset = context.genInfo.currentProc.stackFrameSize,
                            },
                        };
                        try context.genInfo.appendChunk(allocator, movSpInstr);

                        const newWriteLoc = WriteLocInfo{
                            .reg = ptrReg,
                            .value = context.genInfo.currentProc.stackFrameSize,
                        };

                        context.genInfo.currentProc.stackFrameSize += node.typeInfo.size;

                        break :a newWriteLoc;
                    };

                    const itemSize: u8 = if (items.len > 0)
                        @intCast(items[0].typeInfo.size)
                    else
                        0;
                    const isPrimitive = if (items.len > 0)
                        nodeIsPrimitive(items[0])
                    else
                        false;
                    const itemPadding = utils.calculatePadding(itemSize, node.typeInfo.alignment);

                    const prevAccessReturn = context.genInfo.settings.propertyAccessReturnsPointer;
                    context.genInfo.settings.propertyAccessReturnsPointer = true;
                    for (items) |item| {
                        const regOrNull = try genBytecodeUtil(
                            allocator,
                            context,
                            item,
                            writeLocInfo,
                        );

                        if (isPrimitive) {
                            const reg = regOrNull orelse
                                return CodeGenError.ReturnedRegisterNotFound;
                            const storeInstr = storeRegAtRegWithPostInc(
                                reg,
                                writeLocInfo.reg,
                                itemSize + @as(u16, @intCast(itemPadding)),
                                itemSize,
                            );
                            try context.genInfo.appendChunk(allocator, storeInstr);
                        } else if (itemPadding > 0) {
                            const addInstr = Instr{
                                .Add16 = .{
                                    .dest = writeLocInfo.reg,
                                    .reg = writeLocInfo.reg,
                                    .data = itemPadding,
                                },
                            };
                            try context.genInfo.appendChunk(allocator, addInstr);
                        }

                        writeLocInfo.value += itemSize + @as(u16, @intCast(itemPadding));
                    }
                    context.genInfo.settings.propertyAccessReturnsPointer = prevAccessReturn;

                    if (writeLoc == null) {
                        try context.genInfo.appendChunk(allocator, movSpInstr);
                        return writeLocInfo.reg;
                    }
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            var leftReg: TempRegister = undefined;

            const leftDepth = ast.getExprDepth(expr.left);
            const rightDepth = ast.getExprDepth(expr.right);
            const leftExprDeeper = leftDepth >= rightDepth;
            if (leftExprDeeper) {
                leftReg = try genBytecode(allocator, context, expr.left) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
            }
            const rightReg = try genBytecode(allocator, context, expr.right) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            if (!leftExprDeeper) {
                leftReg = try genBytecode(allocator, context, expr.left) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
            }

            var outReg: ?TempRegister = null;

            const buf: Instr = switch (expr.type) {
                .Add, .Sub, .Mult => a: {
                    outReg = try context.genInfo.getNextRegister(allocator);
                    const mathInstr = MathInstr{
                        .dest = outReg.?,
                        .reg1 = leftReg,
                        .reg2 = rightReg,
                    };
                    break :a switch (expr.type) {
                        .Add => .{ .Add = mathInstr },
                        .Sub => .{ .Sub = mathInstr },
                        .Mult => .{ .Mult = mathInstr },
                        else => utils.unimplemented(),
                    };
                },
                .LessThan,
                .GreaterThan,
                .GreaterThanEq,
                .LessThanEq,
                .Equal,
                .NotEqual,
                => a: {
                    var instr = if (context.genInfo.settings.outputCmpAsRegister)
                        exprTypeToCmpSetReg(expr.type)
                    else
                        Instr{ .Cmp = .{} };

                    if (context.genInfo.settings.outputCmpAsRegister) {
                        // NOTE - change if left and right byte sizes become different
                        outReg = try context.genInfo.getNextRegister(allocator);
                        switch (instr) {
                            .CmpSetRegEQ,
                            .CmpSetRegNE,
                            .CmpSetRegGT,
                            .CmpSetRegLT,
                            .CmpSetRegGTE,
                            .CmpSetRegLTE,
                            => |*payload| {
                                payload.dest = outReg.?;
                            },
                            else => unreachable,
                        }
                    }

                    switch (instr) {
                        .Cmp => |*cmp| {
                            cmp.reg1 = leftReg;
                            cmp.reg2 = rightReg;
                        },
                        .CmpSetRegEQ,
                        .CmpSetRegNE,
                        .CmpSetRegGT,
                        .CmpSetRegLT,
                        .CmpSetRegGTE,
                        .CmpSetRegLTE,
                        => |*payload| {
                            payload.reg1 = leftReg;
                            payload.reg2 = rightReg;
                        },
                        else => unreachable,
                    }

                    break :a instr;
                },
                .BitAnd => a: {
                    outReg = try context.genInfo.getNextRegister(allocator);

                    break :a Instr{
                        .BitAnd = .{
                            .dest = outReg.?,
                            .reg1 = leftReg,
                            .reg2 = rightReg,
                        },
                    };
                },
                .BitOr => a: {
                    outReg = try context.genInfo.getNextRegister(allocator);

                    break :a Instr{
                        .BitOr = .{
                            .dest = outReg.?,
                            .reg1 = leftReg,
                            .reg2 = rightReg,
                        },
                    };
                },
                .And => if (context.genInfo.settings.outputCmpAsRegister) a: {
                    outReg = try context.genInfo.getNextRegister(allocator);

                    break :a Instr{
                        .AndSetReg = .{
                            .dest = outReg.?,
                            .reg1 = leftReg,
                            .reg2 = rightReg,
                        },
                    };
                } else Instr{
                    .And = .{
                        .reg1 = leftReg,
                        .reg2 = rightReg,
                    },
                },
                .Or => if (context.genInfo.settings.outputCmpAsRegister) a: {
                    outReg = try context.genInfo.getNextRegister(allocator);

                    break :a Instr{
                        .OrSetReg = .{
                            .dest = outReg.?,
                            .reg1 = leftReg,
                            .reg2 = rightReg,
                        },
                    };
                } else Instr{
                    .Or = .{
                        .reg1 = leftReg,
                        .reg2 = rightReg,
                    },
                },
                .Div => utils.unimplemented(),
            };

            try context.genInfo.appendChunk(allocator, buf);

            return outReg;
        },
        .Variable => |name| {
            const resReg = context.genInfo.getVariableRegister(name);
            if (node.typeInfo.lastVarUse) {
                context.genInfo.removeVariableRegister(name);
            }
            return resReg;
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, context, statement.condition) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const buf = Instr{
                .CmpConst8 = .{
                    .reg = condReg,
                    .data = 1,
                },
            };
            try context.genInfo.appendChunk(allocator, buf);

            const jumpLabelId = context.genInfo.takeLabelId();
            const jumpLabel = Instr{ .Label = jumpLabelId };
            const jumpBuf = Instr{ .JumpNE = jumpLabelId };
            try context.genInfo.appendChunk(allocator, jumpBuf);

            _ = try genBytecode(allocator, context, statement.body);

            if (statement.fallback) |fallback| {
                const jumpEndLabelId = context.genInfo.takeLabelId();
                const jumpEndInstr = Instr{ .Jump = jumpEndLabelId };
                try context.genInfo.appendChunk(allocator, jumpEndInstr);

                try context.genInfo.appendChunk(allocator, jumpLabel);
                try generateFallback(allocator, context, fallback);
                const jumpEndLabel = Instr{ .Label = jumpEndLabelId };
                try context.genInfo.appendChunk(allocator, jumpEndLabel);
            } else {
                try context.genInfo.appendChunk(allocator, jumpLabel);
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |initNode| {
                _ = try genBytecode(allocator, context, initNode);
            }

            const condInfo = prepForLoopCondition(context, loop.condition);

            try context.genInfo.pushLoopInfo(allocator);
            defer context.genInfo.popLoopInfo();

            const preConditionLabelId = context.genInfo.takeLabelId();
            const preConditionLabel = Instr{ .Label = preConditionLabelId };
            try context.genInfo.appendChunk(allocator, preConditionLabel);

            const condReg = try genBytecode(allocator, context, loop.condition);
            context.genInfo.settings.outputCmpAsRegister = condInfo.prevCmpAsReg;

            const loopEndLabelId = context.genInfo.takeLabelId();
            var jumpEndInstr = Instr{ .JumpNE = loopEndLabelId };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, loopEndLabelId, false);
                jumpEndInstr = jumpInstruction;
            } else {
                const regValue = condReg orelse return CodeGenError.ReturnedRegisterNotFound;
                const cmpInstr = Instr{
                    .CmpConst8 = .{
                        .reg = regValue,
                        .data = 1,
                    },
                };
                try context.genInfo.appendChunk(allocator, cmpInstr);
            }

            try context.genInfo.appendChunk(allocator, jumpEndInstr);

            _ = try genBytecode(allocator, context, loop.body);

            const continueLabelId = context.genInfo.takeLabelId();
            const continueLabel = Instr{ .Label = continueLabelId };
            try context.genInfo.appendChunk(allocator, continueLabel);
            context.genInfo.setContinueLabel(continueLabelId);

            _ = try genBytecode(allocator, context, loop.incNode);

            const jumpStartInstr = Instr{ .JumpBack = preConditionLabelId };
            try context.genInfo.appendChunk(allocator, jumpStartInstr);

            const loopEndLabel = Instr{ .Label = loopEndLabelId };
            try context.genInfo.appendChunk(allocator, loopEndLabel);
        },
        .WhileLoop => |loop| {
            try context.genInfo.pushLoopInfo(allocator);
            defer context.genInfo.popLoopInfo();

            const condInfo = prepForLoopCondition(context, loop.condition);

            const preConditionLabelId = context.genInfo.takeLabelId();
            const preConditionLabel = Instr{ .Label = preConditionLabelId };
            try context.genInfo.appendChunk(allocator, preConditionLabel);

            context.genInfo.setContinueLabel(preConditionLabelId);

            const condReg = try genBytecode(allocator, context, loop.condition);

            const loopEndLabelId = context.genInfo.takeLabelId();
            var jumpEndInstr = Instr{ .JumpNE = loopEndLabelId };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, loopEndLabelId, false);
                jumpEndInstr = jumpInstruction;
            } else {
                const regValue = condReg orelse return CodeGenError.ReturnedRegisterNotFound;
                const cmpInstr = Instr{
                    .CmpConst8 = .{
                        .reg = regValue,
                        .data = 1,
                    },
                };
                try context.genInfo.appendChunk(allocator, cmpInstr);
            }

            try context.genInfo.appendChunk(allocator, jumpEndInstr);

            _ = try genBytecode(allocator, context, loop.body);

            const jumpStartInstr = Instr{ .JumpBack = preConditionLabelId };
            try context.genInfo.appendChunk(allocator, jumpStartInstr);

            const preBodyLabel = Instr{ .Label = loopEndLabelId };
            try context.genInfo.appendChunk(allocator, preBodyLabel);
        },
        .IncOne => |inc| {
            const reg = try genBytecode(allocator, context, inc) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const instr = Instr{
                .IncConst8 = .{
                    .reg = reg,
                    .data = 1,
                },
            };

            try context.genInfo.appendChunk(allocator, instr);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, context, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const instr = Instr{
                .DecConst8 = .{
                    .reg = reg,
                    .data = 1,
                },
            };

            try context.genInfo.appendChunk(allocator, instr);
        },
        .Group => |group| {
            return genBytecode(allocator, context, group);
        },
        .ValueSet => |set| {
            const isDeref = set.value.variant == .Dereference;
            const targetNode = if (isDeref)
                set.value.variant.Dereference
            else
                set.value;

            const prevOutput = context.genInfo.settings.propertyAccessReturnsPointer;
            context.genInfo.settings.propertyAccessReturnsPointer = true;
            const destReg = try genBytecode(allocator, context, targetNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            context.genInfo.settings.propertyAccessReturnsPointer = prevOutput;
            const srcReg = try genBytecode(allocator, context, set.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const dbgInstr = Instr{
                .DbgReg = destReg,
            };
            try context.genInfo.appendChunk(allocator, dbgInstr);

            const instr = if (isDeref or
                set.value.variant == .IndexValue or
                set.value.variant == .PropertyAccess)
                storeRegAtRegWithSize(
                    srcReg,
                    destReg,
                    set.setNode.typeInfo.size,
                )
            else
                Instr{
                    .Mov = .{
                        .dest = destReg,
                        .src = srcReg,
                    },
                };
            try context.genInfo.appendChunk(allocator, instr);
        },
        .Bang => |expr| {
            const reg = try genBytecode(allocator, context, expr) orelse
                return CodeGenError.NoAvailableRegisters;
            const setReg = try context.genInfo.getNextRegister(allocator);

            const instr = Instr{
                .XorConst8 = .{
                    .dest = setReg,
                    .reg = reg,
                    .byte = 1,
                },
            };
            try context.genInfo.appendChunk(allocator, instr);

            return setReg;
        },
        .Scope => |scope| {
            _ = try genBytecode(allocator, context, scope);
        },
        .Break => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const breakLabelId = context.genInfo.takeLabelId();
            const buf = Instr{ .Jump = breakLabelId };
            const chunk = try context.genInfo.appendChunkAndReturn(allocator, buf);
            try loopInfo.appendBreak(allocator, chunk, breakLabelId);
        },
        .Continue => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const continueLabelId = context.genInfo.takeLabelId();
            const instr = Instr{ .Jump = continueLabelId };
            const chunk = try context.genInfo.appendChunkAndReturn(allocator, instr);
            try loopInfo.appendContinue(allocator, chunk, continueLabelId);
        },
        .ArrayInit => |init| {
            const initSize = node.typeInfo.size;

            var movSpInstr: Instr = undefined;

            const writeLocInfo = if (writeLoc) |wLoc|
                wLoc
            else a: {
                const writeReg = try context.genInfo.getNextRegister(allocator);

                const padding = utils.calculatePadding(
                    context.genInfo.currentProc.stackFrameSize,
                    node.typeInfo.alignment,
                );
                context.genInfo.currentProc.stackFrameSize += padding;

                movSpInstr = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = writeReg,
                        .offset = context.genInfo.currentProc.stackFrameSize,
                    },
                };
                try context.genInfo.appendChunk(allocator, movSpInstr);

                const newWriteLoc = WriteLocInfo{
                    .reg = writeReg,
                    .value = context.genInfo.currentProc.stackFrameSize,
                };

                context.genInfo.currentProc.stackFrameSize += initSize;

                break :a newWriteLoc;
            };

            const initLen = try std.fmt.parseInt(u64, init.size, 10);
            const lenReg = try context.genInfo.getNextRegister(allocator);
            const movLen = Instr{
                .SetReg64 = .{
                    .reg = lenReg,
                    .data = initLen,
                },
            };
            try context.genInfo.appendChunk(allocator, movLen);

            var indexReg: ?TempRegister = null;
            var ptrReg: ?TempRegister = null;

            if (init.indexIdent) |ident| {
                indexReg = try context.genInfo.getNextRegister(allocator);
                try context.genInfo.setVariableRegister(ident, indexReg.?);

                const setZeroInstr = Instr{
                    .SetReg8 = .{
                        .reg = indexReg.?,
                        .data = 0,
                    },
                };
                try context.genInfo.appendChunk(allocator, setZeroInstr);
            }

            if (init.ptrIdent) |ident| {
                ptrReg = try context.genInfo.getNextRegister(allocator);
                try context.genInfo.setVariableRegister(ident, ptrReg.?);

                const movWriteReg = Instr{
                    .Mov = .{
                        .dest = ptrReg.?,
                        .src = writeLocInfo.reg,
                    },
                };
                try context.genInfo.appendChunk(allocator, movWriteReg);
            }
            const preCmpLabelId = context.genInfo.takeLabelId();
            const preCmpLabel = Instr{ .Label = preCmpLabelId };
            try context.genInfo.appendChunk(allocator, preCmpLabel);

            const cmpLen = Instr{
                .CmpConst8 = .{
                    .reg = lenReg,
                    .data = 0,
                },
            };
            try context.genInfo.appendChunk(allocator, cmpLen);

            const jumpInstrLabelId = context.genInfo.takeLabelId();
            const jumpInstr = Instr{ .JumpEQ = jumpInstrLabelId };
            try context.genInfo.appendChunk(allocator, jumpInstr);

            const resRegOrNull = try genBytecodeUtil(
                allocator,
                context,
                init.initNode,
                writeLocInfo,
            );

            const itemAlignment = init.initType.astType.getAlignment(context);
            const itemSize = try init.initType.astType.getSize(context);
            const itemPadding = utils.calculatePadding(itemSize, itemAlignment);
            if (nodeIsPrimitive(init.initNode)) {
                const resReg = resRegOrNull orelse return CodeGenError.ReturnedRegisterNotFound;
                const writeInstr = storeRegAtRegWithPostInc(
                    resReg,
                    writeLocInfo.reg,
                    @intCast(itemSize + itemPadding),
                    @intCast(itemSize),
                );
                try context.genInfo.appendChunk(allocator, writeInstr);
            }

            const subLen = Instr{
                .Sub8 = .{
                    .dest = lenReg,
                    .reg = lenReg,
                    .data = 1,
                },
            };
            try context.genInfo.appendChunk(allocator, subLen);

            if (indexReg) |reg| {
                const addInstr = Instr{
                    .Add8 = .{
                        .dest = reg,
                        .reg = reg,
                        .data = 1,
                    },
                };
                try context.genInfo.appendChunk(allocator, addInstr);
            }

            if (ptrReg) |reg| {
                const addInstr = Instr{
                    .Add16 = .{
                        .dest = reg,
                        .reg = reg,
                        .data = @intCast(itemSize + itemPadding),
                    },
                };
                try context.genInfo.appendChunk(allocator, addInstr);
            }

            const jumpStart = Instr{ .JumpBack = preCmpLabelId };
            try context.genInfo.appendChunk(allocator, jumpStart);

            const jumpInstrLabel = Instr{ .Label = jumpInstrLabelId };
            try context.genInfo.appendChunk(allocator, jumpInstrLabel);

            if (writeLoc == null) {
                try context.genInfo.appendChunk(allocator, movSpInstr);
                return writeLocInfo.reg;
            }
        },
        .Dereference => |inner| {
            const resReg = try genBytecode(allocator, context, inner) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const destReg = if (context.genInfo.isRegVariable(resReg))
                try context.genInfo.getNextRegister(allocator)
            else
                resReg;

            const loadInstr = loadAtReg(resReg, destReg, node.typeInfo.size);
            try context.genInfo.appendChunk(allocator, loadInstr);

            return destReg;
        },
        .Pointer => |inner| {
            if (node.typeInfo.makesSliceWithLen) |len| {
                const slicePtrReg = try initArraySliceBytecode(
                    allocator,
                    context,
                    inner.node,
                    len,
                    writeLoc,
                );

                if (slicePtrReg) |reg|
                    return reg
                else
                    return null;
            }

            const ptrReg = try context.genInfo.getNextRegister(allocator);
            const resReg = try genBytecode(allocator, context, inner.node) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const varGenInfo = context.genInfo.getRegInfo(resReg);
            const varStackLocation = if (varGenInfo) |info|
                if (info.stackLocation) |location|
                    location
                else
                    null
            else
                null;

            const location = varStackLocation orelse a: {
                const itemSize = inner.node.typeInfo.size;
                const alignment = inner.node.typeInfo.alignment;

                const padding = utils.calculatePadding(
                    context.genInfo.currentProc.stackFrameSize,
                    alignment,
                );
                context.genInfo.currentProc.stackFrameSize += padding;
                const spAfterPadding = context.genInfo.currentProc.stackFrameSize;

                const storeInstr = try storeRegAtSpNegOffset(
                    resReg,
                    context.genInfo.currentProc.stackFrameSize,
                    @intCast(inner.node.typeInfo.size),
                );
                try context.genInfo.appendChunk(allocator, storeInstr);

                context.genInfo.currentProc.stackFrameSize += itemSize;

                break :a spAfterPadding;
            };

            if (varGenInfo) |info| {
                if (info.stackLocation == null) {
                    info.stackLocation = location;
                }
            }

            const setLocInstr = Instr{
                .MovSpNegOffsetAny = .{
                    .reg = ptrReg,
                    .offset = location,
                },
            };
            try context.genInfo.appendChunk(allocator, setLocInstr);

            return ptrReg;
        },
        .VarEqOp => |op| {
            const destReg = try genBytecode(allocator, context, op.variable) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const valueReg = try genBytecode(allocator, context, op.value) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const instr: Instr = switch (op.opType) {
                .Add => .{
                    .Add = .{
                        .dest = destReg,
                        .reg1 = destReg,
                        .reg2 = valueReg,
                    },
                },
                .Sub => .{
                    .Sub = .{
                        .dest = destReg,
                        .reg1 = destReg,
                        .reg2 = valueReg,
                    },
                },
                .Mult => .{
                    .Sub = .{
                        .dest = destReg,
                        .reg1 = destReg,
                        .reg2 = valueReg,
                    },
                },
                else => utils.unimplemented(),
            };

            try context.genInfo.appendChunk(allocator, instr);
        },
        .StructInit => |init| {
            var startLoc = context.genInfo.currentProc.stackFrameSize;

            const writeLocInfo: WriteLocInfo = if (writeLoc) |wLoc|
                wLoc
            else a: {
                const resReg = try context.genInfo.getNextRegister(allocator);

                const padding = utils.calculatePadding(
                    context.genInfo.currentProc.stackFrameSize,
                    node.typeInfo.alignment,
                );
                context.genInfo.currentProc.stackFrameSize += padding;
                startLoc += padding;
                const writeVal = context.genInfo.currentProc.stackFrameSize;

                const writeSp = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = resReg,
                        .offset = context.genInfo.currentProc.stackFrameSize,
                    },
                };
                try context.genInfo.appendChunk(allocator, writeSp);

                context.genInfo.currentProc.stackFrameSize += node.typeInfo.size;

                break :a WriteLocInfo{ .reg = resReg, .value = writeVal };
            };

            const def = context.compInfo.getStructDec(init.name).?;

            for (def.attributes, 0..) |defAttr, index| {
                if (defAttr.attr != .Member) continue;

                const attr = init.findAttribute(defAttr.name).?;

                const regOrNull = try genBytecodeUtil(
                    allocator,
                    context,
                    attr.value,
                    writeLocInfo,
                );

                var itemSize = attr.value.typeInfo.size;

                var nextItemOffset: usize = 1;
                while (index + nextItemOffset < def.attributes.len) : (nextItemOffset += 1) {
                    if (def.attributes[index + nextItemOffset].attr != .Member) continue;
                    const nextAttr = def.attributes[index + nextItemOffset];
                    const nextAttrDef = init.findAttribute(nextAttr.name).?;
                    const nextPadding = utils.calculatePadding(
                        writeLocInfo.value,
                        nextAttrDef.value.typeInfo.alignment,
                    );
                    itemSize += nextPadding;
                }

                if (nodeIsPrimitive(attr.value)) {
                    const reg = regOrNull orelse return CodeGenError.ReturnedRegisterNotFound;

                    const instr = storeRegAtRegWithPostInc(
                        reg,
                        writeLocInfo.reg,
                        @intCast(itemSize),
                        @intCast(itemSize),
                    );
                    try context.genInfo.appendChunk(allocator, instr);
                }
            }

            if (writeLoc == null) {
                const writeSpInstr = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = writeLocInfo.reg,
                        .offset = startLoc,
                    },
                };
                try context.genInfo.appendChunk(allocator, writeSpInstr);
                return writeLocInfo.reg;
            }
        },
        .PropertyAccess => |accessNode| {
            const fromName = node.typeInfo.accessingFrom orelse
                return CodeGenError.AccessTargetDoesNotHaveStructName;
            const dec = context.compInfo.getStructDec(fromName).?;
            const loc = (try dec.getMemberLocation(context, accessNode.property)).?;

            const reg = try calculateAccessOffset(
                allocator,
                context,
                accessNode.value,
                @intCast(loc),
            );
            const outReg = try context.genInfo.getNextRegister(allocator);

            const instr = loadAtReg(reg, outReg, node.typeInfo.size);
            try context.genInfo.appendChunk(allocator, instr);

            return outReg;
        },
        .IndexValue => |indexNode| {
            const indexReg = try genBytecode(allocator, context, indexNode.index) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const reg = try calculateAccessOffset(
                allocator,
                context,
                indexNode.target,
                0,
            );

            const itemPadding = utils.calculatePadding(
                node.typeInfo.size,
                node.typeInfo.alignment,
            );

            const mulAdd = Instr{
                .MulReg16AddReg = .{
                    .dest = reg,
                    .addReg = reg,
                    .mulReg = indexReg,
                    .data = @intCast(node.typeInfo.size + itemPadding),
                },
            };
            try context.genInfo.appendChunk(allocator, mulAdd);

            if (context.genInfo.settings.propertyAccessReturnsPointer) {
                return reg;
            } else {
                const outReg = try context.genInfo.getNextRegister(allocator);

                const loadInstr = loadAtReg(reg, outReg, @intCast(node.typeInfo.size));
                try context.genInfo.appendChunk(allocator, loadInstr);

                return outReg;
            }
        },
        .FuncCall => |call| {
            _ = call;

            // TODO - fill this temp value in
            return 170;
        },
        else => {},
    }

    return null;
}

fn codegenFunctions(allocator: Allocator, context: *Context) !void {
    var funcIter = context.compInfo.functions.valueIterator();
    while (funcIter.next()) |func| {
        try context.genInfo.newProc(allocator);
        _ = try genBytecode(allocator, context, func.*.body);
        try context.genInfo.finishProc(allocator, context, false);
    }
}

fn nodeIsPrimitive(node: *ast.AstNode) bool {
    return switch (node.variant) {
        .StructInit, .ArrayInit => false,
        .Value => |val| val != .ArrayDec,
        .Pointer => node.typeInfo.makesSliceWithLen == null,
        else => true,
    };
}

fn addNumToReg(reg: TempRegister, inc: u64) !Instr {
    const opSize = try getOpSizeFromNum(inc);

    return switch (opSize) {
        .U8 => Instr{
            .Add8 = .{
                .dest = reg,
                .reg = reg,
                .data = @intCast(inc),
            },
        },
        .U16 => Instr{
            .Add16 = .{
                .dest = reg,
                .reg = reg,
                .data = @intCast(inc),
            },
        },
        else => unreachable,
    };
}

fn getSpIncInstructions(size: u64) !struct {
    add: Instr,
    sub: Instr,
} {
    const spOpSize = try getOpSizeFromNum(size);

    return switch (spOpSize) {
        .U8 => .{
            .add = Instr{ .AddSp8 = @intCast(size) },
            .sub = Instr{ .SubSp8 = @intCast(size) },
        },
        .U16 => .{
            .add = Instr{ .AddSp16 = @intCast(size) },
            .sub = Instr{ .SubSp16 = @intCast(size) },
        },
        .U32 => .{
            .add = Instr{ .AddSp32 = @intCast(size) },
            .sub = Instr{ .SubSp32 = @intCast(size) },
        },
        .U64 => .{
            .add = Instr{ .AddSp64 = @intCast(size) },
            .sub = Instr{ .SubSp64 = @intCast(size) },
        },
    };
}

fn getOpSizeFromNum(num: u64) !OpSizes {
    var res: ?OpSizes = null;

    inline for (sizeToOpSizeTuple) |tuple| {
        if (num < tuple[0]) {
            res = tuple[1];
            break;
        }
    }

    return res orelse CodeGenError.StackFrameSizeTooLarge;
}

/// returns register holding a pointer
///
/// the pointer is offset the amount determined by the accessing
///
/// the register returned will never be attributed to a variable
fn calculateAccessOffset(
    allocator: Allocator,
    context: *Context,
    node: *ast.AstNode,
    offset: u16,
) !TempRegister {
    const prevRetFormat = context.genInfo.settings.propertyAccessReturnsPointer;
    context.genInfo.settings.propertyAccessReturnsPointer = true;
    defer context.genInfo.settings.propertyAccessReturnsPointer = prevRetFormat;

    switch (node.variant) {
        .PropertyAccess => |accessNode| {
            const fromName = node.typeInfo.accessingFrom orelse
                return CodeGenError.AccessTargetDoesNotHaveStructName;
            const dec = context.compInfo.getStructDec(fromName).?;
            const loc = (try dec.getMemberLocation(context, accessNode.property)).?;
            return try calculateAccessOffset(
                allocator,
                context,
                accessNode.value,
                offset + @as(u16, @intCast(loc)),
            );
        },
        .IndexValue => |indexNode| {
            const offsetReg = try calculateAccessOffset(
                allocator,
                context,
                indexNode.target,
                offset,
            );

            const indexReg = try genBytecode(allocator, context, indexNode.index) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const itemSize = node.typeInfo.size;
            const itemPadding = utils.calculatePadding(itemSize, node.typeInfo.alignment);

            const offsetInstr = Instr{
                .MulReg16AddReg = .{
                    .dest = offsetReg,
                    .addReg = offsetReg,
                    .mulReg = indexReg,
                    .data = @intCast(itemSize + itemPadding),
                },
            };
            try context.genInfo.appendChunk(allocator, offsetInstr);

            return offsetReg;
        },
        else => {
            const reg = try genBytecode(allocator, context, node) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const isVar = context.genInfo.isRegVariable(reg);
            const outReg = try context.genInfo.getNextRegister(allocator);

            if (node.typeInfo.isSlice) {
                const derefInstr = Instr{
                    .Load64AtReg = .{
                        .dest = outReg,
                        .fromRegPtr = outReg,
                    },
                };
                try context.genInfo.appendChunk(allocator, derefInstr);
            }

            if (offset > 0) {
                const instr = Instr{
                    .Add16 = .{
                        .dest = outReg,
                        .reg = reg,
                        .data = @intCast(offset),
                    },
                };
                try context.genInfo.appendChunk(allocator, instr);
            } else if (isVar) {
                // if the returned register is associated with a variable, outReg will be a
                // new register with unknown contents, so we set it in this case
                const instr = Instr{
                    .Mov = .{
                        .dest = outReg,
                        .src = reg,
                    },
                };
                try context.genInfo.appendChunk(allocator, instr);
            }

            return outReg;
        },
    }
}

fn loadAtRegWithOffset(reg: TempRegister, outReg: TempRegister, readOffset: u64, size: u64) Instr {
    const offset: u16 = @intCast(readOffset);

    if (offset == 0) return loadAtReg(reg, outReg, size);

    return switch (size) {
        1 => Instr{
            .Load8AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        2 => Instr{
            .Load16AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        3, 4 => Instr{
            .Load32AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        5...8 => Instr{
            .Load64AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        else => unreachable,
    };
}

fn loadAtReg(regPtr: TempRegister, outReg: TempRegister, size: u64) Instr {
    return switch (size) {
        1 => Instr{
            .Load8AtReg = .{
                .dest = outReg,
                .fromRegPtr = regPtr,
            },
        },
        2 => Instr{
            .Load16AtReg = .{
                .dest = outReg,
                .fromRegPtr = regPtr,
            },
        },
        3, 4 => Instr{
            .Load32AtReg = .{
                .dest = outReg,
                .fromRegPtr = regPtr,
            },
        },
        5...8 => Instr{
            .Load64AtReg = .{
                .dest = outReg,
                .fromRegPtr = regPtr,
            },
        },
        else => unreachable,
    };
}

fn storeRegAtSpNegOffset(reg: TempRegister, loc: u64, size: u8) !Instr {
    return switch (size) {
        1 => Instr{
            .Store8AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        2 => Instr{
            .Store16AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        3, 4 => Instr{
            .Store32AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        5, 6, 7, 8 => Instr{
            .Store64AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        else => unreachable,
    };
}

fn storeRegAtSpNegOffsetAndSize(regContents: TempRegister, loc: u64, size: u64) Instr {
    const reg = regContents;

    return switch (size) {
        1 => Instr{
            .Store8AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        2 => Instr{
            .Store16AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        3, 4 => Instr{
            .Store32AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        5...8 => Instr{
            .Store64AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        else => unreachable,
    };
}

fn storeRegAtRegWithSize(regContents: TempRegister, ptrReg: TempRegister, size: u64) Instr {
    const reg = regContents;

    return switch (size) {
        1 => Instr{
            .Store8AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        2 => Instr{
            .Store16AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        3, 4 => Instr{
            .Store32AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        5...8 => Instr{
            .Store64AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        else => unreachable,
    };
}

fn storeRegAtRegWithPostInc(
    reg: TempRegister,
    ptrReg: TempRegister,
    inc: u16,
    size: u8,
) Instr {
    return switch (size) {
        1 => Instr{
            .Store8AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        2 => Instr{
            .Store16AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        3, 4 => Instr{
            .Store32AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        5, 6, 7, 8 => Instr{
            .Store64AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        else => unreachable,
    };
}

fn initArraySliceBytecode(
    allocator: Allocator,
    context: *Context,
    node: *ast.AstNode,
    len: u64,
    writeLoc: ?WriteLocInfo,
) !?TempRegister {
    var paddedSpLoc: u64 = 0;
    var arrayStartLoc: u64 = 0;

    const writeLocInfo = if (writeLoc != null) a: {
        const scratchReg = try context.genInfo.getNextRegister(allocator);

        const padding = utils.calculatePadding(
            context.genInfo.currentProc.stackFrameSize,
            node.typeInfo.alignment,
        );
        context.genInfo.currentProc.stackFrameSize += padding;
        arrayStartLoc = context.genInfo.currentProc.stackFrameSize;

        const movSpInstr2 = Instr{
            .MovSpNegOffsetAny = .{
                .reg = scratchReg,
                .offset = arrayStartLoc,
            },
        };
        try context.genInfo.appendChunk(allocator, movSpInstr2);

        break :a WriteLocInfo{
            .reg = scratchReg,
            .value = paddedSpLoc,
        };
    } else a: {
        const writeReg = try context.genInfo.getNextRegister(allocator);

        const padding = utils.calculatePadding(
            context.genInfo.currentProc.stackFrameSize,
            vmInfo.POINTER_SIZE,
        );
        context.genInfo.currentProc.stackFrameSize += padding;
        paddedSpLoc = context.genInfo.currentProc.stackFrameSize;
        context.genInfo.currentProc.stackFrameSize += vmInfo.POINTER_SIZE * 2;
        arrayStartLoc = context.genInfo.currentProc.stackFrameSize;

        const movSpInstr2 = Instr{
            .MovSpNegOffsetAny = .{
                .reg = writeReg,
                .offset = arrayStartLoc,
            },
        };
        try context.genInfo.appendChunk(allocator, movSpInstr2);

        break :a WriteLocInfo{
            .reg = writeReg,
            .value = context.genInfo.currentProc.stackFrameSize,
        };
    };

    const arrRegOrNull = try genBytecodeUtil(allocator, context, node, writeLocInfo);

    const sliceStartPtr = if (writeLoc) |loc| loc.reg else a: {
        const movInstr = Instr{
            .MovSpNegOffsetAny = .{
                .reg = writeLocInfo.reg,
                .offset = paddedSpLoc,
            },
        };
        try context.genInfo.appendChunk(allocator, movInstr);
        break :a writeLocInfo.reg;
    };

    const tempReg = try context.genInfo.getNextRegister(allocator);
    const regInfoOrNull = if (arrRegOrNull) |arrReg|
        context.genInfo.getRegInfo(arrReg)
    else
        null;
    const regLocationOrNull = if (regInfoOrNull) |info|
        info.stackLocation
    else
        null;
    const regLocation = regLocationOrNull orelse arrayStartLoc;

    const setReg = Instr{
        .SetReg64 = .{
            .reg = tempReg,
            .data = regLocation,
        },
    };
    try context.genInfo.appendChunk(allocator, setReg);

    const storeInstr = Instr{
        .Store64AtRegPostInc16 = .{
            .toRegPtr = sliceStartPtr,
            .fromReg = tempReg,
            .inc = vmInfo.POINTER_SIZE,
        },
    };
    try context.genInfo.appendChunk(allocator, storeInstr);

    if (regLocationOrNull == null) {
        context.genInfo.currentProc.stackFrameSize += node.typeInfo.size;
    }

    const setLenReg = Instr{
        .SetReg64 = .{
            .reg = tempReg,
            .data = len,
        },
    };
    try context.genInfo.appendChunk(allocator, setLenReg);

    const storeLenInstr = Instr{
        .Store64AtRegPostInc16 = .{
            .toRegPtr = sliceStartPtr,
            .fromReg = tempReg,
            .inc = vmInfo.POINTER_SIZE,
        },
    };
    try context.genInfo.appendChunk(allocator, storeLenInstr);

    if (writeLoc != null) {
        return null;
    } else {
        const subInstr = Instr{
            .Sub8 = .{
                .dest = writeLocInfo.reg,
                .reg = writeLocInfo.reg,
                .data = vmInfo.POINTER_SIZE * 2,
            },
        };
        try context.genInfo.appendChunk(allocator, subInstr);

        return writeLocInfo.reg;
    }

    return null;
}

fn prepForLoopCondition(context: *Context, condition: *ast.AstNode) LoopCondInfo {
    const prevCmpAsReg = context.genInfo.settings.outputCmpAsRegister;
    const isCompExprType = if (condition.variant == .OpExpr)
        switch (condition.variant.OpExpr.type) {
            .LessThan,
            .GreaterThan,
            .LessThanEq,
            .GreaterThanEq,
            .Equal,
            => true,
            else => false,
        }
    else
        false;

    if (isCompExprType) {
        context.genInfo.settings.outputCmpAsRegister = false;
    }

    return .{
        .prevCmpAsReg = prevCmpAsReg,
        .isCompExpr = isCompExprType,
    };
}

fn setJumpAmount(instr: *Instr, label: vmInfo.LabelType) void {
    switch (instr.*) {
        .Jump,
        .JumpEQ,
        .JumpNE,
        .JumpGT,
        .JumpLT,
        .JumpGTE,
        .JumpLTE,
        .JumpBack,
        .JumpBackEQ,
        .JumpBackNE,
        .JumpBackGT,
        .JumpBackLT,
        .JumpBackGTE,
        .JumpBackLTE,
        => |*labelId| {
            labelId.* = label;
        },
        else => unreachable,
    }
}

fn exprTypeToCmpSetReg(expr: ast.OpExprTypes) Instr {
    return switch (expr) {
        .Equal => .{ .CmpSetRegEQ = .{} },
        .NotEqual => .{ .CmpSetRegNE = .{} },
        .LessThan => .{ .CmpSetRegLT = .{} },
        .GreaterThan => .{ .CmpSetRegGT = .{} },
        .LessThanEq => .{ .CmpSetRegLTE = .{} },
        .GreaterThanEq => .{ .CmpSetRegGTE = .{} },
        else => unreachable,
    };
}

fn compOpToJump(opType: ast.OpExprTypes, labelId: vmInfo.LabelType, back: bool) !Instr {
    return if (back) switch (opType) {
        .Equal => .{ .JumpBackEQ = labelId },
        .NotEqual => .{ .JumpBackNE = labelId },
        .GreaterThan => .{ .JumpBackGT = labelId },
        .LessThan => .{ .JumpBackLT = labelId },
        .GreaterThanEq => .{ .JumpBackGTE = labelId },
        .LessThanEq => .{ .JumpBackLTE = labelId },
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    } else switch (opType) {
        .Equal => .{ .JumpEQ = labelId },
        .NotEqual => .{ .JumpNE = labelId },
        .GreaterThan => .{ .JumpGT = labelId },
        .LessThan => .{ .JumpLT = labelId },
        .GreaterThanEq => .{ .JumpGTE = labelId },
        .LessThanEq => .{ .JumpLTE = labelId },
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    };
}

fn generateFallback(
    allocator: Allocator,
    context: *Context,
    fallback: ast.FallbackInfo,
) !void {
    var jumpLabelId: ?vmInfo.LabelType = null;

    const statement = fallback.node.variant.IfStatement;

    if (fallback.hasCondition) {
        const condReg = try genBytecode(allocator, context, statement.condition) orelse
            return CodeGenError.ReturnedRegisterNotFound;

        const instr = Instr{
            .CmpConst8 = .{
                .reg = condReg,
                .data = 1,
            },
        };
        try context.genInfo.appendChunk(allocator, instr);

        jumpLabelId = context.genInfo.takeLabelId();
        const jumpInstr = Instr{ .JumpNE = jumpLabelId.? };
        try context.genInfo.appendChunk(allocator, jumpInstr);
    }

    _ = try genBytecode(allocator, context, statement.body);

    if (statement.fallback) |newFallback| {
        const jumpEndLabelId = context.genInfo.takeLabelId();
        const jumpEndInstr = Instr{ .Jump = jumpEndLabelId };
        try context.genInfo.appendChunk(allocator, jumpEndInstr);

        if (jumpLabelId) |labelId| {
            const jumpLabel = Instr{ .Label = labelId };
            try context.genInfo.appendChunk(allocator, jumpLabel);
        }

        try generateFallback(allocator, context, newFallback);
        const jumpEndLabel = Instr{ .Label = jumpEndLabelId };
        try context.genInfo.appendChunk(allocator, jumpEndLabel);
    }
}

fn writeChunk(instr: Instr, writer: *Writer) !void {
    if (instr == .Label or instr == .NoOp) return;

    try writer.writeByte(instr.getInstrByte());

    switch (instr) {
        .Label, .NoOp => unreachable,
        .Ret => {},
        .SetReg64 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writeNumber(u64, inner.data, writer);
        },
        .SetReg32 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writeNumber(u32, inner.data, writer);
        },
        .SetReg16 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writeNumber(u16, inner.data, writer);
        },
        .SetReg8 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writeNumber(u8, inner.data, writer);
        },
        .Add,
        .Sub,
        .Mult,
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        .Xor,
        => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .Add8, .Sub8 => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeByte(@intCast(inner.data));
        },
        .Add16, .Sub16 => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u16, @intCast(inner.data), .little);
        },
        .Jump,
        .JumpEQ,
        .JumpNE,
        .JumpGT,
        .JumpLT,
        .JumpGTE,
        .JumpLTE,
        .JumpBack,
        .JumpBackEQ,
        .JumpBackNE,
        .JumpBackGT,
        .JumpBackLT,
        .JumpBackGTE,
        .JumpBackLTE,
        => |inner| {
            try writer.writeInt(u32, @intCast(inner), .little);
        },
        .Cmp => |inner| {
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .CmpConst8, .IncConst8, .DecConst8 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeByte(inner.data);
        },
        .Mov => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.src));
        },
        .MovSpNegOffsetAny => unreachable,
        .MovSpNegOffset16 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u16, inner.offset, .little);
        },
        .MovSpNegOffset32 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u32, inner.offset, .little);
        },
        .MovSpNegOffset64 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u64, inner.offset, .little);
        },
        .XorConst8 => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeByte(inner.byte);
        },
        .AddSp8, .SubSp8 => |inner| {
            try writer.writeByte(inner);
        },
        .AddSp16, .SubSp16 => |inner| {
            try writer.writeInt(u16, inner, .little);
        },
        .AddSp32, .SubSp32 => |inner| {
            try writer.writeInt(u32, inner, .little);
        },
        .AddSp64, .SubSp64 => |inner| {
            try writer.writeInt(u64, inner, .little);
        },
        .Store64AtReg,
        .Store32AtReg,
        .Store16AtReg,
        .Store8AtReg,
        => |inner| {
            try writer.writeByte(@intCast(inner.fromReg));
            try writer.writeByte(@intCast(inner.toRegPtr));
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |inner| {
            try writer.writeByte(@intCast(inner.fromReg));
            try writer.writeByte(@intCast(inner.toRegPtr));
            try writer.writeInt(u16, inner.inc, .little);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u16, inner.offset, .little);
        },
        .Load64AtReg,
        .Load32AtReg,
        .Load16AtReg,
        .Load8AtReg,
        => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.fromRegPtr));
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.fromRegPtr));
            try writer.writeInt(u16, inner.offset, .little);
        },
        .MulReg16AddReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.addReg));
            try writer.writeByte(@intCast(inner.mulReg));
            try writer.writeInt(u16, inner.data, .little);
        },
        .DbgReg => |reg| {
            try writer.writeByte(@intCast(reg));
        },
        .BitAnd, .BitOr => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .And, .Or => |inner| {
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .AndSetReg, .OrSetReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .PrePushRegNegOffsetAny, .PostPopRegNegOffsetAny => unreachable,
        .PrePushRegNegOffset8, .PostPopRegNegOffset8 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeByte(inner.offset);
        },
        .PrePushRegNegOffset16, .PostPopRegNegOffset16 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u16, inner.offset, .little);
        },
        .PrePushRegNegOffset32, .PostPopRegNegOffset32 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u32, inner.offset, .little);
        },
        .PrePushRegNegOffset64, .PostPopRegNegOffset64 => |inner| {
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u64, inner.offset, .little);
        },
    }
}

fn writeNumber(comptime T: type, data: T, writer: *Writer) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
