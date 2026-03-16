const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;
const MemoryPool = std.heap.MemoryPool;
const builtin = @import("builtin");

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const bytecodeBackend = blitz.backends.bytecode;
const TempRegister = vmInfo.TempRegister;
const Context = blitz.context.Context;
const constants = blitz.constants;
const backendUtils = blitz.backendUtils;

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
    MainFunctionNotFound,
};
const GenBytecodeError = CodeGenError ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    ast.AstTypeError;

pub const BackendTypes = enum {
    Bytecode,
};

pub const BackendInterface = struct {
    initMetadata: fn (Allocator, *Context) Allocator.Error!void,
    allocateRegisters: fn (
        Allocator,
        *Context,
        []Instr,
        usize,
        *u64,
    ) Allocator.Error!void,
};

const RegisterUsage = enum {
    const Self = @This();

    Param,
    ParamNext,
    Return,
    ReturnNext,
    Preserved,
    Temporary,

    pub fn isParam(self: Self) bool {
        return switch (self) {
            .Param, .ParamNext => true,
            else => false,
        };
    }
};

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

const InstrInfo = struct {
    len: u8,
    opCount: u8,
    text: []const u8,
};

const instrDataArr = [_]InstrInfo{
    .{ // 0
        .len = 0,
        .opCount = 0,
        .text = "noop",
    },
    .{ // 1
        .len = 0,
        .opCount = 0,
        .text = "(label)",
    },
    .{ // 2
        .len = 10,
        .opCount = 1,
        .text = "set_reg_64",
    },
    .{ // 3
        .len = 6,
        .opCount = 1,
        .text = "set_reg_32",
    },
    .{ // 4
        .len = 4,
        .opCount = 1,
        .text = "set_reg_16",
    },
    .{ // 5
        .len = 3,
        .opCount = 1,
        .text = "set_reg_8",
    },
    .{ // 6
        .len = 4,
        .opCount = 3,
        .text = "add",
    },
    .{ // 7
        .len = 4,
        .opCount = 3,
        .text = "sub",
    },
    .{ // 8
        .len = 4,
        .opCount = 3,
        .text = "mult",
    },
    .{ // 9
        .len = 4,
        .opCount = 2,
        .text = "add_8",
    },
    .{ // 10
        .len = 4,
        .opCount = 2,
        .text = "sub_8",
    },
    .{ // 11
        .len = 5,
        .opCount = 2,
        .text = "add_16",
    },
    .{ // 12
        .len = 5,
        .opCount = 2,
        .text = "sub_16",
    },
    .{ // 13
        .len = 5,
        .opCount = 0,
        .text = "jump",
    },
    .{ // 14
        .len = 5,
        .opCount = 0,
        .text = "jump_eq",
    },
    .{ // 15
        .len = 5,
        .opCount = 0,
        .text = "jump_ne",
    },
    .{ // 16
        .len = 5,
        .opCount = 0,
        .text = "jump_gt",
    },
    .{ // 17
        .len = 5,
        .opCount = 0,
        .text = "jump_lt",
    },
    .{ // 18
        .len = 5,
        .opCount = 0,
        .text = "jump_gte",
    },
    .{ // 19
        .len = 5,
        .opCount = 0,
        .text = "jump_lte",
    },
    .{ // 20
        .len = 5,
        .opCount = 0,
        .text = "jump_back",
    },
    .{ // 21
        .len = 5,
        .opCount = 0,
        .text = "jump_back_eq",
    },
    .{ // 22
        .len = 5,
        .opCount = 0,
        .text = "jump_back_ne",
    },
    .{ // 23
        .len = 5,
        .opCount = 0,
        .text = "jump_back_gt",
    },
    .{ // 24
        .len = 5,
        .opCount = 0,
        .text = "jump_back_lt",
    },
    .{ // 25
        .len = 5,
        .opCount = 0,
        .text = "jump_back_gte",
    },
    .{ // 26
        .len = 5,
        .opCount = 0,
        .text = "jump_back_lte",
    },
    .{ // 27
        .len = 3,
        .opCount = 2,
        .text = "cmp",
    },
    .{ // 28
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_eq",
    },
    .{ // 29
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_ne",
    },
    .{ // 30
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_gt",
    },
    .{ // 31
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_lt",
    },
    .{ // 32
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_gte",
    },
    .{ // 33
        .len = 4,
        .opCount = 3,
        .text = "cmp_set_reg_lte",
    },
    .{ // 34
        .len = 3,
        .opCount = 1,
        .text = "cmp_const_8",
    },
    .{ // 35
        .len = 3,
        .opCount = 1,
        .text = "inc_const_8",
    },
    .{ // 36
        .len = 3,
        .opCount = 1,
        .text = "dec_const_8",
    },
    .{ // 37
        .len = 3,
        .opCount = 2,
        .text = "mov",
    },
    .{ // 38
        .len = undefined, // SHOULD NOT EXIST FOR WRITER
        .opCount = 1,
        .text = "mov_sp_neg_offset_ANY",
    },
    .{ // 39
        .len = 4,
        .opCount = 1,
        .text = "mov_sp_neg_offset_16",
    },
    .{ // 40
        .len = 6,
        .opCount = 1,
        .text = "mov_sp_neg_offset_32",
    },
    .{ // 41
        .len = 10,
        .opCount = 1,
        .text = "mov_sp_neg_offset_64",
    },
    .{ // 42
        .len = 4,
        .opCount = 3,
        .text = "xor",
    },
    .{ // 43
        .len = 4,
        .opCount = 2,
        .text = "xor_const_8",
    },
    .{ // 44
        .len = 2,
        .opCount = 0,
        .text = "add_sp_8",
    },
    .{ // 45
        .len = 2,
        .opCount = 0,
        .text = "sub_sp_8",
    },
    .{ // 46
        .len = 3,
        .opCount = 0,
        .text = "add_sp_16",
    },
    .{ // 47
        .len = 3,
        .opCount = 0,
        .text = "sub_sp_16",
    },
    .{ // 48
        .len = 5,
        .opCount = 0,
        .text = "add_sp_32",
    },
    .{ // 49
        .len = 5,
        .opCount = 0,
        .text = "sub_sp_32",
    },
    .{ // 50
        .len = 9,
        .opCount = 0,
        .text = "add_sp_64",
    },
    .{ // 51
        .len = 9,
        .opCount = 0,
        .text = "sub_sp_64",
    },
    .{ // 52
        .len = 3,
        .opCount = 2,
        .text = "store_64_at_reg",
    },
    .{ // 53
        .len = 3,
        .opCount = 2,
        .text = "store_32_at_reg",
    },
    .{ // 54
        .len = 3,
        .opCount = 2,
        .text = "store_16_at_reg",
    },
    .{ // 55
        .len = 3,
        .opCount = 2,
        .text = "store_8_at_reg",
    },
    .{ // 56
        .len = 5,
        .opCount = 2,
        .text = "store_64_at_reg_post_inc_16",
    },
    .{ // 57
        .len = 5,
        .opCount = 2,
        .text = "store_32_at_reg_post_inc_16",
    },
    .{ // 58
        .len = 5,
        .opCount = 2,
        .text = "store_16_at_reg_post_inc_16",
    },
    .{ // 59
        .len = 5,
        .opCount = 2,
        .text = "store_8_at_reg_post_inc_16",
    },
    .{ // 60
        .len = 4,
        .opCount = 1,
        .text = "store_64_at_sp_neg_offset_16",
    },
    .{ // 61
        .len = 4,
        .opCount = 1,
        .text = "store_32_at_sp_neg_offset_16",
    },
    .{ // 62
        .len = 4,
        .opCount = 1,
        .text = "store_16_at_sp_neg_offset_16",
    },
    .{ // 63
        .len = 4,
        .opCount = 1,
        .text = "store_8_at_sp_neg_offset_16",
    },
    .{ // 64
        .len = 5,
        .opCount = 2,
        .text = "load_64_at_reg_offset_16",
    },
    .{ // 65
        .len = 5,
        .opCount = 2,
        .text = "load_32_at_reg_offset_16",
    },
    .{ // 66
        .len = 5,
        .opCount = 2,
        .text = "load_16_at_reg_offset_16",
    },
    .{ // 67
        .len = 5,
        .opCount = 2,
        .text = "load_8_at_reg_offset_16",
    },
    .{ // 68
        .len = 4,
        .opCount = 1,
        .text = "load_64_at_sp_neg_offset_16",
    },
    .{ // 69
        .len = 4,
        .opCount = 1,
        .text = "load_32_at_sp_neg_offset_16",
    },
    .{ // 70
        .len = 4,
        .opCount = 1,
        .text = "load_16_at_sp_neg_offset_16",
    },
    .{ // 71
        .len = 4,
        .opCount = 1,
        .text = "load_8_at_sp_neg_offset_16",
    },
    .{ // 72
        .len = 3,
        .opCount = 2,
        .text = "load_64_at_reg",
    },
    .{ // 73
        .len = 3,
        .opCount = 2,
        .text = "load_32_at_reg",
    },
    .{ // 74
        .len = 3,
        .opCount = 2,
        .text = "load_16_at_reg",
    },
    .{ // 75
        .len = 3,
        .opCount = 2,
        .text = "load_8_at_reg",
    },
    .{ // 76
        .len = 6,
        .opCount = 3,
        .text = "mul_reg_16_add_reg",
    },
    .{ // 77
        .len = 2,
        .opCount = 1,
        .text = "dbg_reg",
    },
    .{ // 78
        .len = 4,
        .opCount = 3,
        .text = "bit_and",
    },
    .{ // 79
        .len = 4,
        .opCount = 3,
        .text = "bit_or",
    },
    .{ // 80
        .len = 3,
        .opCount = 2,
        .text = "and",
    },
    .{ // 81
        .len = 3,
        .opCount = 2,
        .text = "or",
    },
    .{ // 82
        .len = 4,
        .opCount = 3,
        .text = "and_set_reg",
    },
    .{ // 83
        .len = 4,
        .opCount = 3,
        .text = "or_set_reg",
    },
    .{ // 84
        .len = undefined, // SHOULD NOT EXIST FOR WRITER
        .opCount = 1,
        .text = "push_reg_neg_offset_ANY",
    },
    .{ // 85
        .len = 3,
        .opCount = 1,
        .text = "push_reg_neg_offset_8",
    },
    .{ // 86
        .len = 4,
        .opCount = 1,
        .text = "push_reg_neg_offset_16",
    },
    .{ // 87
        .len = 6,
        .opCount = 1,
        .text = "push_reg_neg_offset_32",
    },
    .{ // 88
        .len = 10,
        .opCount = 1,
        .text = "push_reg_neg_offset_64",
    },
    .{ // 89
        .len = undefined, // SHOULD NOT EXIST FOR WRITER
        .opCount = 0,
        .text = "pop_reg_neg_offset_ANY",
    },
    .{ // 90
        .len = 3,
        .opCount = 0,
        .text = "pop_reg_neg_offset_8",
    },
    .{ // 91
        .len = 4,
        .opCount = 0,
        .text = "pop_reg_neg_offset_16",
    },
    .{ // 92
        .len = 6,
        .opCount = 0,
        .text = "pop_reg_neg_offset_32",
    },
    .{ // 93
        .len = 10,
        .opCount = 0,
        .text = "pop_reg_neg_offset_64",
    },
    .{ // 94
        .len = undefined, // SHOULD NOT EXIST FOR WRITER
        .opCount = 0,
        .text = "pre_push_lr_neg_offset_any",
    },
    .{ // 95
        .len = 2,
        .opCount = 0,
        .text = "pre_push_lr_neg_offset_8",
    },
    .{ // 96
        .len = 3,
        .opCount = 0,
        .text = "pre_push_lr_neg_offset_16",
    },
    .{ // 97
        .len = 5,
        .opCount = 0,
        .text = "pre_push_lr_neg_offset_32",
    },
    .{ // 98
        .len = 9,
        .opCount = 0,
        .text = "pre_push_lr_neg_offset_64",
    },
    .{ // 99
        .len = undefined, // SHOULD NOT EXIST FOR WRITER
        .opCount = 0,
        .text = "post_pop_lr_neg_offset_any",
    },
    .{ // 100
        .len = 2,
        .opCount = 0,
        .text = "post_pop_lr_neg_offset_8",
    },
    .{ // 101
        .len = 3,
        .opCount = 0,
        .text = "post_pop_lr_neg_offset_16",
    },
    .{ // 102
        .len = 5,
        .opCount = 0,
        .text = "post_pop_lr_neg_offset_32",
    },
    .{ // 103
        .len = 9,
        .opCount = 0,
        .text = "post_pop_lr_neg_offset_64",
    },
    .{ // 104
        .len = 1,
        .opCount = 0,
        .text = "ret",
    },
    .{ // 105
        .len = 1,
        .opCount = 0,
        .text = "end",
    },
    .{ // 106
        .len = 5,
        .opCount = 0,
        .text = "branch_link",
    },
    .{ // 107
        .len = 5,
        .opCount = 0,
        .text = "branch_link_back",
    },
};

pub const InstructionVariants = enum(u8) {
    const Self = @This();

    NoOp = 0, // 0B (not in output)
    Label = 1, // 0B (not in output)

    SetReg64 = 2, // inst, reg, 8B data
    SetReg32 = 3, // inst, reg, 4B data
    SetReg16 = 4, // inst, reg, 2B data
    SetReg8 = 5, // inst, reg, 1B data

    Add = 6, // inst, out reg, reg1, reg2
    Add8 = 7, // inst, out reg, reg1, 1B data
    Add16 = 8, // inst, out reg, reg1, 2B data
    Sub = 9, // inst, out reg, reg1, reg2
    Sub8 = 10, // inst, out reg, reg1, 1B data
    Sub16 = 11, // inst, out reg, reg1, 2B data
    Mult = 12, // inst, out reg, reg1, reg2

    Jump = 13, // inst, 4B data
    JumpEQ = 14, // inst, 4B data
    JumpNE = 15, // inst, 4B data
    JumpGT = 16, // inst, 4B data
    JumpLT = 17, // inst, 4B data
    JumpGTE = 18, // inst, 4B data
    JumpLTE = 19, // inst, 4B data
    JumpBack = 20, // inst, 4B data
    JumpBackEQ = 21, // inst, 4B data
    JumpBackNE = 22, // inst, 4B data
    JumpBackGT = 23, // inst, 4B data
    JumpBackLT = 24, // inst, 4B data
    JumpBackGTE = 25, // inst, 4B data
    JumpBackLTE = 26, // inst, 4B data

    Cmp = 27, // inst, reg1, reg2  ;  sets to flags
    CmpSetRegEQ = 28, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegNE = 29, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegGT = 30, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegLT = 31, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegGTE = 32, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpSetRegLTE = 33, // inst, out reg, reg1, reg2  ;  sets to flags
    CmpConst8 = 34, // inst, reg1, 1B data

    IncConst8 = 35, // inst, in/out reg, 1B data
    DecConst8 = 36, // inst, in/out reg, 1B data

    Mov = 37, // inst, reg1, reg2
    MovSpNegOffsetAny = 38, // inst, dest reg, offset (TBD by compiler)B
    MovSpNegOffset16 = 39, // inst, dest reg, offset 2B
    MovSpNegOffset32 = 40, // inst, dest reg, offset 4B
    MovSpNegOffset64 = 41, // inst, dest reg, offset 8B

    Xor = 42, // inst, out reg, reg1, reg2
    XorConst8 = 43, // inst, out reg, reg1, 1B data

    AddSp8 = 44, // inst, 1B data
    SubSp8 = 45, // inst, 1B data
    AddSp16 = 46, // inst, 2B data
    SubSp16 = 47, // inst, 2B data
    AddSp32 = 48, // inst, 4B data
    SubSp32 = 49, // inst, 4B data
    AddSp64 = 50, // inst, 8B data
    SubSp64 = 51, // inst, 8B data

    Store64AtReg = 52, // inst, reg, to reg (ptr)
    Store32AtReg = 53, // inst, reg, to reg (ptr)
    Store16AtReg = 54, // inst, reg, to reg (ptr)
    Store8AtReg = 55, // inst, reg, to reg (ptr)

    Store64AtRegPostInc16 = 56, // inst, reg, to reg (ptr), inc 2B
    Store32AtRegPostInc16 = 57, // inst, reg, to reg (ptr), inc 2B
    Store16AtRegPostInc16 = 58, // inst, reg, to reg (ptr), inc 2B
    Store8AtRegPostInc16 = 59, // inst, reg, to reg (ptr), inc 2B

    Store64AtSpNegOffset16 = 60, // inst, reg, offset 2B
    Store32AtSpNegOffset16 = 61, // inst, reg, offset 2B
    Store16AtSpNegOffset16 = 62, // inst, reg, offset 2B
    Store8AtSpNegOffset16 = 63, // inst, reg, offset 2B

    Load64AtRegOffset16 = 64, // inst, dest reg, from reg (ptr), offset 2B
    Load32AtRegOffset16 = 65, // inst, dest reg, from reg (ptr), offset 2B
    Load16AtRegOffset16 = 66, // inst, dest reg, from reg (ptr), offset 2B
    Load8AtRegOffset16 = 67, // inst, dest reg, from reg (ptr), offset 2B

    Load64AtSpNegOffset16 = 68, // inst, dest reg, offset 2B
    Load32AtSpNegOffset16 = 69, // inst, dest reg, offset 2B
    Load16AtSpNegOffset16 = 70, // inst, dest reg, offset 2B
    Load8AtSpNegOffset16 = 71, // inst, dest reg, offset 2B

    Load64AtReg = 72, // inst, dest reg, from reg (ptr)
    Load32AtReg = 73, // inst, dest reg, from reg (ptr)
    Load16AtReg = 74, // inst, dest reg, from reg (ptr)
    Load8AtReg = 75, // inst, dest reg, from reg (ptr)

    MulReg16AddReg = 76, // inst, dest reg, addReg, mulReg, data 2B ( dest = addReg + (mulReg1 * data) )

    DbgReg = 77, // inst, reg

    BitAnd = 78, // inst, dest reg, reg1, reg2
    BitOr = 79, // inst, dest reg, reg1, reg2

    And = 80, // inst, reg1, reg2  ;  sets to flags
    Or = 81, // inst, reg1, reg2  ;  sets to flags

    AndSetReg = 82, // inst, dest reg, reg1, reg2
    OrSetReg = 83, // inst, dest reg, reg1, reg2

    PrePushRegNegOffsetAny = 84, // inst, top reg, offset (TBD by compiler)B
    PrePushRegNegOffset8 = 85, // inst, top reg, offset 1B
    PrePushRegNegOffset16 = 86, // inst, top reg, offset 2B
    PrePushRegNegOffset32 = 87, // inst, top reg, offset 4B
    PrePushRegNegOffset64 = 88, // inst, top reg, offset 8B

    PostPopRegNegOffsetAny = 89, // inst, top reg, offset (TBD by compiler)B
    PostPopRegNegOffset8 = 90, // inst, top reg, offset 1B
    PostPopRegNegOffset16 = 91, // inst, top reg, offset 2B
    PostPopRegNegOffset32 = 92, // inst, top reg, offset 4B
    PostPopRegNegOffset64 = 93, // inst, top reg, offset 8B

    PrePushLRNegOffsetAny = 94, // inst, offset (TBD by compiler)B
    PrePushLRNegOffset8 = 95, // inst, offset 1B
    PrePushLRNegOffset16 = 96, // inst, offset 2B
    PrePushLRNegOffset32 = 97, // inst, offset 4B
    PrePushLRNegOffset64 = 98, // inst, offset 8B

    PostPopLRNegOffsetAny = 99, // inst, offset (TBD by compiler)B
    PostPopLRNegOffset8 = 100, // inst, offset 1B
    PostPopLRNegOffset16 = 101, // inst, offset 2B
    PostPopLRNegOffset32 = 102, // inst, offset 4B
    PostPopLRNegOffset64 = 103, // inst, offset 8B

    Ret = 104,
    End = 105,

    BranchLink = 106, // inst, 4B data
    BranchLinkBack = 107, // inst, 4B data

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn getInstrInfo(self: Self) InstrInfo {
        return instrDataArr[@intFromEnum(self)];
    }

    fn instrIsAny(self: Self) bool {
        return switch (self) {
            .MovSpNegOffsetAny,
            .PrePushRegNegOffsetAny,
            .PostPopRegNegOffsetAny,
            .PrePushLRNegOffsetAny,
            .PostPopLRNegOffsetAny,
            => true,
            else => false,
        };
    }

    fn ensureNotAny(self: Self) void {
        if (self.instrIsAny()) @panic("Cannot get length of variable length instr");
    }

    pub fn getInstrLen(self: Self) u8 {
        self.ensureNotAny();
        return self.getInstrInfo().len;
    }

    pub fn getOpCount(self: Self) u8 {
        return self.getInstrInfo().opCount;
    }

    pub fn toString(self: Self) []const u8 {
        return self.getInstrInfo().text;
    }

    pub fn maxOpCount() comptime_int {
        var max = 0;
        inline for (@typeInfo(Self).@"enum".fields) |field| {
            const val: Self = @enumFromInt(field.value);
            if (val.instrIsAny()) continue;
            max = @max(val.getOpCount(), max);
        }
        return max;
    }

    pub fn maxInstrSize() comptime_int {
        var max = 0;
        inline for (@typeInfo(Self).@"enum".fields) |field| {
            const val: Self = @enumFromInt(field.value);
            if (val.instrIsAny()) continue;
            max = @max(val.getInstrLen(), max);
        }
        return max;
    }
};

const TwoOpResultInstr = struct {
    dest: TempRegister,
    reg1: TempRegister,
    reg2: TempRegister,
};

fn OneOpResultInstr(comptime T: type) type {
    return struct {
        dest: TempRegister,
        reg: TempRegister,
        data: T,
    };
}

const RegBytePayloadInstr = struct {
    reg: TempRegister,
    data: u8,
};

const MathInstr = TwoOpResultInstr;

const SpInstr = struct {
    amount: TempRegister,
};

const SpRegInstr = struct {
    reg: TempRegister,
};

fn SetRegInstr(comptime T: type) type {
    return struct {
        reg: TempRegister,
        data: T,
    };
}

const CmpInstr = struct {
    reg1: TempRegister = 0,
    reg2: TempRegister = 0,
};

const CmpSetRegInstr = struct {
    dest: TempRegister = 0,
    reg1: TempRegister = 0,
    reg2: TempRegister = 0,
};

fn StoreOffsetInstr(comptime T: type) type {
    return struct {
        fromReg: TempRegister,
        toRegPtr: TempRegister,
        offset: T,
    };
}

fn StoreOrLoadOffsetSpInstr(comptime T: type) type {
    return struct {
        reg: TempRegister,
        offset: T,
    };
}

fn StoreAtRegIncInstr(comptime T: type) type {
    return struct {
        fromReg: TempRegister,
        toRegPtr: TempRegister,
        inc: T,
    };
}

const StoreAtRegInstr = struct {
    fromReg: TempRegister,
    toRegPtr: TempRegister,
};

fn StoreIncSpInstr(comptime T: type) type {
    return struct {
        reg: TempRegister,
        inc: T,
    };
}

const LoadAtReg = struct {
    dest: TempRegister,
    fromRegPtr: TempRegister,
};

const LoadAtRegOffset16 = struct {
    dest: TempRegister,
    fromRegPtr: TempRegister,
    offset: u16,
};

fn MulRegTAddReg(comptime T: type) type {
    return struct {
        dest: TempRegister,
        addReg: TempRegister,
        mulReg: TempRegister,
        data: T,
    };
}

fn MovSpNegOffset(comptime T: type) type {
    return struct {
        reg: TempRegister,
        offset: T,
    };
}

fn PushOrPopRegNegOffset(comptime T: type) type {
    return struct {
        reg: TempRegister,
        offset: T,
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

    Store64AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store32AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store16AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store8AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),

    Load64AtRegOffset16: LoadAtRegOffset16,
    Load32AtRegOffset16: LoadAtRegOffset16,
    Load16AtRegOffset16: LoadAtRegOffset16,
    Load8AtRegOffset16: LoadAtRegOffset16,

    Load64AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load32AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load16AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load8AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),

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

    PrePushLRNegOffsetAny: u64,
    PrePushLRNegOffset8: u8,
    PrePushLRNegOffset16: u16,
    PrePushLRNegOffset32: u32,
    PrePushLRNegOffset64: u64,

    PostPopLRNegOffsetAny: u64,
    PostPopLRNegOffset8: u8,
    PostPopLRNegOffset16: u16,
    PostPopLRNegOffset32: u32,
    PostPopLRNegOffset64: u64,

    Ret: void,
    End: void,

    BranchLink: u64,
    BranchLinkBack: u64,

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

const CtrlFlowInstrInfo = struct {
    chunk: *Instr,
    label: vmInfo.LabelType,
};

const LoopInfo = struct {
    const Self = @This();

    continueLabel: vmInfo.LabelType,
    breaks: *ArrayList(CtrlFlowInstrInfo),
    continues: *ArrayList(CtrlFlowInstrInfo),

    pub fn init(allocator: Allocator) !Self {
        const breaksPtr = try utils.createMut(ArrayList(CtrlFlowInstrInfo), allocator, .empty);
        const continuesPtr = try utils.createMut(ArrayList(CtrlFlowInstrInfo), allocator, .empty);

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

const GenInfoSettings = struct {
    // respected for one expr node, then set to default
    outputCmpAsRegister: bool = true,

    propertyAccessReturnsPointer: bool = false,
};

const RegInfoVarInfo = struct {
    stackLocation: ?u64 = null,
    prevInfo: ?*RegInfoVarInfo = null,
};

const RegStateVariants = enum {
    Unused,
    Normal,
    Spilled,
};

pub const RegStateSpilled = struct {
    by: TempRegister,
    reg: TempRegister,
    until: u32,
    location: u64,
};

pub const RegStateInfo = union(RegStateVariants) {
    Unused,
    Normal: TempRegister,
    Spilled: RegStateSpilled,
};

pub const RegState = struct {
    prevState: ?*RegState = null,
    info: RegStateInfo = .Unused,
};

pub const RegStatus = struct {
    active: bool = false,
    state: RegState = .{},
};

pub const RegUseIndices = struct {
    const Self = @This();

    indices: *ArrayList(u32),
    baseIndex: usize = 0,
    currentIndex: usize = 0,

    pub fn init(allocator: Allocator) !Self {
        const indicesPtr = try utils.createMut(ArrayList(u32), allocator, .empty);
        return .{ .indices = indicesPtr };
    }

    /// should only ever be called with at least one index
    pub fn first(self: Self) u32 {
        return self.indices.items[0];
    }

    /// should only ever be called with at least one index
    pub fn last(self: Self) u32 {
        return self.indices.items[self.indices.items.len - 1];
    }

    pub fn current(self: Self) ?u32 {
        if (self.currentIndex >= self.indices.items.len) return null;
        return self.indices.items[self.currentIndex];
    }

    pub fn baseNext(self: *Self) void {
        self.baseIndex += 1;
        self.currentIndex = self.baseIndex;
    }

    pub fn next(self: *Self) void {
        self.currentIndex += 1;
    }

    pub fn resetIter(self: *Self) void {
        self.currentIndex = self.baseIndex;
    }
};

const RegRemapStateVariants = enum {
    Unused,
    Normal,
    Stored,
    Spilled,
};

const RegRemap = union(RegRemapStateVariants) {
    Unused,
    Normal: vmInfo.TempRegister,
    Stored: struct {
        byVReg: vmInfo.TempRegister,
        location: u64,
    },
    Spilled: struct {
        byVReg: vmInfo.TempRegister,
        reg: vmInfo.TempRegister,
    },
};

pub const RegInfo = struct {
    const Self = @This();

    varInfo: ?RegInfoVarInfo = null,
    usage: RegisterUsage = .Temporary,
    useIndices: RegUseIndices,
    regRemap: RegRemap = .Unused,

    pub fn init(allocator: Allocator) !Self {
        return .{ .useIndices = try RegUseIndices.init(allocator) };
    }

    pub fn initWithUsage(allocator: Allocator, usage: RegisterUsage) !Self {
        return .{
            .usage = usage,
            .useIndices = try RegUseIndices.init(allocator),
        };
    }
};

const LabelInfo = struct {
    byte: u64 = 0,
    exists: bool = false,
};

const LabelByteInfo = struct {
    const Self = @This();
    const WaitingLabelsList = ArrayList(*Instr);
    const WaitingLabels = std.AutoHashMap(vmInfo.LabelType, *WaitingLabelsList);

    labelInfo: *ArrayList(LabelInfo),
    waitingLabels: *WaitingLabels,

    pub fn init(allocator: Allocator) !Self {
        const waitingLabels = WaitingLabels.init(allocator);
        const waitingLabelsPtr = try utils.createMut(WaitingLabels, allocator, waitingLabels);

        const labelInfoPtr = try utils.createMut(ArrayList(LabelInfo), allocator, .empty);

        return .{
            .labelInfo = labelInfoPtr,
            .waitingLabels = waitingLabelsPtr,
        };
    }

    pub fn ensureLabelCount(
        self: *Self,
        allocator: Allocator,
        lastLabel: vmInfo.LabelType,
    ) !void {
        // if label id is 0, no labels have been used
        if (lastLabel == 0) return;

        const prev = self.labelInfo.items.len;
        if (lastLabel <= prev) return;

        try self.labelInfo.ensureTotalCapacity(allocator, lastLabel);
        self.labelInfo.items.len = lastLabel;
        @memset(self.labelInfo.items[prev..], .{});
    }

    pub fn getLabelLocation(self: *Self, labelId: vmInfo.LabelType) !?u64 {
        if (labelId >= self.labelInfo.items.len) {
            return CodeGenError.LabelDoesNotExist;
        }

        const info = self.labelInfo.items[labelId];
        if (!info.exists) return null;
        return info.byte;
    }

    /// labelPtr must point to the label to watch for, and be the memory
    /// location to write the label byte location to
    pub fn waitForLabel(
        self: *Self,
        allocator: Allocator,
        labelId: vmInfo.LabelType,
        instr: *Instr,
    ) !void {
        if (self.waitingLabels.get(labelId)) |arrList| {
            try arrList.append(allocator, instr);
        } else {
            const arrList = try utils.createMut(WaitingLabelsList, allocator, .empty);
            try arrList.append(allocator, instr);
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
    startIndex: u32,
    maxPreserveReg: ?TempRegister = null,
    preProcVirtualReg: TempRegister = 0,
};

pub const RegisterRange = struct {
    // u16 in case there is a backend that wants more than 256 registers
    start: u16 = 0,
    // exclusive
    end: u16 = 0,
};

fn InstrActionInfo(comptime T: type) type {
    return struct {
        const Self = @This();

        action: *ArrayList(T),
        current: usize = 0,

        pub fn init(allocator: Allocator) !Self {
            const actionPtr = try utils.createMut(ArrayList(T), allocator, .empty);

            return Self{
                .action = actionPtr,
            };
        }

        pub fn next(self: *Self) void {
            self.current += 1;
        }

        pub fn getCurrent(self: *Self) ?T {
            if (self.current < self.action.items.len) {
                return self.action.items[self.current];
            }

            return null;
        }
    };
}

pub const InsertInfo = struct {
    instr: Instr,
    pos: u32,
};

pub const InstrActions = struct {
    const Self = @This();

    pub const ActionTypes = enum {
        Insert,
        Skip,

        pub fn toString(self: @This()) []const u8 {
            return switch (self) {
                .Insert => "INSERT",
                .Skip => "SKIP",
            };
        }
    };

    skipInstrInfo: InstrActionInfo(u32),
    insertInstrInfo: InstrActionInfo(InsertInfo),

    pub fn init(allocator: Allocator) !Self {
        return Self{
            .skipInstrInfo = try InstrActionInfo(u32).init(allocator),
            .insertInstrInfo = try InstrActionInfo(InsertInfo).init(allocator),
        };
    }

    pub fn resetIter(self: *Self) void {
        self.skipInstrInfo.current = 0;
        self.insertInstrInfo.current = 0;
    }
};

pub const GenInfo = struct {
    const Self = @This();

    instrList: *ArrayList(Instr),
    currentProc: Proc,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    varNameToReg: *StringHashMap(TempRegister),
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),
    byteCounter: u64,
    currentLabelId: vmInfo.LabelType,
    registers: *ArrayList(*RegInfo),
    registerStatus: *ArrayList(RegStatus),
    registerLimits: struct {
        params: RegisterRange = .{},
        temporary: RegisterRange = .{},
        preserved: RegisterRange = .{},
    } = .{},
    labelByteInfo: *LabelByteInfo,
    instrActions: InstrActions,
    regAllocateUtils: struct {
        furthestInstrReach: u32,
        /// 0 for invalid state
        regNextUseIndex: *ArrayList(u32),
        pendingDeactivations: utils.StaticBufferList(
            vmInfo.TempRegister,
            InstructionVariants.maxOpCount(),
        ),
        protectedRegisters: utils.StaticBufferList(
            vmInfo.TempRegister,
            InstructionVariants.maxOpCount(),
        ),
    },

    pub fn init(allocator: Allocator) !Self {
        const varNameReg = try utils.initMutPtrT(StringHashMap(TempRegister), allocator);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, .empty);

        const labelByteInfo = try LabelByteInfo.init(allocator);
        const labelByteInfoPtr = try utils.createMut(LabelByteInfo, allocator, labelByteInfo);

        const instrListPtr = try utils.createMut(ArrayList(Instr), allocator, .empty);
        const registersPtr = try utils.createMut(ArrayList(*RegInfo), allocator, .empty);
        const activeRegistersPtr = try utils.createMut(ArrayList(RegStatus), allocator, .empty);

        const regNextUseIndexPtr = try utils.createMut(ArrayList(u32), allocator, .empty);

        return .{
            .instrList = instrListPtr,
            .currentProc = .{
                .startIndex = 0,
            },
            .vmInfo = .{
                .stackStartSize = 0,
                .version = 0,
            },
            .varNameToReg = varNameReg,
            .byteCounter = vmInfo.VM_INFO_BYTECODE_LEN,
            .settings = .{},
            .loopInfo = loopInfoPtr,
            .currentLabelId = 0,
            .registers = registersPtr,
            .registerStatus = activeRegistersPtr,
            .labelByteInfo = labelByteInfoPtr,
            .instrActions = try InstrActions.init(allocator),
            .regAllocateUtils = .{
                .furthestInstrReach = 0,
                .regNextUseIndex = regNextUseIndexPtr,
                .pendingDeactivations = .{},
                .protectedRegisters = .{},
            },
        };
    }

    pub fn writeChunks(self: *Self, writer: *Writer) !void {
        try writer.writeByte(self.vmInfo.version);
        var buf: [vmInfo.START_STACK_TYPE_SIZE]u8 = undefined;
        std.mem.writeInt(vmInfo.StartStackType, &buf, self.vmInfo.stackStartSize, .little);
        try writer.writeAll(&buf);

        var i: usize = 0;
        while (i < self.instrList.items.len) : (i += 1) {
            const skipped = self.handleSkipInstruction(i);
            if (!skipped) {
                const instr = self.instrList.items[i];
                try writeChunk(instr, writer);
            }

            while (self.handleInsertInstr(i)) |instr| {
                try writeChunk(instr.*, writer);
            }
        }

        self.instrActions.resetIter();
    }

    pub fn appendChunk(self: *Self, allocator: Allocator, instr: Instr) !void {
        _ = try self.appendChunkAndReturn(allocator, instr);
    }

    pub fn appendChunkAndReturn(self: *Self, allocator: Allocator, instr: Instr) !*Instr {
        try self.instrList.append(allocator, instr);
        const instrIndex = self.instrList.items.len - 1;
        try self.setInstrRegUseIndex(allocator, &self.instrList.items[instrIndex], instrIndex);
        return &self.instrList.items[instrIndex];
    }

    pub fn setInstrRegActiveStatus(self: *Self, instr: *const Instr, instrIndex: usize) void {
        self.applyRegIndexFnToInstr(usize, instr, instrIndex, setRegActiveStatusIndex);
    }

    fn setRegActiveStatusIndex(
        self: *Self,
        reg: TempRegister,
        instrIndex: usize,
    ) void {
        if (self.registers.items[reg].lastUsedIndex) |lastIndex| {
            if (lastIndex == instrIndex) {
                self.registerStatus.items[reg] = false;
                return;
            }
        }

        self.registerStatus.items[reg] = true;
    }

    fn setInstrRegUseIndex(
        self: *Self,
        allocator: Allocator,
        instr: *const Instr,
        instrIndex: usize,
    ) !void {
        try self.applyRegIndexFnToInstr(usize, allocator, instr, instrIndex, addRegUseIndex);
    }

    fn addRegUseIndex(
        self: *Self,
        allocator: Allocator,
        reg: TempRegister,
        instrIndex: usize,
    ) !void {
        try self.registers.items[reg].useIndices.indices.append(allocator, @intCast(instrIndex));
    }

    pub fn applyRegIndexFnToInstr(
        self: *Self,
        comptime T: type,
        allocator: Allocator,
        instr: *const Instr,
        value: T,
        comptime func: fn (*Self, Allocator, TempRegister, T) Allocator.Error!void,
    ) !void {
        switch (instr.*) {
            .NoOp,
            .Label,
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
            .AddSp8,
            .SubSp8,
            .AddSp16,
            .SubSp16,
            .AddSp32,
            .SubSp32,
            .AddSp64,
            .SubSp64,
            .DbgReg,
            .Ret,
            .End,
            .BranchLink,
            .BranchLinkBack,
            .PrePushRegNegOffsetAny,
            .PrePushRegNegOffset8,
            .PrePushRegNegOffset16,
            .PrePushRegNegOffset32,
            .PrePushRegNegOffset64,
            .PostPopRegNegOffsetAny,
            .PostPopRegNegOffset8,
            .PostPopRegNegOffset16,
            .PostPopRegNegOffset32,
            .PostPopRegNegOffset64,
            .PrePushLRNegOffsetAny,
            .PrePushLRNegOffset8,
            .PrePushLRNegOffset16,
            .PrePushLRNegOffset32,
            .PrePushLRNegOffset64,
            .PostPopLRNegOffsetAny,
            .PostPopLRNegOffset8,
            .PostPopLRNegOffset16,
            .PostPopLRNegOffset32,
            .PostPopLRNegOffset64,
            => {},

            .SetReg64 => |inner| try func(self, allocator, inner.reg, value),
            .SetReg32 => |inner| try func(self, allocator, inner.reg, value),
            .SetReg16 => |inner| try func(self, allocator, inner.reg, value),
            .SetReg8 => |inner| try func(self, allocator, inner.reg, value),
            .Add, .Sub, .Mult => |inner| {
                try func(self, allocator, inner.reg1, value);
                try func(self, allocator, inner.reg2, value);
                try func(self, allocator, inner.dest, value);
            },
            .Add8, .Sub8 => |inner| {
                try func(self, allocator, inner.reg, value);
                try func(self, allocator, inner.dest, value);
            },
            .Add16, .Sub16 => |inner| {
                try func(self, allocator, inner.reg, value);
                try func(self, allocator, inner.dest, value);
            },
            .Cmp => |inner| {
                try func(self, allocator, inner.reg1, value);
                try func(self, allocator, inner.reg2, value);
            },
            .CmpSetRegEQ,
            .CmpSetRegNE,
            .CmpSetRegGT,
            .CmpSetRegLT,
            .CmpSetRegGTE,
            .CmpSetRegLTE,
            => |inner| {
                try func(self, allocator, inner.reg1, value);
                try func(self, allocator, inner.reg2, value);
                try func(self, allocator, inner.dest, value);
            },
            .CmpConst8, .IncConst8, .DecConst8 => |inner| {
                try func(self, allocator, inner.reg, value);
            },
            .Mov => |inner| {
                try func(self, allocator, inner.src, value);
                try func(self, allocator, inner.dest, value);
            },
            .MovSpNegOffset16 => |inner| try func(self, allocator, inner.reg, value),
            .MovSpNegOffset32 => |inner| try func(self, allocator, inner.reg, value),
            .MovSpNegOffset64, .MovSpNegOffsetAny => |inner| try func(self, allocator, inner.reg, value),
            .Xor, .BitAnd, .BitOr, .AndSetReg, .OrSetReg => |inner| {
                try func(self, allocator, inner.reg1, value);
                try func(self, allocator, inner.reg2, value);
                try func(self, allocator, inner.dest, value);
            },
            .XorConst8 => |inner| {
                try func(self, allocator, inner.reg, value);
                try func(self, allocator, inner.dest, value);
            },
            .Store64AtReg, .Store32AtReg, .Store16AtReg, .Store8AtReg => |inner| {
                try func(self, allocator, inner.fromReg, value);
                try func(self, allocator, inner.toRegPtr, value);
            },
            .Store64AtRegPostInc16,
            .Store32AtRegPostInc16,
            .Store16AtRegPostInc16,
            .Store8AtRegPostInc16,
            => |inner| {
                try func(self, allocator, inner.fromReg, value);
                try func(self, allocator, inner.toRegPtr, value);
            },
            .Store64AtSpNegOffset16,
            .Store32AtSpNegOffset16,
            .Store16AtSpNegOffset16,
            .Store8AtSpNegOffset16,
            .Load64AtSpNegOffset16,
            .Load32AtSpNegOffset16,
            .Load16AtSpNegOffset16,
            .Load8AtSpNegOffset16,
            => |inner| {
                try func(self, allocator, inner.reg, value);
            },
            .Load64AtRegOffset16,
            .Load32AtRegOffset16,
            .Load16AtRegOffset16,
            .Load8AtRegOffset16,
            => |inner| {
                try func(self, allocator, inner.fromRegPtr, value);
                try func(self, allocator, inner.dest, value);
            },
            .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |inner| {
                try func(self, allocator, inner.fromRegPtr, value);
                try func(self, allocator, inner.dest, value);
            },
            .MulReg16AddReg => |inner| {
                try func(self, allocator, inner.addReg, value);
                try func(self, allocator, inner.mulReg, value);
                try func(self, allocator, inner.dest, value);
            },
            .And, .Or => |inner| {
                try func(self, allocator, inner.reg1, value);
                try func(self, allocator, inner.reg2, value);
            },
        }
    }

    pub fn takeLabelId(self: *Self) vmInfo.LabelType {
        const res = self.currentLabelId;
        self.currentLabelId += 1;
        return res;
    }

    pub fn getNextRegister(self: *Self, allocator: Allocator) !TempRegister {
        return try self.getNextRegisterUtil(allocator, .Temporary);
    }

    pub fn getNextRegisterUtil(
        self: *Self,
        allocator: Allocator,
        usage: RegisterUsage,
    ) !TempRegister {
        const regInfoPtr = try utils.createMut(
            RegInfo,
            allocator,
            try RegInfo.initWithUsage(allocator, usage),
        );
        try self.registers.append(allocator, regInfoPtr);
        return @intCast(self.registers.items.len - 1);
    }

    pub fn getVariableRegister(self: Self, name: []const u8) TempRegister {
        return self.varNameToReg.get(name).?;
    }

    pub fn setVariableRegister(
        self: *Self,
        allocator: Allocator,
        name: []const u8,
        reg: TempRegister,
    ) !void {
        const regInfo = try utils.createMut(RegInfo, allocator, try RegInfo.init(allocator));

        try self.varNameToReg.put(name, reg);
        if (self.registers.items[reg].varInfo) |*varInfo| {
            varInfo.prevInfo = varInfo;
            self.registers.items[reg] = regInfo;
        }
    }

    // deactivates register and removes variable info
    pub fn removeVariableRegister(self: *Self, name: []const u8) void {
        if (self.varNameToReg.get(name)) |reg| {
            const varInfo = self.registers.items[reg].varInfo;

            if (varInfo) |info| {
                const prevInfoOrNull = info.prevInfo;
                if (prevInfoOrNull) |prevInfo| {
                    self.registers.items[reg].varInfo = prevInfo.*;
                } else {
                    _ = self.varNameToReg.remove(name);
                }
            } else {
                _ = self.varNameToReg.remove(name);
            }
        }
    }

    pub fn isRegVariable(self: Self, reg: TempRegister) bool {
        return self.registers.items[reg].varInfo != null;
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

    pub fn getRegInfo(self: Self, reg: TempRegister) ?*RegInfo {
        return self.registers.items[reg];
    }

    pub fn getVarGenInfoFromName(self: Self, name: []const u8) ?struct {
        varInfo: *RegInfo,
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
        // noop for possible register push instr
        try self.instrList.append(allocator, .{ .NoOp = {} });
        // noop for possible lr push instr
        try self.instrList.append(allocator, .{ .NoOp = {} });
        const startIndex = self.instrList.items.len - 3;

        self.currentProc = .{
            .startIndex = @intCast(startIndex),
        };
    }

    pub fn finishProc(
        self: *Self,
        allocator: Allocator,
        context: *Context,
        isRoot: bool,
        comptime backend: BackendInterface,
    ) !void {
        try self.labelByteInfo.ensureLabelCount(allocator, self.currentLabelId);
        try adjustProc(
            allocator,
            context,
            self.currentProc.startIndex,
            &self.currentProc.stackFrameSize,
            isRoot,
            backend,
        );
    }

    pub fn setLabelLocation(
        self: *Self,
        label: vmInfo.LabelType,
        location: u64,
    ) void {
        self.labelByteInfo.labelInfo.items[label] = .{
            .byte = location,
            .exists = true,
        };

        if (self.labelByteInfo.waitingLabels.get(label)) |arrList| {
            for (arrList.items) |instr| {
                updateInstrLabelLocation(instr, location);
            }

            _ = self.labelByteInfo.waitingLabels.remove(label);
        }
    }

    pub fn handleSkipInstruction(self: *Self, index: usize) bool {
        if (self.instrActions.skipInstrInfo.getCurrent()) |current| {
            if (current == index) {
                self.instrActions.skipInstrInfo.next();
                return true;
            }
        }

        return false;
    }

    pub fn handleInsertInstr(self: *Self, index: usize) ?*Instr {
        const currentInsert = self.instrActions.insertInstrInfo.current;
        if (self.instrActions.insertInstrInfo.getCurrent()) |current| {
            if (current.pos == index) {
                const res = &self.instrActions.insertInstrInfo.action.items[currentInsert].instr;
                self.instrActions.insertInstrInfo.next();
                return res;
            }
        }

        return null;
    }
};

fn writeChunk(instr: Instr, writer: *Writer) !void {
    if (instr == .Label or instr == .NoOp) return;

    try writer.writeByte(instr.getInstrByte());

    switch (instr) {
        .Label, .NoOp => unreachable,
        .Ret, .End => {},
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
        .Add, .Sub, .Mult, .Xor => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg1));
            try writer.writeByte(@intCast(inner.reg2));
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
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
        .Load64AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load8AtSpNegOffset16,
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
        .PrePushRegNegOffsetAny,
        .PostPopRegNegOffsetAny,
        .PrePushLRNegOffsetAny,
        .PostPopLRNegOffsetAny,
        => unreachable,
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
        .BranchLink, .BranchLinkBack => |inner| {
            try writer.writeInt(u32, @intCast(inner), .little);
        },
        .PrePushLRNegOffset8, .PostPopLRNegOffset8 => |inner| {
            try writer.writeByte(inner);
        },
        .PrePushLRNegOffset16, .PostPopLRNegOffset16 => |inner| {
            try writer.writeInt(u16, inner, .little);
        },
        .PrePushLRNegOffset32, .PostPopLRNegOffset32 => |inner| {
            try writer.writeInt(u32, inner, .little);
        },
        .PrePushLRNegOffset64, .PostPopLRNegOffset64 => |inner| {
            try writer.writeInt(u64, inner, .little);
        },
    }
}

fn updateInstrLabelLocation(instr: *Instr, location: u64) void {
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
        .BranchLink,
        .BranchLinkBack,
        => |*inner| {
            const data = location - inner.*;
            inner.* = data;
        },
        else => {},
    }
}

fn adjustProc(
    allocator: Allocator,
    context: *Context,
    instrStartIndex: usize,
    frameSize: *u64,
    isRoot: bool,
    comptime backend: BackendInterface,
) !void {
    var i = instrStartIndex;
    var numPushedRegisters: u32 = 0;

    while (i < context.genInfo.instrList.items.len) : (i += 1) {
        const instr = context.genInfo.instrList.items[i];
        switch (instr) {
            .BranchLink, .BranchLinkBack => {
                var procReg: usize = context.genInfo.currentProc.preProcVirtualReg;
                while (procReg < context.genInfo.registers.items.len) : (procReg += 1) {
                    const regInfo = context.genInfo.registers.items[procReg];
                    const firstFound = regInfo.useIndices.first();
                    const lastUsed = regInfo.useIndices.last();

                    if (firstFound < i and i < lastUsed) {
                        context.genInfo.registers.items[procReg].usage = .Preserved;
                        if (context.genInfo.currentProc.maxPreserveReg) |*reg| {
                            reg.* += 1;
                        } else {
                            context.genInfo.currentProc.maxPreserveReg = 0;
                        }

                        numPushedRegisters += 1;
                    }
                }
            },
            else => {},
        }
    }

    const stackOffset =
        numPushedRegisters * vmInfo.POINTER_SIZE +
        vmInfo.POINTER_SIZE * (if (numPushedRegisters > 0) @as(u8, 1) else @as(u8, 0));

    if (context.genInfo.currentProc.maxPreserveReg) |maxReg| {
        const pushInstr = try pushRegNegOffsetAnyInstr(maxReg, frameSize.* + stackOffset);
        const popInstr = try popRegNegOffsetAnyInstr(maxReg, frameSize.* + stackOffset);
        const pushLRInstr = try prePushLRNegOffsetAnyInstr(
            frameSize.* + stackOffset - vmInfo.POINTER_SIZE,
        );
        const popLRInstr = try postPopLRNegOffsetAnyInstr(
            frameSize.* + stackOffset - vmInfo.POINTER_SIZE,
        );

        context.genInfo.instrList.items[instrStartIndex + 2] = pushLRInstr;
        try context.genInfo.instrList.append(allocator, popLRInstr);
        context.genInfo.instrList.items[instrStartIndex + 1] = pushInstr;
        try context.genInfo.instrList.append(allocator, popInstr);
    }

    const instrs = context.genInfo.instrList.items[instrStartIndex..];
    if (context.settings.debug.allocateRegisters) {
        try backend.allocateRegisters(
            allocator,
            context,
            instrs,
            instrStartIndex,
            frameSize,
        );
    }

    if (frameSize.* + stackOffset > 0) {
        const spInstrs = try getSpIncInstructions(frameSize.* + stackOffset);

        context.genInfo.instrList.items[context.genInfo.currentProc.startIndex] = spInstrs.add;
        try context.genInfo.instrList.append(allocator, spInstrs.sub);
    }

    const finishInstr = if (isRoot) Instr{ .End = {} } else Instr{ .Ret = {} };
    try context.genInfo.instrList.append(allocator, finishInstr);

    try adjustInstructions(allocator, context, instrs, instrStartIndex, frameSize.*, stackOffset);

    context.genInfo.instrActions.resetIter();
    context.genInfo.currentProc.preProcVirtualReg = @intCast(context.genInfo.registers.items.len);
}

fn adjustInstructions(
    allocator: Allocator,
    context: *Context,
    instrs: []Instr,
    instrStartIndex: usize,
    frameSize: u64,
    stackOffset: u64,
) !void {
    var i: usize = 0;
    while (i < instrs.len) : (i += 1) {
        const skipped = context.genInfo.handleSkipInstruction(i + instrStartIndex);

        if (!skipped) {
            try adjustInstruction(
                allocator,
                context,
                &context.genInfo.instrList.items[i + instrStartIndex],
                frameSize,
                stackOffset,
            );
            context.genInfo.byteCounter += context.genInfo.instrList.items[i].getInstrLen();
        }

        while (context.genInfo.handleInsertInstr(i + instrStartIndex)) |instr| {
            try adjustInstruction(
                allocator,
                context,
                instr,
                frameSize,
                stackOffset,
            );
            context.genInfo.byteCounter += instr.getInstrLen();
        }
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
            context.genInfo.setLabelLocation(
                label,
                context.genInfo.byteCounter,
            );
        },
        .Store8AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store64AtSpNegOffset16,
        .Load8AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load64AtSpNegOffset16,
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

                try context.genInfo.labelByteInfo.waitForLabel(
                    allocator,
                    @intCast(labelId),
                    instr,
                );
            }
        },
        .BranchLink => |*data| {
            const labelId = data.*;
            data.* = context.genInfo.byteCounter + instr.getInstrLen();
            try context.genInfo.labelByteInfo.waitForLabel(allocator, @intCast(labelId), instr);
        },
        .BranchLinkBack => |*labelId| {
            const loc = try context.genInfo.labelByteInfo.getLabelLocation(@intCast(labelId.*));
            _ = loc;
            // TODO
            unreachable;
            // labelId.* = loc.?;
        },
        // not changed by stackOffset
        .PrePushRegNegOffsetAny => |pushInstr| {
            const newInstr = try pushRegNegOffsetAnyInstr(
                pushInstr.reg,
                frameSize - pushInstr.offset,
            );
            instr.* = newInstr;
        },
        // not changed by stackOffset
        .PostPopRegNegOffsetAny => |popInstr| {
            const newInstr = try popRegNegOffsetAnyInstr(
                popInstr.reg,
                frameSize - popInstr.offset,
            );
            instr.* = newInstr;
        },
        else => {},
    }
}

fn postPopLRNegOffsetAnyInstr(offset: u64) !Instr {
    const spOpSize = try getOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U8 => Instr{
            .PostPopLRNegOffset8 = @intCast(offset),
        },
        .U16 => Instr{
            .PostPopLRNegOffset16 = @intCast(offset),
        },
        .U32 => Instr{
            .PostPopLRNegOffset32 = @intCast(offset),
        },
        .U64 => Instr{
            .PostPopLRNegOffset64 = offset,
        },
    };
}

fn prePushLRNegOffsetAnyInstr(offset: u64) !Instr {
    const spOpSize = try getOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U8 => Instr{
            .PrePushLRNegOffset8 = @intCast(offset),
        },
        .U16 => Instr{
            .PrePushLRNegOffset16 = @intCast(offset),
        },
        .U32 => Instr{
            .PrePushLRNegOffset32 = @intCast(offset),
        },
        .U64 => Instr{
            .PrePushLRNegOffset64 = offset,
        },
    };
}

fn popRegNegOffsetAnyInstr(reg: TempRegister, offset: u64) !Instr {
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

fn pushRegNegOffsetAnyInstr(reg: TempRegister, offset: u64) !Instr {
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

pub fn codegenAst(
    allocator: Allocator,
    context: *Context,
    comptime backendType: BackendTypes,
) !void {
    context.genInfo.vmInfo.version = version.VERSION;

    const backend = switch (backendType) {
        .Bytecode => bytecodeBackend.backend,
    };

    try backend.initMetadata(allocator, context);

    const mainFn = context.compInfo.functions.get(constants.MAIN_FN_NAME) orelse
        return CodeGenError.MainFunctionNotFound;
    try context.genInfo.newProc(allocator);
    _ = try genBytecode(allocator, context, mainFn.body);

    // VERSION 1
    // const slice = &[_]Instr{
    //     Instr{
    //         .AddSp32 = 32, // ADDING SP
    //     },
    //     Instr{ .SetReg32 = .{
    //         .reg = 8,
    //         .data = 2,
    //     } },
    //     Instr{ .SetReg32 = .{
    //         .reg = 9,
    //         .data = 1,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 9,
    //         .reg1 = 8,
    //         .reg2 = 9,
    //     } },
    //     Instr{ .SetReg32 = .{
    //         .reg = 10,
    //         .data = 1,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 10,
    //         .reg1 = 9,
    //         .reg2 = 10,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 9,
    //         .reg2 = 10,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 8,
    //         .reg2 = 11,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 10,
    //         .reg2 = 11,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 9,
    //         .reg2 = 12,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 8,
    //         .reg2 = 12,
    //     } },
    //     Instr{
    //         .Store64AtSpNegOffset16 = .{
    //             .reg = 11,
    //             .offset = 32, // FIRST
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 11,
    //         .reg2 = 12,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 10,
    //         .reg2 = 11,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 9,
    //         .reg2 = 11,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 11,
    //         .reg1 = 8,
    //         .reg2 = 11,
    //     } },
    //     Instr{
    //         .Store64AtSpNegOffset16 = .{
    //             .reg = 11,
    //             .offset = 24, // SECOND X = sp - 24
    //         },
    //     },
    //     Instr{
    //         .Store64AtSpNegOffset16 = .{
    //             .reg = 12,
    //             .offset = 16, // THIRD
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 12,
    //         .reg2 = 11,
    //     } },
    //     Instr{
    //         .Load64AtSpNegOffset16 = .{
    //             .reg = 11,
    //             .offset = 32, // LOAD 1
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 11,
    //         .reg2 = 12,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 10,
    //         .reg2 = 12,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 9,
    //         .reg2 = 12,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 12,
    //         .reg1 = 8,
    //         .reg2 = 12,
    //     } },
    //     Instr{
    //         .Store64AtSpNegOffset16 = .{
    //             .reg = 8,
    //             .offset = 8, // FOURTH
    //         },
    //     },
    //     Instr{
    //         .Load64AtSpNegOffset16 = .{
    //             .reg = 8,
    //             .offset = 24, // LOAD 2
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 8,
    //         .reg1 = 8,
    //         .reg2 = 12,
    //     } },
    //     Instr{
    //         .Load64AtSpNegOffset16 = .{
    //             .reg = 12,
    //             .offset = 16, // LOAD 3
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 8,
    //         .reg1 = 12,
    //         .reg2 = 8,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 8,
    //         .reg1 = 11,
    //         .reg2 = 8,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 8,
    //         .reg1 = 10,
    //         .reg2 = 8,
    //     } },
    //     Instr{ .Add = .{
    //         .dest = 9,
    //         .reg1 = 9,
    //         .reg2 = 8,
    //     } },
    //     Instr{
    //         .Load64AtSpNegOffset16 = .{
    //             .reg = 8,
    //             .offset = 8, // LOAD 4
    //         },
    //     },
    //     Instr{ .Add = .{
    //         .dest = 8,
    //         .reg1 = 8,
    //         .reg2 = 9,
    //     } },
    //     Instr{
    //         .SubSp32 = 32, // SUB SP
    //     },
    //     Instr{ .End = {} },
    // };

    // const listPtr = try utils.createMut(ArrayList(Instr), allocator, .empty);
    // try listPtr.appendSlice(allocator, slice);

    // context.genInfo.instrList = listPtr;

    try context.genInfo.finishProc(allocator, context, true, backend);
    try codegenFunctions(allocator, context, backend);
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
        .StructPlaceholder,
        .StructDec,
        .UndefValue,
        .NoOp,
        .FuncDec,
        .EnumDec,
        .ErrorDec,
        => {},
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
                try context.genInfo.setVariableRegister(allocator, dec.name, varReg);
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
                try context.genInfo.setVariableRegister(allocator, ident, indexReg.?);

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
                try context.genInfo.setVariableRegister(allocator, ident, ptrReg.?);

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
            const varStackLocation = a: {
                const info = varGenInfo orelse break :a null;
                const varInfo = info.varInfo orelse break :a null;
                break :a varInfo.stackLocation;
            };

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
                if (info.varInfo) |*varInfo| {
                    if (varInfo.stackLocation == null) {
                        varInfo.stackLocation = location;
                    }
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
            const func = node.typeInfo.resolvesToFunc.?;
            const labelIdPtr = switch (func.genericState) {
                .Generic => |*generic| &generic
                    .genericInstances
                    .items[node.typeInfo.funcGenInstanceIndex.?]
                    .labelId,
                .Normal => |*normal| &normal.labelId,
            };
            const branchLabelId = labelIdPtr.* orelse a: {
                const labelId = context.genInfo.takeLabelId();
                labelIdPtr.* = labelId;
                break :a labelId;
            };

            for (call.params, 0..) |param, index| {
                const reg = try genBytecode(allocator, context, param) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                const regUsage: RegisterUsage = if (index == 0) .Param else .ParamNext;
                const resReg = try context.genInfo.getNextRegisterUtil(allocator, regUsage);
                const movInstr = Instr{
                    .Mov = .{
                        .src = reg,
                        .dest = resReg,
                    },
                };
                try context.genInfo.appendChunk(allocator, movInstr);
            }

            const branchInstr = if (func.labelsGenerated) Instr{
                .BranchLinkBack = branchLabelId,
            } else Instr{
                .BranchLink = branchLabelId,
            };
            try context.genInfo.appendChunk(allocator, branchInstr);

            return try context.genInfo.getNextRegisterUtil(allocator, .Return);
        },
        .ReturnNode => |inner| {
            const reg = try genBytecode(allocator, context, inner) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const resReg = try context.genInfo.getNextRegisterUtil(allocator, .Return);

            const movInstr = Instr{
                .Mov = .{
                    .src = reg,
                    .dest = resReg,
                },
            };
            try context.genInfo.appendChunk(allocator, movInstr);
        },
        else => {},
    }

    return null;
}

fn codegenFunctions(
    allocator: Allocator,
    context: *Context,
    comptime backend: BackendInterface,
) !void {
    var funcIter = context.compInfo.functions.valueIterator();
    while (funcIter.next()) |funcPtr| {
        const func = funcPtr.*;

        if (utils.compString(func.name, constants.MAIN_FN_NAME)) continue;

        switch (func.genericState) {
            .Generic => |generic| {
                for (generic.genericInstances.items) |*instance| {
                    try functionSetupBytecode(allocator, context, func, &instance.labelId);
                    try context.genInfo.newProc(allocator);
                    _ = try genBytecode(allocator, context, instance.funcRootNode);
                    try context.genInfo.finishProc(allocator, context, false, backend);
                }
            },
            .Normal => |*normal| {
                try functionSetupBytecode(allocator, context, func, &normal.labelId);
                try context.genInfo.newProc(allocator);
                _ = try genBytecode(allocator, context, func.body);
                try context.genInfo.finishProc(allocator, context, false, backend);
            },
        }
    }
}

fn functionSetupBytecode(
    allocator: Allocator,
    context: *Context,
    func: *ast.FuncDecNode,
    labelId: *?u32,
) !void {
    const labelIdOrNew = labelId.* orelse a: {
        const newLabelId = context.genInfo.takeLabelId();
        labelId.* = newLabelId;
        break :a newLabelId;
    };
    try context.genInfo.labelByteInfo.ensureLabelCount(
        allocator,
        context.genInfo.currentLabelId,
    );
    const label = Instr{
        .Label = labelIdOrNew,
    };
    try context.genInfo.appendChunk(allocator, label);
    context.genInfo.setLabelLocation(labelIdOrNew, context.genInfo.byteCounter);

    for (func.params.params, 0..) |param, index| {
        const usage: RegisterUsage = if (index == 0) .Param else .ParamNext;
        const paramReg = try context.genInfo.getNextRegisterUtil(allocator, usage);
        try context.genInfo.setVariableRegister(allocator, param.name, paramReg);
    }

    if (func.params.selfInfo != null) {
        const paramReg = try context.genInfo.getNextRegisterUtil(allocator, .Param);
        try context.genInfo.setVariableRegister(allocator, constants.SELF_NAME, paramReg);
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

            if (offset == 0) {
                const instr = Instr{
                    .SetReg8 = .{
                        .reg = outReg,
                        .data = 0,
                    },
                };
                try context.genInfo.appendChunk(allocator, instr);
            } else if (offset > 0) {
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
    const regLocationOrNull = a: {
        const arrReg = arrRegOrNull orelse break :a null;
        const regInfoOrNull = context.genInfo.getRegInfo(arrReg);
        const info = regInfoOrNull orelse break :a null;
        const varInfo = info.varInfo orelse break :a null;
        break :a varInfo.stackLocation;
    };
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

fn writeNumber(comptime T: type, data: T, writer: *Writer) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
