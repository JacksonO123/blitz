const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const MemoryPool = std.heap.MemoryPool;
const builtin = @import("builtin");

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const constants = blitz.constants;
const backendUtils = blitz.backendUtils;
const scanner = blitz.scanner;
const clone = blitz.clone;
const builtins = blitz.builtins;
const identStore = blitz.identStore;
const bytecodeBackend = blitz.backends.bytecode;
const instructions = blitz.instructions;
const Context = blitz.context.Context;

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
    ExpectedLoopInfo,
    ImmediateValueTooLarge,
    RegInteractionNotSupported,
    NoTrivialRegister,
    AccessTargetDoesNotHaveStructName,
    LabelDoesNotExist,
    MainFunctionNotFound,
    ResultOfAccessRegNotFound,
};
const GenBytecodeError = CodeGenError ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    ast.AstTypeError ||
    scanner.ScanError ||
    clone.CloneError;

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

pub const RegisterUsageVariants = enum {
    const Self = @This();

    Param,
    Return,
    Preserved,
    ParamPreserved,
    Temporary,

    pub fn isParam(self: Self) bool {
        return switch (self) {
            .Param, .ParamPreserved => true,
            else => false,
        };
    }
};

pub const RegisterUsage = union(RegisterUsageVariants) {
    const Self = @This();

    Param: u8,
    Return: u8,
    Preserved,
    ParamPreserved: u8,
    Temporary,

    pub fn isParam(self: Self) bool {
        const tag: RegisterUsageVariants = std.meta.activeTag(self);
        return tag.isParam();
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

pub const instrDataArr = getTestArr();

inline fn getTestArr() [@typeInfo(InstructionVariants).@"enum".fields.len]InstrInfo {
    const info = @typeInfo(InstructionVariants).@"enum";
    var arr: [info.fields.len]InstrInfo = undefined;

    for (info.fields, 0..) |field, index| {
        const enumVariant: InstructionVariants = @enumFromInt(field.value);

        arr[index] = enumVariant.getInstrInfoDispatch();
    }

    return arr;
}

pub const InstructionVariants = enum(u8) {
    const Self = @This();

    NoOp, // 0B (not in output)
    Label, // 0B (not in output)

    SetReg64, // inst, reg, 8B data
    SetReg32, // inst, reg, 4B data
    SetReg16, // inst, reg, 2B data
    SetReg8, // inst, reg, 1B data

    Add, // inst, out reg, reg1, reg2
    Sub, // inst, out reg, reg1, reg2
    Mult, // inst, out reg, reg1, reg2

    Add8, // inst, out reg, reg1, 1B data
    Sub8, // inst, out reg, reg1, 1B data
    Add16, // inst, out reg, reg1, 2B data
    Sub16, // inst, out reg, reg1, 2B data
    Add32, // inst, out reg, reg1, 4B data
    Sub32, // inst, out reg, reg1, 4B data
    Add64, // inst, out reg, reg1, 8B data
    Sub64, // inst, out reg, reg1, 8B data

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
    MovSpNegOffsetAny, // inst, dest reg, offset (TBD by compiler)B
    MovSpNegOffset16, // inst, dest reg, offset 2B
    MovSpNegOffset32, // inst, dest reg, offset 4B
    MovSpNegOffset64, // inst, dest reg, offset 8B

    Xor, // inst, out reg, reg1, reg2
    XorConst8, // inst, out reg, reg1, 1B data

    AddSp8, // inst, 1B data
    SubSp8, // inst, 1B data
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

    Store64AtRegOffset16, // inst, reg, to reg (ptr), offset 2B
    Store32AtRegOffset16, // inst, reg, to reg (ptr), offset 2B
    Store16AtRegOffset16, // inst, reg, to reg (ptr), offset 2B
    Store8AtRegOffset16, // inst, reg, to reg (ptr), offset 2B

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

    Load64AtRegPostInc16, // inst, dest reg, from reg (ptr), inc 2B
    Load32AtRegPostInc16, // inst, dest reg, from reg (ptr), inc 2B
    Load16AtRegPostInc16, // inst, dest reg, from reg (ptr), inc 2B
    Load8AtRegPostInc16, // inst, dest reg, from reg (ptr), inc 2B

    Load64AtSpNegOffset16, // inst, dest reg, offset 2B
    Load32AtSpNegOffset16, // inst, dest reg, offset 2B
    Load16AtSpNegOffset16, // inst, dest reg, offset 2B
    Load8AtSpNegOffset16, // inst, dest reg, offset 2B

    Load64AtReg, // inst, dest reg, from reg (ptr)
    Load32AtReg, // inst, dest reg, from reg (ptr)
    Load16AtReg, // inst, dest reg, from reg (ptr)
    Load8AtReg, // inst, dest reg, from reg (ptr)

    MulReg8AddReg, // inst, dest reg, addReg, mulReg, data 1B ( dest = addReg + (mulReg1 * data) )
    MulReg16AddReg, // inst, dest reg, addReg, mulReg, data 2B ( dest = addReg + (mulReg1 * data) )
    MulReg32AddReg, // inst, dest reg, addReg, mulReg, data 4B ( dest = addReg + (mulReg1 * data) )
    MulReg64AddReg, // inst, dest reg, addReg, mulReg, data 8B ( dest = addReg + (mulReg1 * data) )

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

    PrePushLRNegOffsetAny, // inst, offset (TBD by compiler)B
    PrePushLRNegOffset8, // inst, offset 1B
    PrePushLRNegOffset16, // inst, offset 2B
    PrePushLRNegOffset32, // inst, offset 4B
    PrePushLRNegOffset64, // inst, offset 8B

    PostPopLRNegOffsetAny, // inst, offset (TBD by compiler)B
    PostPopLRNegOffset8, // inst, offset 1B
    PostPopLRNegOffset16, // inst, offset 2B
    PostPopLRNegOffset32, // inst, offset 4B
    PostPopLRNegOffset64, // inst, offset 8B

    Ret, // inst
    End, // inst

    BranchLink, // inst, 4B data
    BranchLinkBack, // inst, 4B data

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

    pub fn getInstrInfoDispatch(self: Self) InstrInfo {
        if (!@inComptime()) {
            @compileError(
                \\getInstrInfoDispatch function should not be used at runtime,
                \\use compile time instr info util functions instead
            );
        }

        return switch (self) {
            .NoOp => .{
                .len = 0,
                .opCount = 0,
                .text = "noop",
            },
            .Label => .{
                .len = 0,
                .opCount = 0,
                .text = "(label)",
            },
            .SetReg64 => .{
                .len = 10,
                .opCount = 1,
                .text = "set_reg_64",
            },
            .SetReg32 => .{
                .len = 6,
                .opCount = 1,
                .text = "set_reg_32",
            },
            .SetReg16 => .{
                .len = 4,
                .opCount = 1,
                .text = "set_reg_16",
            },
            .SetReg8 => .{
                .len = 3,
                .opCount = 1,
                .text = "set_reg_8",
            },
            .Add => .{
                .len = 4,
                .opCount = 3,
                .text = "add",
            },
            .Sub => .{
                .len = 4,
                .opCount = 3,
                .text = "sub",
            },
            .Mult => .{
                .len = 4,
                .opCount = 3,
                .text = "mult",
            },
            .Add8 => .{
                .len = 4,
                .opCount = 2,
                .text = "add_8",
            },
            .Sub8 => .{
                .len = 4,
                .opCount = 2,
                .text = "sub_8",
            },
            .Add16 => .{
                .len = 5,
                .opCount = 2,
                .text = "add_16",
            },
            .Sub16 => .{
                .len = 5,
                .opCount = 2,
                .text = "sub_16",
            },
            .Add32 => .{
                .len = 7,
                .opCount = 2,
                .text = "add_32",
            },
            .Sub32 => .{
                .len = 7,
                .opCount = 2,
                .text = "sub_32",
            },
            .Add64 => .{
                .len = 11,
                .opCount = 2,
                .text = "add_64",
            },
            .Sub64 => .{
                .len = 11,
                .opCount = 2,
                .text = "sub_64",
            },
            .Jump => .{
                .len = 5,
                .opCount = 0,
                .text = "jump",
            },
            .JumpEQ => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_eq",
            },
            .JumpNE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_ne",
            },
            .JumpGT => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_gt",
            },
            .JumpLT => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_lt",
            },
            .JumpGTE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_gte",
            },
            .JumpLTE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_lte",
            },
            .JumpBack => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back",
            },
            .JumpBackEQ => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_eq",
            },
            .JumpBackNE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_ne",
            },
            .JumpBackGT => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_gt",
            },
            .JumpBackLT => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_lt",
            },
            .JumpBackGTE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_gte",
            },
            .JumpBackLTE => .{
                .len = 5,
                .opCount = 0,
                .text = "jump_back_lte",
            },
            .Cmp => .{
                .len = 3,
                .opCount = 2,
                .text = "cmp",
            },
            .CmpSetRegEQ => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_eq",
            },
            .CmpSetRegNE => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_ne",
            },
            .CmpSetRegGT => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_gt",
            },
            .CmpSetRegLT => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_lt",
            },
            .CmpSetRegGTE => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_gte",
            },
            .CmpSetRegLTE => .{
                .len = 4,
                .opCount = 3,
                .text = "cmp_set_reg_lte",
            },
            .CmpConst8 => .{
                .len = 3,
                .opCount = 1,
                .text = "cmp_const_8",
            },
            .IncConst8 => .{
                .len = 3,
                .opCount = 1,
                .text = "inc_const_8",
            },
            .DecConst8 => .{
                .len = 3,
                .opCount = 1,
                .text = "dec_const_8",
            },
            .Mov => .{
                .len = 3,
                .opCount = 2,
                .text = "mov",
            },
            .MovSpNegOffsetAny => .{
                .len = undefined, // SHOULD NOT EXIST FOR WRITER
                .opCount = 1,
                .text = "mov_sp_neg_offset_ANY",
            },
            .MovSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "mov_sp_neg_offset_16",
            },
            .MovSpNegOffset32 => .{
                .len = 6,
                .opCount = 1,
                .text = "mov_sp_neg_offset_32",
            },
            .MovSpNegOffset64 => .{
                .len = 10,
                .opCount = 1,
                .text = "mov_sp_neg_offset_64",
            },
            .Xor => .{
                .len = 4,
                .opCount = 3,
                .text = "xor",
            },
            .XorConst8 => .{
                .len = 4,
                .opCount = 2,
                .text = "xor_const_8",
            },
            .AddSp8 => .{
                .len = 2,
                .opCount = 0,
                .text = "add_sp_8",
            },
            .SubSp8 => .{
                .len = 2,
                .opCount = 0,
                .text = "sub_sp_8",
            },
            .AddSp16 => .{
                .len = 3,
                .opCount = 0,
                .text = "add_sp_16",
            },
            .SubSp16 => .{
                .len = 3,
                .opCount = 0,
                .text = "sub_sp_16",
            },
            .AddSp32 => .{
                .len = 5,
                .opCount = 0,
                .text = "add_sp_32",
            },
            .SubSp32 => .{
                .len = 5,
                .opCount = 0,
                .text = "sub_sp_32",
            },
            .AddSp64 => .{
                .len = 9,
                .opCount = 0,
                .text = "add_sp_64",
            },
            .SubSp64 => .{
                .len = 9,
                .opCount = 0,
                .text = "sub_sp_64",
            },
            .Store64AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "store_64_at_reg",
            },
            .Store32AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "store_32_at_reg",
            },
            .Store16AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "store_16_at_reg",
            },
            .Store8AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "store_8_at_reg",
            },
            .Store64AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_64_at_reg_offset_16",
            },
            .Store32AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_32_at_reg_offset_16",
            },
            .Store16AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_16_at_reg_offset_16",
            },
            .Store8AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_8_at_reg_offset_16",
            },
            .Store64AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_64_at_reg_post_inc_16",
            },
            .Store32AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_32_at_reg_post_inc_16",
            },
            .Store16AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_16_at_reg_post_inc_16",
            },
            .Store8AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "store_8_at_reg_post_inc_16",
            },
            .Store64AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "store_64_at_sp_neg_offset_16",
            },
            .Store32AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "store_32_at_sp_neg_offset_16",
            },
            .Store16AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "store_16_at_sp_neg_offset_16",
            },
            .Store8AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "store_8_at_sp_neg_offset_16",
            },
            .Load64AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_64_at_reg_offset_16",
            },
            .Load32AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_32_at_reg_offset_16",
            },
            .Load16AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_16_at_reg_offset_16",
            },
            .Load8AtRegOffset16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_8_at_reg_offset_16",
            },
            .Load64AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_64_at_reg_post_inc_16",
            },
            .Load32AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_32_at_reg_post_inc_16",
            },
            .Load16AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_16_at_reg_post_inc_16",
            },
            .Load8AtRegPostInc16 => .{
                .len = 5,
                .opCount = 2,
                .text = "load_8_at_reg_post_inc_16",
            },
            .Load64AtSpNegOffset16,
            => .{
                .len = 4,
                .opCount = 1,
                .text = "load_64_at_sp_neg_offset_16",
            },
            .Load32AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "load_32_at_sp_neg_offset_16",
            },
            .Load16AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "load_16_at_sp_neg_offset_16",
            },
            .Load8AtSpNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "load_8_at_sp_neg_offset_16",
            },
            .Load64AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "load_64_at_reg",
            },
            .Load32AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "load_32_at_reg",
            },
            .Load16AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "load_16_at_reg",
            },
            .Load8AtReg => .{
                .len = 3,
                .opCount = 2,
                .text = "load_8_at_reg",
            },
            .MulReg8AddReg => .{
                .len = 5,
                .opCount = 3,
                .text = "mul_reg_8_add_reg",
            },
            .MulReg16AddReg => .{
                .len = 6,
                .opCount = 3,
                .text = "mul_reg_16_add_reg",
            },
            .MulReg32AddReg => .{
                .len = 8,
                .opCount = 3,
                .text = "mul_reg_32_add_reg",
            },
            .MulReg64AddReg => .{
                .len = 12,
                .opCount = 3,
                .text = "mul_reg_64_add_reg",
            },
            .DbgReg => .{
                .len = 2,
                .opCount = 1,
                .text = "dbg_reg",
            },
            .BitAnd => .{
                .len = 4,
                .opCount = 3,
                .text = "bit_and",
            },
            .BitOr => .{
                .len = 4,
                .opCount = 3,
                .text = "bit_or",
            },
            .And => .{
                .len = 3,
                .opCount = 2,
                .text = "and",
            },
            .Or => .{
                .len = 3,
                .opCount = 2,
                .text = "or",
            },
            .AndSetReg => .{
                .len = 4,
                .opCount = 3,
                .text = "and_set_reg",
            },
            .OrSetReg => .{
                .len = 4,
                .opCount = 3,
                .text = "or_set_reg",
            },
            .PrePushRegNegOffsetAny => .{
                .len = undefined, // SHOULD NOT EXIST FOR WRITER
                .opCount = 1,
                .text = "push_reg_neg_offset_ANY",
            },
            .PrePushRegNegOffset8 => .{
                .len = 3,
                .opCount = 1,
                .text = "push_reg_neg_offset_8",
            },
            .PrePushRegNegOffset16 => .{
                .len = 4,
                .opCount = 1,
                .text = "push_reg_neg_offset_16",
            },
            .PrePushRegNegOffset32 => .{
                .len = 6,
                .opCount = 1,
                .text = "push_reg_neg_offset_32",
            },
            .PrePushRegNegOffset64 => .{
                .len = 10,
                .opCount = 1,
                .text = "push_reg_neg_offset_64",
            },
            .PostPopRegNegOffsetAny => .{
                .len = undefined, // SHOULD NOT EXIST FOR WRITER
                .opCount = 0,
                .text = "pop_reg_neg_offset_ANY",
            },
            .PostPopRegNegOffset8 => .{
                .len = 3,
                .opCount = 0,
                .text = "pop_reg_neg_offset_8",
            },
            .PostPopRegNegOffset16 => .{
                .len = 4,
                .opCount = 0,
                .text = "pop_reg_neg_offset_16",
            },
            .PostPopRegNegOffset32 => .{
                .len = 6,
                .opCount = 0,
                .text = "pop_reg_neg_offset_32",
            },
            .PostPopRegNegOffset64 => .{
                .len = 10,
                .opCount = 0,
                .text = "pop_reg_neg_offset_64",
            },
            .PrePushLRNegOffsetAny => .{
                .len = undefined, // SHOULD NOT EXIST FOR WRITER
                .opCount = 0,
                .text = "pre_push_lr_neg_offset_any",
            },
            .PrePushLRNegOffset8 => .{
                .len = 2,
                .opCount = 0,
                .text = "pre_push_lr_neg_offset_8",
            },
            .PrePushLRNegOffset16 => .{
                .len = 3,
                .opCount = 0,
                .text = "pre_push_lr_neg_offset_16",
            },
            .PrePushLRNegOffset32 => .{
                .len = 5,
                .opCount = 0,
                .text = "pre_push_lr_neg_offset_32",
            },
            .PrePushLRNegOffset64 => .{
                .len = 9,
                .opCount = 0,
                .text = "pre_push_lr_neg_offset_64",
            },
            .PostPopLRNegOffsetAny => .{
                .len = undefined, // SHOULD NOT EXIST FOR WRITER
                .opCount = 0,
                .text = "post_pop_lr_neg_offset_any",
            },
            .PostPopLRNegOffset8 => .{
                .len = 2,
                .opCount = 0,
                .text = "post_pop_lr_neg_offset_8",
            },
            .PostPopLRNegOffset16 => .{
                .len = 3,
                .opCount = 0,
                .text = "post_pop_lr_neg_offset_16",
            },
            .PostPopLRNegOffset32 => .{
                .len = 5,
                .opCount = 0,
                .text = "post_pop_lr_neg_offset_32",
            },
            .PostPopLRNegOffset64 => .{
                .len = 9,
                .opCount = 0,
                .text = "post_pop_lr_neg_offset_64",
            },
            .Ret => .{
                .len = 1,
                .opCount = 0,
                .text = "ret",
            },
            .End => .{
                .len = 1,
                .opCount = 0,
                .text = "end",
            },
            .BranchLink => .{
                .len = 5,
                .opCount = 0,
                .text = "branch_link",
            },
            .BranchLinkBack => .{
                .len = 5,
                .opCount = 0,
                .text = "branch_link_back",
            },
        };
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
    dest: vmInfo.TempRegister,
    reg1: vmInfo.TempRegister,
    reg2: vmInfo.TempRegister,
};

fn OneOpResultInstr(comptime T: type) type {
    return struct {
        dest: vmInfo.TempRegister,
        reg: vmInfo.TempRegister,
        data: T,
    };
}

const RegBytePayloadInstr = struct {
    reg: vmInfo.TempRegister,
    data: u8,
};

const MathInstr = TwoOpResultInstr;

const SpInstr = struct {
    amount: vmInfo.TempRegister,
};

const SpRegInstr = struct {
    reg: vmInfo.TempRegister,
};

fn SetRegInstr(comptime T: type) type {
    return struct {
        reg: vmInfo.TempRegister,
        data: T,
    };
}

const CmpInstr = struct {
    reg1: vmInfo.TempRegister = 0,
    reg2: vmInfo.TempRegister = 0,
};

const CmpSetRegInstr = struct {
    dest: vmInfo.TempRegister = 0,
    reg1: vmInfo.TempRegister = 0,
    reg2: vmInfo.TempRegister = 0,
};

fn StoreOffsetInstr(comptime T: type) type {
    return struct {
        fromReg: vmInfo.TempRegister,
        toRegPtr: vmInfo.TempRegister,
        offset: T,
    };
}

fn StoreOrLoadOffsetSpInstr(comptime T: type) type {
    return struct {
        reg: vmInfo.TempRegister,
        offset: T,
    };
}

fn StoreAtRegPostIncInstr(comptime T: type) type {
    return struct {
        fromReg: vmInfo.TempRegister,
        toRegPtr: vmInfo.TempRegister,
        inc: T,
    };
}

fn LoadAtRegPostIncInstr(comptime T: type) type {
    return struct {
        fromRegPtr: vmInfo.TempRegister,
        toReg: vmInfo.TempRegister,
        inc: T,
    };
}

const StoreAtRegInstr = struct {
    fromReg: vmInfo.TempRegister,
    toRegPtr: vmInfo.TempRegister,
};

fn StoreIncSpInstr(comptime T: type) type {
    return struct {
        reg: vmInfo.TempRegister,
        inc: T,
    };
}

const LoadAtReg = struct {
    dest: vmInfo.TempRegister,
    fromRegPtr: vmInfo.TempRegister,
};

const LoadAtRegOffset16 = struct {
    dest: vmInfo.TempRegister,
    fromRegPtr: vmInfo.TempRegister,
    offset: u16,
};

fn MulRegTAddReg(comptime T: type) type {
    return struct {
        dest: vmInfo.TempRegister,
        addReg: vmInfo.TempRegister,
        mulReg: vmInfo.TempRegister,
        data: T,
    };
}

fn MovSpNegOffset(comptime T: type) type {
    return struct {
        reg: vmInfo.TempRegister,
        offset: T,
    };
}

fn PushOrPopRegNegOffset(comptime T: type) type {
    return struct {
        reg: vmInfo.TempRegister,
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
    Sub: MathInstr,
    Mult: MathInstr,

    Add8: OneOpResultInstr(u8),
    Sub8: OneOpResultInstr(u8),
    Add16: OneOpResultInstr(u16),
    Sub16: OneOpResultInstr(u16),
    Add32: OneOpResultInstr(u32),
    Sub32: OneOpResultInstr(u32),
    Add64: OneOpResultInstr(u64),
    Sub64: OneOpResultInstr(u64),

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
        dest: vmInfo.TempRegister = 0,
        src: vmInfo.TempRegister = 0,
    },
    MovSpNegOffsetAny: MovSpNegOffset(u64),
    MovSpNegOffset16: MovSpNegOffset(u16),
    MovSpNegOffset32: MovSpNegOffset(u32),
    MovSpNegOffset64: MovSpNegOffset(u64),

    Xor: TwoOpResultInstr,
    XorConst8: struct {
        dest: vmInfo.TempRegister = 0,
        reg: vmInfo.TempRegister = 0,
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

    Store64AtRegOffset16: StoreOffsetInstr(u16),
    Store32AtRegOffset16: StoreOffsetInstr(u16),
    Store16AtRegOffset16: StoreOffsetInstr(u16),
    Store8AtRegOffset16: StoreOffsetInstr(u16),

    Store64AtRegPostInc16: StoreAtRegPostIncInstr(u16),
    Store32AtRegPostInc16: StoreAtRegPostIncInstr(u16),
    Store16AtRegPostInc16: StoreAtRegPostIncInstr(u16),
    Store8AtRegPostInc16: StoreAtRegPostIncInstr(u16),

    Store64AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store32AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store16AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Store8AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),

    Load64AtRegOffset16: LoadAtRegOffset16,
    Load32AtRegOffset16: LoadAtRegOffset16,
    Load16AtRegOffset16: LoadAtRegOffset16,
    Load8AtRegOffset16: LoadAtRegOffset16,

    Load64AtRegPostInc16: LoadAtRegPostIncInstr(u16),
    Load32AtRegPostInc16: LoadAtRegPostIncInstr(u16),
    Load16AtRegPostInc16: LoadAtRegPostIncInstr(u16),
    Load8AtRegPostInc16: LoadAtRegPostIncInstr(u16),

    Load64AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load32AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load16AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),
    Load8AtSpNegOffset16: StoreOrLoadOffsetSpInstr(u16),

    Load64AtReg: LoadAtReg,
    Load32AtReg: LoadAtReg,
    Load16AtReg: LoadAtReg,
    Load8AtReg: LoadAtReg,

    MulReg8AddReg: MulRegTAddReg(u8),
    MulReg16AddReg: MulRegTAddReg(u16),
    MulReg32AddReg: MulRegTAddReg(u32),
    MulReg64AddReg: MulRegTAddReg(u64),

    DbgReg: vmInfo.TempRegister,

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
    reg: vmInfo.TempRegister,
};

const CtrlFlowInstrInfo = struct {
    chunk: *Instr,
    label: vmInfo.LabelType,
};

const LoopInfo = struct {
    const Self = @This();

    continueLabel: vmInfo.LabelType,
    breaks: *std.ArrayList(CtrlFlowInstrInfo),
    continues: *std.ArrayList(CtrlFlowInstrInfo),

    pub fn init(allocator: Allocator) !Self {
        const breaksPtr = try utils.createMut(std.ArrayList(CtrlFlowInstrInfo), allocator, .empty);
        const continuesPtr = try utils.createMut(std.ArrayList(CtrlFlowInstrInfo), allocator, .empty);

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

    propAccessReturnsPtr: bool = false,
    sliceAccessGoToSlicePtr: bool = false,
};

const RegInfoVarInfo = struct {
    stackLocation: ?u64 = null,
    prevInfo: ?*RegInfoVarInfo = null,
};

pub const RegStateVariants = enum {
    Unused,
    Normal,
    Spilled,
};

pub const RegStateSpilled = struct {
    by: vmInfo.TempRegister,
    reg: vmInfo.TempRegister,
    until: u32,
    location: u64,
};

pub const RegStateInfo = union(RegStateVariants) {
    Unused,
    Normal: vmInfo.TempRegister,
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

    indices: *std.ArrayList(u32),
    baseIndex: usize = 0,
    currentIndex: usize = 0,

    pub fn init(allocator: Allocator) !Self {
        const indicesPtr = try utils.createMut(std.ArrayList(u32), allocator, .empty);
        return .{ .indices = indicesPtr };
    }

    pub fn isUsed(self: Self) bool {
        return self.indices.items.len > 0;
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

pub const RegRemapStateVariants = enum {
    Unused,
    Normal,
    Stored,
    Spilled,
};

pub const RegRemap = union(RegRemapStateVariants) {
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
    const WaitingLabelsList = std.ArrayList(u32);
    const WaitingLabels = std.AutoHashMap(vmInfo.LabelType, *WaitingLabelsList);

    labelInfo: std.ArrayList(LabelInfo),
    waitingLabels: WaitingLabels,

    pub inline fn init(allocator: Allocator) !Self {
        const waitingLabels = WaitingLabels.init(allocator);

        return .{
            .labelInfo = .empty,
            .waitingLabels = waitingLabels,
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
        instrIndex: usize,
    ) !void {
        if (self.waitingLabels.get(labelId)) |arrList| {
            try arrList.append(allocator, @intCast(instrIndex));
        } else {
            const arrList = try utils.createMut(WaitingLabelsList, allocator, .empty);
            try arrList.append(allocator, @intCast(instrIndex));
            try self.waitingLabels.put(labelId, arrList);
        }
    }
};

pub const WriteLocInfo = struct {
    reg: vmInfo.TempRegister,
    value: u64,
};

pub const Proc = struct {
    stackFrameSize: u64 = 0,
    startIndex: u32,
    maxPreserveReg: ?vmInfo.TempRegister = null,
    preProcVirtualReg: vmInfo.TempRegister = 0,
    retStructPtrReg: ?vmInfo.TempRegister = null,
};

/// used for passing to newProc
const ProcConfig = struct {
    retStructPtrReg: ?vmInfo.TempRegister = null,
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

        action: *std.ArrayList(T),
        current: usize = 0,

        pub fn init(allocator: Allocator) !Self {
            const actionPtr = try utils.createMut(std.ArrayList(T), allocator, .empty);

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

pub const BackendRegLimits = struct {
    params: RegisterRange = .{},
    temporary: RegisterRange = .{},
    preserved: RegisterRange = .{},
};

const DataSection = struct {
    const Self = @This();

    data: std.ArrayList(u8),
    strPtrMap: std.StringHashMap(u64),

    pub inline fn init(allocator: Allocator) !Self {
        const strPtrMap = std.StringHashMap(u64).init(allocator);

        return .{
            .data = .empty,
            .strPtrMap = strPtrMap,
        };
    }

    fn adjustPtrToBytecodeLocation(ptr: u64) u64 {
        return ptr + vmInfo.PADDED_BYTECODE_HEADER_LEN;
    }

    pub fn appendString(self: *Self, allocator: Allocator, str: []const u8) !u64 {
        if (self.strPtrMap.get(str)) |ptr| {
            return ptr;
        }

        const ptr = adjustPtrToBytecodeLocation(self.data.items.len);
        try self.data.appendSlice(allocator, str);
        try self.strPtrMap.put(str, ptr);
        return ptr;
    }
};

pub const GenInfo = struct {
    const Self = @This();

    instrList: utils.ArenaArrayList(Instr),
    dataSection: DataSection,
    currentProc: Proc,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    varNameToReg: std.AutoHashMap(identStore.IdentId, vmInfo.TempRegister),
    settings: GenInfoSettings,
    loopInfo: std.ArrayList(*LoopInfo),
    byteCounter: u64,
    currentLabelId: vmInfo.LabelType,
    registers: std.ArrayList(*RegInfo),
    registerStatus: std.ArrayList(RegStatus),
    registerLimits: BackendRegLimits,
    labelByteInfo: LabelByteInfo,
    instrActions: InstrActions,
    regAllocateUtils: struct {
        furthestInstrReach: u32,
        /// 0 for invalid state
        regNextUseIndex: std.ArrayList(u32),
        pendingDeactivations: utils.StaticBufferList(
            vmInfo.TempRegister,
            InstructionVariants.maxOpCount(),
        ),
        protectedRegisters: utils.StaticBufferList(
            vmInfo.TempRegister,
            InstructionVariants.maxOpCount(),
        ),
    },

    pub inline fn init(allocator: Allocator) !Self {
        const varNameReg = std.AutoHashMap(identStore.IdentId, vmInfo.TempRegister).init(allocator);
        const labelByteInfo = try LabelByteInfo.init(allocator);

        return .{
            .instrList = utils.ArenaArrayList(Instr).init(),
            .dataSection = try DataSection.init(allocator),
            .currentProc = .{
                .startIndex = 0,
            },
            .vmInfo = .{
                .stackStartSize = 0,
                .version = version.VERSION,
            },
            .varNameToReg = varNameReg,
            .byteCounter = vmInfo.BYTECODE_HEADER_LEN,
            .settings = .{},
            .loopInfo = .empty,
            .currentLabelId = 0,
            .registers = .empty,
            .registerStatus = .empty,
            .labelByteInfo = labelByteInfo,
            .instrActions = try InstrActions.init(allocator),
            .regAllocateUtils = .{
                .furthestInstrReach = 0,
                .regNextUseIndex = .empty,
                .pendingDeactivations = .{},
                .protectedRegisters = .{},
            },
            .registerLimits = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.instrList.deinit();
    }

    pub fn calculateBytecodeHeaderSize(self: *Self) u32 {
        var binHeadSize = @as(u32, @intCast(
            vmInfo.PADDED_BYTECODE_HEADER_LEN + self.dataSection.data.items.len,
        ));
        const padding = utils.calculatePadding(binHeadSize, vmInfo.POINTER_SIZE);
        binHeadSize += padding;
        return binHeadSize;
    }

    pub fn writeChunks(self: *Self, writer: *Writer) !void {
        const binHeadSize = self.calculateBytecodeHeaderSize();

        var buf: [vmInfo.BYTECODE_HEADER_LEN]u8 = undefined;
        buf[vmInfo.VERSION_LOCATION] = self.vmInfo.version;
        std.mem.writeInt(
            u32,
            buf[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
            binHeadSize,
            .little,
        );
        std.mem.writeInt(
            u32,
            buf[vmInfo.STACK_START_LOCATION .. vmInfo.STACK_START_LOCATION + 4],
            self.vmInfo.stackStartSize,
            .little,
        );
        try writer.writeAll(&buf);

        const padAmount = vmInfo.PADDED_BYTECODE_HEADER_LEN - vmInfo.BYTECODE_HEADER_LEN;
        var pad: [padAmount]u8 = undefined;
        @memset(&pad, 0);
        try writer.writeAll(&pad);

        try writer.writeAll(self.dataSection.data.items);
        var i = vmInfo.PADDED_BYTECODE_HEADER_LEN + self.dataSection.data.items.len;
        while (i < binHeadSize) : (i += 1) {
            try writer.writeByte(@as(u8, 0));
        }

        i = 0;
        while (i < self.instrList.list.items.len) : (i += 1) {
            const skipped = self.handleSkipInstruction(i);
            if (!skipped) {
                const instr = self.instrList.list.items[i];
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
        try self.instrList.append(instr);
        const instrIndex = self.instrList.list.items.len - 1;
        try self.runInstrAddEffects(allocator, instrIndex);
        return &self.instrList.list.items[instrIndex];
    }

    pub fn runInstrAddEffects(self: *Self, allocator: Allocator, instrIndex: usize) !void {
        try self.setInstrRegUseIndex(allocator, &self.instrList.list.items[instrIndex], instrIndex);
    }

    pub fn setInstrRegActiveStatus(self: *Self, instr: *const Instr, instrIndex: usize) void {
        self.applyRegIndexFnToInstr(usize, instr, instrIndex, setRegActiveStatusIndex);
    }

    fn setRegActiveStatusIndex(
        self: *Self,
        reg: vmInfo.TempRegister,
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
        reg: vmInfo.TempRegister,
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
        comptime func: fn (*Self, Allocator, vmInfo.TempRegister, T) Allocator.Error!void,
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
            .Add32, .Sub32 => |inner| {
                try func(self, allocator, inner.reg, value);
                try func(self, allocator, inner.dest, value);
            },
            .Add64, .Sub64 => |inner| {
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
            .MovSpNegOffset64, .MovSpNegOffsetAny => |inner| try func(
                self,
                allocator,
                inner.reg,
                value,
            ),
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
            .Store64AtRegOffset16,
            .Store32AtRegOffset16,
            .Store16AtRegOffset16,
            .Store8AtRegOffset16,
            => |inner| {
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
            .Load64AtRegPostInc16,
            .Load32AtRegPostInc16,
            .Load16AtRegPostInc16,
            .Load8AtRegPostInc16,
            => |inner| {
                try func(self, allocator, inner.fromRegPtr, value);
                try func(self, allocator, inner.toReg, value);
            },
            .MulReg8AddReg => |inner| {
                try func(self, allocator, inner.addReg, value);
                try func(self, allocator, inner.mulReg, value);
                try func(self, allocator, inner.dest, value);
            },
            .MulReg16AddReg => |inner| {
                try func(self, allocator, inner.addReg, value);
                try func(self, allocator, inner.mulReg, value);
                try func(self, allocator, inner.dest, value);
            },
            .MulReg32AddReg => |inner| {
                try func(self, allocator, inner.addReg, value);
                try func(self, allocator, inner.mulReg, value);
                try func(self, allocator, inner.dest, value);
            },
            .MulReg64AddReg => |inner| {
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

    pub fn getNextRegister(self: *Self, allocator: Allocator) !vmInfo.TempRegister {
        return try self.getNextRegisterUtil(allocator, .Temporary);
    }

    pub fn getNextRegisterUtil(
        self: *Self,
        allocator: Allocator,
        usage: RegisterUsage,
    ) !vmInfo.TempRegister {
        const regInfoPtr = try utils.createMut(
            RegInfo,
            allocator,
            try RegInfo.initWithUsage(allocator, usage),
        );
        try self.registers.append(allocator, regInfoPtr);
        return @intCast(self.registers.items.len - 1);
    }

    pub fn getVariableRegister(self: Self, nameIdentId: identStore.IdentId) vmInfo.TempRegister {
        return self.varNameToReg.get(nameIdentId).?;
    }

    /// will override data about reg, only use this on newly assigned registers
    pub fn setVariableRegister(
        self: *Self,
        allocator: Allocator,
        nameIdentId: identStore.IdentId,
        reg: vmInfo.TempRegister,
    ) !void {
        try self.varNameToReg.put(nameIdentId, reg);

        const prevInfoOrNull = self.registers.items[reg].varInfo;
        self.registers.items[reg].varInfo = RegInfoVarInfo{
            .prevInfo = if (prevInfoOrNull) |prevInfo|
                try utils.createMut(RegInfoVarInfo, allocator, prevInfo)
            else
                null,
        };
    }

    // deactivates register and removes variable info
    pub fn removeVariableRegister(self: *Self, nameIdentId: identStore.IdentId) void {
        const reg = self.varNameToReg.get(nameIdentId) orelse return;
        const varInfo = self.registers.items[reg].varInfo;

        if (varInfo) |info| {
            const prevInfoOrNull = info.prevInfo;
            if (prevInfoOrNull) |prevInfo| {
                self.registers.items[reg].varInfo = prevInfo.*;
            } else {
                _ = self.varNameToReg.remove(nameIdentId);
            }
        } else {
            _ = self.varNameToReg.remove(nameIdentId);
        }
    }

    pub fn isRegVariable(self: Self, reg: vmInfo.TempRegister) bool {
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

    pub fn getRegInfo(self: Self, reg: vmInfo.TempRegister) ?*RegInfo {
        return self.registers.items[reg];
    }

    pub fn getVarGenInfoFromName(self: Self, name: []const u8) ?struct {
        *RegInfo,
        vmInfo.empRegister,
    } {
        if (self.varNameToReg.get(name)) |reg| {
            const infoOrNull = self.registers.items[reg];
            if (infoOrNull) |info| {
                return .{ info, reg };
            }
        }

        return null;
    }

    pub fn newProc(self: *Self) !void {
        try self.newProcConfig(.{});
    }

    pub fn newProcConfig(self: *Self, procConfig: ProcConfig) !void {
        // noop for possible sp add instr
        try self.instrList.append(.{ .NoOp = {} });
        // noop for possible register push instr
        try self.instrList.append(.{ .NoOp = {} });
        // noop for possible lr push instr
        try self.instrList.append(.{ .NoOp = {} });
        const startIndex = self.instrList.list.items.len - 3;

        self.currentProc = .{
            .startIndex = @intCast(startIndex),
            .retStructPtrReg = procConfig.retStructPtrReg,
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
            for (arrList.items) |instrIndex| {
                self.updateInstrLabelLocation(instrIndex, location);
            }

            _ = self.labelByteInfo.waitingLabels.remove(label);
        }
    }

    fn updateInstrLabelLocation(self: Self, instrIndex: usize, location: u64) void {
        const instr = &self.instrList.list.items[instrIndex];
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
        .Add32, .Sub32 => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u32, @intCast(inner.data), .little);
        },
        .Add64, .Sub64 => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.reg));
            try writer.writeInt(u64, @intCast(inner.data), .little);
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
        .Store64AtRegOffset16,
        .Store32AtRegOffset16,
        .Store16AtRegOffset16,
        .Store8AtRegOffset16,
        => |inner| {
            try writer.writeByte(@intCast(inner.fromReg));
            try writer.writeByte(@intCast(inner.toRegPtr));
            try writer.writeInt(u16, inner.offset, .little);
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
        .Load64AtRegPostInc16,
        .Load32AtRegPostInc16,
        .Load16AtRegPostInc16,
        .Load8AtRegPostInc16,
        => |inner| {
            try writer.writeByte(@intCast(inner.fromRegPtr));
            try writer.writeByte(@intCast(inner.toReg));
            try writer.writeInt(u16, inner.inc, .little);
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
        .MulReg8AddReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.addReg));
            try writer.writeByte(@intCast(inner.mulReg));
            try writer.writeInt(u8, inner.data, .little);
        },
        .MulReg16AddReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.addReg));
            try writer.writeByte(@intCast(inner.mulReg));
            try writer.writeInt(u16, inner.data, .little);
        },
        .MulReg32AddReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.addReg));
            try writer.writeByte(@intCast(inner.mulReg));
            try writer.writeInt(u32, inner.data, .little);
        },
        .MulReg64AddReg => |inner| {
            try writer.writeByte(@intCast(inner.dest));
            try writer.writeByte(@intCast(inner.addReg));
            try writer.writeByte(@intCast(inner.mulReg));
            try writer.writeInt(u64, inner.data, .little);
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
    var hasBranchLink = false;

    while (i < context.genInfo.instrList.list.items.len) : (i += 1) {
        const instr = context.genInfo.instrList.list.items[i];
        switch (instr) {
            .BranchLink, .BranchLinkBack => {
                hasBranchLink = true;
                var procReg: usize = context.genInfo.currentProc.preProcVirtualReg;
                while (procReg < context.genInfo.registers.items.len) : (procReg += 1) {
                    const regInfo = context.genInfo.registers.items[procReg];
                    if (!regInfo.useIndices.isUsed()) continue;
                    const firstFound = regInfo.useIndices.first();
                    const lastUsed = regInfo.useIndices.last();

                    if (firstFound < i and i < lastUsed) {
                        const usage = context.genInfo.registers.items[procReg].usage;
                        context.genInfo.registers.items[procReg].usage = if (usage == .Param)
                            .{ .ParamPreserved = usage.Param }
                        else
                            .{ .Preserved = {} };

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
        vmInfo.POINTER_SIZE * (if (hasBranchLink and !isRoot) @as(u8, 1) else @as(u8, 0));

    if (hasBranchLink and !isRoot) {
        const startOffset: u8 = if (context.genInfo.currentProc.maxPreserveReg != null)
            vmInfo.POINTER_SIZE
        else
            0;

        const pushLRInstr = prePushLRNegOffsetAnyInstr(
            frameSize.* + stackOffset - startOffset,
        );
        const popLRInstr = postPopLRNegOffsetAnyInstr(
            frameSize.* + stackOffset - startOffset,
        );

        const index = instrStartIndex + vmInfo.PUSH_LR_BASE_OFFSET;
        context.genInfo.instrList.list.items[index] = pushLRInstr;
        try context.genInfo.instrList.append(popLRInstr);
    }

    if (context.genInfo.currentProc.maxPreserveReg) |maxReg| {
        const pushInstr = pushRegNegOffsetAnyInstr(maxReg, frameSize.* + stackOffset);
        const popInstr = popRegNegOffsetAnyInstr(maxReg, frameSize.* + stackOffset);

        const index = instrStartIndex + vmInfo.PUSH_REG_BASE_OFFSET;
        context.genInfo.instrList.list.items[index] = pushInstr;
        try context.genInfo.instrList.append(popInstr);
    }

    const instrs = context.genInfo.instrList.list.items[instrStartIndex..];
    if (context.settings.debug.allocateRegisters) {
        try backend.allocateRegisters(
            allocator,
            context,
            instrs,
            instrStartIndex,
            frameSize,
        );
    }

    var spInstrByteCountOffset: u32 = 0;
    if (frameSize.* + stackOffset > 0) {
        frameSize.* += stackOffset;
        frameSize.* += utils.calculatePadding(frameSize.*, vmInfo.POINTER_SIZE);
        const spInstrs = instructions.getSpIncInstructions(frameSize.*);
        spInstrByteCountOffset += spInstrs.sub.getInstrLen();

        const procStartIndex = context.genInfo.currentProc.startIndex;
        context.genInfo.instrList.list.items[procStartIndex] = spInstrs.add;
        try context.genInfo.instrList.append(spInstrs.sub);
    }

    try adjustInstructions(allocator, context, instrs, instrStartIndex, frameSize.*, stackOffset);

    const finishInstr = if (isRoot) Instr{ .End = {} } else Instr{ .Ret = {} };
    try context.genInfo.instrList.append(finishInstr);
    context.genInfo.byteCounter += 1 + spInstrByteCountOffset;

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
                &context.genInfo.instrList.list.items[i + instrStartIndex],
                i + instrStartIndex,
                frameSize,
                stackOffset,
            );
            const instrLen = context.genInfo.instrList.list.items[i + instrStartIndex].getInstrLen();
            context.genInfo.byteCounter += instrLen;
        }

        while (context.genInfo.handleInsertInstr(i + instrStartIndex)) |instr| {
            try adjustInstruction(
                allocator,
                context,
                instr,
                undefined, // NOTE, IMPORTANT: not meant to be used
                frameSize,
                stackOffset,
            );
            context.genInfo.byteCounter += instr.getInstrLen();
        }
    }
}

/// adjusts store instructions that are based on sp offsets
/// instrIndex is for storing instructions listening for label positions
/// NOTE: not designed for inserted instructions that reference a label
fn adjustInstruction(
    allocator: Allocator,
    context: *Context,
    instr: *Instr,
    instrIndex: usize,
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
            const newInstr = movSpNegOffset(
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
                    instrIndex,
                );
            }
        },
        .BranchLink => |*data| {
            const labelId = data.*;
            data.* = context.genInfo.byteCounter + instr.getInstrLen();
            try context.genInfo.labelByteInfo.waitForLabel(
                allocator,
                @intCast(labelId),
                instrIndex,
            );
        },
        .BranchLinkBack => |*labelId| {
            const loc = try context.genInfo.labelByteInfo.getLabelLocation(@intCast(labelId.*));
            labelId.* = context.genInfo.byteCounter - loc.?;
        },
        // not changed by stackOffset
        .PrePushRegNegOffsetAny => |pushInstr| {
            const newInstr = pushRegNegOffsetAnyInstr(
                pushInstr.reg,
                frameSize - pushInstr.offset,
            );
            instr.* = newInstr;
        },
        // not changed by stackOffset
        .PostPopRegNegOffsetAny => |popInstr| {
            const newInstr = popRegNegOffsetAnyInstr(
                popInstr.reg,
                frameSize - popInstr.offset,
            );
            instr.* = newInstr;
        },
        else => {},
    }
}

fn postPopLRNegOffsetAnyInstr(offset: u64) Instr {
    const spOpSize = getOpSizeFromNum(offset);

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

fn prePushLRNegOffsetAnyInstr(offset: u64) Instr {
    const spOpSize = getOpSizeFromNum(offset);

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

fn popRegNegOffsetAnyInstr(reg: vmInfo.TempRegister, offset: u64) Instr {
    const spOpSize = getOpSizeFromNum(offset);

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

fn pushRegNegOffsetAnyInstr(reg: vmInfo.TempRegister, offset: u64) Instr {
    const spOpSize = getOpSizeFromNum(offset);

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

fn movSpNegOffset(reg: vmInfo.TempRegister, offset: u64) Instr {
    const spOpSize = getOpSizeFromNum(offset);

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
    const backend = switch (backendType) {
        .Bytecode => bytecodeBackend.backend,
    };

    try backend.initMetadata(allocator, context);

    const mainFn = context.compInfo.functions.get(identStore.KNOWN_IDENT_IDS.main) orelse
        return CodeGenError.MainFunctionNotFound;
    try context.genInfo.newProc();
    _ = try genBytecode(allocator, context, mainFn.body);

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
) GenBytecodeError!?vmInfo.TempRegister {
    return genBytecodeUtil(allocator, context, node, null);
}

pub fn genBytecodeUtil(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
    writeLoc: ?*WriteLocInfo,
) GenBytecodeError!?vmInfo.TempRegister {
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

            if (node.typeInfo.data.VarOrVarDec.lastVarUse) {
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

                context.genInfo.registers.items[newReg].varInfo =
                    context.genInfo.registers.items[reg].varInfo;

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

                context.genInfo.registers.items[outReg].varInfo =
                    context.genInfo.registers.items[reg].varInfo;

                break :a outReg;
            };

            if (!node.typeInfo.data.VarOrVarDec.lastVarUse) {
                try context.genInfo.setVariableRegister(allocator, dec.nameIdentId, varReg);
            }
        },
        .Value => |value| {
            switch (value) {
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
                .Number => |num| {
                    const reg = try context.genInfo.getNextRegister(allocator);

                    const instr = switch (num) {
                        .U8 => |byte| Instr{
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

                    const writeLocInfo: *WriteLocInfo = if (writeLoc) |wLoc|
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

                        var newWriteLoc = WriteLocInfo{
                            .reg = ptrReg,
                            .value = context.genInfo.currentProc.stackFrameSize,
                        };

                        const prevInfoOrNull = context.genInfo.registers.items[ptrReg].varInfo;
                        const prevInfoPtr = if (prevInfoOrNull) |prevInfo| try utils.createMut(
                            RegInfoVarInfo,
                            allocator,
                            prevInfo,
                        ) else null;
                        context.genInfo.registers.items[ptrReg].varInfo = .{
                            .prevInfo = prevInfoPtr,
                            .stackLocation = context.genInfo.currentProc.stackFrameSize,
                        };

                        context.genInfo.currentProc.stackFrameSize += node.typeInfo.size;

                        break :a &newWriteLoc;
                    };

                    const itemSize: u64 = if (items.len > 0)
                        items[0].typeInfo.size
                    else
                        0;
                    const isPrimitive = if (items.len > 0)
                        nodeIsPrimitive(items[0])
                    else
                        false;
                    const itemPadding = utils.calculatePadding(itemSize, node.typeInfo.alignment);

                    {
                        const prevAccessReturn = context.genInfo.settings.propAccessReturnsPtr;
                        context.genInfo.settings.propAccessReturnsPtr = true;
                        defer context.genInfo.settings.propAccessReturnsPtr = prevAccessReturn;

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
                                const storeInstr = instructions.storeRegAtPtrPostInc(
                                    reg,
                                    writeLocInfo.reg,
                                    @intCast(itemSize + itemPadding),
                                    @intCast(itemSize),
                                );
                                try context.genInfo.appendChunk(allocator, storeInstr);

                                writeLocInfo.value += itemSize + itemPadding;
                            } else if (itemPadding > 0) {
                                const addInstr = Instr{
                                    .Add8 = .{
                                        .dest = writeLocInfo.reg,
                                        .reg = writeLocInfo.reg,
                                        .data = itemPadding,
                                    },
                                };
                                try context.genInfo.appendChunk(allocator, addInstr);
                            }
                        }
                    }

                    if (writeLoc == null) {
                        try context.genInfo.appendChunk(allocator, movSpInstr);
                        return writeLocInfo.reg;
                    }
                },
                .String => |str| {
                    const ptr = try context.genInfo.dataSection.appendString(
                        allocator,
                        // remove quotes
                        str[1 .. str.len - 1],
                    );
                    return try sliceBytecodeFromPtr(allocator, context, ptr, str.len);
                },
                else => utils.unimplemented(),
            }
        },
        .OpExpr => |expr| {
            var leftReg: vmInfo.TempRegister = undefined;

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

            var outReg: ?vmInfo.TempRegister = null;

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

            if (node.typeInfo.data.VarOrVarDec.lastVarUse) {
                context.genInfo.removeVariableRegister(name);
            }

            if (context.genInfo.getRegInfo(resReg)) |regInfo| a: {
                if (node.typeInfo.nodeType != .Other) break :a;

                if (regInfo.varInfo) |varInfo| {
                    const location = varInfo.stackLocation orelse break :a;
                    const instr = instructions.loadRegAtSpNegOffset(
                        resReg,
                        location,
                        node.typeInfo.size,
                    );
                    try context.genInfo.appendChunk(allocator, instr);
                }
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
            const forLoopStartInstrIndex = context.genInfo.instrList.list.items.len - 1;

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

            const currentInstrIndex = context.genInfo.instrList.list.items.len - 1;
            const startVReg = context.genInfo.currentProc.preProcVirtualReg;
            const endVReg = context.genInfo.registers.items.len;
            for (startVReg..endVReg) |vReg| {
                const useIndices = &context.genInfo.registers.items[vReg].useIndices;
                const first = useIndices.last();
                if (first > forLoopStartInstrIndex) {
                    try useIndices.indices.append(allocator, @intCast(currentInstrIndex));
                }
            }
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

            const prevOutput = context.genInfo.settings.propAccessReturnsPtr;
            context.genInfo.settings.propAccessReturnsPtr = true;
            const destReg = try genBytecode(allocator, context, targetNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            context.genInfo.settings.propAccessReturnsPtr = prevOutput;
            const srcReg = try genBytecode(allocator, context, set.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const instr = if (isDeref or
                set.value.variant == .IndexValue or
                set.value.variant == .PropertyAccess)
                instructions.storeRegAtPtr(
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

            const writeLocInfo: *WriteLocInfo = if (writeLoc) |wLoc|
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

                var newWriteLoc = WriteLocInfo{
                    .reg = writeReg,
                    .value = context.genInfo.currentProc.stackFrameSize,
                };

                context.genInfo.currentProc.stackFrameSize += initSize;

                break :a &newWriteLoc;
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

            var indexReg: ?vmInfo.TempRegister = null;
            var ptrReg: ?vmInfo.TempRegister = null;

            if (init.indexIdentId) |ident| {
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

            if (init.ptrIdentId) |ident| {
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

            const itemAlignment = try init.initType.astType.getAlignment(allocator, context);
            const itemSize = try init.initType.astType.getSize(allocator, context);
            const itemPadding = utils.calculatePadding(itemSize, itemAlignment);
            if (nodeIsPrimitive(init.initNode)) {
                const resReg = resRegOrNull orelse return CodeGenError.ReturnedRegisterNotFound;
                const writeInstr = instructions.storeRegAtPtrPostInc(
                    resReg,
                    writeLocInfo.reg,
                    @intCast(itemSize + itemPadding),
                    @intCast(itemSize),
                );
                try context.genInfo.appendChunk(allocator, writeInstr);

                writeLocInfo.value += itemSize + itemPadding;
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
                const addInstr = instructions.addConst(reg, reg, itemSize + itemPadding);
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

            const loadInstr = instructions.loadRegAtPtr(resReg, destReg, node.typeInfo.size);
            try context.genInfo.appendChunk(allocator, loadInstr);

            return destReg;
        },
        .Pointer => |inner| {
            if (node.typeInfo.data == .ArrDecPtr) {
                const len = node.typeInfo.data.ArrDecPtr.makesSliceWithLen;
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

                const storeInstr = instructions.storeRegAtSpNegOffset(
                    resReg,
                    @intCast(inner.node.typeInfo.size),
                    context.genInfo.currentProc.stackFrameSize,
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

            var writeLocInfo: *WriteLocInfo = if (writeLoc) |wLoc|
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

                var newWriteLoc = WriteLocInfo{ .reg = resReg, .value = writeVal };
                break :a &newWriteLoc;
            };

            const def = context.compInfo.getStructDec(init.nameIdentId).?;

            for (def.attributes, 0..) |defAttr, index| {
                if (defAttr.attr != .Member) continue;

                const attr = init.findAttribute(defAttr.nameIdentId).?;

                const regOrNull = try genBytecodeUtil(
                    allocator,
                    context,
                    attr.value,
                    writeLocInfo,
                );

                const attrSizeInfo = init.attrSizes[index];
                const padding = if (index + 1 < def.attributes.len) a: {
                    if (def.attributes[index + 1].attr != .Member) break :a 0;
                    const nextSizeInfo = init.attrSizes[index + 1];
                    const nextPadding = utils.calculatePadding(
                        writeLocInfo.value + attrSizeInfo.size,
                        nextSizeInfo.alignment,
                    );
                    break :a nextPadding;
                } else 0;

                if (nodeIsPrimitive(attr.value)) {
                    const reg = regOrNull orelse return CodeGenError.ReturnedRegisterNotFound;

                    const instr = instructions.storeRegAtPtrPostInc(
                        reg,
                        writeLocInfo.reg,
                        @intCast(attrSizeInfo.size + padding),
                        @intCast(attrSizeInfo.size),
                    );
                    try context.genInfo.appendChunk(allocator, instr);

                    writeLocInfo.value += attrSizeInfo.size + padding;
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
            const loc, const isFunction = try getPropLocation(
                allocator,
                context,
                node,
                accessNode.property,
            );

            const reg = try calculateAccessOffset(
                allocator,
                context,
                accessNode.value,
                loc,
            );

            if (isFunction or context.genInfo.settings.propAccessReturnsPtr) return reg;

            const outReg = try context.genInfo.getNextRegister(allocator);
            const instr = instructions.loadRegAtPtr(reg, outReg, node.typeInfo.size);
            try context.genInfo.appendChunk(allocator, instr);

            return outReg;
        },
        .IndexValue => |indexNode| {
            const indexReg = try genBytecode(allocator, context, indexNode.index) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const reg = a: {
                const prev = context.genInfo.settings.sliceAccessGoToSlicePtr;
                context.genInfo.settings.sliceAccessGoToSlicePtr = true;
                defer context.genInfo.settings.sliceAccessGoToSlicePtr = prev;

                break :a try calculateAccessOffset(
                    allocator,
                    context,
                    indexNode.target,
                    0,
                );
            };

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

            if (context.genInfo.settings.propAccessReturnsPtr) {
                return reg;
            } else {
                const outReg = try context.genInfo.getNextRegister(allocator);

                const loadInstr = instructions.loadRegAtPtr(reg, outReg, @intCast(node.typeInfo.size));
                try context.genInfo.appendChunk(allocator, loadInstr);

                return outReg;
            }
        },
        .FuncCall => |call| {
            const func = node.typeInfo.data.Others.resolvesToFunc.?;
            const labelIdPtr = switch (func.genericState) {
                .Generic => |*generic| &generic
                    .genericInstances
                    .items[node.typeInfo.data.Others.funcGenInstanceIndex.?]
                    .labelId,
                .Normal => |*normal| &normal.labelId,
            };
            const branchLabelId = labelIdPtr.* orelse a: {
                const labelId = context.genInfo.takeLabelId();
                labelIdPtr.* = labelId;
                break :a labelId;
            };

            const hasSelf = func.params.selfInfo != null;
            const returnsStruct = func.returnType.info.astType.* == .Custom;

            if (returnsStruct) {
                const padding = utils.calculatePadding(
                    context.genInfo.currentProc.stackFrameSize,
                    func.returnType.alignment,
                );
                context.genInfo.currentProc.stackFrameSize += padding;
                const retStackPos = context.genInfo.currentProc.stackFrameSize;
                context.genInfo.currentProc.stackFrameSize += func.returnType.size;

                const regPtrReg = try context.genInfo.getNextRegisterUtil(
                    allocator,
                    .{ .Param = 0 },
                );

                const movInstr = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = regPtrReg,
                        .offset = retStackPos,
                    },
                };
                try context.genInfo.appendChunk(allocator, movInstr);
            }

            const paramRegStart = a: {
                const retRegInc: u8 = if (returnsStruct) 1 else 0;
                const selfRegInc: u8 = if (hasSelf) 1 else 0;
                break :a retRegInc + selfRegInc;
            };

            if (hasSelf) {
                const srcReg = try genBytecode(allocator, context, call.func) orelse
                    return CodeGenError.ResultOfAccessRegNotFound;
                const destReg = try context.genInfo.getNextRegisterUtil(
                    allocator,
                    .{ .Param = paramRegStart },
                );

                const movInstr = Instr{
                    .Mov = .{
                        .src = srcReg,
                        .dest = destReg,
                    },
                };
                try context.genInfo.appendChunk(allocator, movInstr);
            }

            for (call.params, 0..) |param, index| {
                const reg = try genBytecode(allocator, context, param) orelse
                    return CodeGenError.ReturnedRegisterNotFound;

                const paramReg = if (param.typeInfo.nodeType == .Struct) a: {
                    const spReg = try context.genInfo.getNextRegister(allocator);
                    const padding = utils.calculatePadding(
                        context.genInfo.currentProc.stackFrameSize,
                        param.typeInfo.alignment,
                    );
                    context.genInfo.currentProc.stackFrameSize += padding;
                    const spOffset = context.genInfo.currentProc.stackFrameSize;
                    context.genInfo.currentProc.stackFrameSize += param.typeInfo.size;

                    const movSpInstr = Instr{
                        .MovSpNegOffsetAny = .{
                            .reg = spReg,
                            .offset = spOffset,
                        },
                    };
                    try context.genInfo.appendChunk(allocator, movSpInstr);

                    try memCpyInstrs(allocator, context, reg, spReg, param.typeInfo.size);
                    break :a spReg;
                } else reg;

                const regUsage: RegisterUsage = .{ .Param = @intCast(paramRegStart + index) };
                const resReg = try context.genInfo.getNextRegisterUtil(allocator, regUsage);
                const movInstr = Instr{
                    .Mov = .{
                        .src = paramReg,
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

            if (func.returnType.info.astType.* == .Void) {
                return null;
            }

            return try context.genInfo.getNextRegisterUtil(allocator, .{ .Return = 0 });
        },
        .ReturnNode => |inner| {
            const regRes = try genBytecode(allocator, context, inner) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const retReg = try context.genInfo.getNextRegisterUtil(
                allocator,
                .{ .Return = 0 },
            );

            if (context.genInfo.currentProc.retStructPtrReg) |ptrReg| {
                try memCpyInstrs(allocator, context, regRes, ptrReg, inner.typeInfo.size);
            } else {
                const movInstr = Instr{
                    .Mov = .{
                        .src = regRes,
                        .dest = retReg,
                    },
                };
                try context.genInfo.appendChunk(allocator, movInstr);
            }
        },
        else => {},
    }

    return null;
}

fn memCpyInstrs(
    allocator: Allocator,
    context: *Context,
    fromPtrReg: vmInfo.TempRegister,
    toPtrReg: vmInfo.TempRegister,
    size: u64,
) !void {
    const scratchReg = try context.genInfo.getNextRegister(allocator);

    var offset: u64 = 0;
    while (offset < size) {
        const diff = size - offset;
        const copySize: u8 = switch (diff) {
            1 => 1,
            2, 3 => 2,
            4...7 => 4,
            else => 8,
        };

        const loadInstr = if (diff == copySize)
            instructions.loadRegAtPtr(fromPtrReg, scratchReg, copySize)
        else
            loadAtRegPostInc(fromPtrReg, scratchReg, copySize, copySize);
        const storeInstr = if (diff == copySize)
            instructions.storeRegAtPtr(scratchReg, toPtrReg, copySize)
        else
            instructions.storeRegAtPtrPostInc(scratchReg, toPtrReg, copySize, copySize);

        try context.genInfo.appendChunk(allocator, loadInstr);
        try context.genInfo.appendChunk(allocator, storeInstr);

        if (diff == copySize) break;

        offset += copySize;
    }

    const srcSubInstr = instructions.subConst(fromPtrReg, fromPtrReg, offset);
    const destSubInstr = instructions.subConst(toPtrReg, toPtrReg, offset);

    try context.genInfo.appendChunk(allocator, srcSubInstr);
    try context.genInfo.appendChunk(allocator, destSubInstr);
}

fn loadAtRegPostInc(
    outReg: vmInfo.TempRegister,
    ptrReg: vmInfo.TempRegister,
    size: u64,
    inc: u16,
) Instr {
    return switch (size) {
        1 => Instr{
            .Load8AtRegPostInc16 = .{
                .toReg = outReg,
                .fromRegPtr = ptrReg,
                .inc = inc,
            },
        },
        2 => Instr{
            .Load16AtRegPostInc16 = .{
                .toReg = outReg,
                .fromRegPtr = ptrReg,
                .inc = inc,
            },
        },
        3, 4 => Instr{
            .Load32AtRegPostInc16 = .{
                .toReg = outReg,
                .fromRegPtr = ptrReg,
                .inc = inc,
            },
        },
        5...8 => Instr{
            .Load64AtRegPostInc16 = .{
                .toReg = outReg,
                .fromRegPtr = ptrReg,
                .inc = inc,
            },
        },
        else => unreachable,
    };
}

fn getPropLocation(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
    propIdentId: identStore.IdentId,
) !struct { u64, bool } {
    if (node.typeInfo.data == .Slice or
        node.variant.PropertyAccess.value.typeInfo.nodeType == .Slice)
    {
        return .{ builtins.getSlicePropLocations(propIdentId).?, false };
    }

    const fromName = node.typeInfo.data.PropertyAccess;
    const dec = context.compInfo.getStructDec(fromName).?;
    const isFunction = dec.isPropFunction(propIdentId);

    return if (isFunction)
        .{ @as(u64, 0), isFunction }
    else
        .{ (try dec.getMemberLocation(allocator, context, propIdentId)).?, isFunction };
}

fn codegenFunctions(
    allocator: Allocator,
    context: *Context,
    comptime backend: BackendInterface,
) !void {
    for (context.compInfo.methodFunctions.items) |func| {
        try generateFunction(allocator, context, backend, func);
    }

    var funcIter = context.compInfo.functions.valueIterator();
    while (funcIter.next()) |func| {
        if (func.*.nameIdentId == identStore.KNOWN_IDENT_IDS.main) continue;
        try generateFunction(allocator, context, backend, func.*);
    }
}

fn generateFunction(
    allocator: Allocator,
    context: *Context,
    comptime backend: BackendInterface,
    func: *ast.FuncDecNode,
) !void {
    switch (func.genericState) {
        .Generic => |generic| {
            for (generic.genericInstances.items) |*instance| {
                const procConfig = try functionSetupBytecode(
                    allocator,
                    context,
                    func,
                    &instance.labelId,
                );
                try context.genInfo.newProcConfig(procConfig);
                _ = try genBytecode(allocator, context, instance.funcRootNode);

                try context.genInfo.finishProc(allocator, context, false, backend);
            }
        },
        .Normal => |*normal| {
            const procConfig = try functionSetupBytecode(
                allocator,
                context,
                func,
                &normal.labelId,
            );
            try context.genInfo.newProcConfig(procConfig);
            _ = try genBytecode(allocator, context, func.body);

            try context.genInfo.finishProc(allocator, context, false, backend);
        },
    }

    func.labelsGenerated = true;
}

fn functionSetupBytecode(
    allocator: Allocator,
    context: *Context,
    func: *ast.FuncDecNode,
    labelId: *?u32,
) !ProcConfig {
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

    const returnsStruct = func.returnType.info.astType.* == .Custom;
    const retStructReg = if (returnsStruct)
        try context.genInfo.getNextRegisterUtil(allocator, .{ .Param = 0 })
    else
        null;

    const indexStart: usize = if (returnsStruct) 1 else 0;
    for (func.params.params, indexStart..) |param, index| {
        const usage: RegisterUsage = .{ .Param = @intCast(index) };
        const paramReg = try context.genInfo.getNextRegisterUtil(allocator, usage);
        try context.genInfo.setVariableRegister(allocator, param.nameIdentId, paramReg);
    }

    return .{
        .retStructPtrReg = retStructReg,
    };
}

fn nodeIsPrimitive(node: *ast.AstNode) bool {
    return switch (node.variant) {
        .StructInit, .ArrayInit => false,
        .Value => |val| val != .ArrayDec,
        .Pointer => if (node.typeInfo.data == .ArrDecPtr) false else true,
        else => true,
    };
}

pub fn getOpSizeFromNum(num: u64) OpSizes {
    var res: ?OpSizes = null;

    inline for (sizeToOpSizeTuple) |tuple| {
        if (num < tuple[0]) {
            res = tuple[1];
            break;
        }
    }

    return res.?;
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
    offset: u64,
) !vmInfo.TempRegister {
    const prevRetFormat = context.genInfo.settings.propAccessReturnsPtr;
    context.genInfo.settings.propAccessReturnsPtr = true;
    defer context.genInfo.settings.propAccessReturnsPtr = prevRetFormat;

    switch (node.variant) {
        .PropertyAccess => |accessNode| {
            const fromName = node.typeInfo.data.PropertyAccess;
            const dec = context.compInfo.getStructDec(fromName).?;
            const loc = (try dec.getMemberLocation(allocator, context, accessNode.property)).?;
            return try calculateAccessOffset(
                allocator,
                context,
                accessNode.value,
                offset + loc +
                    if (node.typeInfo.nodeType == .Slice) @as(u8, vmInfo.POINTER_SIZE) * 2 else 0,
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
            var reg = try genBytecode(allocator, context, node) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const isVar = context.genInfo.isRegVariable(reg);

            if (node.typeInfo.nodeType == .Slice and
                context.genInfo.settings.sliceAccessGoToSlicePtr)
            {
                const outReg = try context.genInfo.getNextRegister(allocator);
                const derefInstr = Instr{
                    .Load64AtReg = .{
                        .dest = outReg,
                        .fromRegPtr = reg,
                    },
                };
                try context.genInfo.appendChunk(allocator, derefInstr);
                reg = outReg;
            }

            if (offset == 0) {
                return reg;
            }

            const outReg = try context.genInfo.getNextRegister(allocator);
            if (offset > 0) {
                const instr = instructions.addConst(outReg, reg, offset);
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

fn initArraySliceBytecode(
    allocator: Allocator,
    context: *Context,
    node: *ast.AstNode,
    len: u64,
    writeLoc: ?*WriteLocInfo,
) !?vmInfo.TempRegister {
    var paddedSpLoc: u64 = 0;
    var arrayStartLoc: u64 = 0;

    const writeLocInfo: *WriteLocInfo = if (writeLoc != null) a: {
        const scratchReg = try context.genInfo.getNextRegister(allocator);

        const padding = utils.calculatePadding(
            context.genInfo.currentProc.stackFrameSize,
            node.typeInfo.alignment,
        );
        context.genInfo.currentProc.stackFrameSize += padding;
        arrayStartLoc = context.genInfo.currentProc.stackFrameSize;

        const movSpInstr = Instr{
            .MovSpNegOffsetAny = .{
                .reg = scratchReg,
                .offset = arrayStartLoc,
            },
        };
        try context.genInfo.appendChunk(allocator, movSpInstr);

        var newWriteLoc = WriteLocInfo{
            .reg = scratchReg,
            .value = paddedSpLoc,
        };
        break :a &newWriteLoc;
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

        const movSpInstr = Instr{
            .MovSpNegOffsetAny = .{
                .reg = writeReg,
                .offset = arrayStartLoc,
            },
        };
        try context.genInfo.appendChunk(allocator, movSpInstr);

        var newWriteLoc = WriteLocInfo{
            .reg = writeReg,
            .value = context.genInfo.currentProc.stackFrameSize,
        };
        break :a &newWriteLoc;
    };

    const prevRetPtrBehavior = context.genInfo.settings.propAccessReturnsPtr;
    context.genInfo.settings.propAccessReturnsPtr = true;
    const arrRegOrNull = try genBytecodeUtil(allocator, context, node, writeLocInfo);
    context.genInfo.settings.propAccessReturnsPtr = prevRetPtrBehavior;

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

    if (node.variant == .PropertyAccess) {
        const movInstr = Instr{
            .Mov = .{
                .dest = tempReg,
                .src = arrRegOrNull.?,
            },
        };
        try context.genInfo.appendChunk(allocator, movInstr);
    } else {
        const regLocationOrNull = a: {
            const arrReg = arrRegOrNull orelse break :a null;
            const regInfoOrNull = context.genInfo.getRegInfo(arrReg);
            const info = regInfoOrNull orelse break :a null;
            const varInfo = info.varInfo orelse break :a null;
            break :a varInfo.stackLocation;
        };

        const regLocation = regLocationOrNull orelse arrayStartLoc;

        const setReg = Instr{
            .MovSpNegOffsetAny = .{
                .reg = tempReg,
                .offset = regLocation,
            },
        };
        try context.genInfo.appendChunk(allocator, setReg);

        if (regLocationOrNull == null) {
            context.genInfo.currentProc.stackFrameSize += node.typeInfo.size;
        }
    }

    const storeInstr = Instr{
        .Store64AtRegPostInc16 = .{
            .toRegPtr = sliceStartPtr,
            .fromReg = tempReg,
            .inc = vmInfo.POINTER_SIZE,
        },
    };
    try context.genInfo.appendChunk(allocator, storeInstr);

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

fn sliceBytecodeFromPtr(
    allocator: Allocator,
    context: *Context,
    ptr: u64,
    len: u64,
) !vmInfo.TempRegister {
    const scratch = try context.genInfo.getNextRegister(allocator);

    const padding = utils.calculatePadding(
        context.genInfo.currentProc.stackFrameSize,
        vmInfo.POINTER_SIZE,
    );
    context.genInfo.currentProc.stackFrameSize += padding;

    const movPtrInstr = Instr{
        .SetReg64 = .{
            .reg = scratch,
            .data = ptr,
        },
    };
    try context.genInfo.appendChunk(allocator, movPtrInstr);

    const storePtrInstr = Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = scratch,
            .offset = @intCast(context.genInfo.currentProc.stackFrameSize),
        },
    };
    try context.genInfo.appendChunk(allocator, storePtrInstr);

    context.genInfo.currentProc.stackFrameSize += vmInfo.POINTER_SIZE;

    const movLenInstr = Instr{
        .SetReg64 = .{
            .reg = scratch,
            .data = len,
        },
    };
    try context.genInfo.appendChunk(allocator, movLenInstr);

    const storeLenInstr = Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = scratch,
            .offset = @intCast(context.genInfo.currentProc.stackFrameSize),
        },
    };
    try context.genInfo.appendChunk(allocator, storeLenInstr);

    context.genInfo.currentProc.stackFrameSize += vmInfo.POINTER_SIZE;

    const subInstr = Instr{
        .Sub8 = .{
            .reg = scratch,
            .dest = scratch,
            .data = vmInfo.POINTER_SIZE * 2,
        },
    };
    try context.genInfo.appendChunk(allocator, subInstr);

    return scratch;
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
