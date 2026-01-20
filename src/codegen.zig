const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const blitzContext = blitz.context;
const allocPools = blitz.allocPools;
const free = blitz.free;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const TempRegister = vmInfo.TempRegister;
const Writer = std.Io.Writer;
const Context = blitzContext.Context;
const MemoryPool = std.heap.MemoryPool;

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
    ExpectedLoopInfo,
    StackFrameSizeTooLarge,
    TooManyStructContentRegistersUsed,
    NoCurrentStructContentRegister,
    RegInteractionNotSupported,
    NoTrivialRegister,
    AccessTargetDoesNotHaveStructName,
    LabelDoesNotExist,
};
const GenBytecodeError = CodeGenError || Allocator.Error || std.fmt.ParseIntError;

const LoopCondInfo = struct {
    prevCmpAsReg: bool,
    isCompExpr: bool,
};

const SpOpSizes = enum {
    U16,
    U32,
    U64,
};

const sizeToOpSizeTuple = .{
    .{ std.math.maxInt(u16), SpOpSizes.U16 },
    .{ std.math.maxInt(u32), SpOpSizes.U32 },
    .{ std.math.maxInt(u64), SpOpSizes.U64 },
};

pub const InstructionVariants = enum(u8) {
    const Self = @This();

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

    AddSp16, // inst, 2B data
    SubSp16, // inst, 2B data
    AddSp32, // inst, 4B data
    SubSp32, // inst, 4B data
    AddSp64, // inst, 8B data
    SubSp64, // inst, 8B data

    Store64AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store32AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store16AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store8AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B

    StoreSpSub16AtSpNegOffset16, // inst, sub 2B data, offset 2B

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

    MovByteRange, // inst, dest reg, src reg, start 1B, end 1B

    MulReg16AddReg, // inst, dest, addReg, mulReg, data 2B ( dest = addReg + (mulReg1 * data) )

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn getInstrLen(self: Self) u8 {
        return switch (self) {
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

            .AddSp16, .SubSp16 => 3,
            .AddSp32, .SubSp32 => 5,
            .AddSp64, .SubSp64 => 9,

            .Store64AtRegPostInc16,
            .Store32AtRegPostInc16,
            .Store16AtRegPostInc16,
            .Store8AtRegPostInc16,
            => 5,

            .StoreSpSub16AtSpNegOffset16 => 5,

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

            .MovByteRange => 5,

            .MulReg16AddReg => 6,
        };
    }

    pub fn maxInstrSize() u8 {
        var max: u8 = 0;
        inline for (@typeInfo(Self).@"enum".fields) |field| {
            const val: Self = @enumFromInt(field.value);

            // place holders, not actual instructions
            if (val == .MovSpNegOffsetAny) continue;

            max = @max(val.getInstrLen(), max);
        }
        return max;
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
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

            .AddSp16 => "add_sp_16",
            .SubSp16 => "sub_sp_16",
            .AddSp32 => "add_sp_32",
            .SubSp32 => "sub_sp_32",
            .AddSp64 => "add_sp_64",
            .SubSp64 => "sub_sp_64",

            .Store64AtRegPostInc16 => "store_64_at_reg_post_inc_16",
            .Store32AtRegPostInc16 => "store_32_at_reg_post_inc_16",
            .Store16AtRegPostInc16 => "store_16_at_reg_post_inc_16",
            .Store8AtRegPostInc16 => "store_8_at_reg_post_inc_16",

            .StoreSpSub16AtSpNegOffset16 => "store_sp_sub_16_at_sp_neg_offset_16",

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

            .MovByteRange => "mov_byte_range",

            .MulReg16AddReg => "mul_reg_16_add_reg",
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

fn StoreIncInstr(comptime T: type) type {
    return struct {
        fromReg: TempRegister = 0,
        toRegPtr: TempRegister = 0,
        inc: T = 0,
    };
}

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

fn MovByteRange(comptime T: type) type {
    return struct {
        reg: TempRegister = 0,
        start: u8 = 0,
        end: u8 = 0,
        data: T = 0,
    };
}

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
        reg: TempRegister = 0,
        offset: T = 0,
    };
}

pub const Instr = union(InstructionVariants) {
    const Self = @This();

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

    AddSp16: u16,
    SubSp16: u16,
    AddSp32: u32,
    SubSp32: u32,
    AddSp64: u64,
    SubSp64: u64,

    Store64AtRegPostInc16: StoreIncInstr(u16),
    Store32AtRegPostInc16: StoreIncInstr(u16),
    Store16AtRegPostInc16: StoreIncInstr(u16),
    Store8AtRegPostInc16: StoreIncInstr(u16),

    StoreSpSub16AtSpNegOffset16: struct {
        subTo: u16 = 0,
        offset: u16 = 0,
    },

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

    MovByteRange: struct {
        dest: TempRegister = 0,
        src: TempRegister = 0,
        start: u8 = 0,
        end: u8 = 0,
    },

    MulReg16AddReg: MulRegTAddReg(u16),

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

pub const InstrChunk = struct {
    const Self = @This();

    next: ?*InstrChunk,
    prev: ?*InstrChunk,
    data: Instr,

    pub fn init(data: Instr) Self {
        return .{
            .next = null,
            .prev = null,
            .data = data,
        };
    }
};

const RegScope = struct {
    const Self = @This();

    allocator: Allocator,
    registers: *ArrayList(TempRegister),

    pub fn init(allocator: Allocator) !Self {
        const registers = try utils.createMut(ArrayList(TempRegister), allocator, .empty);

        return .{
            .allocator = allocator,
            .registers = registers,
        };
    }

    pub fn deinit(self: *Self) void {
        self.registers.deinit(self.allocator);
        self.allocator.destroy(self.registers);
    }

    pub fn empty(self: *Self) ![]TempRegister {
        return try self.registers.toOwnedSlice(self.allocator);
    }

    pub fn addRegister(self: *Self, reg: TempRegister) !void {
        try self.registers.append(self.allocator, reg);
    }
};

const RegScopes = struct {
    const Self = @This();

    allocator: Allocator,
    scopes: *ArrayList(*RegScope),

    pub fn init(allocator: Allocator) !Self {
        const firstScope = try RegScope.init(allocator);
        const firstScopePtr = try utils.createMut(RegScope, allocator, firstScope);
        var scopes = try utils.createMut(ArrayList(*RegScope), allocator, .empty);
        try scopes.append(allocator, firstScopePtr);

        return .{
            .allocator = allocator,
            .scopes = scopes,
        };
    }

    fn deinitScope(self: *Self, scope: *RegScope) void {
        scope.deinit();
        self.allocator.destroy(scope);
    }

    pub fn deinit(self: *Self) void {
        for (self.scopes.items) |scope| {
            self.deinitScope(scope);
        }

        self.scopes.deinit(self.allocator);
        self.allocator.destroy(self.scopes);
    }

    pub fn pushScope(self: *Self) !void {
        const newScope = try RegScope.init(self.allocator);
        const newScopePtr = try utils.createMut(RegScope, self.allocator, newScope);
        try self.scopes.append(self.allocator, newScopePtr);
    }

    pub fn popScope(self: *Self) ![]TempRegister {
        if (self.scopes.items.len == 1) {
            const oldContents = try self.scopes.items[0].empty();
            return oldContents;
        }

        const scope = self.scopes.pop().?;
        const oldContents = try scope.registers.toOwnedSlice(self.allocator);
        self.deinitScope(scope);
        return oldContents;
    }

    pub fn getCurrentScopeContents(self: *Self) []TempRegister {
        return self.scopes.getLast().registers.items;
    }

    pub fn getCurrentScope(self: Self) *RegScope {
        return self.scopes.getLast();
    }

    pub fn addRegister(self: *Self, reg: TempRegister) !void {
        const scope = self.getCurrentScope();
        try scope.addRegister(reg);
    }

    pub fn removeRegister(self: *Self, reg: TempRegister) void {
        const currentScope = self.getCurrentScope();
        const index = std.mem.indexOf(TempRegister, currentScope.registers.items, reg) orelse
            return;
        currentScope.registers.swapRemove(index);
    }
};

const InstrInfo = struct {
    chunk: *InstrChunk,
    label: vmInfo.LabelType,
};

const LoopInfo = struct {
    const Self = @This();

    allocator: Allocator,
    continueLabel: vmInfo.LabelType,
    breaks: *ArrayList(InstrInfo),
    continues: *ArrayList(InstrInfo),

    pub fn init(allocator: Allocator) !Self {
        const breaksPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);
        const continuesPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);

        return .{
            .allocator = allocator,
            .continueLabel = 0,
            .breaks = breaksPtr,
            .continues = continuesPtr,
        };
    }

    pub fn deinit(self: *Self) void {
        self.breaks.deinit(self.allocator);
        self.allocator.destroy(self.breaks);

        self.continues.deinit(self.allocator);
        self.allocator.destroy(self.continues);
    }

    pub fn appendBreak(self: *Self, instr: *InstrChunk, label: vmInfo.LabelType) !void {
        try self.breaks.append(self.allocator, .{
            .chunk = instr,
            .label = label,
        });
    }

    pub fn appendContinue(self: *Self, instr: *InstrChunk, label: vmInfo.LabelType) !void {
        try self.continues.append(self.allocator, .{
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

    writeStructToReg: bool = false,
    propertyAccessReturnsPointer: bool = false,
};

const VarGenInfo = struct {
    stackLocation: ?u64 = null,
    name: []const u8,
    reg: RegisterContents,
};

const ProcInfo = struct {
    stackFrameSize: u64,
    startInstr: ?*InstrChunk,
};

const VAR_GEN_INFO_POOL_SIZE = 1024;
const VarGenInfoPool = MemoryPool(VarGenInfo);

const CHUNK_POOL_SIZE = 1024 * 64;
const ChunkPool = MemoryPool(InstrChunk);

const GenInfoChunks = struct {
    const Self = @This();

    pool: *ChunkPool,
    listStart: ?*InstrChunk,
    listEnd: ?*InstrChunk,

    pub fn init(allocator: Allocator) !Self {
        const chunkPool = try ChunkPool.initPreheated(allocator, CHUNK_POOL_SIZE);
        const chunkPoolPtr = try utils.createMut(ChunkPool, allocator, chunkPool);

        return .{
            .pool = chunkPoolPtr,
            .listStart = null,
            .listEnd = null,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.pool.deinit();
        allocator.destroy(self.pool);
    }

    pub fn newChunk(self: *Self, data: Instr) !*InstrChunk {
        const ptr = try self.pool.create();
        ptr.* = InstrChunk.init(data);
        return ptr;
    }
};

const StructContentInfo = struct {
    const Self = @This();

    currentContentRegister: ?TempRegister = null,
    structContentRegisters: [vmInfo.NUM_STRUCT_CONTENT_REGISTERS]TempRegister =
        [_]TempRegister{0} ** vmInfo.NUM_STRUCT_CONTENT_REGISTERS,
    regBytesUsed: [vmInfo.NUM_STRUCT_CONTENT_REGISTERS]u8 =
        [_]u8{0} ** vmInfo.NUM_STRUCT_CONTENT_REGISTERS,
};

const RegisterContentVariants = enum {
    Bytes1,
    Bytes2,
    Bytes4,
    Bytes8,
    Pointer,
    StructContents,
};

const RegisterContents = union(RegisterContentVariants) {
    const Self = @This();

    Bytes1: TempRegister,
    Bytes2: TempRegister,
    Bytes4: TempRegister,
    Bytes8: TempRegister,
    Pointer: TempRegister,
    StructContents,

    pub fn initWithSize(reg: TempRegister, size: u8) Self {
        return switch (size) {
            0, 1 => .{ .Bytes1 = reg },
            2 => .{ .Bytes2 = reg },
            3, 4 => .{ .Bytes4 = reg },
            5...8 => .{ .Bytes8 = reg },
            9...16 => utils.unimplemented(),
            // TODO - possibly assign to struct content registers
            else => utils.unimplemented(),
        };
    }

    pub fn transferWithSize(self: Self, reg: TempRegister) Self {
        return switch (self) {
            .Bytes1 => .{ .Bytes1 = reg },
            .Bytes2 => .{ .Bytes2 = reg },
            .Bytes4 => .{ .Bytes4 = reg },
            .Bytes8 => .{ .Bytes8 = reg },
            .Pointer => .{ .Pointer = reg },
            .StructContents => .{ .StructContents = {} },
        };
    }

    pub fn getRegister(self: Self) !TempRegister {
        return switch (self) {
            .Bytes1,
            .Bytes2,
            .Bytes4,
            .Bytes8,
            .Pointer,
            => |reg| reg,
            else => CodeGenError.NoTrivialRegister,
        };
    }
};

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

    pub fn deinit(self: *Self, allocator: Allocator) void {
        if (self.labelBytes.len > 0) {
            allocator.free(self.labelBytes);
        }

        if (self.labelExists.len > 0) {
            allocator.free(self.labelExists);
        }

        self.freeWaitingMapContents(allocator);
        allocator.destroy(self.waitingLabels);
    }

    pub fn reserveLabelCount(
        self: *Self,
        allocator: Allocator,
        lastLabel: vmInfo.LabelType,
    ) !void {
        // if label id is 0, no labels have been used
        if (lastLabel == 0) return;

        if (self.labelExists.len > 0) {
            allocator.free(self.labelExists);
        }

        if (self.labelBytes.len > 0) {
            allocator.free(self.labelBytes);
        }

        self.freeWaitingMapContents(allocator);

        self.labelBytes = try allocator.alloc(u64, lastLabel);
        self.labelExists = try allocator.alloc(bool, lastLabel);
        @memset(self.labelExists, false);
    }

    fn freeWaitingMapContents(self: *Self, allocator: Allocator) void {
        var waitingLabels = self.waitingLabels.valueIterator();
        while (waitingLabels.next()) |arrList| {
            arrList.*.deinit(allocator);
            allocator.destroy(arrList.*);
        }
        self.waitingLabels.clearAndFree();
    }

    pub fn setLabelLocation(
        self: *Self,
        allocator: Allocator,
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

            arrList.deinit(allocator);
            allocator.destroy(arrList);
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

pub const GenInfo = struct {
    const Self = @This();

    allocator: Allocator,
    procInfo: ProcInfo,
    chunks: *GenInfoChunks,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    registers: struct {
        infos: *ArrayList(RegInfo),
        scopes: *RegScopes,
        structContentInfo: StructContentInfo,
    },
    varGenInfoPool: *VarGenInfoPool,
    varNameReg: *StringHashMap(*VarGenInfo),
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),
    byteCounter: u64,
    currentLabelId: vmInfo.LabelType,
    labelByteInfo: *LabelByteInfo,

    pub fn init(allocator: Allocator) !Self {
        const capacityScale = 8;
        const scaledNumRegisters = vmInfo.NUM_REGISTERS * capacityScale;

        const varNameReg = try utils.initMutPtrT(StringHashMap(*VarGenInfo), allocator);
        const regScopes = try RegScopes.init(allocator);
        const regScopesPtr = try utils.createMut(RegScopes, allocator, regScopes);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, .empty);

        const registers = try ArrayList(RegInfo).initCapacity(allocator, scaledNumRegisters);
        const registersPtr = try utils.createMut(ArrayList(RegInfo), allocator, registers);

        const tempPool = try VarGenInfoPool.initPreheated(allocator, VAR_GEN_INFO_POOL_SIZE);
        const varGenInfoPool = try utils.createMut(VarGenInfoPool, allocator, tempPool);

        const chunks = try GenInfoChunks.init(allocator);
        const chunksPtr = try utils.createMut(GenInfoChunks, allocator, chunks);

        const labelByteInfo = try LabelByteInfo.init(allocator);
        const labelByteInfoPtr = try utils.createMut(LabelByteInfo, allocator, labelByteInfo);

        return .{
            .allocator = allocator,
            .vmInfo = .{
                .stackStartSize = 0,
                .version = 0,
            },
            .chunks = chunksPtr,
            .varGenInfoPool = varGenInfoPool,
            .varNameReg = varNameReg,
            .byteCounter = 0,
            .settings = .{},
            .loopInfo = loopInfoPtr,
            .registers = .{
                .infos = registersPtr,
                .scopes = regScopesPtr,
                .structContentInfo = .{},
            },
            .procInfo = .{
                .stackFrameSize = 0,
                .startInstr = null,
            },
            .currentLabelId = 0,
            .labelByteInfo = labelByteInfoPtr,
        };
    }

    pub fn deinit(self: Self) void {
        self.chunks.deinit(self.allocator);
        self.allocator.destroy(self.chunks);

        self.registers.infos.deinit(self.allocator);
        self.allocator.destroy(self.registers.infos);

        self.varNameReg.deinit();
        self.allocator.destroy(self.varNameReg);

        self.registers.scopes.deinit();
        self.allocator.destroy(self.registers.scopes);

        self.varGenInfoPool.deinit();
        self.allocator.destroy(self.varGenInfoPool);

        for (self.loopInfo.items) |item| {
            item.deinit();
            self.allocator.destroy(item);
        }
        self.loopInfo.deinit(self.allocator);
        self.allocator.destroy(self.loopInfo);

        self.labelByteInfo.deinit(self.allocator);
        self.allocator.destroy(self.labelByteInfo);
    }

    pub fn writeChunks(self: *Self, writer: *Writer) !void {
        try writer.writeByte(self.vmInfo.version);
        var buf: [vmInfo.START_STACK_SIZE]u8 = undefined;
        std.mem.writeInt(vmInfo.StartStackType, &buf, self.vmInfo.stackStartSize, .little);
        try writer.writeAll(&buf);

        var current: ?*InstrChunk = self.chunks.listStart;
        while (current) |chunk| : (current = chunk.next) {
            try writeChunk(chunk, writer);
        }
    }

    pub fn appendChunk(self: *Self, data: Instr) !*InstrChunk {
        const newChunk = try self.chunks.pool.create();
        newChunk.* = InstrChunk.init(data);

        if (self.procInfo.startInstr == null) {
            self.procInfo.startInstr = newChunk;
        }

        if (self.chunks.listEnd) |last| {
            last.next = newChunk;
            newChunk.prev = last;
            self.chunks.listEnd = newChunk;
        } else {
            self.chunks.listStart = newChunk;
            self.chunks.listEnd = newChunk;
        }

        return newChunk;
    }

    pub fn takeLabelId(self: *Self) vmInfo.LabelType {
        const res = self.currentLabelId;
        self.currentLabelId += 1;
        return res;
    }

    pub fn getAvailableReg(self: *Self) !TempRegister {
        // searches linearly to ensure lowest register numbers are used for
        // (slightly) better caching in vm, also makes the bytecode prettier
        for (self.registers.infos.items, 0..) |reg, index| {
            if (!reg.active) {
                return @intCast(index);
            }
        }

        try self.registers.infos.append(self.allocator, .{});
        return @intCast(self.registers.infos.items.len - 1);
    }

    pub fn isRegActive(self: Self, reg: TempRegister) bool {
        return self.registers.infos.items[reg].active;
    }

    pub fn availableRegReplaceReserve(self: *Self, reg: TempRegister) !TempRegister {
        if (self.isRegVariable(reg)) {
            const res = try self.getAvailableReg();
            try self.reserveRegister(res);
            return res;
        }

        return reg;
    }

    pub fn availableRegReplaceRelease(
        self: *Self,
        reg1: TempRegister,
        reg2: TempRegister,
    ) !TempRegister {
        if (!self.isRegVariable(reg1)) {
            self.releaseIfPossible(reg2);
            return reg1;
        } else if (!self.isRegVariable(reg2)) {
            self.releaseIfPossible(reg1);
            return reg2;
        }

        const reg = try self.getAvailableReg();
        try self.reserveRegister(reg);
        return reg;
    }

    pub fn releaseIfPossible(self: *Self, reg: TempRegister) void {
        if (self.isRegVariable(reg)) return;
        self.releaseRegister(reg);
    }

    pub fn reserveRegister(self: *Self, reg: TempRegister) !void {
        self.registers.infos.items[reg].active = true;
        try self.registers.scopes.addRegister(reg);
    }

    pub fn releaseRegister(self: *Self, reg: TempRegister) void {
        const info = self.registers.infos.items[reg];

        if (info.varInfo) |varInfo| {
            _ = self.varNameReg.remove(varInfo.name);
            self.varGenInfoPool.destroy(varInfo);
        }

        self.registers.infos.items[reg] = .{};
    }

    pub fn getVariableRegister(self: Self, name: []const u8) TempRegister {
        return self.varNameReg.get(name).?.reg;
    }

    pub fn setVariableRegister(
        self: *Self,
        name: []const u8,
        reg: RegisterContents,
    ) !void {
        const varGenInfo = try self.varGenInfoPool.create();
        varGenInfo.* = .{
            .reg = reg,
            .name = name,
        };
        try self.varNameReg.put(name, varGenInfo);
        const regValue = try reg.getRegister();
        try self.registers.scopes.addRegister(regValue);
        self.registers.infos.items[regValue].varInfo = varGenInfo;
    }

    // deactivates register and removes it from variable scopes
    pub fn removeVariableRegister(self: *Self, name: []const u8) void {
        if (self.varNameReg.get(name)) |info| {
            self.registers.scopes.removeRegister(info.reg);
            self.registers.infos.items[info.reg].varInfo = null;
            self.registers.infos.items[info.reg].active = false;
            self.varGenInfoPool.destroy(info);
            self.varNameReg.remove(name);
        }
    }

    pub fn isRegVariable(self: Self, reg: TempRegister) bool {
        return self.registers.infos.items[reg].varInfo != null;
    }

    pub fn pushScope(self: *Self) !void {
        try self.registers.scopes.pushScope();
    }

    fn popScope(self: *Self) ![]TempRegister {
        return self.registers.scopes.popScope();
    }

    pub fn pushLoopInfo(self: *Self) !void {
        const newLoop = try LoopInfo.init(self.allocator);
        const newLoopPtr = try utils.createMut(LoopInfo, self.allocator, newLoop);
        try self.loopInfo.append(self.allocator, newLoopPtr);
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

            info.deinit();
            self.allocator.destroy(info);
        }
    }

    pub fn currentLoopInfo(self: Self) ?*LoopInfo {
        return self.loopInfo.getLastOrNull();
    }

    pub fn releaseScope(self: *Self) !void {
        const old = try self.popScope();
        self.releaseRegisters(old);
        self.allocator.free(old);
    }

    fn releaseRegisters(self: *Self, regs: []TempRegister) void {
        for (regs) |reg| {
            self.releaseRegister(reg);
        }
    }

    pub fn setContinueLabel(self: *Self, label: vmInfo.LabelType) void {
        const loopInfo = self.currentLoopInfo();
        if (loopInfo) |info| {
            info.setContinueLabel(label);
        }
    }

    pub fn getRegInfo(self: Self, reg: TempRegister) RegInfo {
        return self.registers.infos.items[reg];
    }

    pub fn getVarGenInfoFromReg(self: Self, reg: TempRegister) ?*VarGenInfo {
        return self.registers.infos.items[reg].varInfo;
    }

    pub fn getVarGenInfoFromName(self: Self, name: []const u8) ?*VarGenInfo {
        return self.varNameReg.get(name);
    }

    pub fn finishProc(self: *Self, allocator: Allocator, context: *Context) !void {
        const listStart = self.chunks.listStart orelse return;
        const start = self.procInfo.startInstr orelse return;
        const end = self.chunks.listEnd orelse return;

        try self.labelByteInfo.reserveLabelCount(allocator, self.currentLabelId);

        const spInstructions = try getSpIncInstructions(self.procInfo.stackFrameSize);

        self.byteCounter = vmInfo.VM_INFO_BYTECODE_LEN;
        try adjustInstructions(allocator, context, start, self.procInfo.stackFrameSize);

        if (self.procInfo.stackFrameSize == 0) return;

        self.byteCounter += spInstructions.add.getInstrLen() + spInstructions.sub.getInstrLen();

        const addChunk = try self.chunks.newChunk(spInstructions.add);
        const subChunk = try self.chunks.newChunk(spInstructions.sub);

        if (start.prev) |prev| {
            prev.next = addChunk;
            addChunk.prev = prev;
        }

        start.prev = addChunk;
        addChunk.next = start;

        if (start == listStart) {
            self.chunks.listStart = addChunk;
        }

        end.next = subChunk;
        subChunk.prev = end;
        self.chunks.listEnd = subChunk;
    }

    pub fn fillNextStructContentReg(self: *Self) !TempRegister {
        const reg = try self.getAvailableReg();
        try self.reserveRegister(reg);

        if (self.registers.structContentInfo.currentContentRegister) |*currentReg| {
            if (currentReg.* == vmInfo.NUM_STRUCT_CONTENT_REGISTERS) {
                return CodeGenError.TooManyStructContentRegistersUsed;
            }

            currentReg.* += 1;
            self.registers.structContentInfo.structContentRegisters[currentReg.*] = reg;
        } else {
            self.registers.structContentInfo.currentContentRegister = 0;
            self.registers.structContentInfo.structContentRegisters[0] = reg;
        }

        return reg;
    }

    pub fn resetStructContentRegisters(self: *Self) void {
        const currentReg = self.registers.structContentInfo.currentContentRegister orelse return;

        self.releaseRegisters(
            self.registers.structContentInfo.structContentRegisters[0 .. currentReg + 1],
        );
        self.registers.structContentInfo.currentContentRegister = null;
    }

    pub fn setCurrentStructContentNumBytesUsed(self: *Self, used: u8) void {
        const currentReg = self.registers.structContentInfo.currentContentRegister orelse return;
        self.registers.structContentInfo.regBytesUsed[currentReg] = used;
    }

    pub fn getCurrentStructContentRegister(self: Self) !TempRegister {
        const currentReg = self.registers.structContentInfo.currentContentRegister orelse {
            return CodeGenError.NoCurrentStructContentRegister;
        };
        return self.registers.structContentInfo.structContentRegisters[currentReg];
    }
};

fn adjustInstructions(
    allocator: Allocator,
    context: *Context,
    chunk: *InstrChunk,
    frameSize: u64,
) !void {
    var current: ?*InstrChunk = chunk;
    while (current) |instr| : (current = current.?.next) {
        try adjustInstruction(allocator, context, instr, frameSize);
        context.genInfo.byteCounter += instr.data.getInstrLen();
    }
}

/// adjusts store instructions that are based on sp offsets
fn adjustInstruction(
    allocator: Allocator,
    context: *Context,
    chunk: *InstrChunk,
    frameSize: u64,
) !void {
    switch (chunk.data) {
        .Label => |label| {
            context.genInfo.labelByteInfo.setLabelLocation(
                allocator,
                label,
                context.genInfo.byteCounter,
            );
        },
        .Store8AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store64AtSpNegOffset16,
        => |*instr| {
            instr.offset = @intCast(frameSize - instr.offset);
        },
        .StoreSpSub16AtSpNegOffset16 => |*instr| {
            instr.subTo = @intCast(frameSize - instr.subTo);
            instr.offset = @intCast(frameSize - instr.offset);
        },
        .MovSpNegOffsetAny => |*instr| {
            const newInstr = try movSpNegOffset(instr.reg, frameSize - instr.offset);
            chunk.data = newInstr;
        },
        .MovSpNegOffset16, .MovSpNegOffset32, .MovSpNegOffset64 => {},
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
                const byteOffset = switch (chunk.data) {
                    .JumpBack,
                    .JumpBackEQ,
                    .JumpBackNE,
                    .JumpBackGT,
                    .JumpBackLT,
                    .JumpBackGTE,
                    .JumpBackLTE,
                    => 0,
                    else => chunk.data.getInstrLen(),
                };
                data.* = context.genInfo.byteCounter + byteOffset;

                try context.genInfo.labelByteInfo.waitForLabel(allocator, @intCast(labelId), data);
            }
        },
        else => {},
    }
}

fn movSpNegOffset(reg: TempRegister, offset: u64) !Instr {
    const spOpSize = try getSpOpSizeFromNum(offset);

    return switch (spOpSize) {
        .U16 => Instr{
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
    writeStartVMInfo(context);
    _ = try genBytecode(allocator, context, tree.root);
    try context.genInfo.finishProc(allocator, context);
}

fn writeStartVMInfo(context: *Context) void {
    context.genInfo.vmInfo.version = version.VERSION;
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
) GenBytecodeError!?RegisterContents {
    switch (node.variant) {
        .StructPlaceholder => {},
        .NoOp => {},
        .Seq => |seq| {
            for (seq) |seqNode| {
                _ = try genBytecode(allocator, context, seqNode);
            }
        },
        .VarDec => |dec| {
            const regContents = try genBytecode(allocator, context, dec.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            switch (regContents) {
                .Bytes1,
                .Bytes2,
                .Bytes4,
                .Bytes8,
                .Pointer,
                => |reg| {
                    const varReg = if (context.genInfo.isRegVariable(reg)) a: {
                        const newReg = try context.genInfo.getAvailableReg();
                        try context.genInfo.reserveRegister(newReg);

                        var instr = Instr{ .Mov = .{} };
                        instr.Mov.dest = newReg;
                        instr.Mov.src = reg;
                        _ = try context.genInfo.appendChunk(instr);

                        break :a newReg;
                    } else a: {
                        if (!context.genInfo.isRegActive(reg)) {
                            try context.genInfo.reserveRegister(reg);
                        }
                        break :a reg;
                    };

                    try context.genInfo.setVariableRegister(
                        dec.name,
                        regContents.transferWithSize(varReg),
                    );
                },
                .StructContents => utils.unimplemented(),
            }
        },
        .Value => |value| {
            switch (value) {
                .Null => {},
                .RawNumber => |num| {
                    const reg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(reg);

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
                        .U8, .Char => Instr{
                            .SetReg8 = .{
                                .reg = reg,
                                .data = try std.fmt.parseInt(u8, num.digits, 10),
                            },
                        },
                        else => utils.unimplemented(),
                    };

                    _ = try context.genInfo.appendChunk(instr);
                    return RegisterContents.initWithSize(reg, num.numType.getSize());
                },
                .Bool => |b| {
                    const reg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(reg);

                    const instr = Instr{
                        .SetReg8 = .{
                            .reg = reg,
                            .data = @as(u8, @intFromBool(b)),
                        },
                    };

                    _ = try context.genInfo.appendChunk(instr);
                    return .{ .Bytes1 = reg };
                },
                .Char => |ch| {
                    const reg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(reg);

                    var instr = Instr{ .SetReg8 = .{} };
                    instr.SetReg8.reg = reg;
                    instr.SetReg8.data = ch;

                    _ = try context.genInfo.appendChunk(instr);
                    return .{ .Bytes1 = reg };
                },
                .Number => |num| {
                    const reg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(reg);

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

                    _ = try context.genInfo.appendChunk(instr);
                    return RegisterContents.initWithSize(reg, num.toAstNumberVariant().getSize());
                },
                .ArrayDec => |items| {
                    const sliceSize = node.typeInfo.size;

                    const ptrReg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(ptrReg);

                    const moveSpInstr = Instr{
                        .MovSpNegOffsetAny = .{
                            .reg = ptrReg,
                            .offset = context.genInfo.procInfo.stackFrameSize,
                        },
                    };
                    _ = try context.genInfo.appendChunk(moveSpInstr);

                    context.genInfo.procInfo.stackFrameSize += sliceSize;

                    const itemSize = if (items.len > 0) items[0].typeInfo.size else 0;

                    const prevAccessReturn = context.genInfo.settings.propertyAccessReturnsPointer;
                    context.genInfo.settings.propertyAccessReturnsPointer = true;
                    for (items) |item| {
                        try context.genInfo.pushScope();

                        const reg = try genBytecode(allocator, context, item) orelse
                            return CodeGenError.ReturnedRegisterNotFound;

                        try context.genInfo.releaseScope();

                        const storeInstr = try storeRegAtRegWithPostInc(
                            reg,
                            ptrReg,
                            @intCast(itemSize),
                        );
                        _ = try context.genInfo.appendChunk(storeInstr);
                    }
                    context.genInfo.settings.propertyAccessReturnsPointer = prevAccessReturn;

                    _ = try context.genInfo.appendChunk(moveSpInstr);

                    return .{ .Pointer = ptrReg };
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            var leftReg: RegisterContents = undefined;

            const leftDepth = ast.getExprDepth(expr.left);
            const rightDepth = ast.getExprDepth(expr.right);
            const leftExprDeeper = leftDepth > rightDepth;
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

            var outReg: ?RegisterContents = null;

            const buf: Instr = switch (expr.type) {
                .Add, .Sub, .Mult => a: {
                    // TODO - change if left and right byte sizes become different
                    outReg = leftReg.transferWithSize(
                        try context.genInfo.availableRegReplaceRelease(
                            try leftReg.getRegister(),
                            try rightReg.getRegister(),
                        ),
                    );
                    const mathInstr = MathInstr{
                        .dest = try outReg.?.getRegister(),
                        .reg1 = try leftReg.getRegister(),
                        .reg2 = try rightReg.getRegister(),
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
                => a: {
                    var instr = if (context.genInfo.settings.outputCmpAsRegister)
                        exprTypeToCmpSetReg(expr.type)
                    else
                        Instr{ .Cmp = .{} };

                    if (context.genInfo.settings.outputCmpAsRegister) {
                        // TODO - change if left and right byte sizes become different
                        outReg = leftReg.transferWithSize(
                            try context.genInfo.availableRegReplaceRelease(
                                try leftReg.getRegister(),
                                try rightReg.getRegister(),
                            ),
                        );
                        switch (instr) {
                            .CmpSetRegEQ,
                            .CmpSetRegNE,
                            .CmpSetRegGT,
                            .CmpSetRegLT,
                            .CmpSetRegGTE,
                            .CmpSetRegLTE,
                            => |*payload| {
                                payload.dest = try outReg.?.getRegister();
                            },
                            else => unreachable,
                        }
                    } else {
                        context.genInfo.releaseIfPossible(try leftReg.getRegister());
                        context.genInfo.releaseIfPossible(try rightReg.getRegister());
                    }

                    switch (instr) {
                        .Cmp => |*cmp| {
                            cmp.reg1 = try leftReg.getRegister();
                            cmp.reg2 = try rightReg.getRegister();
                        },
                        .CmpSetRegEQ,
                        .CmpSetRegNE,
                        .CmpSetRegGT,
                        .CmpSetRegLT,
                        .CmpSetRegGTE,
                        .CmpSetRegLTE,
                        => |*payload| {
                            payload.reg1 = try leftReg.getRegister();
                            payload.reg2 = try rightReg.getRegister();
                        },
                        else => unreachable,
                    }

                    break :a instr;
                },
                else => utils.unimplemented(),
            };

            _ = try context.genInfo.appendChunk(buf);

            return outReg;
        },
        .Variable => |name| {
            const varInfo = context.genInfo.getVarGenInfoFromName(name).?;
            return varInfo.reg;
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, context, statement.condition) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            var buf = Instr{ .CmpConst8 = .{} };
            buf.CmpConst8.reg = try condReg.getRegister();
            buf.CmpConst8.data = 1;
            _ = try context.genInfo.appendChunk(buf);

            context.genInfo.releaseIfPossible(try condReg.getRegister());

            const jumpLabelId = context.genInfo.takeLabelId();
            const jumpLabel = Instr{ .Label = jumpLabelId };
            const jumpBuf = Instr{ .JumpNE = jumpLabelId };
            _ = try context.genInfo.appendChunk(jumpBuf);

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, statement.body);
            try context.genInfo.releaseScope();

            if (statement.fallback) |fallback| {
                const jumpEndLabelId = context.genInfo.takeLabelId();
                const jumpEndInstr = Instr{ .Jump = jumpEndLabelId };
                _ = try context.genInfo.appendChunk(jumpEndInstr);

                _ = try context.genInfo.appendChunk(jumpLabel);
                try generateFallback(allocator, context, fallback);
                const jumpEndLabel = Instr{ .Label = jumpEndLabelId };
                _ = try context.genInfo.appendChunk(jumpEndLabel);
            } else {
                _ = try context.genInfo.appendChunk(jumpLabel);
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |initNode| {
                _ = try genBytecode(allocator, context, initNode);
            }

            const condInfo = prepForLoopCondition(context, loop.condition);

            try context.genInfo.pushLoopInfo();
            defer context.genInfo.popLoopInfo();

            const preConditionLabelId = context.genInfo.takeLabelId();
            const preConditionLabel = Instr{ .Label = preConditionLabelId };
            _ = try context.genInfo.appendChunk(preConditionLabel);

            const condReg = try genBytecode(allocator, context, loop.condition);
            context.genInfo.settings.outputCmpAsRegister = condInfo.prevCmpAsReg;

            const preBodyLabelId = context.genInfo.takeLabelId();
            var jumpEndInstr = Instr{ .JumpNE = preBodyLabelId };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, preBodyLabelId, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConst8 = .{} };
                const regValue = condReg orelse return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConst8.reg = try regValue.getRegister();
                cmpInstr.CmpConst8.data = 1;
                _ = try context.genInfo.appendChunk(cmpInstr);
            }

            _ = try context.genInfo.appendChunk(jumpEndInstr);

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, loop.body);
            try context.genInfo.releaseScope();

            const continueLabelId = context.genInfo.takeLabelId();
            const continueLabel = Instr{ .Label = continueLabelId };
            _ = try context.genInfo.appendChunk(continueLabel);
            context.genInfo.setContinueLabel(continueLabelId);

            _ = try genBytecode(allocator, context, loop.incNode);

            const jumpStartInstr = Instr{ .JumpBack = preConditionLabelId };
            _ = try context.genInfo.appendChunk(jumpStartInstr);

            const preBodyLabel = Instr{ .Label = preBodyLabelId };
            _ = try context.genInfo.appendChunk(preBodyLabel);
        },
        .WhileLoop => |loop| {
            try context.genInfo.pushLoopInfo();
            defer context.genInfo.popLoopInfo();

            const condInfo = prepForLoopCondition(context, loop.condition);

            const preConditionLabelId = context.genInfo.takeLabelId();
            const preConditionLabel = Instr{ .Label = preConditionLabelId };
            _ = try context.genInfo.appendChunk(preConditionLabel);
            const condReg = try genBytecode(allocator, context, loop.condition);

            const preBodyLabelId = context.genInfo.takeLabelId();
            var jumpEndInstr = Instr{ .JumpNE = preBodyLabelId };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, preBodyLabelId, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConst8 = .{} };
                const regValue = condReg orelse return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConst8.reg = try regValue.getRegister();
                cmpInstr.CmpConst8.data = 1;
                _ = try context.genInfo.appendChunk(cmpInstr);
            }

            _ = try context.genInfo.appendChunk(jumpEndInstr);

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, loop.body);
            try context.genInfo.releaseScope();

            const continueLabelId = context.genInfo.takeLabelId();
            const continueLabel = Instr{ .Label = continueLabelId };
            _ = try context.genInfo.appendChunk(continueLabel);
            context.genInfo.setContinueLabel(continueLabelId);

            const preBodyLabel = Instr{ .Label = preBodyLabelId };
            _ = try context.genInfo.appendChunk(preBodyLabel);

            const jumpStartInstr = Instr{ .JumpBack = preConditionLabelId };
            _ = try context.genInfo.appendChunk(jumpStartInstr);
        },
        .IncOne => |inc| {
            const reg = try genBytecode(allocator, context, inc) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .IncConst8 = .{} };
            instr.IncConst8.reg = try reg.getRegister();
            instr.IncConst8.data = 1;

            _ = try context.genInfo.appendChunk(instr);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, context, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .DecConst8 = .{} };
            instr.DecConst8.reg = try reg.getRegister();
            instr.DecConst8.data = 1;

            _ = try context.genInfo.appendChunk(instr);
        },
        .Group => |group| {
            return genBytecode(allocator, context, group);
        },
        .ValueSet => |set| {
            const srcReg = try genBytecode(allocator, context, set.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const destReg = try genBytecode(allocator, context, set.value) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            var instr = Instr{ .Mov = .{} };
            instr.Mov.dest = try destReg.getRegister();
            instr.Mov.src = try srcReg.getRegister();
            _ = try context.genInfo.appendChunk(instr);
        },
        .Bang => |expr| {
            const reg = try genBytecode(allocator, context, expr) orelse
                return CodeGenError.NoAvailableRegisters;
            const setReg = try context.genInfo.availableRegReplaceReserve(try reg.getRegister());

            var instr = Instr{ .XorConst8 = .{} };
            instr.XorConst8.dest = setReg;
            instr.XorConst8.reg = try reg.getRegister();
            instr.XorConst8.byte = 1;
            _ = try context.genInfo.appendChunk(instr);

            return .{ .Bytes1 = setReg };
        },
        .Scope => |scope| {
            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, scope);
            try context.genInfo.releaseScope();
        },
        .Break => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const breakLabelId = context.genInfo.takeLabelId();
            const buf = Instr{ .Jump = breakLabelId };
            const chunk = try context.genInfo.appendChunk(buf);
            try loopInfo.appendBreak(chunk, breakLabelId);
        },
        .Continue => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const continueLabelId = context.genInfo.takeLabelId();
            const instr = Instr{ .Jump = continueLabelId };
            const chunk = try context.genInfo.appendChunk(instr);
            try loopInfo.appendContinue(chunk, continueLabelId);
        },
        .ArrayInit => |init| {
            const initSize = node.typeInfo.size;

            // where to write arr data
            const ptrReg = try context.genInfo.getAvailableReg();
            try context.genInfo.reserveRegister(ptrReg);
            const moveSpInstr = Instr{
                .MovSpNegOffsetAny = .{
                    .reg = ptrReg,
                    .offset = context.genInfo.procInfo.stackFrameSize,
                },
            };
            _ = try context.genInfo.appendChunk(moveSpInstr);

            const initLen = try std.fmt.parseInt(u64, init.size, 10);
            context.genInfo.procInfo.stackFrameSize += initSize;

            const lenReg = try context.genInfo.getAvailableReg();
            try context.genInfo.reserveRegister(lenReg);
            defer context.genInfo.releaseRegister(lenReg);
            const movLen = Instr{
                .SetReg64 = .{
                    .reg = lenReg,
                    .data = initLen,
                },
            };
            _ = try context.genInfo.appendChunk(movLen);

            // register for holding current index
            var indexReg: ?TempRegister = null;
            defer if (indexReg) |reg| {
                context.genInfo.releaseRegister(reg);
            };
            if (init.indexIdent) |ident| {
                indexReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(indexReg.?);
                try context.genInfo.setVariableRegister(
                    ident,
                    RegisterContents.initWithSize(indexReg.?, vmInfo.POINTER_SIZE),
                );

                const setZeroInstr = Instr{
                    .SetReg8 = .{
                        .reg = indexReg.?,
                        .data = 0,
                    },
                };
                _ = try context.genInfo.appendChunk(setZeroInstr);
            }

            const preCmpLabelId = context.genInfo.takeLabelId();
            const preCmpLabel = Instr{ .Label = preCmpLabelId };
            _ = try context.genInfo.appendChunk(preCmpLabel);

            const cmpLen = Instr{
                .CmpConst8 = .{
                    .reg = lenReg,
                    .data = 0,
                },
            };
            _ = try context.genInfo.appendChunk(cmpLen);

            const postCmpLabelId = context.genInfo.takeLabelId();
            const postCmpLabel = Instr{ .Label = postCmpLabelId };
            _ = try context.genInfo.appendChunk(postCmpLabel);

            const jumpInstrLabelId = context.genInfo.takeLabelId();
            const jumpInstr = Instr{ .JumpEQ = jumpInstrLabelId };
            _ = try context.genInfo.appendChunk(jumpInstr);

            try context.genInfo.pushScope();
            const resReg = try genBytecode(allocator, context, init.initNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            try context.genInfo.releaseScope();

            const writeInstr = try storeRegAtRegWithPostInc(
                resReg,
                ptrReg,
                @intCast(try init.initType.astType.getSize(context)),
            );
            _ = try context.genInfo.appendChunk(writeInstr);

            const subLen = Instr{
                .Sub8 = .{
                    .dest = lenReg,
                    .reg = lenReg,
                    .data = 1,
                },
            };
            _ = try context.genInfo.appendChunk(subLen);

            if (indexReg) |reg| {
                const addInstr = Instr{
                    .Add8 = .{
                        .dest = reg,
                        .reg = reg,
                        .data = 1,
                    },
                };
                _ = try context.genInfo.appendChunk(addInstr);
            }

            const jumpInstrLabel = Instr{ .Label = jumpInstrLabelId };
            _ = try context.genInfo.appendChunk(jumpInstrLabel);

            const jumpStart = Instr{ .JumpBack = preCmpLabelId };
            _ = try context.genInfo.appendChunk(jumpStart);

            _ = try context.genInfo.appendChunk(moveSpInstr);

            return .{ .Pointer = ptrReg };
        },
        .Dereference => |inner| {
            const resReg = try genBytecode(allocator, context, inner) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const destReg = if (context.genInfo.isRegVariable(try resReg.getRegister())) a: {
                const newReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(newReg);
                break :a newReg;
            } else try resReg.getRegister();

            const loadInstr = loadAtReg(
                try resReg.getRegister(),
                destReg,
                node.typeInfo.size,
            );
            _ = try context.genInfo.appendChunk(loadInstr);

            return resReg.transferWithSize(destReg);
        },
        .Pointer => |inner| {
            const resReg = try genBytecode(allocator, context, inner.node) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            if (context.genInfo.getVarGenInfoFromReg(try resReg.getRegister())) |info| {
                const ptrReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(ptrReg);

                const location = if (info.stackLocation) |location| location else a: {
                    const itemSize = inner.node.typeInfo.size;
                    const alignment = inner.node.typeInfo.alignment;

                    const spCount = context.genInfo.procInfo.stackFrameSize;
                    context.genInfo.procInfo.stackFrameSize += itemSize;

                    const paddingInfo = utils.calculatePadding(spCount, alignment);
                    context.genInfo.procInfo.stackFrameSize += paddingInfo.padding;

                    const storeInstr = try storeRegAtSpNegOffset(resReg, paddingInfo.offset);
                    _ = try context.genInfo.appendChunk(storeInstr);

                    break :a spCount;
                };

                const setLocInstr = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = ptrReg,
                        .offset = location,
                    },
                };
                _ = try context.genInfo.appendChunk(setLocInstr);

                return .{ .Pointer = ptrReg };
            }
        },
        .VarEqOp => |op| {
            const destReg = try genBytecode(allocator, context, op.variable) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const valueReg = try genBytecode(allocator, context, op.value) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const destRegValue = try destReg.getRegister();
            const valueRegValue = try valueReg.getRegister();

            const instr: Instr = switch (op.opType) {
                .Add => .{
                    .Add = .{
                        .dest = destRegValue,
                        .reg1 = destRegValue,
                        .reg2 = valueRegValue,
                    },
                },
                .Sub => .{
                    .Sub = .{
                        .dest = destRegValue,
                        .reg1 = destRegValue,
                        .reg2 = valueRegValue,
                    },
                },
                .Mult => .{
                    .Sub = .{
                        .dest = destRegValue,
                        .reg1 = destRegValue,
                        .reg2 = valueRegValue,
                    },
                },
                else => utils.unimplemented(),
            };

            _ = try context.genInfo.appendChunk(instr);
        },
        .StructInit => |init| {
            const writeToReg = context.genInfo.settings.writeStructToReg;

            var resPtrReg: TempRegister = undefined;

            // if gen settings say to write into registers, then the location is the
            // position in the current struct content register to write to
            // otherwise it is the stack location
            var loc: u64 = if (writeToReg) a: {
                resPtrReg = try context.genInfo.fillNextStructContentReg();
                break :a 0;
            } else a: {
                resPtrReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(resPtrReg);

                const paddingInfo = utils.calculatePadding(
                    context.genInfo.procInfo.stackFrameSize,
                    node.typeInfo.alignment,
                );
                context.genInfo.procInfo.stackFrameSize += paddingInfo.padding;

                const resLoc = context.genInfo.procInfo.stackFrameSize;
                context.genInfo.procInfo.stackFrameSize += node.typeInfo.size;

                break :a resLoc;
            };
            const startLoc = loc;

            const def = context.compInfo.getStructDec(init.name).?;

            for (def.attributes) |defAttr| {
                try context.genInfo.pushScope();

                const attr = init.findAttribute(defAttr.name).?;
                const paddingInfo = utils.calculatePadding(loc, attr.value.typeInfo.alignment);
                loc += paddingInfo.padding;

                context.genInfo.settings.writeStructToReg = true;
                const reg = try genBytecode(allocator, context, attr.value) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                context.genInfo.settings.writeStructToReg = writeToReg;
                const regValue = try reg.getRegister();

                if (context.genInfo.settings.writeStructToReg) {
                    // TODO - support this at some point
                    if (attr.value.typeInfo.size > vmInfo.REGISTER_SIZE) {
                        utils.unimplemented();
                    }

                    if (loc + attr.value.typeInfo.size > vmInfo.REGISTER_SIZE) {
                        loc = 0;
                        _ = try context.genInfo.fillNextStructContentReg();
                    }

                    const contentRegister = try context.genInfo.getCurrentStructContentRegister();
                    const instr = if (loc == 0)
                        Instr{
                            .Mov = .{
                                .dest = contentRegister,
                                .src = regValue,
                            },
                        }
                    else
                        Instr{
                            .MovByteRange = .{
                                .dest = contentRegister,
                                .src = regValue,
                                .start = @intCast(loc),
                                .end = @intCast(loc + attr.value.typeInfo.size),
                            },
                        };
                    _ = try context.genInfo.appendChunk(instr);
                } else {
                    const instr = try storeRegAtSpNegOffsetAndSize(
                        reg,
                        loc,
                        attr.value.typeInfo.size,
                    );
                    _ = try context.genInfo.appendChunk(instr);
                }

                loc += attr.value.typeInfo.size;
                try context.genInfo.releaseScope();
            }

            if (!writeToReg) {
                const storePtrInstr = Instr{
                    .MovSpNegOffsetAny = .{
                        .reg = resPtrReg,
                        .offset = startLoc,
                    },
                };
                _ = try context.genInfo.appendChunk(storePtrInstr);
            }

            return .{ .Pointer = resPtrReg };
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
            const regValue = try reg.getRegister();
            const outReg = try context.genInfo.availableRegReplaceReserve(regValue);

            const instr = loadAtReg(regValue, outReg, node.typeInfo.size);
            _ = try context.genInfo.appendChunk(instr);

            return RegisterContents.initWithSize(outReg, @intCast(node.typeInfo.size));
        },
        .IndexValue => |indexNode| {
            const indexReg = try genBytecode(allocator, context, indexNode.index) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const indexValue = try indexReg.getRegister();

            const reg = try calculateAccessOffset(
                allocator,
                context,
                indexNode.target,
                0,
            );
            const regValue = try reg.getRegister();

            context.genInfo.releaseIfPossible(indexValue);

            if (context.genInfo.settings.propertyAccessReturnsPointer) {
                return reg;
            } else {
                const mulAdd = Instr{
                    .MulReg16AddReg = .{
                        .dest = regValue,
                        .addReg = regValue,
                        .mulReg = indexValue,
                        .data = @intCast(node.typeInfo.size),
                    },
                };
                _ = try context.genInfo.appendChunk(mulAdd);

                const outReg = try context.genInfo.availableRegReplaceRelease(
                    indexValue,
                    regValue,
                );

                const loadInstr = loadAtReg(regValue, outReg, @intCast(node.typeInfo.size));
                _ = try context.genInfo.appendChunk(loadInstr);

                return RegisterContents.initWithSize(outReg, @intCast(node.typeInfo.size));
            }
        },
        else => {},
    }

    return null;
}

fn getSpIncInstructions(size: u64) !struct {
    add: Instr,
    sub: Instr,
} {
    const spOpSize = try getSpOpSizeFromNum(size);

    return switch (spOpSize) {
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

fn getSpOpSizeFromNum(num: u64) !SpOpSizes {
    var res: ?SpOpSizes = null;

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
) !RegisterContents {
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
        else => {
            const reg = try genBytecode(allocator, context, node) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const regValue = try reg.getRegister();

            const isVar = context.genInfo.isRegVariable(regValue);
            const outReg = try context.genInfo.availableRegReplaceReserve(regValue);

            if (offset > 0) {
                const instr = Instr{
                    .Add16 = .{
                        .dest = outReg,
                        .reg = regValue,
                        .data = @intCast(offset),
                    },
                };
                _ = try context.genInfo.appendChunk(instr);
            } else if (isVar) {
                // if the returned register is associated with a variable, outReg will be a
                // new register with unknown contents, so we set it in this case
                const instr = Instr{
                    .Mov = .{
                        .dest = outReg,
                        .src = regValue,
                    },
                };
                _ = try context.genInfo.appendChunk(instr);
            }

            return reg.transferWithSize(outReg);
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
        else => utils.unimplemented(),
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
        else => utils.unimplemented(),
    };
}

fn storeRegAtSpNegOffset(regContents: RegisterContents, loc: u64) !Instr {
    return switch (regContents) {
        .Bytes1 => |reg| Instr{
            .Store8AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        .Bytes2 => |reg| Instr{
            .Store16AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        .Bytes4 => |reg| Instr{
            .Store32AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        .Bytes8, .Pointer => |reg| Instr{
            .Store64AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        else => utils.unimplemented(),
    };
}

fn storeRegAtSpNegOffsetAndSize(regContents: RegisterContents, loc: u64, size: u64) !Instr {
    const reg = try regContents.getRegister();

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
        else => utils.unimplemented(),
    };
}

fn storeRegAtRegWithPostInc(regContents: RegisterContents, ptrReg: TempRegister, inc: u16) !Instr {
    return switch (regContents) {
        .Bytes1 => |reg| Instr{
            .Store8AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        .Bytes2 => |reg| Instr{
            .Store16AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        .Bytes4 => |reg| Instr{
            .Store32AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        .Bytes8 => |reg| Instr{
            .Store64AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        else => utils.unimplemented(),
    };
}

fn initSliceBytecode(
    context: *Context,
    len: u64,
    stackLocation: u64,
) !SliceBytecodeInfo {
    const slicePadding = utils.calculatePadding(stackLocation, vmInfo.POINTER_SIZE);
    context.genInfo.procInfo.stackFrameSize += slicePadding.padding + vmInfo.POINTER_SIZE * 2;

    const writeAtSp = Instr{
        .StoreSpSub16AtSpNegOffset16 = .{
            .subTo = @intCast(slicePadding.offset + vmInfo.POINTER_SIZE * 2),
            .offset = @intCast(slicePadding.offset),
        },
    };
    _ = try context.genInfo.appendChunk(writeAtSp);

    const reg = try context.genInfo.getAvailableReg();
    try context.genInfo.reserveRegister(reg);
    const setLenInstr = Instr{
        .SetReg64 = .{
            .reg = reg,
            .data = len,
        },
    };
    _ = try context.genInfo.appendChunk(setLenInstr);

    const writeLen = Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = reg,
            .offset = @intCast(slicePadding.offset + vmInfo.POINTER_SIZE),
        },
    };
    _ = try context.genInfo.appendChunk(writeLen);

    const setArrStart = Instr{
        .MovSpNegOffsetAny = .{
            .reg = reg,
            .offset = slicePadding.offset + vmInfo.POINTER_SIZE * 2,
        },
    };
    _ = try context.genInfo.appendChunk(setArrStart);

    return .{
        .sliceLocation = slicePadding.offset,
        .reg = reg,
    };
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

fn setJumpAmount(chunk: *InstrChunk, label: vmInfo.LabelType) void {
    switch (chunk.data) {
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
        => |*instr| {
            instr.* = label;
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

        var instr = Instr{ .CmpConst8 = .{} };
        instr.CmpConst8.reg = try condReg.getRegister();
        instr.CmpConst8.data = 1;
        _ = try context.genInfo.appendChunk(instr);

        context.genInfo.releaseIfPossible(try condReg.getRegister());

        jumpLabelId = context.genInfo.takeLabelId();
        const jumpInstr = Instr{ .JumpNE = jumpLabelId.? };
        _ = try context.genInfo.appendChunk(jumpInstr);
    }

    _ = try genBytecode(allocator, context, statement.body);

    if (statement.fallback) |newFallback| {
        const jumpEndLabelId = context.genInfo.takeLabelId();
        const jumpEndInstr = Instr{ .Jump = jumpEndLabelId };
        _ = try context.genInfo.appendChunk(jumpEndInstr);

        if (jumpLabelId) |labelId| {
            const jumpLabel = Instr{ .Label = labelId };
            _ = try context.genInfo.appendChunk(jumpLabel);
        }

        try generateFallback(allocator, context, newFallback);
        const jumpEndLabel = Instr{ .Label = jumpEndLabelId };
        _ = try context.genInfo.appendChunk(jumpEndLabel);
    }
}

fn writeChunk(chunk: *InstrChunk, writer: *Writer) !void {
    if (chunk.data == .Label) return;

    try writer.writeByte(chunk.data.getInstrByte());

    switch (chunk.data) {
        .Label => unreachable,
        .SetReg64 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writeNumber(u64, instr.data, writer);
        },
        .SetReg32 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writeNumber(u32, instr.data, writer);
        },
        .SetReg16 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writeNumber(u16, instr.data, writer);
        },
        .SetReg8 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writeNumber(u8, instr.data, writer);
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
        => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.reg1));
            try writer.writeByte(@intCast(instr.reg2));
        },
        .Add8, .Sub8 => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeByte(@intCast(instr.data));
        },
        .Add16, .Sub16 => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeInt(u16, @intCast(instr.data), .little);
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
        => |instr| {
            try writer.writeInt(u32, @intCast(instr), .little);
        },
        .Cmp => |instr| {
            try writer.writeByte(@intCast(instr.reg1));
            try writer.writeByte(@intCast(instr.reg2));
        },
        .CmpConst8, .IncConst8, .DecConst8 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeByte(instr.data);
        },
        .Mov => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.src));
        },
        .MovSpNegOffsetAny => unreachable,
        .MovSpNegOffset16 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeInt(u16, instr.offset, .little);
        },
        .MovSpNegOffset32 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeInt(u32, instr.offset, .little);
        },
        .MovSpNegOffset64 => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeInt(u64, instr.offset, .little);
        },
        .XorConst8 => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeByte(instr.byte);
        },
        .AddSp16, .SubSp16 => |instr| {
            try writer.writeInt(u16, instr, .little);
        },
        .AddSp32, .SubSp32 => |instr| {
            try writer.writeInt(u32, instr, .little);
        },
        .AddSp64, .SubSp64 => |instr| {
            try writer.writeInt(u64, instr, .little);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |instr| {
            try writer.writeByte(@intCast(instr.fromReg));
            try writer.writeByte(@intCast(instr.toRegPtr));
            try writer.writeInt(u16, instr.inc, .little);
        },
        .StoreSpSub16AtSpNegOffset16 => |instr| {
            try writer.writeInt(u16, instr.subTo, .little);
            try writer.writeInt(u16, instr.offset, .little);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        => |instr| {
            try writer.writeByte(@intCast(instr.reg));
            try writer.writeInt(u16, instr.offset, .little);
        },
        .Load64AtReg,
        .Load32AtReg,
        .Load16AtReg,
        .Load8AtReg,
        => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.fromRegPtr));
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.fromRegPtr));
            try writer.writeInt(u16, instr.offset, .little);
        },
        .MovByteRange => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.src));
            try writer.writeByte(instr.start);
            try writer.writeByte(instr.end);
        },
        .MulReg16AddReg => |instr| {
            try writer.writeByte(@intCast(instr.dest));
            try writer.writeByte(@intCast(instr.addReg));
            try writer.writeByte(@intCast(instr.mulReg));
            try writer.writeInt(u16, instr.data, .little);
        },
    }
}

fn writeNumber(comptime T: type, data: T, writer: *Writer) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
