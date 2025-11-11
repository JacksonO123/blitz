const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
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
};
const GenBytecodeError = CodeGenError || Allocator.Error || std.fmt.ParseIntError;

const LoopCondInfo = struct {
    prevCmpAsReg: bool,
    isCompExpr: bool,
};

pub const InstructionVariants = enum(u8) {
    const Self = @This();

    SetReg64, // inst, reg, 8B data
    SetReg32, // inst, reg, 4B data
    SetReg16, // inst, reg, 2B data
    SetReg8, // inst, reg, 1B data
    Add, // inst, out reg, reg1, reg2
    Add8, // inst, out reg, reg1, 1B data
    Sub, // inst, out reg, reg1, reg2
    Sub8, // inst, out reg, reg1, 1B data
    Mult, // inst, out reg, reg1, reg2
    Jump, // inst, 2B data
    JumpEQ, // inst, 2B data
    JumpNE, // inst, 2B data
    JumpGT, // inst, 2B data
    JumpLT, // inst, 2B data
    JumpGTE, // inst, 2B data
    JumpLTE, // inst, 2B data
    JumpBack, // inst, 2B data
    JumpBackEQ, // inst, 2B data
    JumpBackNE, // inst, 2B data
    JumpBackGT, // inst, 2B data
    JumpBackLT, // inst, 2B data
    JumpBackGTE, // inst, 2B data
    JumpBackLTE, // inst, 2B data
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
    MovSp, // inst, dest
    MovSpNegOffset16, // inst, dest, offset 2B
    Xor, // inst, out reg, reg1, reg2
    XorConst8, // inst, out reg, reg1, 1B data
    AddSp16, // inst, 2B data
    SubSp16, // inst, 2B data
    Store64Offset8, // inst, reg, to reg (ptr), offset 1B
    Store64AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store32AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store16AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    Store8AtRegPostInc16, // inst, reg, to reg (ptr), inc 2B
    StoreSpAtSpNegOffset16, // inst, offset 2B
    StoreSpSub16AtSpNegOffset16, // inst, sub 2B data, offset 2B
    Store64AtSpNegOffset16, // inst, reg, offset 2B
    Store32AtSpNegOffset16, // inst, reg, offset 2B
    Store16AtSpNegOffset16, // inst, reg, offset 2B
    Store8AtSpNegOffset16, // inst, reg, offset 2B
    Load64AtReg, // inst, dest, from reg (ptr)
    Load32AtReg, // inst, dest, from reg (ptr)
    Load16AtReg, // inst, dest, from reg (ptr)
    Load8AtReg, // inst, dest, from reg (ptr)

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn getInstrLen(self: Self) u8 {
        return switch (self) {
            .SetReg64 => 10,
            .SetReg32 => 6,
            .SetReg16 => 4,
            .SetReg8 => 3,
            .Add, .Sub, .Mult, .Add8, .Sub8 => 4,
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
            => 3,
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
            .MovSp => 2,
            .MovSpNegOffset16 => 4,
            .Xor, .XorConst8 => 4,
            .AddSp16, .SubSp16 => 3,
            .Store64Offset8 => 4,
            .Store64AtRegPostInc16 => 5,
            .Store32AtRegPostInc16 => 5,
            .Store16AtRegPostInc16 => 5,
            .Store8AtRegPostInc16 => 5,
            .StoreSpAtSpNegOffset16 => 3,
            .StoreSpSub16AtSpNegOffset16 => 5,
            .Store64AtSpNegOffset16,
            .Store32AtSpNegOffset16,
            .Store16AtSpNegOffset16,
            .Store8AtSpNegOffset16,
            => 4,
            .Load64AtReg => 3,
            .Load32AtReg => 3,
            .Load16AtReg => 3,
            .Load8AtReg => 3,
        };
    }

    pub fn maxInstrSize() u8 {
        var max: u8 = 0;
        inline for (@typeInfo(Self).@"enum".fields) |field| {
            const val: Self = @enumFromInt(field.value);
            max = @max(val.getInstrLen(), max);
        }
        return max;
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .SetReg64 => "set_reg_64",
            .SetReg32 => "set_reg_32",
            .SetReg16 => "set_reg_16",
            .SetReg8 => "set_reg_8",
            .Add => "add",
            .Sub => "sub",
            .Mult => "mult",
            .Add8 => "add_8",
            .Sub8 => "sub_8",
            .CmpConst8 => "cmp_const_byte",
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
            .Cmp => "cmp",
            .CmpSetRegEQ => "cmp_set_reg_eq",
            .CmpSetRegNE => "cmp_set_reg_ne",
            .CmpSetRegGT => "cmp_set_reg_gt",
            .CmpSetRegLT => "cmp_set_reg_lt",
            .CmpSetRegGTE => "cmp_set_reg_gte",
            .CmpSetRegLTE => "cmp_set_reg_lte",
            .IncConst8 => "inc_const_byte",
            .DecConst8 => "dec_const_byte",
            .Mov => "mov",
            .MovSp => "mov_sp",
            .MovSpNegOffset16 => "mov_sp_neg_offset_16",
            .Xor => "xor",
            .XorConst8 => "xor_const_byte",
            .AddSp16 => "add_sp_16",
            .SubSp16 => "sub_sp_16",
            .Store64Offset8 => "store_64_offset_8",
            .Store64AtRegPostInc16 => "store_64_at_reg_post_inc_16",
            .Store32AtRegPostInc16 => "store_32_at_reg_post_inc_16",
            .Store16AtRegPostInc16 => "store_16_at_reg_post_inc_16",
            .Store8AtRegPostInc16 => "store_8_at_reg_post_inc_16",
            .StoreSpAtSpNegOffset16 => "store_sp_at_sp_neg_offset_16",
            .StoreSpSub16AtSpNegOffset16 => "store_sp_sub_16_at_sp_neg_offset_16",
            .Store64AtSpNegOffset16 => "store_64_at_sp_neg_offset_16",
            .Store32AtSpNegOffset16 => "store_32_at_sp_neg_offset_16",
            .Store16AtSpNegOffset16 => "store_16_at_sp_neg_offset_16",
            .Store8AtSpNegOffset16 => "store_8_at_sp_neg_offset_16",
            .Load64AtReg => "load_64_at_reg",
            .Load32AtReg => "load_32_at_reg",
            .Load16AtReg => "load_16_at_reg",
            .Load8AtReg => "load_8_at_reg",
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

const JumpInstr = struct {
    amount: u16 = 0,
};

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
    dest: TempRegister,
    fromRegPtr: TempRegister,
};

pub const Instr = union(InstructionVariants) {
    const Self = @This();

    SetReg64: SetRegInstr(u64),
    SetReg32: SetRegInstr(u32),
    SetReg16: SetRegInstr(u16),
    SetReg8: SetRegInstr(u8),
    Add: MathInstr,
    Add8: OneOpResultInstr(u8),
    Sub: MathInstr,
    Sub8: OneOpResultInstr(u8),
    Mult: MathInstr,
    Jump: JumpInstr,
    JumpEQ: JumpInstr,
    JumpNE: JumpInstr,
    JumpGT: JumpInstr,
    JumpLT: JumpInstr,
    JumpGTE: JumpInstr,
    JumpLTE: JumpInstr,
    JumpBack: JumpInstr,
    JumpBackEQ: JumpInstr,
    JumpBackNE: JumpInstr,
    JumpBackGT: JumpInstr,
    JumpBackLT: JumpInstr,
    JumpBackGTE: JumpInstr,
    JumpBackLTE: JumpInstr,
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
    MovSp: TempRegister,
    MovSpNegOffset16: struct {
        reg: TempRegister,
        offset: u16,
    },
    Xor: TwoOpResultInstr,
    XorConst8: struct {
        dest: TempRegister = 0,
        reg: TempRegister = 0,
        byte: u8 = 0,
    },
    AddSp16: u16,
    SubSp16: u16,
    Store64Offset8: StoreOffsetInstr(u8),
    Store64AtRegPostInc16: StoreIncInstr(u16),
    Store32AtRegPostInc16: StoreIncInstr(u16),
    Store16AtRegPostInc16: StoreIncInstr(u16),
    Store8AtRegPostInc16: StoreIncInstr(u16),
    StoreSpAtSpNegOffset16: struct {
        offset: u16,
    },
    StoreSpSub16AtSpNegOffset16: struct {
        sub: u16,
        offset: u16,
    },
    Store64AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store32AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store16AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Store8AtSpNegOffset16: StoreOffsetSpInstr(u16),
    Load64AtReg: LoadAtReg,
    Load32AtReg: LoadAtReg,
    Load16AtReg: LoadAtReg,
    Load8AtReg: LoadAtReg,

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

    pub fn getCurrentScope(self: *Self) *RegScope {
        return self.scopes.getLast();
    }

    pub fn addRegister(self: *Self, reg: TempRegister) !void {
        const scope = self.getCurrentScope();
        try scope.addRegister(reg);
    }
};

const InstrInfo = struct {
    chunk: *InstrChunk,
    location: u64,
};

const LoopInfo = struct {
    const Self = @This();

    allocator: Allocator,
    continueInstr: u64,
    breaks: *ArrayList(InstrInfo),
    continues: *ArrayList(InstrInfo),

    pub fn init(allocator: Allocator) !Self {
        const breaksPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);
        const continuesPtr = try utils.createMut(ArrayList(InstrInfo), allocator, .empty);

        return .{
            .allocator = allocator,
            .continueInstr = 0,
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

    pub fn appendBreak(self: *Self, instr: *InstrChunk, location: u64) !void {
        try self.breaks.append(self.allocator, .{
            .chunk = instr,
            .location = location,
        });
    }

    pub fn appendContinue(self: *Self, instr: *InstrChunk, location: u64) !void {
        try self.continues.append(self.allocator, .{
            .chunk = instr,
            .location = location,
        });
    }

    pub fn setContinueByte(self: *Self, byte: u64) void {
        self.continueInstr = byte;
    }
};

const RegInfo = struct {
    varInfo: ?*VarGenInfo = null,
    active: bool = false,
};

const GenInfoSettings = struct {
    // respected for one expr node, then set to default
    outputCmpAsRegister: bool,
};

const CODEGEN_DEFAULT_SETTINGS = GenInfoSettings{
    .outputCmpAsRegister = true,
};

const VarGenInfo = struct {
    stackLocation: ?u64 = null,
    name: []const u8,
    dataSize: u64,
    reg: TempRegister,
};

const ProcInfo = struct {
    stackFrameSize: u64,
    startInstr: ?*InstrChunk,
};

fn blankFreeVarGenInfo(_: Allocator, _: *Context, _: *VarGenInfo) void {}
fn blankPrintVarGenInfo(_: *Context, _: *VarGenInfo, _: *Writer) !void {}

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

pub const GenInfo = struct {
    const Self = @This();

    allocator: Allocator,
    procInfo: ProcInfo,
    chunks: *GenInfoChunks,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    registers: *ArrayList(RegInfo),
    regScopes: *RegScopes,
    varGenInfoPool: *VarGenInfoPool,
    varNameReg: *StringHashMap(*VarGenInfo),
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),
    byteCounter: u64,

    pub fn init(allocator: Allocator) !Self {
        const varNameReg = try utils.initMutPtrT(StringHashMap(*VarGenInfo), allocator);
        const regScopes = try RegScopes.init(allocator);
        const regScopesPtr = try utils.createMut(RegScopes, allocator, regScopes);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, .empty);

        const registers = try ArrayList(RegInfo).initCapacity(allocator, vmInfo.NUM_REGISTERS * 8);
        const registersPtr = try utils.createMut(ArrayList(RegInfo), allocator, registers);

        const tempPool = try VarGenInfoPool.initPreheated(allocator, VAR_GEN_INFO_POOL_SIZE);
        const varGenInfoPool = try utils.createMut(VarGenInfoPool, allocator, tempPool);

        const chunks = try GenInfoChunks.init(allocator);
        const chunksPtr = try utils.createMut(GenInfoChunks, allocator, chunks);

        return .{
            .allocator = allocator,
            .vmInfo = .{
                .stackStartSize = 0,
                .version = 0,
            },
            .chunks = chunksPtr,
            .varGenInfoPool = varGenInfoPool,
            .varNameReg = varNameReg,
            .regScopes = regScopesPtr,
            .byteCounter = 0,
            .settings = CODEGEN_DEFAULT_SETTINGS,
            .loopInfo = loopInfoPtr,
            .registers = registersPtr,
            .procInfo = .{
                .stackFrameSize = 0,
                .startInstr = null,
            },
        };
    }

    pub fn deinit(self: Self) void {
        self.chunks.deinit(self.allocator);
        self.allocator.destroy(self.chunks);

        self.registers.deinit(self.allocator);
        self.allocator.destroy(self.registers);

        self.varNameReg.deinit();
        self.allocator.destroy(self.varNameReg);

        self.regScopes.deinit();
        self.allocator.destroy(self.regScopes);

        self.varGenInfoPool.deinit();
        self.allocator.destroy(self.varGenInfoPool);

        for (self.loopInfo.items) |item| {
            item.deinit();
            self.allocator.destroy(item);
        }
        self.loopInfo.deinit(self.allocator);
        self.allocator.destroy(self.loopInfo);
    }

    pub fn clear(self: *Self) void {
        _ = self;
        utils.unimplemented();
    }

    pub fn writeChunks(self: Self, writer: *Writer) !void {
        try writer.writeByte(self.vmInfo.version);
        var buf: [vmInfo.startStackSize]u8 = undefined;
        std.mem.writeInt(vmInfo.StartStackType, &buf, self.vmInfo.stackStartSize, .little);
        try writer.writeAll(&buf);

        var current: ?*InstrChunk = self.chunks.listStart;
        while (current) |chunk| : (current = chunk.next) {
            try writeChunk(chunk, writer);
        }
    }

    pub fn appendChunk(self: *Self, data: Instr) !*InstrChunk {
        self.byteCounter += data.getInstrLen();
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

    pub fn getAvailableReg(self: Self) !TempRegister {
        // TODO - refactor to use available stack
        for (self.registers.items, 0..) |reg, index| {
            if (!reg.active) {
                return @intCast(index);
            }
        }

        try self.registers.append(self.allocator, .{});
        return @intCast(self.registers.items.len - 1);
    }

    pub fn availableRegReplace(self: *Self, reg: TempRegister) !TempRegister {
        if (self.isRegVariable(reg)) {
            return try self.getAvailableReg();
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
        self.registers.items[reg].active = true;
        try self.regScopes.addRegister(reg);
    }

    pub fn releaseRegister(self: *Self, reg: TempRegister) void {
        self.registers.items[reg] = .{};
    }

    pub fn getVariableRegister(self: Self, name: []const u8) TempRegister {
        return self.varNameReg.get(name).?.reg;
    }

    pub fn setVariableRegister(self: *Self, name: []const u8, reg: TempRegister, size: u64) !void {
        const varGenInfo = try self.varGenInfoPool.create();
        varGenInfo.* = .{
            .reg = reg,
            .name = name,
            .dataSize = size,
        };
        try self.varNameReg.put(name, varGenInfo);
        try self.regScopes.addRegister(reg);
        self.registers.items[reg].varInfo = varGenInfo;
    }

    pub fn isRegVariable(self: Self, reg: TempRegister) bool {
        return self.registers.items[reg].varInfo != null;
    }

    pub fn pushScope(self: *Self) !void {
        try self.regScopes.pushScope();
    }

    pub fn popScope(self: *Self) ![]TempRegister {
        return self.regScopes.popScope();
    }

    pub fn pushLoopInfo(self: *Self) !void {
        const newLoop = try LoopInfo.init(self.allocator);
        const newLoopPtr = try utils.createMut(LoopInfo, self.allocator, newLoop);
        try self.loopInfo.append(self.allocator, newLoopPtr);
    }

    pub fn popLoopInfo(self: *Self) void {
        const last = self.loopInfo.pop();
        const endByte = self.byteCounter;

        if (last) |info| {
            for (info.breaks.items) |chunk| {
                writeLoopJump(chunk, endByte);
            }

            for (info.continues.items) |chunk| {
                writeLoopJump(chunk, info.continueInstr);
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

    pub fn setContinueByte(self: *Self) void {
        const loopInfo = self.currentLoopInfo();
        if (loopInfo) |info| {
            info.setContinueByte(self.byteCounter);
        }
    }

    pub fn getRegInfo(self: Self, reg: TempRegister) RegInfo {
        return self.registers.items[reg];
    }

    pub fn getVarGenInfo(self: Self, reg: TempRegister) ?*VarGenInfo {
        return self.registers.items[reg].varInfo;
    }

    pub fn finishProc(self: *Self) !void {
        const listStart = self.chunks.listStart orelse return;
        const start = self.procInfo.startInstr orelse return;
        const end = self.chunks.listEnd orelse return;

        // NOTE - may cause issues u64 -> u16 but prob not much, change if problems happen
        if (self.procInfo.stackFrameSize > std.math.maxInt(u16)) {
            return CodeGenError.StackFrameSizeTooLarge;
        }

        const addSpInstr = Instr{ .AddSp16 = @intCast(self.procInfo.stackFrameSize) };
        const subSpInstr = Instr{ .SubSp16 = @intCast(self.procInfo.stackFrameSize) };
        self.byteCounter += addSpInstr.getInstrLen() + subSpInstr.getInstrLen();

        const addChunk = try self.chunks.newChunk(addSpInstr);
        const subChunk = try self.chunks.newChunk(subSpInstr);

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
};

fn writeLoopJump(info: InstrInfo, endByte: u64) void {
    const diff = @as(u16, @intCast(endByte - info.location));
    setJumpAmount(info.chunk, diff);
}

pub fn codegenAst(allocator: Allocator, context: *Context, ast: blitzAst.Ast) !void {
    writeStartVMInfo(context);
    _ = try genBytecode(allocator, context, ast.root);
    try context.genInfo.finishProc();
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
    node: *const blitzAst.AstNode,
) GenBytecodeError!?TempRegister {
    switch (node.variant) {
        .StructPlaceholder => {},
        .NoOp => {},
        .Seq => |seq| {
            for (seq) |seqNode| {
                _ = try genBytecode(allocator, context, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, context, dec.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            if (context.genInfo.isRegVariable(reg)) {
                const newReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(reg);

                var instr = Instr{ .Mov = .{} };
                instr.Mov.dest = newReg;
                instr.Mov.src = reg;
                _ = try context.genInfo.appendChunk(instr);
            }

            try context.genInfo.setVariableRegister(dec.name, reg, dec.setNode.typeInfo.size);
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
                    return reg;
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
                    return reg;
                },
                .Char => |ch| {
                    const reg = try context.genInfo.getAvailableReg();
                    try context.genInfo.reserveRegister(reg);

                    var instr = Instr{ .SetReg8 = .{} };
                    instr.SetReg8.reg = reg;
                    instr.SetReg8.data = ch;

                    _ = try context.genInfo.appendChunk(instr);
                    return reg;
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
                    return reg;
                },
                .ArraySlice => |items| {
                    context.genInfo.procInfo.stackFrameSize += node.typeInfo.size;
                    const sfSize = context.genInfo.procInfo.stackFrameSize;
                    const sliceInfo = try initSliceBytecode(
                        context,
                        items.len,
                        sfSize,
                    );

                    const setSliceAddrInstr = Instr{
                        .MovSpNegOffset16 = .{
                            .reg = sliceInfo.reg,
                            .offset = @intCast(sfSize),
                        },
                    };

                    const itemSize = if (items.len > 0) items[0].typeInfo.size else 0;

                    for (items) |item| {
                        try context.genInfo.pushScope();

                        const reg = try genBytecode(allocator, context, item) orelse
                            return CodeGenError.ReturnedRegisterNotFound;

                        try context.genInfo.releaseScope();

                        const instData: StoreIncInstr(u16) = .{
                            .fromReg = reg,
                            .toRegPtr = sliceInfo.reg,
                            .inc = @intCast(itemSize),
                        };

                        const store8 = switch (itemSize) {
                            8, 7, 6, 5 => Instr{
                                .Store64AtRegPostInc16 = instData,
                            },
                            4, 3 => Instr{
                                .Store32AtRegPostInc16 = instData,
                            },
                            2 => Instr{
                                .Store16AtRegPostInc16 = instData,
                            },
                            1 => Instr{
                                .Store8AtRegPostInc16 = instData,
                            },
                            else => utils.unimplemented(),
                        };
                        _ = try context.genInfo.appendChunk(store8);
                    }

                    _ = try context.genInfo.appendChunk(setSliceAddrInstr);

                    context.genInfo.releaseRegister(sliceInfo.reg);
                    return sliceInfo.reg;
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            var leftReg: TempRegister = undefined;

            const leftDepth = blitzAst.getExprDepth(expr.left);
            const rightDepth = blitzAst.getExprDepth(expr.right);
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

            var outReg: ?TempRegister = null;

            const buf: Instr = switch (expr.type) {
                .Add, .Sub, .Mult => a: {
                    outReg = try context.genInfo.availableRegReplaceRelease(leftReg, rightReg);
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
                => a: {
                    var instr = if (context.genInfo.settings.outputCmpAsRegister)
                        exprTypeToCmpSetReg(expr.type)
                    else
                        Instr{ .Cmp = .{} };

                    if (context.genInfo.settings.outputCmpAsRegister) {
                        outReg = try context.genInfo.availableRegReplaceRelease(leftReg, rightReg);
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
                    } else {
                        context.genInfo.releaseIfPossible(leftReg);
                        context.genInfo.releaseIfPossible(rightReg);
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
                else => utils.unimplemented(),
            };

            _ = try context.genInfo.appendChunk(buf);

            return outReg;
        },
        .Variable => |name| {
            const storedReg = context.genInfo.getVariableRegister(name);
            return storedReg;
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, context, statement.condition) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            var buf = Instr{ .CmpConst8 = .{} };
            buf.CmpConst8.reg = condReg;
            buf.CmpConst8.data = 1;
            _ = try context.genInfo.appendChunk(buf);

            context.genInfo.releaseIfPossible(condReg);

            const jumpBuf = Instr{ .JumpNE = .{} };
            const jumpChunk = try context.genInfo.appendChunk(jumpBuf);

            const byteCount = context.genInfo.byteCounter;

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, statement.body);
            try context.genInfo.releaseScope();

            if (statement.fallback) |fallback| {
                const jumpEndInstr = Instr{ .Jump = .{} };
                const jumpEndChunk = try context.genInfo.appendChunk(jumpEndInstr);

                const preFallbackByteCount = context.genInfo.byteCounter;

                const diff = @as(u16, @intCast(context.genInfo.byteCounter - byteCount));
                jumpChunk.data.JumpNE.amount = diff;

                try generateFallback(allocator, context, fallback);
                const byteDiff = context.genInfo.byteCounter - preFallbackByteCount;
                const jumpEndDiff = @as(u16, @intCast(byteDiff));
                jumpEndChunk.data.Jump.amount = jumpEndDiff;
            } else {
                const diff = @as(u16, @intCast(context.genInfo.byteCounter - byteCount));
                jumpChunk.data.JumpNE.amount = diff;
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |initNode| {
                _ = try genBytecode(allocator, context, initNode);
            }

            const condInfo = prepForLoopCondition(context, loop.condition);

            try context.genInfo.pushLoopInfo();
            defer context.genInfo.popLoopInfo();

            const preConditionByteCount = context.genInfo.byteCounter;
            const condReg = try genBytecode(allocator, context, loop.condition);
            context.genInfo.settings.outputCmpAsRegister = condInfo.prevCmpAsReg;

            const preBodyByteCount = context.genInfo.byteCounter;
            var jumpEndInstr = Instr{ .JumpNE = .{} };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConst8 = .{} };
                cmpInstr.CmpConst8.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConst8.data = 1;
                _ = try context.genInfo.appendChunk(cmpInstr);
            }

            const jumpEndChunk = try context.genInfo.appendChunk(jumpEndInstr);

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, loop.body);
            try context.genInfo.releaseScope();
            context.genInfo.setContinueByte();
            _ = try genBytecode(allocator, context, loop.incNode);

            const jumpEndDiff: u16 = @intCast(context.genInfo.byteCounter - preBodyByteCount);
            setJumpAmount(jumpEndChunk, jumpEndDiff);

            var jumpStartInstr = Instr{ .JumpBack = .{} };
            const byteDiff = context.genInfo.byteCounter - preConditionByteCount;
            const jumpStartDiff: u16 = @intCast(byteDiff);
            jumpStartInstr.JumpBack.amount = jumpStartDiff;
            _ = try context.genInfo.appendChunk(jumpStartInstr);
        },
        .WhileLoop => |loop| {
            try context.genInfo.pushLoopInfo();
            defer context.genInfo.popLoopInfo();

            const condInfo = prepForLoopCondition(context, loop.condition);

            const preConditionByteCount = context.genInfo.byteCounter;
            const condReg = try genBytecode(allocator, context, loop.condition);

            const preBodyByteCount = context.genInfo.byteCounter;
            var jumpEndInstr = Instr{ .JumpNE = .{} };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.variant.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConst8 = .{} };
                cmpInstr.CmpConst8.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConst8.data = 1;
                _ = try context.genInfo.appendChunk(cmpInstr);
            }

            const jumpEndChunk = try context.genInfo.appendChunk(jumpEndInstr);

            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, loop.body);
            try context.genInfo.releaseScope();
            context.genInfo.setContinueByte();

            const jumpEndDiff = @as(u16, @intCast(context.genInfo.byteCounter - preBodyByteCount));
            setJumpAmount(jumpEndChunk, jumpEndDiff);

            var jumpStartInstr = Instr{ .JumpBack = .{} };
            const byteDiff = context.genInfo.byteCounter - preConditionByteCount;
            const jumpStartDiff = @as(u16, @intCast(byteDiff));
            jumpStartInstr.JumpBack.amount = jumpStartDiff;
            _ = try context.genInfo.appendChunk(jumpStartInstr);
        },
        .IncOne => |inc| {
            const reg = try genBytecode(allocator, context, inc) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .IncConst8 = .{} };
            instr.IncConst8.reg = reg;
            instr.IncConst8.data = 1;

            _ = try context.genInfo.appendChunk(instr);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, context, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .DecConst8 = .{} };
            instr.DecConst8.reg = reg;
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
            instr.Mov.dest = destReg;
            instr.Mov.src = srcReg;
            _ = try context.genInfo.appendChunk(instr);
        },
        .Bang => |expr| {
            const reg = try genBytecode(allocator, context, expr) orelse
                return CodeGenError.NoAvailableRegisters;
            const setReg = try context.genInfo.availableRegReplace(reg);

            var instr = Instr{ .XorConst8 = .{} };
            instr.XorConst8.dest = setReg;
            instr.XorConst8.reg = reg;
            instr.XorConst8.byte = 1;
            _ = try context.genInfo.appendChunk(instr);

            return setReg;
        },
        .Scope => |scope| {
            try context.genInfo.pushScope();
            _ = try genBytecode(allocator, context, scope);
            try context.genInfo.releaseScope();
        },
        .Break => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const buf = Instr{ .Jump = .{} };
            const chunk = try context.genInfo.appendChunk(buf);
            const byteCount = context.genInfo.byteCounter;
            try loopInfo.appendBreak(chunk, byteCount);
        },
        .Continue => {
            const loopInfo = context.genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const instr = Instr{ .Jump = .{} };
            const chunk = try context.genInfo.appendChunk(instr);
            const byteCount = context.genInfo.byteCounter;
            try loopInfo.appendContinue(chunk, byteCount);
        },
        .ArrayInit => |init| {
            context.genInfo.procInfo.stackFrameSize += node.typeInfo.size;
            const sfSize = context.genInfo.procInfo.stackFrameSize;

            const initLen = try std.fmt.parseInt(u64, init.size, 10);
            const sliceInfo = try initSliceBytecode(context, initLen, sfSize);
            defer context.genInfo.releaseRegister(sliceInfo.reg);

            const setSliceAddrInstr = Instr{
                .MovSpNegOffset16 = .{
                    .reg = sliceInfo.reg,
                    .offset = @intCast(sfSize),
                },
            };

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

            const preCmpByteCount = context.genInfo.byteCounter;

            const cmpLen = Instr{
                .CmpConst8 = .{
                    .reg = lenReg,
                    .data = 0,
                },
            };
            _ = try context.genInfo.appendChunk(cmpLen);

            const postCmpByteCount = context.genInfo.byteCounter;

            const jumpInstr = Instr{ .JumpEQ = .{} };
            const jumpChunk = try context.genInfo.appendChunk(jumpInstr);

            try context.genInfo.pushScope();
            const resReg = try genBytecode(allocator, context, init.initNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            try context.genInfo.releaseScope();

            const writeInstr = Instr{
                .Store64AtRegPostInc16 = .{
                    .fromReg = resReg,
                    .toRegPtr = sliceInfo.reg,
                    .inc = @intCast(try init.initType.astType.getSize(context)),
                },
            };
            _ = try context.genInfo.appendChunk(writeInstr);

            const subLen = Instr{
                .Sub8 = .{
                    .dest = lenReg,
                    .reg = lenReg,
                    .data = 1,
                },
            };
            _ = try context.genInfo.appendChunk(subLen);

            const diff: u16 = @intCast(context.genInfo.byteCounter - postCmpByteCount);
            jumpChunk.data.JumpEQ.amount = diff;

            const jumpStart = Instr{
                .JumpBack = .{
                    .amount = @intCast(context.genInfo.byteCounter - preCmpByteCount),
                },
            };
            _ = try context.genInfo.appendChunk(jumpStart);

            _ = try context.genInfo.appendChunk(setSliceAddrInstr);

            return sliceInfo.reg;
        },
        .Dereference => |inner| {
            const resReg = try genBytecode(allocator, context, inner) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const destReg = if (context.genInfo.isRegVariable(resReg)) a: {
                const newReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(newReg);
                break :a newReg;
            } else resReg;

            const loadAtReg: LoadAtReg = .{
                .dest = destReg,
                .fromRegPtr = resReg,
            };

            const loadInstr = switch (node.typeInfo.size) {
                8, 7, 6, 5 => Instr{ .Load64AtReg = loadAtReg },
                4, 3 => Instr{ .Load32AtReg = loadAtReg },
                2 => Instr{ .Load16AtReg = loadAtReg },
                1 => Instr{ .Load8AtReg = loadAtReg },
                else => utils.unimplemented(),
            };
            _ = try context.genInfo.appendChunk(loadInstr);

            return destReg;
        },
        .Pointer => |inner| {
            const resReg = try genBytecode(allocator, context, inner.node) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            if (context.genInfo.getVarGenInfo(resReg)) |info| {
                const ptrReg = try context.genInfo.getAvailableReg();
                try context.genInfo.reserveRegister(ptrReg);

                const location = if (info.stackLocation) |location| location else a: {
                    const itemSize = inner.node.typeInfo.size;
                    context.genInfo.procInfo.stackFrameSize += itemSize;
                    const spCount = context.genInfo.procInfo.stackFrameSize;

                    const storeInstr = switch (info.dataSize) {
                        8, 7, 6, 5 => Instr{
                            .Store64AtSpNegOffset16 = .{
                                .reg = resReg,
                                .offset = @intCast(spCount),
                            },
                        },
                        4, 3 => Instr{
                            .Store32AtSpNegOffset16 = .{
                                .reg = resReg,
                                .offset = @intCast(spCount),
                            },
                        },
                        2 => Instr{
                            .Store16AtSpNegOffset16 = .{
                                .reg = resReg,
                                .offset = @intCast(spCount),
                            },
                        },
                        1 => Instr{
                            .Store8AtSpNegOffset16 = .{
                                .reg = resReg,
                                .offset = @intCast(spCount),
                            },
                        },
                        else => utils.unimplemented(),
                    };
                    _ = try context.genInfo.appendChunk(storeInstr);

                    break :a spCount;
                };

                const u16Location: u16 = @intCast(location);
                const setLocInstr = Instr{
                    .MovSpNegOffset16 = .{
                        .reg = ptrReg,
                        .offset = u16Location,
                    },
                };
                _ = try context.genInfo.appendChunk(setLocInstr);

                return ptrReg;
            }
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
                else => unreachable,
            };

            _ = try context.genInfo.appendChunk(instr);
        },
        else => {},
    }

    return null;
}

fn initSliceBytecode(context: *Context, len: u64, offset: u64) !SliceBytecodeInfo {
    const writeAtSp = Instr{
        .StoreSpSub16AtSpNegOffset16 = .{
            .sub = @intCast(offset - vmInfo.POINTER_SIZE * 2),
            .offset = @intCast(offset),
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
            .offset = @intCast(offset - vmInfo.POINTER_SIZE),
        },
    };
    _ = try context.genInfo.appendChunk(writeLen);

    const setArrStart = Instr{
        .MovSpNegOffset16 = .{
            .reg = reg,
            .offset = @intCast(offset - vmInfo.POINTER_SIZE * 2),
        },
    };
    _ = try context.genInfo.appendChunk(setArrStart);

    return .{
        .sliceLocation = offset,
        .reg = reg,
    };
}

fn prepForLoopCondition(context: *Context, condition: *blitzAst.AstNode) LoopCondInfo {
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

fn setJumpAmount(chunk: *InstrChunk, amount: u16) void {
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
            instr.amount = amount;
        },
        else => unreachable,
    }
}

fn exprTypeToCmpSetReg(expr: blitzAst.OpExprTypes) Instr {
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

fn compOpToJump(opType: blitzAst.OpExprTypes, back: bool) !Instr {
    return if (back) switch (opType) {
        .Equal => .{ .JumpBackEQ = .{} },
        .NotEqual => .{ .JumpBackNE = .{} },
        .GreaterThan => .{ .JumpBackGT = .{} },
        .LessThan => .{ .JumpBackLT = .{} },
        .GreaterThanEq => .{ .JumpBackGTE = .{} },
        .LessThanEq => .{ .JumpBackLTE = .{} },
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    } else switch (opType) {
        .Equal => .{ .JumpEQ = .{} },
        .NotEqual => .{ .JumpNE = .{} },
        .GreaterThan => .{ .JumpGT = .{} },
        .LessThan => .{ .JumpLT = .{} },
        .GreaterThanEq => .{ .JumpGTE = .{} },
        .LessThanEq => .{ .JumpLTE = .{} },
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    };
}

fn generateFallback(
    allocator: Allocator,
    context: *Context,
    fallback: blitzAst.FallbackInfo,
) !void {
    var jumpChunk: ?*InstrChunk = null;

    const statement = fallback.node.variant.IfStatement;

    if (fallback.hasCondition) {
        const condReg = try genBytecode(allocator, context, statement.condition) orelse
            return CodeGenError.ReturnedRegisterNotFound;

        var instr = Instr{ .CmpConst8 = .{} };
        instr.CmpConst8.reg = condReg;
        instr.CmpConst8.data = 1;
        _ = try context.genInfo.appendChunk(instr);

        context.genInfo.releaseIfPossible(condReg);

        const jumpInstr = Instr{ .JumpNE = .{} };
        jumpChunk = try context.genInfo.appendChunk(jumpInstr);
    }

    const preBodyByteCount = context.genInfo.byteCounter;

    _ = try genBytecode(allocator, context, statement.body);

    if (statement.fallback) |newFallback| {
        const jumpEndInstr = Instr{ .Jump = .{} };
        const jumpEndChunk = try context.genInfo.appendChunk(jumpEndInstr);

        const preFallbackByteCount = context.genInfo.byteCounter;

        if (jumpChunk) |chunk| {
            const diff = @as(u16, @intCast(context.genInfo.byteCounter - preBodyByteCount));
            chunk.data.JumpNE.amount = diff;
        }

        try generateFallback(allocator, context, newFallback);
        const diff = @as(u16, @intCast(context.genInfo.byteCounter - preFallbackByteCount));
        jumpEndChunk.data.Jump.amount = diff;
    }
}

fn writeChunk(chunk: *InstrChunk, writer: *Writer) !void {
    try writer.writeByte(chunk.data.getInstrByte());

    switch (chunk.data) {
        .SetReg64 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writeNumber(u64, inst.data, writer);
        },
        .SetReg32 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writeNumber(u32, inst.data, writer);
        },
        .SetReg16 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writeNumber(u16, inst.data, writer);
        },
        .SetReg8 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writeNumber(u8, inst.data, writer);
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
        => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.reg1));
            try writer.writeByte(@intCast(inst.reg2));
        },
        .Add8, .Sub8 => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(@intCast(inst.data));
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
        => |inst| {
            try writer.writeInt(u16, inst.amount, .little);
        },
        .Cmp => |inst| {
            try writer.writeByte(@intCast(inst.reg1));
            try writer.writeByte(@intCast(inst.reg2));
        },
        .CmpConst8, .IncConst8, .DecConst8 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.data);
        },
        .Mov => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.src));
        },
        .MovSp => |inst| {
            try writer.writeByte(@intCast(inst));
        },
        .MovSpNegOffset16 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeInt(u16, inst.offset, .little);
        },
        .XorConst8 => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.byte);
        },
        .AddSp16, .SubSp16 => |inst| {
            try writer.writeInt(u16, inst, .little);
        },
        .Store64Offset8 => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toRegPtr));
            try writer.writeByte(inst.offset);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toRegPtr));
            try writer.writeInt(u16, inst.inc, .little);
        },
        .StoreSpAtSpNegOffset16 => |inst| {
            try writer.writeInt(u16, inst.offset, .little);
        },
        .StoreSpSub16AtSpNegOffset16 => |inst| {
            try writer.writeInt(u16, inst.sub, .little);
            try writer.writeInt(u16, inst.offset, .little);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeInt(u16, inst.offset, .little);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.fromRegPtr));
        },
    }
}

fn writeNumber(comptime T: type, data: T, writer: *Writer) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
