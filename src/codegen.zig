const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const blitzContext = blitz.context;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const PointerType = vmInfo.PointerType;
const TempRegister = vmInfo.TempRegister;
const Writer = std.Io.Writer;
const Context = blitzContext.Context;

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
    ExpectedLoopInfo,
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
    CmpConstByte, // inst, reg1, 1B data
    IncConstByte, // inst, in/out reg, 1B data
    DecConstByte, // inst, in/out reg, 1B data
    Mov, // inst, reg1, reg2
    MovSp, // inst, dest
    Xor, // inst, out reg, reg1, reg2
    XorConstByte, // inst, out reg, reg1, 1B data
    AddSp8, // inst, 1B data
    SubSp8, // inst, 1B data
    AddSpReg, // inst, reg
    SubSpReg, // inst, reg
    Store64Offset8, // inst, reg, to reg (ptr), offset 1B
    Store64AtRegPostInc8, // inst, reg, to reg (ptr), inc 1B
    Store64AtRegPostInc64, // inst, reg, to reg (ptr), inc 1B
    Store64AtSpPostInc8, // inst, reg, inc 1B
    StoreAtSpPostInc8, // inst, to reg (ptr), inc 1B

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
            .CmpConstByte => 3,
            .IncConstByte, .DecConstByte => 3,
            .Mov => 3,
            .MovSp => 2,
            .Xor, .XorConstByte => 4,
            .AddSp8, .SubSp8 => 2,
            .AddSpReg, .SubSpReg => 2,
            .Store64Offset8 => 4,
            .Store64AtRegPostInc8 => 4,
            .Store64AtRegPostInc64 => 11,
            .Store64AtSpPostInc8 => 3,
            .StoreAtSpPostInc8 => 3,
        };
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
            .CmpConstByte => "cmp_const_byte",
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
            .IncConstByte => "inc_const_byte",
            .DecConstByte => "dec_const_byte",
            .Mov => "mov",
            .MovSp => "mov_sp",
            .Xor => "xor",
            .XorConstByte => "xor_const_byte",
            .AddSp8 => "add_sp_8",
            .SubSp8 => "sub_sp_8",
            .AddSpReg => "add_sp_reg",
            .SubSpReg => "sub_sp_reg",
            .Store64Offset8 => "store_64_offset_8",
            .Store64AtRegPostInc8 => "store_64_at_reg_post_inc_8",
            .Store64AtRegPostInc64 => "store_64_at_reg_post_inc_64",
            .Store64AtSpPostInc8 => "store_64_at_sp_post_inc_8",
            .StoreAtSpPostInc8 => "store_at_sp_post_inc_8",
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
    CmpConstByte: RegBytePayloadInstr,
    IncConstByte: RegBytePayloadInstr,
    DecConstByte: RegBytePayloadInstr,
    Mov: struct {
        dest: TempRegister = 0,
        src: TempRegister = 0,
    },
    MovSp: TempRegister,
    Xor: TwoOpResultInstr,
    XorConstByte: struct {
        dest: TempRegister = 0,
        reg: TempRegister = 0,
        byte: u8 = 0,
    },
    AddSp8: u8,
    SubSp8: u8,
    AddSpReg: SpRegInstr,
    SubSpReg: SpRegInstr,
    Store64Offset8: StoreOffsetInstr(u8),
    Store64AtRegPostInc8: StoreIncInstr(u8),
    Store64AtRegPostInc64: StoreIncInstr(u64),
    Store64AtSpPostInc8: StoreIncSpInstr(u8),
    StoreAtSpPostInc8: StoreIncSpInstr(u8),

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
};

const SliceBytecodeInfo = struct {
    sliceReg: TempRegister,
    spReg: TempRegister,
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

const VarRegLocInfoVariants = enum {
    Register,
    StackLocation,
};

const VariableRegLocationInfo = union(VarRegLocInfoVariants) {
    Register: TempRegister,
    StackLocation: PointerType,
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
    active: bool = false,
    isVar: bool = false,
};

const GenInfoSettings = struct {
    // respected for one expr node, then set to default
    outputCmpAsRegister: bool,
};

const CODEGEN_DEFAULT_SETTINGS = GenInfoSettings{
    .outputCmpAsRegister = true,
};

pub const GenInfo = struct {
    const Self = @This();

    allocator: Allocator,
    vmInfo: struct {
        stackStartSize: u32,
        version: u8,
    },
    instructionList: ?*InstrChunk,
    last: ?*InstrChunk,
    registers: *ArrayList(RegInfo),
    regScopes: *RegScopes,
    varNameReg: *StringHashMap(u32),
    byteCounter: u64,
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),

    pub fn init(
        allocator: Allocator,
    ) !Self {
        const varNameReg = try utils.initMutPtrT(
            StringHashMap(TempRegister),
            allocator,
        );
        const regScopes = try RegScopes.init(allocator);
        const regScopesPtr = try utils.createMut(RegScopes, allocator, regScopes);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, .empty);

        const registers = try ArrayList(RegInfo).initCapacity(allocator, vmInfo.NUM_REGISTERS);
        const registersPtr = try utils.createMut(ArrayList(RegInfo), allocator, registers);

        return .{
            .allocator = allocator,
            .vmInfo = .{
                .stackStartSize = 0,
                .version = 0,
            },
            .instructionList = null,
            .last = null,
            .varNameReg = varNameReg,
            .regScopes = regScopesPtr,
            .byteCounter = 0,
            .settings = CODEGEN_DEFAULT_SETTINGS,
            .loopInfo = loopInfoPtr,
            .registers = registersPtr,
        };
    }

    pub fn deinit(self: Self) void {
        var current = self.instructionList;
        while (current != null) : (current = current.?.next) {
            self.allocator.destroy(current.?);
        }

        self.registers.deinit(self.allocator);
        self.allocator.destroy(self.registers);

        self.varNameReg.deinit();
        self.allocator.destroy(self.varNameReg);

        self.regScopes.deinit();
        self.allocator.destroy(self.regScopes);

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

        var current: ?*InstrChunk = self.instructionList;
        while (current) |chunk| : (current = chunk.next) {
            try writeChunk(chunk, writer);
        }
    }

    pub fn appendChunk(self: *Self, data: Instr) !*InstrChunk {
        self.byteCounter += data.getInstrLen();
        const newChunk = try utils.createMut(InstrChunk, self.allocator, InstrChunk.init(data));

        if (self.last) |last| {
            last.next = newChunk;
            newChunk.prev = last;
            self.last = newChunk;
        } else {
            self.instructionList = newChunk;
            self.last = newChunk;
        }

        return newChunk;
    }

    pub fn getAvailableReg(self: Self) !TempRegister {
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
        if (!self.registers.items[reg1].isVar) {
            self.releaseIfPossible(reg2);
            return reg1;
        } else if (!self.registers.items[reg2].isVar) {
            self.releaseIfPossible(reg1);
            return reg2;
        }

        const reg = try self.getAvailableReg();
        try self.reserveRegister(reg);
        return reg;
    }

    pub fn releaseIfPossible(self: *Self, reg: TempRegister) void {
        if (self.registers.items[reg].isVar) return;
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
        return self.varNameReg.get(name).?;
    }

    pub fn setVariableRegister(self: *Self, name: []const u8, reg: TempRegister) !void {
        try self.varNameReg.put(name, reg);
        self.registers.items[reg].isVar = true;
        try self.regScopes.addRegister(reg);
    }

    pub fn isRegVariable(self: Self, reg: TempRegister) bool {
        return self.registers.items[reg].isVar;
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
};

fn writeLoopJump(info: InstrInfo, endByte: u64) void {
    const diff = @as(u16, @intCast(endByte - info.location));
    setJumpAmount(info.chunk, diff);
}

pub fn codegenAst(allocator: Allocator, context: *Context, ast: blitzAst.Ast) !void {
    writeStartVMInfo(context);
    _ = try genBytecode(allocator, context, ast.root);
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
            try context.genInfo.setVariableRegister(dec.name, reg);
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
                    const sliceInfo = try initSliceBytecode(context, items.len);

                    const offset: u8 = @intCast(node.typeSize);

                    for (items) |item| {
                        try context.genInfo.pushScope();

                        const reg = try genBytecode(allocator, context, item) orelse
                            return CodeGenError.ReturnedRegisterNotFound;

                        try context.genInfo.releaseScope();

                        const store8 = Instr{
                            .Store64AtRegPostInc8 = .{
                                .fromReg = reg,
                                .toRegPtr = sliceInfo.spReg,
                                .inc = offset,
                            },
                        };
                        _ = try context.genInfo.appendChunk(store8);
                    }

                    context.genInfo.releaseRegister(sliceInfo.spReg);
                    return sliceInfo.sliceReg;
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

            var buf = Instr{ .CmpConstByte = .{} };
            buf.CmpConstByte.reg = condReg;
            buf.CmpConstByte.data = 1;
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
                var cmpInstr = Instr{ .CmpConstByte = .{} };
                cmpInstr.CmpConstByte.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConstByte.data = 1;
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
                var cmpInstr = Instr{ .CmpConstByte = .{} };
                cmpInstr.CmpConstByte.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConstByte.data = 1;
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
            var instr = Instr{ .IncConstByte = .{} };
            instr.IncConstByte.reg = reg;
            instr.IncConstByte.data = 1;

            _ = try context.genInfo.appendChunk(instr);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, context, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .DecConstByte = .{} };
            instr.DecConstByte.reg = reg;
            instr.DecConstByte.data = 1;

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

            var instr = Instr{ .XorConstByte = .{} };
            instr.XorConstByte.dest = setReg;
            instr.XorConstByte.reg = reg;
            instr.XorConstByte.byte = 1;
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
            const initLen = try std.fmt.parseInt(u64, init.size, 10);
            std.debug.print("@@ {s} -> {d}\n", .{ init.size, initLen });
            const sliceInfo = try initSliceBytecode(context, initLen);

            const lenReg = try context.genInfo.getAvailableReg();
            try context.genInfo.reserveRegister(lenReg);

            const movLen = Instr{
                .SetReg64 = .{
                    .reg = lenReg,
                    .data = initLen,
                },
            };
            _ = try context.genInfo.appendChunk(movLen);

            const preBodyByteCount = context.genInfo.byteCounter;

            const cmpLen = Instr{
                .CmpConstByte = .{
                    .reg = lenReg,
                    .data = 0,
                },
            };
            _ = try context.genInfo.appendChunk(cmpLen);

            const jumpInstr = Instr{ .JumpEQ = .{} };
            const jumpChunk = try context.genInfo.appendChunk(jumpInstr);

            const resReg = try genBytecode(allocator, context, init.initNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const writeInstr = Instr{
                .Store64AtRegPostInc64 = .{
                    .fromReg = resReg,
                    .toRegPtr = sliceInfo.spReg,
                    .inc = init.initType.astType.getSize(),
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

            const diff: u16 = @intCast(context.genInfo.byteCounter - preBodyByteCount);
            jumpChunk.data.JumpEQ.amount = diff;

            const jumpStart = Instr{
                .JumpBack = .{
                    .amount = @intCast(context.genInfo.byteCounter - preBodyByteCount),
                },
            };
            _ = try context.genInfo.appendChunk(jumpStart);

            return sliceInfo.sliceReg;
        },
        else => {},
    }

    return null;
}

fn initSliceBytecode(context: *Context, len: u64) !SliceBytecodeInfo {
    const sliceReg = try context.genInfo.getAvailableReg();
    try context.genInfo.reserveRegister(sliceReg);
    const moveSpInstr = Instr{ .MovSp = sliceReg };
    _ = try context.genInfo.appendChunk(moveSpInstr);

    const spReg = try context.genInfo.getAvailableReg();
    try context.genInfo.reserveRegister(spReg);

    const addReg = Instr{
        .Add8 = .{
            .dest = spReg,
            .reg = sliceReg,
            .data = vmInfo.POINTER_SIZE * 2,
        },
    };
    _ = try context.genInfo.appendChunk(addReg);

    const writeAtSp = Instr{
        .StoreAtSpPostInc8 = .{
            .reg = spReg,
            .inc = vmInfo.POINTER_SIZE,
        },
    };
    _ = try context.genInfo.appendChunk(writeAtSp);

    const setLen = Instr{
        .SetReg64 = .{
            .reg = spReg,
            .data = len,
        },
    };
    _ = try context.genInfo.appendChunk(setLen);

    const writeLen = Instr{
        .Store64AtSpPostInc8 = .{
            .reg = spReg,
            .inc = vmInfo.POINTER_SIZE,
        },
    };
    _ = try context.genInfo.appendChunk(writeLen);

    const addSp = Instr{
        .AddSpReg = .{
            .reg = spReg,
        },
    };
    _ = try context.genInfo.appendChunk(addSp);

    const regAsSp = Instr{
        .Add8 = .{
            .dest = spReg,
            .reg = sliceReg,
            .data = vmInfo.POINTER_SIZE * 2,
        },
    };
    _ = try context.genInfo.appendChunk(regAsSp);

    return .{
        .sliceReg = sliceReg,
        .spReg = spReg,
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

        var instr = Instr{ .CmpConstByte = .{} };
        instr.CmpConstByte.reg = condReg;
        instr.CmpConstByte.data = 1;
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
        .CmpConstByte,
        .IncConstByte,
        .DecConstByte,
        => |inst| {
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
        .XorConstByte => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.byte);
        },
        .AddSp8, .SubSp8 => |inst| {
            try writer.writeByte(inst);
        },
        .AddSpReg, .SubSpReg => |inst| {
            try writer.writeByte(@intCast(inst.reg));
        },
        .Store64Offset8 => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toRegPtr));
            try writer.writeByte(inst.offset);
        },
        .Store64AtRegPostInc8 => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toRegPtr));
            try writer.writeByte(inst.inc);
        },
        .Store64AtRegPostInc64 => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toRegPtr));
            try writer.writeInt(u64, inst.inc, .little);
        },
        .Store64AtSpPostInc8 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.inc);
        },
        .StoreAtSpPostInc8 => |inst| {
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.inc);
        },
    }
}

fn writeNumber(comptime T: type, data: T, writer: *Writer) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
