const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const vmInfo = blitz.vmInfo;
const version = blitz.version;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const ArrayList = std.ArrayList;
const PointerType = vmInfo.PointerType;

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
    Sub, // inst, out reg, reg1, reg2
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
    Xor, // inst, out reg, reg1, reg2
    XorConstByte, // inst, out reg, reg1, 1B data
    AddSp, // inst, 4B data
    SubSp, // inst, 4B data
    AddSpReg, // inst, reg
    SubSpReg, // inst, reg
    StoreOffsetByte, // inst, reg, to reg (ptr), offset 1B

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn getInstrLen(self: Self) u8 {
        return switch (self) {
            .SetReg64 => 10,
            .SetReg32 => 6,
            .SetReg16 => 4,
            .SetReg8 => 3,
            .Add, .Sub, .Mult => 4,
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
            .Xor, .XorConstByte => 4,
            .AddSp, .SubSp => 5,
            .AddSpReg, .SubSpReg => 2,
            .StoreOffsetByte => 4,
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
            .Xor => "xor",
            .XorConstByte => "xor_const_byte",
            .AddSp => "add_sp",
            .SubSp => "sub_sp",
            .AddSpReg => "add_sp_reg",
            .SubSpReg => "sub_sp_reg",
            .StoreOffsetByte => "store_offset_byte",
        };
    }
};

const TwoOpResultInstr = struct {
    dest: u32 = 0,
    reg1: u32 = 0,
    reg2: u32 = 0,
};

const RegBytePayloadInstr = struct {
    reg: u32 = 0,
    data: u8 = 0,
};

const MathInstr = TwoOpResultInstr;

const JumpInstr = struct {
    amount: u16 = 0,
};

const SpInstr = struct {
    amount: u32 = 0,
};

const SpRegInstr = struct {
    amount: u32 = 0,
};

fn SetRegInstr(comptime T: type) type {
    return struct {
        reg: u32 = 0,
        data: T = 0,
    };
}

const CmpInstr = struct {
    reg1: u32 = 0,
    reg2: u32 = 0,
};

const CmpSetRegInstr = TwoOpResultInstr;

pub const Instr = union(InstructionVariants) {
    const Self = @This();

    SetReg64: SetRegInstr(u64),
    SetReg32: SetRegInstr(u32),
    SetReg16: SetRegInstr(u16),
    SetReg8: SetRegInstr(u8),
    Add: MathInstr,
    Sub: MathInstr,
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
        dest: u32 = 0,
        src: u32 = 0,
    },
    Xor: TwoOpResultInstr,
    XorConstByte: struct {
        dest: u32 = 0,
        reg: u32 = 0,
        byte: u8 = 0,
    },
    AddSp: SpInstr,
    SubSp: SpInstr,
    AddSpReg: SpRegInstr,
    SubSpReg: SpRegInstr,
    StoreOffsetByte: struct {
        fromReg: u32 = 0,
        toReg: u32 = 0,
        offset: u8 = 0,
    },

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
    Register: u32,
    StackLocation: PointerType,
};

const RegScope = struct {
    const Self = @This();

    allocator: Allocator,
    registers: *ArrayList(u32),

    pub fn init(allocator: Allocator) !Self {
        const registers = try utils.initMutPtrT(ArrayList(u32), allocator);

        return .{
            .allocator = allocator,
            .registers = registers,
        };
    }

    pub fn deinit(self: *Self) void {
        self.registers.deinit();
        self.allocator.destroy(self.registers);
    }

    pub fn empty(self: *Self) ![]u32 {
        return self.registers.toOwnedSlice();
    }

    pub fn addRegister(self: *Self, reg: u32) !void {
        try self.registers.append(reg);
    }
};

const RegScopes = struct {
    const Self = @This();

    allocator: Allocator,
    scopes: *ArrayList(*RegScope),

    pub fn init(allocator: Allocator) !Self {
        const firstScope = try RegScope.init(allocator);
        const firstScopePtr = try utils.createMut(RegScope, allocator, firstScope);
        var scopes = try utils.initMutPtrT(ArrayList(*RegScope), allocator);
        try scopes.append(firstScopePtr);

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

        self.scopes.deinit();
        self.allocator.destroy(self.scopes);
    }

    pub fn pushScope(self: *Self) !void {
        const newScope = try RegScope.init(self.allocator);
        const newScopePtr = try utils.createMut(RegScope, self.allocator, newScope);
        try self.scopes.append(newScopePtr);
    }

    pub fn popScope(self: *Self) ![]u32 {
        if (self.scopes.items.len == 1) {
            const oldContents = try self.scopes.items[0].empty();
            return oldContents;
        }

        const scope = self.scopes.pop().?;
        const oldContents = try scope.registers.toOwnedSlice();
        self.deinitScope(scope);
        return oldContents;
    }

    pub fn getCurrentScopeContents(self: *Self) []u32 {
        return self.scopes.getLast().registers.items;
    }

    pub fn getCurrentScope(self: *Self) *RegScope {
        return self.scopes.getLast();
    }

    pub fn addRegister(self: *Self, reg: u32) !void {
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
        const breaks = ArrayList(InstrInfo).init(allocator);
        const breaksPtr = try utils.createMut(ArrayList(InstrInfo), allocator, breaks);
        const continues = ArrayList(InstrInfo).init(allocator);
        const continuesPtr = try utils.createMut(ArrayList(InstrInfo), allocator, continues);

        return .{
            .allocator = allocator,
            .continueInstr = 0,
            .breaks = breaksPtr,
            .continues = continuesPtr,
        };
    }

    pub fn deinit(self: *Self) void {
        self.breaks.deinit();
        self.allocator.destroy(self.breaks);

        self.continues.deinit();
        self.allocator.destroy(self.continues);
    }

    pub fn appendBreak(self: *Self, instr: *InstrChunk, location: u64) !void {
        try self.breaks.append(.{
            .chunk = instr,
            .location = location,
        });
    }

    pub fn appendContinue(self: *Self, instr: *InstrChunk, location: u64) !void {
        try self.continues.append(.{
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
    varNameRegRel: *StringHashMap(VariableRegLocationInfo),
    byteCounter: u64,
    settings: GenInfoSettings,
    loopInfo: *ArrayList(*LoopInfo),

    pub fn init(
        allocator: Allocator,
    ) !Self {
        const varNameRegRel = try utils.initMutPtrT(
            StringHashMap(VariableRegLocationInfo),
            allocator,
        );
        const regScopes = try RegScopes.init(allocator);
        const regScopesPtr = try utils.createMut(RegScopes, allocator, regScopes);
        const loopInfo = ArrayList(*LoopInfo).init(allocator);
        const loopInfoPtr = try utils.createMut(ArrayList(*LoopInfo), allocator, loopInfo);

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
            .varNameRegRel = varNameRegRel,
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

        self.registers.deinit();
        self.allocator.destroy(self.registers);

        self.varNameRegRel.deinit();
        self.allocator.destroy(self.varNameRegRel);

        self.regScopes.deinit();
        self.allocator.destroy(self.regScopes);

        for (self.loopInfo.items) |item| {
            item.deinit();
            self.allocator.destroy(item);
        }
        self.loopInfo.deinit();
        self.allocator.destroy(self.loopInfo);
    }

    pub fn writeChunks(self: Self, writer: anytype) !void {
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

    pub fn getAvailableReg(self: Self) !u32 {
        for (self.registers.items, 0..) |reg, index| {
            if (!reg.active) {
                return @intCast(index);
            }
        }

        try self.registers.append(.{});
        return @intCast(self.registers.items.len - 1);
    }

    pub fn availableRegReplace(self: *Self, reg: u32) !u32 {
        if (self.isRegVariable(reg)) {
            return try self.getAvailableReg();
        }

        return reg;
    }

    pub fn availableRegReplaceRelease(
        self: *Self,
        reg1: u32,
        reg2: u32,
    ) !u32 {
        if (!self.registers.items[reg1].isVar) {
            self.releaseIfPossible(reg2);
            return reg1;
        } else if (!self.registers.items[reg2].isVar) {
            self.releaseIfPossible(reg1);
            return reg2;
        }

        const reg = try self.getAvailableReg();
        self.reserveRegister(reg);
        return reg;
    }

    pub fn releaseIfPossible(self: *Self, reg: u32) void {
        if (self.registers.items[reg].isVar) return;
        self.releaseRegister(reg);
    }

    pub fn reserveRegister(self: *Self, reg: u32) void {
        self.registers.items[reg].active = true;
    }

    pub fn releaseRegister(self: *Self, reg: u32) void {
        self.registers.items[reg] = .{};
    }

    pub fn getVariableRegister(self: Self, name: []u8) u32 {
        return self.varNameRegRel.get(name).?.Register;
    }

    pub fn setVariableRegister(self: *Self, name: []u8, reg: u32) !void {
        try self.varNameRegRel.put(name, .{ .Register = reg });
        self.registers.items[reg].isVar = true;
        try self.regScopes.addRegister(reg);
    }

    pub fn isRegVariable(self: Self, reg: u32) bool {
        return self.registers.items[reg].isVar;
    }

    pub fn pushScope(self: *Self) !void {
        try self.regScopes.pushScope();
    }

    pub fn popScope(self: *Self) ![]u32 {
        return self.regScopes.popScope();
    }

    pub fn popScopeAndRelease(self: *Self) !void {
        const old = try self.popScope();
        self.releaseRegisters(old);
    }

    pub fn pushLoopInfo(self: *Self) !void {
        const newLoop = try LoopInfo.init(self.allocator);
        const newLoopPtr = try utils.createMut(LoopInfo, self.allocator, newLoop);
        try self.loopInfo.append(newLoopPtr);
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

    fn releaseRegisters(self: *Self, regs: []u32) void {
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

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    writeStartVMInfo(genInfo);
    _ = try genBytecode(allocator, genInfo, ast.root);
}

fn writeStartVMInfo(genInfo: *GenInfo) void {
    genInfo.vmInfo.version = version.VERSION;
}

fn writeIntSliceToInstr(
    comptime T: type,
    buf: []u8,
    offset: usize,
    num: []u8,
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
    genInfo: *GenInfo,
    node: *const blitzAst.AstNode,
) GenBytecodeError!?u32 {
    switch (node.*) {
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                _ = try genBytecode(allocator, genInfo, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            if (genInfo.isRegVariable(reg)) {
                const newReg = try genInfo.getAvailableReg();
                genInfo.reserveRegister(reg);

                var instr = Instr{ .Mov = .{} };
                instr.Mov.dest = newReg;
                instr.Mov.src = reg;
                _ = try genInfo.appendChunk(instr);
            }
            try genInfo.setVariableRegister(dec.name, reg);
        },
        .Value => |value| {
            switch (value) {
                .RawNumber => |num| {
                    const reg = try genInfo.getAvailableReg();
                    genInfo.reserveRegister(reg);

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

                    _ = try genInfo.appendChunk(instr);
                    return reg;
                },
                .Bool => |b| {
                    const reg = try genInfo.getAvailableReg();
                    genInfo.reserveRegister(reg);

                    const instr = Instr{
                        .SetReg8 = .{
                            .reg = reg,
                            .data = @as(u8, @intFromBool(b)),
                        },
                    };

                    _ = try genInfo.appendChunk(instr);
                    return reg;
                },
                .Char => |ch| {
                    const reg = try genInfo.getAvailableReg();
                    genInfo.reserveRegister(reg);

                    var instr = Instr{ .SetReg8 = .{} };
                    instr.SetReg8.reg = reg;
                    instr.SetReg8.data = ch;

                    _ = try genInfo.appendChunk(instr);
                    return reg;
                },
                .Number => |num| {
                    const reg = try genInfo.getAvailableReg();
                    genInfo.reserveRegister(reg);

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

                    _ = try genInfo.appendChunk(instr);
                    return reg;
                },
                .ArraySlice => |items| {
                    for (items) |item| {
                        try genInfo.pushScope();

                        const reg = try genBytecode(allocator, genInfo, item);
                        _ = reg;

                        try genInfo.popScopeAndRelease();
                    }
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            var leftReg: u32 = undefined;

            const leftExprDeeper = blitzAst.getExprDepth(expr.left) > blitzAst.getExprDepth(expr.right);
            if (leftExprDeeper) {
                leftReg = try genBytecode(allocator, genInfo, expr.left) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
            }
            const rightReg = try genBytecode(allocator, genInfo, expr.right) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            if (!leftExprDeeper) {
                leftReg = try genBytecode(allocator, genInfo, expr.left) orelse
                    return CodeGenError.ReturnedRegisterNotFound;
            }

            var outReg: ?u32 = null;

            const buf: Instr = switch (expr.type) {
                .Add, .Sub, .Mult => a: {
                    outReg = try genInfo.availableRegReplaceRelease(leftReg, rightReg);
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
                    var instr = if (genInfo.settings.outputCmpAsRegister)
                        exprTypeToCmpSetReg(expr.type)
                    else
                        Instr{ .Cmp = .{} };

                    if (genInfo.settings.outputCmpAsRegister) {
                        outReg = try genInfo.availableRegReplaceRelease(leftReg, rightReg);
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
                        genInfo.releaseIfPossible(leftReg);
                        genInfo.releaseIfPossible(rightReg);
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

            _ = try genInfo.appendChunk(buf);

            return outReg;
        },
        .Variable => |name| {
            const storedReg = genInfo.getVariableRegister(name);
            return storedReg;
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, genInfo, statement.condition) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            var buf = Instr{ .CmpConstByte = .{} };
            buf.CmpConstByte.reg = condReg;
            buf.CmpConstByte.data = 1;
            _ = try genInfo.appendChunk(buf);

            genInfo.releaseIfPossible(condReg);

            var jumpBuf = Instr{ .JumpNE = .{} };
            _ = try genInfo.appendChunk(jumpBuf);

            const byteCount = genInfo.byteCounter;

            _ = try genBytecode(allocator, genInfo, statement.body);

            if (statement.fallback) |fallback| {
                const jumpEndInstr = Instr{ .Jump = .{} };
                const jumpEndChunk = try genInfo.appendChunk(jumpEndInstr);

                const preFallbackByteCount = genInfo.byteCounter;

                const diff = @as(u16, @intCast(genInfo.byteCounter - byteCount));
                jumpBuf.JumpNE.amount = diff;

                try generateFallback(allocator, genInfo, fallback);
                const jumpEndDiff = @as(u16, @intCast(genInfo.byteCounter - preFallbackByteCount));
                jumpEndChunk.data.Jump.amount = jumpEndDiff;
            } else {
                const diff = @as(u16, @intCast(genInfo.byteCounter - byteCount));
                jumpBuf.JumpNE.amount = diff;
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |initNode| {
                _ = try genBytecode(allocator, genInfo, initNode);
            }

            const condInfo = prepForLoopCondition(genInfo, loop.condition);

            try genInfo.pushLoopInfo();
            defer genInfo.popLoopInfo();

            const preConditionByteCount = genInfo.byteCounter;
            const condReg = try genBytecode(allocator, genInfo, loop.condition);
            genInfo.settings.outputCmpAsRegister = condInfo.prevCmpAsReg;

            const preBodyByteCount = genInfo.byteCounter;
            var jumpEndInstr = Instr{ .JumpNE = .{} };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConstByte = .{} };
                cmpInstr.CmpConstByte.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConstByte.data = 1;
                _ = try genInfo.appendChunk(cmpInstr);
            }

            const jumpEndChunk = try genInfo.appendChunk(jumpEndInstr);

            try genInfo.pushScope();
            _ = try genBytecode(allocator, genInfo, loop.body);
            const oldContents = try genInfo.popScope();
            for (oldContents) |oldReg| {
                genInfo.releaseRegister(oldReg);
            }
            allocator.free(oldContents);
            genInfo.setContinueByte();
            _ = try genBytecode(allocator, genInfo, loop.incNode);

            const jumpEndDiff = @as(u16, @intCast(genInfo.byteCounter - preBodyByteCount));
            setJumpAmount(jumpEndChunk, jumpEndDiff);

            var jumpStartInstr = Instr{ .JumpBack = .{} };
            const jumpStartDiff = @as(u16, @intCast(genInfo.byteCounter - preConditionByteCount));
            jumpStartInstr.JumpBack.amount = jumpStartDiff;
            _ = try genInfo.appendChunk(jumpStartInstr);
        },
        .WhileLoop => |loop| {
            try genInfo.pushLoopInfo();
            defer genInfo.popLoopInfo();

            const condInfo = prepForLoopCondition(genInfo, loop.condition);

            const preConditionByteCount = genInfo.byteCounter;
            const condReg = try genBytecode(allocator, genInfo, loop.condition);

            const preBodyByteCount = genInfo.byteCounter;
            var jumpEndInstr = Instr{ .JumpNE = .{} };

            if (condInfo.isCompExpr) {
                const oppositeComp = loop.condition.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, false);
                jumpEndInstr = jumpInstruction;
            } else {
                var cmpInstr = Instr{ .CmpConstByte = .{} };
                cmpInstr.CmpConstByte.reg = condReg orelse
                    return CodeGenError.ReturnedRegisterNotFound;
                cmpInstr.CmpConstByte.data = 1;
                _ = try genInfo.appendChunk(cmpInstr);
            }

            const jumpEndChunk = try genInfo.appendChunk(jumpEndInstr);

            try genInfo.pushScope();
            _ = try genBytecode(allocator, genInfo, loop.body);
            const oldContents = try genInfo.popScope();
            for (oldContents) |oldReg| {
                genInfo.releaseRegister(oldReg);
            }
            allocator.free(oldContents);
            genInfo.setContinueByte();

            const jumpEndDiff = @as(u16, @intCast(genInfo.byteCounter - preBodyByteCount));
            setJumpAmount(jumpEndChunk, jumpEndDiff);

            var jumpStartInstr = Instr{ .JumpBack = .{} };
            const jumpStartDiff = @as(u16, @intCast(genInfo.byteCounter - preConditionByteCount));
            jumpStartInstr.JumpBack.amount = jumpStartDiff;
            _ = try genInfo.appendChunk(jumpStartInstr);
        },
        .IncOne => |inc| {
            const reg = try genBytecode(allocator, genInfo, inc) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .IncConstByte = .{} };
            instr.IncConstByte.reg = reg;
            instr.IncConstByte.data = 1;

            _ = try genInfo.appendChunk(instr);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            var instr = Instr{ .DecConstByte = .{} };
            instr.DecConstByte.reg = reg;
            instr.DecConstByte.data = 1;

            _ = try genInfo.appendChunk(instr);
        },
        .Group => |group| {
            return genBytecode(allocator, genInfo, group);
        },
        .ValueSet => |set| {
            const srcReg = try genBytecode(allocator, genInfo, set.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const destReg = try genBytecode(allocator, genInfo, set.value) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            var instr = Instr{ .Mov = .{} };
            instr.Mov.dest = destReg;
            instr.Mov.src = srcReg;
            _ = try genInfo.appendChunk(instr);
        },
        .Bang => |expr| {
            const reg = try genBytecode(allocator, genInfo, expr) orelse
                return CodeGenError.NoAvailableRegisters;
            const setReg = try genInfo.availableRegReplace(reg);

            var instr = Instr{ .XorConstByte = .{} };
            instr.XorConstByte.dest = setReg;
            instr.XorConstByte.reg = reg;
            instr.XorConstByte.byte = 1;
            _ = try genInfo.appendChunk(instr);

            return setReg;
        },
        .Scope => |scope| {
            try genInfo.pushScope();
            _ = try genBytecode(allocator, genInfo, scope);
            try genInfo.releaseScope();
        },
        .Break => {
            const loopInfo = genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const buf = Instr{ .Jump = .{} };
            const chunk = try genInfo.appendChunk(buf);
            const byteCount = genInfo.byteCounter;
            try loopInfo.appendBreak(chunk, byteCount);
        },
        .Continue => {
            const loopInfo = genInfo.currentLoopInfo() orelse
                return CodeGenError.ExpectedLoopInfo;

            const instr = Instr{ .Jump = .{} };
            const chunk = try genInfo.appendChunk(instr);
            const byteCount = genInfo.byteCounter;
            try loopInfo.appendContinue(chunk, byteCount);
        },
        else => {},
    }

    return null;
}

fn prepForLoopCondition(genInfo: *GenInfo, condition: *blitzAst.AstNode) LoopCondInfo {
    const prevCmpAsReg = genInfo.settings.outputCmpAsRegister;
    const isCompExprType = if (condition.* == .OpExpr)
        switch (condition.OpExpr.type) {
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
        genInfo.settings.outputCmpAsRegister = false;
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
    genInfo: *GenInfo,
    fallback: *const blitzAst.IfFallback,
) !void {
    var jumpChunk: ?*InstrChunk = null;

    if (fallback.condition) |condition| {
        const condReg = try genBytecode(allocator, genInfo, condition) orelse
            return CodeGenError.ReturnedRegisterNotFound;

        var instr = Instr{ .CmpConstByte = .{} };
        instr.CmpConstByte.reg = condReg;
        instr.CmpConstByte.data = 1;
        _ = try genInfo.appendChunk(instr);

        genInfo.releaseIfPossible(condReg);

        const jumpInstr = Instr{ .JumpNE = .{} };
        jumpChunk = try genInfo.appendChunk(jumpInstr);
    }

    const preBodyByteCount = genInfo.byteCounter;

    _ = try genBytecode(allocator, genInfo, fallback.body);

    if (fallback.fallback) |newFallback| {
        const jumpEndInstr = Instr{ .Jump = .{} };
        const jumpEndChunk = try genInfo.appendChunk(jumpEndInstr);

        const preFallbackByteCount = genInfo.byteCounter;

        if (jumpChunk) |chunk| {
            const diff = @as(u16, @intCast(genInfo.byteCounter - preBodyByteCount));
            chunk.data.JumpNE.amount = diff;
        }

        try generateFallback(allocator, genInfo, newFallback);
        const diff = @as(u16, @intCast(genInfo.byteCounter - preFallbackByteCount));
        jumpEndChunk.data.Jump.amount = diff;
    }
}

fn writeChunk(chunk: *InstrChunk, writer: anytype) !void {
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
            try writeNumber(u32, inst.data, writer);
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
            var buf: [2]u8 = undefined;
            std.mem.writeInt(u16, &buf, inst.amount, .little);
            try writer.writeAll(&buf);
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
        .XorConstByte => |inst| {
            try writer.writeByte(@intCast(inst.dest));
            try writer.writeByte(@intCast(inst.reg));
            try writer.writeByte(inst.byte);
        },
        .AddSp, .SubSp => |inst| {
            try writeNumber(u32, inst.amount, writer);
        },
        .AddSpReg, .SubSpReg => |inst| {
            try writeNumber(u32, inst.amount, writer);
        },
        .StoreOffsetByte => |inst| {
            try writer.writeByte(@intCast(inst.fromReg));
            try writer.writeByte(@intCast(inst.toReg));
            try writer.writeByte(inst.offset);
        },
    }
}

fn writeNumber(comptime T: type, data: T, writer: anytype) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, data, .little);
    try writer.writeAll(&buf);
}
