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
const RegisterNumber = vmInfo.RegisterNumber;
const PointerType = vmInfo.PointerType;

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
    NoJumpInstructionMatchingComp,
};

const GenBytecodeError = CodeGenError || Allocator.Error || std.fmt.ParseIntError;

pub const Instructions = enum(u8) {
    const Self = @This();

    SetReg, // inst, reg, 8B data
    SetRegHalf, // inst, reg, 4B data
    SetRegByte, // inst, reg, 1B data
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
    Mov,

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }

    pub fn allocBuf(self: Self, allocator: Allocator) ![]u8 {
        const buf = try allocator.alloc(u8, self.getInstrLen());
        buf[0] = self.getInstrByte();
        return buf;
    }

    pub fn getInstrLen(self: Self) u8 {
        return switch (self) {
            .SetReg => 10,
            .SetRegHalf => 6,
            .SetRegByte => 3,
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
        };
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .SetReg => "set_reg",
            .SetRegHalf => "set_reg_half",
            .SetRegByte => "set_reg_byte",
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
        };
    }
};

pub const InstrChunk = struct {
    const Self = @This();

    next: ?*InstrChunk,
    prev: ?*InstrChunk,
    chunk: []u8,

    pub fn init(chunk: []u8) Self {
        return .{
            .next = null,
            .prev = null,
            .chunk = chunk,
        };
    }
};

const VarRegLocInfoVariants = enum {
    Register,
    StackLocation,
};

const VariableRegLocationInfo = union(VarRegLocInfoVariants) {
    Register: RegisterNumber,
    StackLocation: PointerType,
};

const RegScope = struct {
    const Self = @This();

    allocator: Allocator,
    registers: *ArrayList(RegisterNumber),

    pub fn init(allocator: Allocator) !Self {
        const registers = try utils.initMutPtrT(ArrayList(RegisterNumber), allocator);

        return .{
            .allocator = allocator,
            .registers = registers,
        };
    }

    pub fn deinit(self: *Self) void {
        self.registers.deinit();
        self.allocator.destroy(self.registers);
    }

    pub fn empty(self: *Self) ![]RegisterNumber {
        return self.registers.toOwnedSlice();
    }

    pub fn addRegister(self: *Self, reg: RegisterNumber) !void {
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

    pub fn popScope(self: *Self) ![]RegisterNumber {
        if (self.scopes.items.len == 1) {
            const oldContents = try self.scopes.items[0].empty();
            return oldContents;
        }

        const scope = self.scopes.pop().?;
        const oldContents = try scope.registers.toOwnedSlice();
        self.deinitScope(scope);
        return oldContents;
    }

    pub fn getCurrentScopeContents(self: *Self) []RegisterNumber {
        return self.scopes.getLast().registers.items;
    }

    pub fn getCurrentScope(self: *Self) *RegScope {
        return self.scopes.getLast();
    }

    pub fn addRegister(self: *Self, reg: RegisterNumber) !void {
        const scope = self.getCurrentScope();
        try scope.addRegister(reg);
    }
};

const GenInfoSettings = struct {
    // is preserved until codegen for expr node
    // setting only respected for for one expr node, then set to default
    outputCmpAsRegister: bool,
};

const CODEGEN_DEFAULT_SETTINGS = GenInfoSettings{
    .outputCmpAsRegister = true,
};

pub const GenInfo = struct {
    const Self = @This();

    allocator: Allocator,
    stackStartSize: u32,
    instructionList: ?*InstrChunk,
    last: ?*InstrChunk,
    availableRegisters: [vmInfo.NUM_REGISTERS]bool = [_]bool{false} ** vmInfo.NUM_REGISTERS,
    variableRegisters: [vmInfo.NUM_REGISTERS]bool = [_]bool{false} ** vmInfo.NUM_REGISTERS,
    regScopes: *RegScopes,
    varNameRegRel: *StringHashMap(VariableRegLocationInfo),
    byteCounter: usize,
    settings: GenInfoSettings,

    pub fn init(
        allocator: Allocator,
    ) !Self {
        const varNameRegRel = try utils.initMutPtrT(
            StringHashMap(VariableRegLocationInfo),
            allocator,
        );
        const regScopes = try RegScopes.init(allocator);
        const regScopesPtr = try utils.createMut(RegScopes, allocator, regScopes);

        return .{
            .allocator = allocator,
            .stackStartSize = 0,
            .instructionList = null,
            .last = null,
            .varNameRegRel = varNameRegRel,
            .regScopes = regScopesPtr,
            .byteCounter = 0,
            .settings = CODEGEN_DEFAULT_SETTINGS,
        };
    }

    pub fn deinit(self: Self) void {
        var current = self.instructionList;
        while (current != null) : (current = current.?.next) {
            self.allocator.free(current.?.chunk);
            self.allocator.destroy(current.?);
        }

        self.varNameRegRel.deinit();
        self.allocator.destroy(self.varNameRegRel);

        self.regScopes.deinit();
        self.allocator.destroy(self.regScopes);
    }

    pub fn writeChunks(self: Self, writer: anytype) !void {
        var current = self.instructionList;
        while (current) |instr| {
            try writer.writeAll(instr.chunk);
            current = instr.next;
        }
    }

    pub fn appendChunk(self: *Self, chunk: []u8) !void {
        self.byteCounter += chunk.len;
        const newChunk = try utils.createMut(InstrChunk, self.allocator, InstrChunk.init(chunk));

        if (self.last) |last| {
            last.next = newChunk;
            newChunk.prev = last;
            self.last = newChunk;
        } else {
            self.instructionList = newChunk;
            self.last = newChunk;
        }
    }

    pub fn getAvailableReg(self: Self) ?RegisterNumber {
        for (self.availableRegisters, 0..) |reg, index| {
            if (!reg) {
                return @intCast(index);
            }
        }

        return null;
    }

    pub fn availableRegReplaceRelease(
        self: *Self,
        reg1: RegisterNumber,
        reg2: RegisterNumber,
    ) !RegisterNumber {
        if (!self.variableRegisters[reg1]) {
            self.releaseIfPossible(reg2);
            return reg1;
        } else if (!self.variableRegisters[reg2]) {
            self.releaseIfPossible(reg1);
            return reg2;
        }

        const reg = self.getAvailableReg() orelse return CodeGenError.NoAvailableRegisters;
        self.reserveRegister(reg);
        return reg;
    }

    pub fn releaseIfPossible(self: *Self, reg: RegisterNumber) void {
        if (self.variableRegisters[reg]) return;
        self.releaseRegister(reg);
    }

    pub fn reserveRegister(self: *Self, reg: RegisterNumber) void {
        self.availableRegisters[reg] = true;
    }

    pub fn releaseRegister(self: *Self, reg: RegisterNumber) void {
        self.availableRegisters[reg] = false;
        self.variableRegisters[reg] = false;
    }

    pub fn getVariableRegister(self: Self, name: []u8) RegisterNumber {
        // TODO - address this
        return self.varNameRegRel.get(name).?.Register;
    }

    pub fn setVariableRegister(self: *Self, name: []u8, reg: RegisterNumber) !void {
        try self.varNameRegRel.put(name, .{ .Register = reg });
        self.variableRegisters[reg] = true;
        try self.regScopes.addRegister(reg);
    }

    pub fn pushScope(self: *Self) !void {
        try self.regScopes.pushScope();
    }

    pub fn popScope(self: *Self) ![]RegisterNumber {
        return self.regScopes.popScope();
    }
};

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    try writeStartVMInfo(allocator, genInfo);
    _ = try genBytecode(allocator, genInfo, ast.root);
}

fn writeStartVMInfo(allocator: Allocator, genInfo: *GenInfo) !void {
    var buf = try allocator.alloc(u8, vmInfo.VM_INFO_BYTECODE_LEN);
    buf[0] = version.VERSION;
    std.mem.writeInt(u32, @ptrCast(buf[1..]), genInfo.stackStartSize, .little);
    try genInfo.appendChunk(buf);
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
) GenBytecodeError!?RegisterNumber {
    switch (node.*) {
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                _ = try genBytecode(allocator, genInfo, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec.setNode) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            if (genInfo.variableRegisters[reg]) {
                const newReg = genInfo.getAvailableReg() orelse
                    return CodeGenError.NoAvailableRegisters;
                genInfo.reserveRegister(reg);

                const buf = try Instructions.Mov.allocBuf(allocator);
                buf[1] = newReg;
                buf[2] = reg;
                try genInfo.appendChunk(buf);
            }
            try genInfo.setVariableRegister(dec.name, reg);
        },
        .Value => |value| {
            switch (value) {
                .RawNumber => |num| {
                    const reg = genInfo.getAvailableReg() orelse
                        return CodeGenError.NoAvailableRegisters;
                    genInfo.reserveRegister(reg);

                    const buf = try Instructions.SetRegHalf.allocBuf(allocator);
                    buf[1] = reg;
                    try writeIntSliceToInstr(u32, buf, 2, num.digits);

                    try genInfo.appendChunk(buf);
                    return reg;
                },
                .Bool => |b| {
                    const reg = genInfo.getAvailableReg() orelse
                        return CodeGenError.NoAvailableRegisters;
                    genInfo.reserveRegister(reg);

                    const buf = try Instructions.SetRegByte.allocBuf(allocator);
                    buf[1] = reg;
                    buf[2] = @as(u8, @intFromBool(b));

                    try genInfo.appendChunk(buf);
                    return reg;
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            var leftReg: RegisterNumber = undefined;

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

            var outReg: ?RegisterNumber = null;

            const instr: Instructions = switch (expr.type) {
                .Add => Instructions.Add,
                .Sub => Instructions.Sub,
                .Mult => Instructions.Mult,
                .LessThan,
                .GreaterThan,
                .GreaterThanEq,
                .LessThanEq,
                .Equal,
                .NotEqual,
                => a: {
                    if (genInfo.settings.outputCmpAsRegister) {
                        break :a exprTypeToCmpSetReg(expr.type);
                    } else {
                        break :a Instructions.Cmp;
                    }
                },
                else => unreachable,
            };

            const buf = switch (expr.type) {
                .Add, .Sub, .Mult => a: {
                    outReg = try genInfo.availableRegReplaceRelease(leftReg, rightReg);
                    const buf = try instr.allocBuf(allocator);
                    buf[1] = outReg.?;
                    buf[2] = leftReg;
                    buf[3] = rightReg;
                    break :a buf;
                },
                .LessThan,
                .GreaterThan,
                .GreaterThanEq,
                .LessThanEq,
                .Equal,
                => a: {
                    const bufSize: usize = if (genInfo.settings.outputCmpAsRegister) 4 else 3;
                    const offset: u8 = if (genInfo.settings.outputCmpAsRegister) 1 else 0;

                    const buf = try allocator.alloc(u8, bufSize);
                    buf[0] = instr.getInstrByte();
                    buf[1 + offset] = leftReg;
                    buf[2 + offset] = rightReg;

                    if (genInfo.settings.outputCmpAsRegister) {
                        outReg = try genInfo.availableRegReplaceRelease(leftReg, rightReg);
                        buf[1] = outReg.?;
                    } else {
                        genInfo.releaseIfPossible(leftReg);
                        genInfo.releaseIfPossible(rightReg);
                    }

                    break :a buf;
                },
                else => unreachable,
            };

            try genInfo.appendChunk(buf);

            return outReg;
        },
        .Variable => |name| {
            const storedReg = genInfo.getVariableRegister(name);
            return storedReg;
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, genInfo, statement.condition) orelse
                return CodeGenError.ReturnedRegisterNotFound;

            const buf = try Instructions.CmpConstByte.allocBuf(allocator);
            buf[1] = condReg;
            buf[2] = 1;
            try genInfo.appendChunk(buf);

            genInfo.releaseIfPossible(condReg);

            const jumpBuf = try Instructions.JumpNE.allocBuf(allocator);
            try genInfo.appendChunk(jumpBuf);

            const byteCount = genInfo.byteCounter;

            _ = try genBytecode(allocator, genInfo, statement.body);

            if (statement.fallback) |fallback| {
                const jumpEndBuf = try Instructions.Jump.allocBuf(allocator);
                try genInfo.appendChunk(jumpEndBuf);

                const preFallbackByteCount = genInfo.byteCounter;

                const diff = @as(u16, @intCast(genInfo.byteCounter - byteCount));
                std.mem.writeInt(u16, @ptrCast(jumpBuf[1..]), diff, .little);

                try generateFallback(allocator, genInfo, fallback);
                const jumpEndDiff = @as(u16, @intCast(genInfo.byteCounter - preFallbackByteCount));
                std.mem.writeInt(u16, @ptrCast(jumpEndBuf[1..]), jumpEndDiff, .little);
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |initNode| {
                _ = try genBytecode(allocator, genInfo, initNode);
            }

            const prevCmpAsReg = genInfo.settings.outputCmpAsRegister;
            const isCompExprType = if (loop.condition.* == .OpExpr)
                switch (loop.condition.OpExpr.type) {
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

            const preConditionByteCount = genInfo.byteCounter;
            const condReg = try genBytecode(allocator, genInfo, loop.condition);
            genInfo.settings.outputCmpAsRegister = prevCmpAsReg;

            // TODO
            _ = condReg;

            const preBodyByteCount = genInfo.byteCounter;
            const jumpEndBuf = try Instructions.JumpGTE.allocBuf(allocator);

            if (isCompExprType) {
                const oppositeComp = loop.condition.OpExpr.type.getOppositeCompOp();
                const jumpInstruction = try compOpToJump(oppositeComp, false);
                jumpEndBuf[0] = jumpInstruction.getInstrByte();
            }

            try genInfo.appendChunk(jumpEndBuf);

            try genInfo.pushScope();
            _ = try genBytecode(allocator, genInfo, loop.body);
            const oldContents = try genInfo.popScope();
            for (oldContents) |oldReg| {
                genInfo.releaseRegister(oldReg);
            }
            allocator.free(oldContents);
            _ = try genBytecode(allocator, genInfo, loop.incNode);

            const jumpEndDiff = @as(u16, @intCast(genInfo.byteCounter - preBodyByteCount));
            std.mem.writeInt(u16, @ptrCast(jumpEndBuf[1..]), jumpEndDiff, .little);

            const jumpStartBuf = try Instructions.JumpBack.allocBuf(allocator);
            const jumpStartDiff = @as(u16, @intCast(genInfo.byteCounter - preConditionByteCount));
            std.mem.writeInt(u16, @ptrCast(jumpStartBuf[1..]), jumpStartDiff, .little);
            try genInfo.appendChunk(jumpStartBuf);
        },
        .IncOne => |inc| {
            const reg = try genBytecode(allocator, genInfo, inc) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const buf = try Instructions.IncConstByte.allocBuf(allocator);
            buf[1] = reg;
            buf[2] = 1;

            try genInfo.appendChunk(buf);
        },
        .DecOne => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec) orelse
                return CodeGenError.ReturnedRegisterNotFound;
            const buf = try Instructions.DecConstByte.allocBuf(allocator);
            buf[1] = reg;
            buf[2] = 1;

            try genInfo.appendChunk(buf);
        },
        .Group => |group| {
            return genBytecode(allocator, genInfo, group);
        },
        else => {},
    }

    return null;
}

fn exprTypeToCmpSetReg(expr: blitzAst.OpExprTypes) Instructions {
    return switch (expr) {
        .Equal => .CmpSetRegEQ,
        .NotEqual => .CmpSetRegNE,
        .LessThan => .CmpSetRegLT,
        .GreaterThan => .CmpSetRegGT,
        .LessThanEq => .CmpSetRegLTE,
        .GreaterThanEq => .CmpSetRegGTE,
        else => unreachable,
    };
}

fn compOpToJump(opType: blitzAst.OpExprTypes, back: bool) !Instructions {
    return if (back) switch (opType) {
        .Equal => .JumpBackEQ,
        .NotEqual => .JumpBackNE,
        .GreaterThan => .JumpBackGT,
        .LessThan => .JumpBackLT,
        .GreaterThanEq => .JumpBackGTE,
        .LessThanEq => .JumpBackLTE,
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    } else switch (opType) {
        .Equal => .JumpEQ,
        .NotEqual => .JumpNE,
        .GreaterThan => .JumpGT,
        .LessThan => .JumpLT,
        .GreaterThanEq => .JumpGTE,
        .LessThanEq => .JumpLTE,
        else => return CodeGenError.NoJumpInstructionMatchingComp,
    };
}

fn generateFallback(
    allocator: Allocator,
    genInfo: *GenInfo,
    fallback: *const blitzAst.IfFallback,
) !void {
    var jumpSlice: ?[]u8 = null;

    if (fallback.condition) |condition| {
        const condReg = try genBytecode(allocator, genInfo, condition) orelse
            return CodeGenError.ReturnedRegisterNotFound;

        const buf = try Instructions.CmpConstByte.allocBuf(allocator);
        buf[1] = condReg;
        buf[2] = 1;
        try genInfo.appendChunk(buf);

        genInfo.releaseIfPossible(condReg);

        const jumpBuf = try Instructions.JumpNE.allocBuf(allocator);
        try genInfo.appendChunk(jumpBuf);
        jumpSlice = jumpBuf[1..];
    }

    const preBodyByteCount = genInfo.byteCounter;

    _ = try genBytecode(allocator, genInfo, fallback.body);

    if (fallback.fallback) |newFallback| {
        const jumpEndBuf = try Instructions.Jump.allocBuf(allocator);
        try genInfo.appendChunk(jumpEndBuf);

        const preFallbackByteCount = genInfo.byteCounter;

        if (jumpSlice) |slice| {
            const diff = @as(u16, @intCast(genInfo.byteCounter - preBodyByteCount));
            std.mem.writeInt(u16, @ptrCast(slice), diff, .little);
        }

        try generateFallback(allocator, genInfo, newFallback);
        const diff = @as(u16, @intCast(genInfo.byteCounter - preFallbackByteCount));
        std.mem.writeInt(u16, @ptrCast(jumpEndBuf[1..]), diff, .little);
    }
}
