const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const version = blitz.version;
const settings = blitz.settings;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;

pub const PointerType = u64;
pub const RegisterNumber = u8;
pub const NUM_REGISTERS = 256;
pub const REGISTER_SIZE = 8; // bytes

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
    ReturnedRegisterNotFound,
};

const GenBytecodeError = CodeGenError || Allocator.Error || std.fmt.ParseIntError;

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

pub const GenInfo = struct {
    const Self = @This();

    allocator: Allocator,
    stackStartSize: u32,
    instructionList: ?*InstrChunk,
    last: ?*InstrChunk,
    availableRegisters: [NUM_REGISTERS]bool = [_]bool{false} ** NUM_REGISTERS,
    varNameRegRel: *StringHashMap(VariableRegLocationInfo),
    varRegisters: *AutoHashMap(RegisterNumber, void),
    byteCounter: u64,

    pub fn init(
        allocator: Allocator,
    ) !Self {
        const varNameRegRel = try utils.initMutPtrT(StringHashMap(VariableRegLocationInfo), allocator);
        const varRegisters = try utils.initMutPtrT(AutoHashMap(RegisterNumber, void), allocator);

        return .{
            .allocator = allocator,
            .stackStartSize = 0,
            .instructionList = null,
            .last = null,
            .varNameRegRel = varNameRegRel,
            .varRegisters = varRegisters,
            .byteCounter = 0,
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

        self.varRegisters.deinit();
        self.allocator.destroy(self.varRegisters);
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

    pub fn reserveRegister(self: *Self, reg: RegisterNumber) void {
        self.availableRegisters[reg] = true;
    }

    pub fn releaseRegister(self: *Self, reg: RegisterNumber) void {
        self.availableRegisters[reg] = false;
    }

    pub fn getVariableRegister(self: Self, name: []u8) RegisterNumber {
        // TODO - address this
        return self.varNameRegRel.get(name).?.Register;
    }

    pub fn setVariableRegister(self: *Self, name: []u8, reg: RegisterNumber) !void {
        try self.varNameRegRel.put(name, .{ .Register = reg });
        try self.varRegisters.put(reg, {});
    }
};

pub const Instructions = enum(u8) {
    const Self = @This();

    Store,
    SetReg, // inst, reg, 8B data
    SetRegHalf, // inst, reg, 4B data
    SetRegByte, // inst, reg, 4B data
    Add, // inst, out reg, reg1, reg2
    Sub, // inst, out reg, reg1, reg2
    Mult, // inst, out reg, reg1, reg2
    CmpConstByte, // inst, reg1, reg2
    BranchNotEqual, // inst, label (u64)

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }
};

const RegisterContext = struct {
    const Self = @This();

    reg: RegisterNumber,
    preserve: bool,

    pub fn init(reg: RegisterNumber, preserve: bool) Self {
        return .{
            .reg = reg,
            .preserve = preserve,
        };
    }
};

const regContext = RegisterContext.init;

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    try writeStartVMInfo(allocator, genInfo);
    _ = try genBytecode(allocator, genInfo, ast.root);
}

fn writeStartVMInfo(allocator: Allocator, genInfo: *GenInfo) !void {
    const startStackTypeSize = @sizeOf(@TypeOf(genInfo.stackStartSize));
    var buf = try allocator.alloc(u8, startStackTypeSize + 1);

    buf[0] = version.VERSION;
    std.mem.writeInt(@TypeOf(genInfo.stackStartSize), @ptrCast(buf[1..]), genInfo.stackStartSize, .big);

    try genInfo.appendChunk(buf);
}

fn writeIntSliceToInstr(
    comptime T: type,
    buf: []u8,
    offset: usize,
    num: []u8,
) !void {
    const size = @sizeOf(T);
    std.mem.writeInt(T, @ptrCast(buf[offset .. size + offset]), try std.fmt.parseInt(T, num, 10), .big);
}

pub fn genBytecode(
    allocator: Allocator,
    genInfo: *GenInfo,
    node: *const blitzAst.AstNode,
) GenBytecodeError!?RegisterContext {
    switch (node.*) {
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                _ = try genBytecode(allocator, genInfo, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec.setNode) orelse return CodeGenError.ReturnedRegisterNotFound;
            try genInfo.setVariableRegister(dec.name, reg.reg);
        },
        .Value => |value| {
            switch (value) {
                .RawNumber => |num| {
                    const reg = genInfo.getAvailableReg() orelse return CodeGenError.NoAvailableRegisters;
                    genInfo.reserveRegister(reg);

                    const inst = Instructions.SetRegHalf.getInstrByte();
                    const buf = try allocator.alloc(u8, 6);
                    buf[0] = inst;
                    buf[1] = reg;
                    try writeIntSliceToInstr(u32, buf, 2, num);

                    try genInfo.appendChunk(buf);
                    return regContext(reg, false);
                },
                .Bool => |b| {
                    const reg = genInfo.getAvailableReg() orelse return CodeGenError.NoAvailableRegisters;
                    genInfo.reserveRegister(reg);

                    const inst = Instructions.SetRegByte.getInstrByte();
                    const buf = try allocator.alloc(u8, 3);
                    buf[0] = inst;
                    buf[1] = reg;
                    buf[2] = @as(u8, @intFromBool(b));

                    try genInfo.appendChunk(buf);
                    return regContext(reg, false);
                },
                else => {},
            }
        },
        .OpExpr => |expr| {
            const leftReg = try genBytecode(allocator, genInfo, expr.left) orelse return CodeGenError.ReturnedRegisterNotFound;
            const rightReg = try genBytecode(allocator, genInfo, expr.right) orelse return CodeGenError.ReturnedRegisterNotFound;

            var reg: RegisterNumber = undefined;

            if (!leftReg.preserve) {
                reg = leftReg.reg;
            } else if (!rightReg.preserve) {
                reg = rightReg.reg;
            } else {
                reg = genInfo.getAvailableReg() orelse return CodeGenError.NoAvailableRegisters;
                genInfo.reserveRegister(reg);
            }

            const buf = try allocator.alloc(u8, 4);
            buf[1] = reg;
            buf[2] = leftReg.reg;
            buf[3] = rightReg.reg;

            buf[0] = switch (expr.type) {
                .Add => Instructions.Add,
                .Sub => Instructions.Sub,
                .Mult => Instructions.Mult,
                else => unreachable,
            }.getInstrByte();

            try genInfo.appendChunk(buf);

            if (!leftReg.preserve and reg != leftReg.reg) {
                genInfo.releaseRegister(leftReg.reg);
            }

            if (!rightReg.preserve and reg != rightReg.reg) {
                genInfo.releaseRegister(rightReg.reg);
            }

            return regContext(reg, false);
        },
        .Variable => |name| {
            const storedReg = genInfo.getVariableRegister(name);
            return regContext(storedReg, true);
        },
        .IfStatement => |statement| {
            const condReg = try genBytecode(allocator, genInfo, statement.condition) orelse return CodeGenError.ReturnedRegisterNotFound;

            const buf = try allocator.alloc(u8, 3);
            buf[0] = Instructions.CmpConstByte.getInstrByte();
            buf[1] = condReg.reg;
            buf[2] = 1;
            try genInfo.appendChunk(buf);

            const branchBuf = try allocator.alloc(u8, 3);
            branchBuf[0] = Instructions.BranchNotEqual.getInstrByte();
            try genInfo.appendChunk(branchBuf);

            const byteCount = genInfo.byteCounter;

            _ = try genBytecode(allocator, genInfo, statement.body);

            const diff = @as(u16, @intCast(genInfo.byteCounter - byteCount));
            std.mem.writeInt(u16, @ptrCast(branchBuf[1..]), diff, .big);

            if (statement.fallback) |fallback| {
                try generateFallback(allocator, genInfo, fallback);
            }
        },
        else => {},
    }

    return null;
}

fn generateFallback(allocator: Allocator, genInfo: *GenInfo, fallback: *const blitzAst.IfFallback) !void {
    var jumpSlice: ?[]u8 = null;

    if (fallback.condition) |condition| {
        const condReg = try genBytecode(allocator, genInfo, condition) orelse return CodeGenError.ReturnedRegisterNotFound;

        const buf = try allocator.alloc(u8, 3);
        buf[0] = Instructions.CmpConstByte.getInstrByte();
        buf[1] = condReg.reg;
        buf[2] = 1;
        try genInfo.appendChunk(buf);

        const branchBuf = try allocator.alloc(u8, 3);
        branchBuf[0] = Instructions.BranchNotEqual.getInstrByte();
        try genInfo.appendChunk(branchBuf);
        jumpSlice = branchBuf[1..];
    }

    const byteCount = genInfo.byteCounter;

    _ = try genBytecode(allocator, genInfo, fallback.body);

    if (jumpSlice) |slice| {
        const diff = @as(u16, @intCast(genInfo.byteCounter - byteCount));
        std.mem.writeInt(u16, @ptrCast(slice), diff, .big);
    }

    if (fallback.fallback) |newFallback| {
        try generateFallback(allocator, genInfo, newFallback);
    }
}
