const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const codegen = blitz.codegen;
const version = blitz.version;
const Allocator = std.mem.Allocator;
const RegisterType = vmInfo.RegisterType;
const Writer = std.Io.Writer;

const InterpreterError = error{
    NoInputFile,
    IncompatibleInterpreterVersions,
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return InterpreterError.NoInputFile;
    }

    const filename = args[1];
    const bzcFilename = try allocator.alloc(u8, filename.len + 4);
    defer allocator.free(bzcFilename);
    @memcpy(bzcFilename[0..filename.len], filename);
    @memcpy(bzcFilename[filename.len .. filename.len + 4], ".bzc");

    const bytecode = try utils.readRelativeFile(allocator, bzcFilename);
    defer allocator.free(bytecode);
    const bytecodeVersion = bytecode[0];
    if (bytecodeVersion != version.VERSION) {
        return InterpreterError.IncompatibleInterpreterVersions;
    }

    const stackSize = std.mem.readInt(u32, @ptrCast(bytecode[1..5]), .little);
    var runtimeInfo = try RuntimeInfo.init(allocator, stackSize);
    defer runtimeInfo.deinit();

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    interpretBytecode(allocator, &runtimeInfo, bytecode) catch |e| {
        switch (e) {
            error.OutOfMemory => {
                try writer.writeAll("Out of memory, cannot resize stack\n");
            },
        }
    };

    try runtimeInfo.writeMemDebug(12, 32, writer);
}

const Flags = struct {
    EQ: bool = false,
    NE: bool = false,
    GT: bool = false,
    LT: bool = false,
    GTE: bool = false,
    LTE: bool = false,
};

const RuntimePtrs = struct {
    sp: u64,
};

const RuntimeInfo = struct {
    const Self = @This();

    allocator: Allocator,
    registers: [vmInfo.NUM_REGISTERS]RegisterType = [_]RegisterType{0} ** vmInfo.NUM_REGISTERS,
    flags: Flags,
    stack: *std.ArrayList(u8),
    ptrs: RuntimePtrs,

    pub fn init(allocator: Allocator, stackSize: u32) !Self {
        const tempStack = try std.ArrayList(u8).initCapacity(allocator, stackSize);
        const stack = try utils.createMut(std.ArrayList(u8), allocator, tempStack);
        stack.items.len = stackSize;

        return .{
            .allocator = allocator,
            .flags = Flags{},
            .stack = stack,
            .ptrs = .{
                .sp = 0,
            },
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit(self.allocator);
        self.allocator.destroy(self.stack);
    }

    pub fn writeMemDebug(self: Self, untilReg: usize, untilStack: usize, writer: *Writer) !void {
        try writer.writeAll("##REG_START##\n");
        var i: usize = 0;
        while (i < vmInfo.NUM_REGISTERS) : (i += 1) {
            if (i == untilReg) break;
            try writer.printInt(i, 10, .lower, .{});
            try writer.writeAll(") ");
            try writer.printInt(self.registers[i], 10, .lower, .{});
            try writer.writeAll("\n");
        }
        try writer.writeAll("##REG_END##\n");

        try writer.writeAll("##STACK_START##\n");
        i = 0;
        while (i < untilStack) : (i += 1) {
            if (i == untilStack) break;
            try writer.printInt(i, 10, .lower, .{});
            try writer.writeAll(") ");
            try writer.printInt(self.stack.items[i], 10, .lower, .{});
            try writer.writeAll("\n");
        }
        try writer.writeAll("##STACK_END##\n");
    }
};

fn interpretBytecode(allocator: Allocator, runtimeInfo: *RuntimeInfo, bytecode: []u8) !void {
    var current: usize = vmInfo.VM_INFO_BYTECODE_LEN;
    while (current < bytecode.len) {
        const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const instLen = inst.getInstrLen();
        switch (inst) {
            .Mov => {
                runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.registers[bytecode[current + 2]];
            },
            .SetReg64 => {
                const value = std.mem.readInt(u64, @ptrCast(bytecode[current + 2 .. current + 10]), .little);
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetReg32 => {
                const value = std.mem.readInt(u32, @ptrCast(bytecode[current + 2 .. current + 6]), .little);
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetReg16 => {
                const value = std.mem.readInt(u16, @ptrCast(bytecode[current + 2 .. current + 4]), .little);
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetReg8 => {
                runtimeInfo.registers[bytecode[current + 1]] = bytecode[current + 2];
            },
            .Add => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val + reg2Val;
            },
            .Add8 => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val + bytecode[current + 3];
            },
            .Sub => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val - reg2Val;
            },
            .Sub8 => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val - bytecode[current + 3];
            },
            .Mult => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val * reg2Val;
            },
            .Cmp => {
                const reg1Value = runtimeInfo.registers[bytecode[current + 1]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.flags = compareOrder(reg1Value, reg2Value);
            },
            .CmpConstByte => {
                const reg1Value = runtimeInfo.registers[bytecode[current + 1]];
                const reg2Value: u64 = @intCast(bytecode[current + 2]);
                runtimeInfo.flags = compareOrder(reg1Value, reg2Value);
            },
            .CmpSetRegEQ,
            .CmpSetRegNE,
            .CmpSetRegGT,
            .CmpSetRegLT,
            .CmpSetRegGTE,
            .CmpSetRegLTE,
            => {
                const reg1Value = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.flags = compareOrder(reg1Value, reg2Value);
                const flags = runtimeInfo.flags;
                runtimeInfo.registers[bytecode[current + 1]] = @intFromBool(switch (inst) {
                    .CmpSetRegEQ => flags.EQ,
                    .CmpSetRegNE => flags.NE,
                    .CmpSetRegGT => flags.GT,
                    .CmpSetRegLT => flags.LT,
                    .CmpSetRegGTE => flags.GTE,
                    .CmpSetRegLTE => flags.LTE,
                    else => unreachable,
                });
            },
            .Jump => {
                const amount = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 1 .. current + 3]),
                    .little,
                );
                current += amount;
            },
            .JumpBack => {
                const amount = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 1 .. current + 3]),
                    .little,
                );
                current -= amount;
                continue;
            },
            .JumpEQ,
            .JumpNE,
            .JumpGT,
            .JumpLT,
            .JumpGTE,
            .JumpLTE,
            => {
                const amount = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 1 .. current + 3]),
                    .little,
                );
                if (getFlagFromJump(inst, runtimeInfo.flags)) {
                    current += amount;
                }
            },
            .JumpBackEQ,
            .JumpBackNE,
            .JumpBackGT,
            .JumpBackLT,
            .JumpBackGTE,
            .JumpBackLTE,
            => {
                const amount = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 1 .. current + 3]),
                    .little,
                );
                if (getFlagFromJump(inst, runtimeInfo.flags)) {
                    current -= amount;
                    continue;
                }
            },
            .IncConstByte => {
                runtimeInfo.registers[bytecode[current + 1]] += bytecode[current + 2];
            },
            .DecConstByte => {
                runtimeInfo.registers[bytecode[current + 1]] -= bytecode[current + 2];
            },
            .Xor => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val ^ reg2Val;
            },
            .XorConstByte => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const byte = bytecode[current + 3];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val ^ byte;
            },
            .AddSp8 => {
                runtimeInfo.ptrs.sp += bytecode[current + 1];
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    runtimeInfo.ptrs.sp,
                );
            },
            .SubSp8 => {
                runtimeInfo.ptrs.sp -= bytecode[current + 1];
            },
            .AddSpReg => {
                const value = runtimeInfo.registers[bytecode[current + 1]];
                runtimeInfo.ptrs.sp += value;
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    runtimeInfo.ptrs.sp,
                );
            },
            .SubSpReg => {
                const value = runtimeInfo.registers[bytecode[current + 1]];
                runtimeInfo.ptrs.sp -= value;
            },
            .MovSp => {
                runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.ptrs.sp;
            },
            .Store64Offset8 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const dest = runtimeInfo.registers[bytecode[current + 2]];
                const offset = bytecode[current + 3];
                const end = dest + offset + 8;
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    end,
                );
                @memcpy(runtimeInfo.stack.items[dest + offset .. end], &byteData);
            },
            .Store64AtRegPostInc8 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const ptrReg = bytecode[current + 2];
                const dest = runtimeInfo.registers[ptrReg];
                const inc = bytecode[current + 3];
                const end = dest + 8;
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    end,
                );
                @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
                runtimeInfo.registers[ptrReg] += inc;
            },
            .Store64AtRegPostInc64 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const ptrReg = bytecode[current + 2];
                const dest = runtimeInfo.registers[ptrReg];
                const inc = std.mem.readInt(
                    u64,
                    @ptrCast(bytecode[current + 3 .. current + 11]),
                    .little,
                );
                const end = dest + 8;
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    end,
                );
                @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
                runtimeInfo.registers[ptrReg] += inc;
            },
            .Store64AtSpPostInc8 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const dest = runtimeInfo.ptrs.sp;
                const inc = bytecode[current + 2];
                const end = dest + 8;
                const maxSize = @max(runtimeInfo.ptrs.sp + inc, end);
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    maxSize,
                );
                @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
                runtimeInfo.ptrs.sp += inc;
            },
            .StoreAtSpPostInc8 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const dest = runtimeInfo.ptrs.sp;
                const inc = bytecode[current + 2];
                const end = dest + 8;
                const maxSize = @max(runtimeInfo.ptrs.sp + inc, end);
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    maxSize,
                );
                @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
                runtimeInfo.ptrs.sp += inc;
            },
        }

        current += instLen;
    }
}

fn getFlagFromJump(jump: codegen.InstructionVariants, flags: Flags) bool {
    return switch (jump) {
        .JumpEQ, .JumpBackEQ => flags.EQ,
        .JumpNE, .JumpBackNE => flags.NE,
        .JumpGT, .JumpBackGT => flags.GT,
        .JumpLT, .JumpBackLT => flags.LT,
        .JumpGTE, .JumpBackGTE => flags.GTE,
        .JumpLTE, .JumpBackLTE => flags.LTE,
        else => unreachable,
    };
}

inline fn compareOrder(value1: u64, value2: u64) Flags {
    const result = std.math.order(value1, value2);
    return switch (result) {
        .lt => .{
            .EQ = false,
            .NE = true,
            .GT = false,
            .LT = true,
            .GTE = false,
            .LTE = true,
        },
        .eq => .{
            .EQ = true,
            .NE = false,
            .GT = false,
            .LT = false,
            .GTE = true,
            .LTE = true,
        },
        .gt => .{
            .EQ = false,
            .NE = true,
            .GT = true,
            .LT = false,
            .GTE = true,
            .LTE = false,
        },
    };
}

fn ensureStackCapacityAndLength(
    allocator: Allocator,
    stack: *std.ArrayList(u8),
    minCapacity: u64,
) !void {
    try stack.ensureTotalCapacity(allocator, minCapacity);
    stack.items.len = stack.capacity;
}
