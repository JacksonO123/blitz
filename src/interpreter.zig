const std = @import("std");
const builtin = @import("builtin");
pub const blitz = @import("blitz.zig");
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const codegen = blitz.codegen;
const version = blitz.version;
const Allocator = std.mem.Allocator;
const RegisterType = vmInfo.RegisterType;

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

    interpretBytecode(&runtimeInfo, bytecode);

    var buf = utils.getBufferedWriter();
    defer buf.flush() catch {};
    const writer = buf.writer();
    try runtimeInfo.dumpRegisters(12, writer);
}

const Flags = struct {
    EQ: bool = false,
    NE: bool = false,
    GT: bool = false,
    LT: bool = false,
    GTE: bool = false,
    LTE: bool = false,
};

const RuntimeInfo = struct {
    const Self = @This();

    allocator: Allocator,
    registers: [vmInfo.NUM_REGISTERS]RegisterType = [_]RegisterType{0} ** vmInfo.NUM_REGISTERS,
    flags: Flags,
    stack: []u8,

    pub fn init(allocator: Allocator, stackSize: u32) !Self {
        const stack = try allocator.alloc(u8, stackSize);
        return .{
            .allocator = allocator,
            .flags = Flags{},
            .stack = stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.stack);
    }

    pub fn dumpRegisters(self: Self, until: usize, writer: anytype) !void {
        try writer.writeAll("##STACK_START##\n");
        var i: usize = 0;
        while (i < vmInfo.NUM_REGISTERS) : (i += 1) {
            if (i == until) break;
            try std.fmt.formatInt(i, 10, .lower, .{}, writer);
            try writer.writeAll(") ");
            try std.fmt.formatInt(self.registers[i], 10, .lower, .{}, writer);
            try writer.writeByte('\n');
        }
        try writer.writeAll("##STACK_END##\n");
    }
};

fn interpretBytecode(runtimeInfo: *RuntimeInfo, bytecode: []u8) void {
    var current: usize = vmInfo.VM_INFO_BYTECODE_LEN;
    while (current < bytecode.len) {
        const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const instLen = inst.getInstrLen();
        switch (inst) {
            .Mov => {
                runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.registers[bytecode[current + 2]];
            },
            .SetReg => {
                const value = std.mem.readInt(u64, @ptrCast(bytecode[current + 2 .. current + 10]), .little);
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetRegHalf => {
                const value = std.mem.readInt(u32, @ptrCast(bytecode[current + 2 .. current + 6]), .little);
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetRegByte => {
                runtimeInfo.registers[bytecode[current + 1]] = bytecode[current + 2];
            },
            .Add => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val + reg2Val;
            },
            .Sub => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val - reg2Val;
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
            else => {},
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
