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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        return InterpreterError.NoInputFile;
    }

    const filename = args[1];
    const bzcFilename = try allocator.alloc(u8, filename.len + 4);
    @memcpy(bzcFilename[0..filename.len], filename);
    @memcpy(bzcFilename[filename.len .. filename.len + 4], ".bzc");

    const bytecode = try utils.readRelativeFile(allocator, bzcFilename);
    const bytecodeVersion = bytecode[0];
    if (bytecodeVersion != version.VERSION) {
        return InterpreterError.IncompatibleInterpreterVersions;
    }

    const stackSize = std.mem.readInt(u32, @ptrCast(bytecode[1..5]), .little);
    var runtimeInfo = try RuntimeInfo.init(allocator, stackSize);

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;
    defer writer.flush() catch {};

    interpretBytecode(allocator, &runtimeInfo, bytecode, writer) catch |e| {
        try writer.writeAll("Error: ");
        try writer.writeAll(@errorName(e));
        try writer.writeByte('\n');
    };

    try runtimeInfo.writeMemDebug(12, 64, writer);
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

    registers: [vmInfo.NUM_REGISTERS]RegisterType = [_]RegisterType{0} ** vmInfo.NUM_REGISTERS,
    flags: Flags,
    stack: *std.ArrayListAligned(u8, .@"64"),
    ptrs: RuntimePtrs,

    pub fn init(allocator: Allocator, stackSize: u32) !Self {
        const tempStack = try std.ArrayListAligned(u8, .@"64").initCapacity(allocator, stackSize);
        const stack = try utils.createMut(std.ArrayListAligned(u8, .@"64"), allocator, tempStack);
        stack.items.len = stackSize;

        return .{
            .flags = Flags{},
            .stack = stack,
            .ptrs = .{
                .sp = 0,
            },
        };
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

fn interpretBytecode(
    allocator: Allocator,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    writer: *Writer,
) !void {
    var current: u64 = vmInfo.VM_INFO_BYTECODE_LEN;
    while (current < bytecode.len) {
        const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const instLen = inst.getInstrLen();
        switch (inst) {
            .Label => unreachable,
            .Mov => {
                runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.registers[bytecode[current + 2]];
            },
            .SetReg64 => {
                const value = std.mem.readInt(
                    u64,
                    @ptrCast(bytecode[current + 2 .. current + 10]),
                    .little,
                );
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetReg32 => {
                const value = std.mem.readInt(
                    u32,
                    @ptrCast(bytecode[current + 2 .. current + 6]),
                    .little,
                );
                runtimeInfo.registers[bytecode[current + 1]] = value;
            },
            .SetReg16 => {
                const value = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 2 .. current + 4]),
                    .little,
                );
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
                const regVal = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.registers[bytecode[current + 1]] = regVal + bytecode[current + 3];
            },
            .Add16 => {
                const regVal = runtimeInfo.registers[bytecode[current + 2]];
                const val = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 3 .. current + 5]),
                    .little,
                );
                runtimeInfo.registers[bytecode[current + 1]] = regVal + val;
            },
            .Sub => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val - reg2Val;
            },
            .Sub8 => {
                const regVal = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.registers[bytecode[current + 1]] = regVal - bytecode[current + 3];
            },
            .Sub16 => {
                const regVal = runtimeInfo.registers[bytecode[current + 2]];
                const val = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 3 .. current + 5]),
                    .little,
                );
                runtimeInfo.registers[bytecode[current + 1]] = regVal - val;
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
            .CmpConst8 => {
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
            .IncConst8 => {
                runtimeInfo.registers[bytecode[current + 1]] += bytecode[current + 2];
            },
            .DecConst8 => {
                runtimeInfo.registers[bytecode[current + 1]] -= bytecode[current + 2];
            },
            .Xor => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Val = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val ^ reg2Val;
            },
            .XorConst8 => {
                const reg1Val = runtimeInfo.registers[bytecode[current + 2]];
                const byte = bytecode[current + 3];
                runtimeInfo.registers[bytecode[current + 1]] = reg1Val ^ byte;
            },
            .AddSp16 => {
                try addSp(u16, allocator, runtimeInfo, bytecode, current);
            },
            .AddSp32 => {
                try addSp(u32, allocator, runtimeInfo, bytecode, current);
            },
            .AddSp64 => {
                try addSp(u64, allocator, runtimeInfo, bytecode, current);
            },
            .SubSp16 => {
                subSp(u16, runtimeInfo, bytecode, current);
            },
            .SubSp32 => {
                subSp(u32, runtimeInfo, bytecode, current);
            },
            .SubSp64 => {
                subSp(u64, runtimeInfo, bytecode, current);
            },
            .MovSpNegOffsetAny => unreachable,
            .MovSpNegOffset16 => {
                movSpNegOffset(u16, runtimeInfo, bytecode, current);
            },
            .MovSpNegOffset32 => {
                movSpNegOffset(u32, runtimeInfo, bytecode, current);
            },
            .MovSpNegOffset64 => {
                movSpNegOffset(u64, runtimeInfo, bytecode, current);
            },
            .Store64AtReg => {
                try storeAtReg(u64, allocator, runtimeInfo, bytecode, current);
            },
            .Store32AtReg => {
                try storeAtReg(u32, allocator, runtimeInfo, bytecode, current);
            },
            .Store16AtReg => {
                try storeAtReg(u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store8AtReg => {
                const byteData: u8 = @intCast(runtimeInfo.registers[bytecode[current + 1]]);
                const ptrReg = bytecode[current + 2];
                const dest = runtimeInfo.registers[ptrReg];
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    dest + 1,
                );
                runtimeInfo.stack.items[dest] = byteData;
            },
            .Store64AtRegPostInc16 => {
                try storeAtRegPostInc(u64, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store32AtRegPostInc16 => {
                try storeAtRegPostInc(u32, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store16AtRegPostInc16 => {
                try storeAtRegPostInc(u16, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store8AtRegPostInc16 => {
                const byteData: u8 = @intCast(runtimeInfo.registers[bytecode[current + 1]]);
                const ptrReg = bytecode[current + 2];
                const dest = runtimeInfo.registers[ptrReg];
                const inc = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 3 .. current + 5]),
                    .little,
                );
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    dest + 1,
                );
                runtimeInfo.stack.items[dest] = byteData;
                runtimeInfo.registers[ptrReg] += inc;
            },
            .Store64AtSpNegOffset16 => {
                try storeAtSpNegOffset(u64, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store32AtSpNegOffset16 => {
                try storeAtSpNegOffset(u32, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store16AtSpNegOffset16 => {
                try storeAtSpNegOffset(u16, u16, allocator, runtimeInfo, bytecode, current);
            },
            .Store8AtSpNegOffset16 => {
                const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[current + 1]]);
                const offset = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 2 .. current + 4]),
                    .little,
                );
                const dest = runtimeInfo.ptrs.sp - offset;
                try ensureStackCapacityAndLength(
                    allocator,
                    runtimeInfo.stack,
                    dest + 1,
                );
                runtimeInfo.stack.items[dest] = byteData[0];
            },
            .Load64AtReg => {
                loadAtReg(u64, runtimeInfo, bytecode, current);
            },
            .Load32AtReg => {
                loadAtReg(u32, runtimeInfo, bytecode, current);
            },
            .Load16AtReg => {
                loadAtReg(u16, runtimeInfo, bytecode, current);
            },
            .Load8AtReg => {
                const source = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.stack.items[source];
            },
            .Load8AtRegOffset16 => {
                const source = runtimeInfo.registers[bytecode[current + 2]];
                const offset = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 3 .. current + 5]),
                    .little,
                );
                const byteData = runtimeInfo.stack.items[source + offset];
                runtimeInfo.registers[bytecode[current + 1]] = byteData;
            },
            .Load16AtRegOffset16 => {
                loadAtRegOffset16(u16, runtimeInfo, bytecode, current);
            },
            .Load32AtRegOffset16 => {
                loadAtRegOffset16(u32, runtimeInfo, bytecode, current);
            },
            .Load64AtRegOffset16 => {
                loadAtRegOffset16(u64, runtimeInfo, bytecode, current);
            },
            .MulReg16AddReg => {
                const dest = bytecode[current + 1];
                const toAdd = runtimeInfo.registers[bytecode[current + 2]];
                const mulReg = runtimeInfo.registers[bytecode[current + 3]];
                const data = std.mem.readInt(
                    u16,
                    @ptrCast(bytecode[current + 4 .. current + 6]),
                    .little,
                );
                runtimeInfo.registers[dest] = toAdd + (mulReg * data);
            },
            .DbgReg => if (builtin.mode == .Debug) {
                try writer.writeByte('r');
                try writer.printInt(bytecode[current + 1], 10, .lower, .{});
                try writer.writeAll(" :: (");
                try writer.printInt(runtimeInfo.registers[bytecode[current + 1]], 10, .lower, .{});
                try writer.writeAll(")\n");
            } else unreachable,
            .BitAnd => {
                const dest = bytecode[current + 1];
                const reg1Value = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[dest] = reg1Value & reg2Value;
            },
            .BitOr => {
                const dest = bytecode[current + 1];
                const reg1Value = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[dest] = reg1Value | reg2Value;
            },
            .And => {
                const reg1Value = runtimeInfo.registers[bytecode[current + 1]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.flags = .{
                    .EQ = reg1Value == 1 and reg2Value == 1,
                };
            },
            .Or => {
                const reg1Value = runtimeInfo.registers[bytecode[current + 1]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 2]];
                runtimeInfo.flags = .{
                    .EQ = reg1Value == 1 or reg2Value == 1,
                };
            },
            .AndSetReg => {
                const dest = bytecode[current + 1];
                const reg1Value = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[dest] = @intFromBool(reg1Value == 1 and reg2Value == 1);
            },
            .OrSetReg => {
                const dest = bytecode[current + 1];
                const reg1Value = runtimeInfo.registers[bytecode[current + 2]];
                const reg2Value = runtimeInfo.registers[bytecode[current + 3]];
                runtimeInfo.registers[dest] = @intFromBool(reg1Value == 1 or reg2Value == 1);
            },
            .PushNRegNegOffsetAny => unreachable,
            .PushNRegNegOffset8 => {
                const count = bytecode[current + 1];
                const offset = bytecode[1 + count];

                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[2 + i]]);
                    @memcpy(runtimeInfo.stack.items[offset .. offset + 8], &byteData);
                }
            },
            .PushNRegNegOffset16 => {
                pushNRegNegOffset(u16, runtimeInfo, bytecode, current);
            },
            .PushNRegNegOffset32 => {
                pushNRegNegOffset(u32, runtimeInfo, bytecode, current);
            },
            .PushNRegNegOffset64 => {
                pushNRegNegOffset(u64, runtimeInfo, bytecode, current);
            },
        }

        current += instLen;
    }
}

fn pushNRegNegOffset(
    comptime T: type,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) void {
    const byteLen = @sizeOf(T);

    const count = bytecode[current + 1];
    const offset = std.mem.readInt(
        T,
        @ptrCast(bytecode[1 + count .. 1 + count + byteLen]),
        .little,
    );

    var i: usize = 0;
    while (i < count) : (i += 1) {
        const byteData: [8]u8 = @bitCast(runtimeInfo.registers[bytecode[2 + i]]);
        @memcpy(runtimeInfo.stack.items[offset .. offset + 8], &byteData);
    }
}

fn movSpNegOffset(
    comptime T: type,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) void {
    const tBytes = @sizeOf(T);
    const offset = std.mem.readInt(
        T,
        @ptrCast(bytecode[current + 2 .. current + 2 + tBytes]),
        .little,
    );
    runtimeInfo.registers[bytecode[current + 1]] = runtimeInfo.ptrs.sp - offset;
}

fn addSp(
    comptime T: type,
    allocator: Allocator,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) !void {
    const tBytes = @sizeOf(T);

    const amount = std.mem.readInt(
        T,
        @ptrCast(bytecode[current + 1 .. current + 1 + tBytes]),
        .little,
    );
    runtimeInfo.ptrs.sp += amount;
    try ensureStackCapacityAndLength(
        allocator,
        runtimeInfo.stack,
        runtimeInfo.ptrs.sp,
    );
}

fn subSp(
    comptime T: type,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) void {
    const tBytes = @sizeOf(T);
    const amount = std.mem.readInt(
        T,
        @ptrCast(bytecode[current + 1 .. current + 1 + tBytes]),
        .little,
    );
    runtimeInfo.ptrs.sp -= amount;
}

fn storeAtReg(
    comptime StoreType: type,
    allocator: Allocator,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) !void {
    const storeTypeBytes = @divExact(@typeInfo(StoreType).int.bits, 8);

    const intData: StoreType = @intCast(runtimeInfo.registers[bytecode[current + 1]]);
    const byteData: [storeTypeBytes]u8 = @bitCast(intData);
    const ptrReg = bytecode[current + 2];
    const dest = runtimeInfo.registers[ptrReg];
    const end = dest + storeTypeBytes;
    try ensureStackCapacityAndLength(
        allocator,
        runtimeInfo.stack,
        end,
    );
    @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
}

fn storeAtRegPostInc(
    comptime StoreType: type,
    comptime OffsetType: type,
    allocator: Allocator,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) !void {
    const storeTypeBytes = @divExact(@typeInfo(StoreType).int.bits, 8);
    const offsetTypeBytes = @divExact(@typeInfo(OffsetType).int.bits, 8);

    const intData: StoreType = @intCast(runtimeInfo.registers[bytecode[current + 1]]);
    const byteData: [storeTypeBytes]u8 = @bitCast(intData);
    const ptrReg = bytecode[current + 2];
    const dest = runtimeInfo.registers[ptrReg];
    const inc = std.mem.readInt(
        OffsetType,
        @ptrCast(bytecode[current + 3 .. current + 3 + offsetTypeBytes]),
        .little,
    );
    const end = dest + storeTypeBytes;
    try ensureStackCapacityAndLength(
        allocator,
        runtimeInfo.stack,
        end,
    );
    @memcpy(runtimeInfo.stack.items[dest..end], &byteData);
    runtimeInfo.registers[ptrReg] += inc;
}

fn loadAtRegOffset16(
    comptime T: type,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) void {
    const tBytes = @divExact(@typeInfo(T).int.bits, 8);

    const dest = bytecode[current + 1];
    const source = runtimeInfo.registers[bytecode[current + 2]];
    const offset = std.mem.readInt(u16, @ptrCast(bytecode[current + 3 .. current + 5]), .little);
    const byteData: *const [tBytes]u8 = @ptrCast(runtimeInfo.stack.items[source + offset .. source + offset + tBytes]);
    const resInt: T = @bitCast(byteData.*);
    runtimeInfo.registers[dest] = @intCast(resInt);
}

fn loadAtReg(
    comptime T: type,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) void {
    const tBytes = @divExact(@typeInfo(T).int.bits, 8);

    const dest = bytecode[current + 1];
    const source = runtimeInfo.registers[bytecode[current + 2]];
    const byteData: *const [tBytes]u8 = @ptrCast(runtimeInfo.stack.items[source .. source + tBytes]);
    const resInt: T = @bitCast(byteData.*);
    runtimeInfo.registers[dest] = @intCast(resInt);
}

fn storeAtSpNegOffset(
    comptime StoreType: type,
    comptime OffsetType: type,
    allocator: Allocator,
    runtimeInfo: *RuntimeInfo,
    bytecode: []const u8,
    current: u64,
) !void {
    const offsetBytes = @divExact(@typeInfo(OffsetType).int.bits, 8);
    const storeBytes = @divExact(@typeInfo(StoreType).int.bits, 8);

    const intData: StoreType = @intCast(runtimeInfo.registers[bytecode[current + 1]]);
    const byteData: [storeBytes]u8 = @bitCast(intData);
    const offset = std.mem.readInt(
        OffsetType,
        @ptrCast(bytecode[current + 2 .. current + 2 + offsetBytes]),
        .little,
    );
    const dest = runtimeInfo.ptrs.sp - offset;
    try ensureStackCapacityAndLength(
        allocator,
        runtimeInfo.stack,
        dest + storeBytes,
    );
    @memcpy(runtimeInfo.stack.items[dest .. dest + storeBytes], &byteData);
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
    stack: *std.ArrayListAligned(u8, .@"64"),
    minCapacity: u64,
) !void {
    try stack.ensureTotalCapacity(allocator, minCapacity);
    stack.items.len = stack.capacity;
}
