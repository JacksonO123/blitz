const std = @import("std");
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const blitz = @import("blitz");
const utils = blitz.utils;
const codegen = blitz.codegen;
const vmInfo = blitz.vmInfo;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    if (args.len != 2) {
        return error.InvalidArgCount;
    }

    const filename = args[1];
    const bzcFilename = try allocator.alloc(u8, filename.len + 4);
    @memcpy(bzcFilename[0..filename.len], filename);
    @memcpy(bzcFilename[filename.len .. filename.len + 4], ".bzc");

    const bytecode = try utils.readRelativeFile(allocator, bzcFilename);

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    try printBytecode(bytecode, writer);
}

pub fn printBytecode(bytecode: []u8, writer: *Writer) !void {
    try printVMStartInfo(bytecode[0..vmInfo.VM_INFO_BYTECODE_LEN], writer);
    var current: u64 = std.mem.readInt(
        u32,
        bytecode[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
        .little,
    );

    try printHexViewer(bytecode[vmInfo.PADDED_VM_INFO_BYTECODE_LEN..current], writer);

    const byteCountFloat: f64 = @floatFromInt(bytecode.len);
    const numDigits: u64 = @intFromFloat(@floor(@log10(byteCountFloat)) + 1);
    const numInstrLenDigits = utils.getNumberDigitCount(u8, codegen.Instr.maxInstrSize());

    while (current < bytecode.len) {
        const instr = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const size = instr.getInstrLen();

        try writer.writeByte('[');
        try writer.printInt(current, 10, .lower, .{ .width = numDigits, .fill = '.' });
        try writer.writeAll("] (");
        try writer.printInt(size, 10, .lower, .{ .width = numInstrLenDigits, .fill = '.' });
        try writer.writeAll(") ");

        try printBytecodeSlice(bytecode[current .. current + size], writer);
        current += size;
    }
}

fn printHexViewer(bytes: []const u8, writer: *Writer) !void {
    const WIDTH = 16; // bytes

    try writer.writeByte('\n');

    var i: usize = 0;
    while (i < bytes.len) : (i += WIDTH) {
        const row = bytes[i..@min(i + WIDTH, bytes.len)];

        try writer.writeAll("0x");
        try writer.printInt(i, 16, .lower, .{ .width = 4, .fill = '0' });
        try writer.writeAll("  ");

        var gotTo: usize = 0;
        for (row, 0..) |byte, index| {
            try writer.printInt(byte, 16, .lower, .{ .width = 2, .fill = '0' });
            try writer.writeByte(' ');
            gotTo = index;
        }

        while (gotTo < WIDTH) : (gotTo += 1) {
            try writer.writeAll("   ");
        }

        try writer.writeByte(' ');

        gotTo = 0;
        for (row, 0..) |byte, index| {
            if (byte != 0) {
                try writer.writeByte(byte);
                try writer.writeByte(' ');
            } else {
                try writer.writeAll(". ");
            }

            gotTo = index;
        }

        while (gotTo < WIDTH) : (gotTo += 1) {
            try writer.writeAll(". ");
        }

        try writer.writeByte('\n');
    }

    try writer.writeByte('\n');
}

pub fn printVMStartInfo(info: []u8, writer: *Writer) !void {
    try writer.writeAll("blitz bytecode version ");
    try writer.printInt(info[vmInfo.VERSION_LOCATION], 10, .lower, .{});
    try writer.writeAll("\nFirst instr: ");
    try writeHexNumberSlice(
        info[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
        writer,
    );
    try writer.writeAll("\nMakeStack ");
    try writeHexDecNumberSlice(
        info[vmInfo.STACK_START_LOCATION .. vmInfo.STACK_START_LOCATION + 4],
        writer,
    );
    try writer.writeByte('\n');
}

fn printBytecodeSlice(bytecode: []u8, writer: *Writer) !void {
    const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[0]));

    try writer.writeAll(inst.toString());

    switch (inst) {
        .Label, .NoOp, .Ret, .End => {},
        .SetReg64 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            const num = bytecode[2..10];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg32 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            const num = bytecode[2..6];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg16 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            const num = bytecode[2..4];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg8, .CmpConst8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..3], writer);
        },
        .Cmp => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        .Xor,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[3], 10, .lower, .{});
        },
        .Add,
        .Sub,
        .Mult,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[3], 10, .lower, .{});
        },
        .Add8, .Sub8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
        .Add16, .Sub16 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..5], writer);
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
        .BranchLink,
        .BranchLinkBack,
        => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..], writer);
        },
        .IncConst8,
        .DecConst8,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..3], writer);
        },
        .Mov => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
        },
        .MovSpNegOffsetAny => unreachable,
        .MovSpNegOffset16 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..4], writer);
        },
        .MovSpNegOffset32 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..6], writer);
        },
        .MovSpNegOffset64 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..10], writer);
        },
        .XorConst8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
        .AddSp8, .SubSp8 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..2], writer);
        },
        .AddSp16, .SubSp16 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..3], writer);
        },
        .AddSp32, .SubSp32 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..5], writer);
        },
        .AddSp64, .SubSp64 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..9], writer);
        },
        .Store64AtReg,
        .Store32AtReg,
        .Store16AtReg,
        .Store8AtReg,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..5], writer);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        .Load64AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load8AtSpNegOffset16,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..4], writer);
        },
        .Load64AtReg,
        .Load32AtReg,
        .Load16AtReg,
        .Load8AtReg,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" [r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(']');
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" [r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(", ");
            try writeHexDecNumberSlice(bytecode[3..5], writer);
            try writer.writeByte(']');
        },
        .MulReg16AddReg => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[3], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[4..6], writer);
        },
        .DbgReg => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
        },
        .BitAnd, .BitOr => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[3], 10, .lower, .{});
        },
        .And, .Or => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
        },
        .AndSetReg, .OrSetReg => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[3], 10, .lower, .{});
        },
        .PrePushRegNegOffsetAny, .PostPopRegNegOffsetAny => unreachable,
        .PrePushRegNegOffset8,
        .PostPopRegNegOffset8,
        => try printPushOrPopRegNegOffset(u8, bytecode, writer),
        .PrePushRegNegOffset16,
        .PostPopRegNegOffset16,
        => try printPushOrPopRegNegOffset(u16, bytecode, writer),
        .PrePushRegNegOffset32,
        .PostPopRegNegOffset32,
        => try printPushOrPopRegNegOffset(u32, bytecode, writer),
        .PrePushRegNegOffset64,
        .PostPopRegNegOffset64,
        => try printPushOrPopRegNegOffset(u64, bytecode, writer),
        .PrePushLRNegOffsetAny, .PostPopLRNegOffsetAny => unreachable,
        .PrePushLRNegOffset8, .PostPopLRNegOffset8 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..2], writer);
        },
        .PrePushLRNegOffset16, .PostPopLRNegOffset16 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..3], writer);
        },
        .PrePushLRNegOffset32, .PostPopLRNegOffset32 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..5], writer);
        },
        .PrePushLRNegOffset64, .PostPopLRNegOffset64 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..9], writer);
        },
    }

    try writer.writeByte('\n');
}

fn printPushOrPopRegNegOffset(comptime T: type, bytecode: []const u8, writer: *Writer) !void {
    const byteLen = @sizeOf(T);

    try writer.writeAll(" r");
    try writer.printInt(bytecode[1], 10, .lower, .{});
    try writer.writeByte(' ');
    try writeHexDecNumberSlice(bytecode[2 .. 2 + byteLen], writer);
}

fn writeHexDecNumberSlice(constStr: []const u8, writer: *Writer) !void {
    switch (constStr.len) {
        1 => try formatHexDecNumberUtil(u8, constStr, writer),
        2 => try formatHexDecNumberUtil(u16, constStr, writer),
        4 => try formatHexDecNumberUtil(u32, constStr, writer),
        8 => try formatHexDecNumberUtil(u64, constStr, writer),
        else => utils.unimplemented(),
    }
}

fn formatHexDecNumberUtil(comptime T: type, str: []const u8, writer: *Writer) !void {
    const num = std.mem.readInt(T, @ptrCast(str), .little);
    try blitz.debug.writeHexDecNumber(T, num, writer);
}

fn writeHexNumberSlice(constStr: []const u8, writer: *Writer) !void {
    switch (constStr.len) {
        1 => try formatHexNumberSlice(u8, constStr, writer),
        2 => try formatHexNumberSlice(u16, constStr, writer),
        4 => try formatHexNumberSlice(u32, constStr, writer),
        8 => try formatHexNumberSlice(u64, constStr, writer),
        else => utils.unimplemented(),
    }
}

fn formatHexNumberSlice(comptime T: type, str: []const u8, writer: *Writer) !void {
    const num = std.mem.readInt(T, @ptrCast(str), .little);
    try blitz.debug.writeHexNumber(T, num, writer);
}
