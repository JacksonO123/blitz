const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const utils = blitz.utils;
const codegen = blitz.codegen;
const vmInfo = blitz.vmInfo;
const Writer = std.Io.Writer;

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        return error.InvalidArgCount;
    }

    const filename = args[1];
    const bzcFilename = try allocator.alloc(u8, filename.len + 4);
    defer allocator.free(bzcFilename);
    @memcpy(bzcFilename[0..filename.len], filename);
    @memcpy(bzcFilename[filename.len .. filename.len + 4], ".bzc");

    const bytecode = try utils.readRelativeFile(allocator, bzcFilename);
    defer allocator.free(bytecode);

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    try printBytecode(bytecode, writer);
}

pub fn printBytecode(bytecode: []u8, writer: *Writer) !void {
    try printVMStartInfo(bytecode[0..vmInfo.VM_INFO_BYTECODE_LEN], writer);
    var current: u64 = vmInfo.VM_INFO_BYTECODE_LEN;

    const byteCountFloat: f64 = @floatFromInt(bytecode.len);
    const numDigits: u64 = @intFromFloat(@floor(@log10(byteCountFloat)) + 1);

    while (current < bytecode.len) {
        const instr = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const size = instr.getInstrLen();

        try writer.writeByte('[');
        try writer.printInt(current, 10, .lower, .{ .width = numDigits, .fill = '.' });
        try writer.writeAll("] (");
        try writer.printInt(size, 10, .lower, .{ .width = numDigits, .fill = '.' });
        try writer.writeAll(") ");

        try printBytecodeSlice(bytecode[current .. current + size], writer);
        current += size;
    }
}

pub fn printVMStartInfo(info: []u8, writer: *Writer) !void {
    try writer.writeAll("blitz bytecode version ");
    try writer.printInt(info[0], 10, .lower, .{});
    try writer.writeAll("\nMakeStack ");
    try writeHexDecNumberSlice(info[1..5], writer);
    try writer.writeByte('\n');
}

fn printBytecodeSlice(bytecode: []u8, writer: *Writer) !void {
    const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[0]));

    try writer.writeAll(inst.toString());

    switch (inst) {
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
        .MovSp => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
        },
        .MovSpNegOffset16 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[2..4], writer);
        },
        .XorConst8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
        .AddSp16, .SubSp16 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..3], writer);
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
        .StoreSpSub16AtSpNegOffset16 => {
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[1..3], writer);
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..5], writer);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
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
        .MovByteRange => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[3..4], writer);
            try writer.writeByte(' ');
            try writeHexDecNumberSlice(bytecode[4..5], writer);
        },
    }

    try writer.writeByte('\n');
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
