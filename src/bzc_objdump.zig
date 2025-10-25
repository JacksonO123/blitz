const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const utils = blitz.utils;
const codegen = blitz.codegen;
const vmInfo = blitz.vmInfo;
const Writer = std.Io.Writer;

const BzcObjDumpError = error{
    InvalidArgCount,
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        return BzcObjDumpError.InvalidArgCount;
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
    while (current < bytecode.len) {
        const instr = @as(codegen.InstructionVariants, @enumFromInt(bytecode[current]));
        const size = instr.getInstrLen();

        try writer.writeAll("[");
        try writer.printInt(current, 10, .lower, .{});
        try writer.writeAll("] (");
        try writer.printInt(size, 10, .lower, .{});
        try writer.writeAll(") ");

        try printBytecodeSlice(bytecode[current .. current + size], writer);
        current += size;
    }
}

pub fn printVMStartInfo(info: []u8, writer: *Writer) !void {
    try writer.writeAll("blitz bytecode version ");
    try writer.printInt(info[0], 10, .lower, .{});
    try writer.writeAll("\nstarting stack size: ");
    const startStackSize = std.mem.readInt(u32, @ptrCast(info[1..5]), .little);
    try writer.printInt(startStackSize, 10, .lower, .{});
    try writer.writeAll("\n");
}

fn printBytecodeSlice(bytecode: []u8, writer: *Writer) !void {
    const inst = @as(codegen.InstructionVariants, @enumFromInt(bytecode[0]));

    try writer.writeAll(inst.toString());

    switch (inst) {
        .SetReg64 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
            const num = bytecode[2..10];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg32 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
            const num = bytecode[2..6];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg16 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
            const num = bytecode[2..4];
            try writeHexDecNumberSlice(num, writer);
        },
        .SetReg8, .CmpConstByte => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
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
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..4], writer);
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
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[1..], writer);
        },
        .IncConstByte,
        .DecConstByte,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
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
        .XorConstByte => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
        .AddSp8, .SubSp8 => {
            try writer.writeAll(" ");
            try writer.printInt(bytecode[1], 10, .lower, .{});
        },
        .AddSpReg,
        .SubSpReg,
        => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
        },
        .Store64Offset8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
        .Store64PostIncReg8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(bytecode[2], 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..11], writer);
        },
        .Store64PostIncSp8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..11], writer);
        },
        .StoreAtSpPostInc8 => {
            try writer.writeAll(" r");
            try writer.printInt(bytecode[1], 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumberSlice(bytecode[3..4], writer);
        },
    }

    try writer.writeAll("\n");
}

fn writeHexDecNumberSlice(constStr: []u8, writer: *Writer) !void {
    try writer.writeAll("0x");
    try formatHexByteSlice(constStr, writer);
    try writer.writeAll("(");
    try formatIntByteSlice(constStr, writer);
    try writer.writeAll(")");
}

fn formatHexByteSlice(slice: []u8, writer: *Writer) !void {
    for (slice) |byte| {
        try writer.printInt(byte, 16, .lower, .{
            .width = 2,
            .fill = '0',
        });
    }
}

fn formatIntByteSlice(slice: []u8, writer: *Writer) !void {
    switch (slice.len) {
        1 => try formatIntByteSliceUtil(u8, slice, writer),
        2 => try formatIntByteSliceUtil(u16, slice, writer),
        4 => try formatIntByteSliceUtil(u32, slice, writer),
        8 => try formatIntByteSliceUtil(u64, slice, writer),
        else => utils.unimplemented(),
    }
}

fn formatIntByteSliceUtil(comptime T: type, slice: []u8, writer: *Writer) !void {
    var temp: T = 0;

    var i: usize = slice.len - 1;
    while (true) : (i -= 1) {
        var byte: T = slice[i];
        byte = byte << @intCast(i * 8);
        temp += byte;
        if (i == 0) break;
    }

    try writer.printInt(temp, 10, .lower, .{});
}
