const std = @import("std");
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const blitz = @import("blitz");
const utils = blitz.utils;
const codegen = blitz.codegen;
const vmInfo = blitz.vmInfo;
const debug = blitz.debug;

const InstrSegmentVariant = enum {
    const Self = @This();

    Reg,
    Immediate8,
    Immediate16,
    Immediate32,
    Immediate64,

    pub fn getSize(self: Self) u8 {
        return switch (self) {
            .Register => 1,
            .Immediate8 => 1,
            .Immediate16 => 2,
            .Immediate32 => 4,
            .Immediate64 => 8,
        };
    }
};

const InstrPrintUtil = struct {
    const Self = @This();

    bytes: []const u8,
    currentByte: usize = 0,
    currentInstr: usize = 0,
    currentInstrByte: usize = 0,

    pub fn init(instrs: []const u8) InstrPrintUtil {
        return .{ .bytes = instrs };
    }

    pub fn take(self: *Self, bytes: u32) []const u8 {
        const slice = self.bytes[self.currentByte .. self.currentByte + bytes];
        self.currentByte += bytes;
        return slice;
    }

    pub fn hasNext(self: *Self) bool {
        return self.currentByte < self.bytes.len;
    }

    pub fn nextInstr(self: *Self) void {
        self.currentInstr += 1;
        const instr = @as(
            codegen.InstructionVariants,
            @enumFromInt(self.bytes[self.currentInstrByte]),
        );
        self.currentInstrByte += @max(instr.getInstrLen(), 1);
        self.currentByte = self.currentInstrByte;
    }
};

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
    const instrStart: u64 = std.mem.readInt(
        u32,
        bytecode[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
        .little,
    );

    try blitz.debug.printHexViewer(bytecode[vmInfo.PADDED_VM_INFO_BYTECODE_LEN..instrStart], writer);

    const byteCountFloat: f64 = @floatFromInt(bytecode.len);
    const numDigits: u64 = @intFromFloat(@floor(@log10(byteCountFloat)) + 1);
    const numInstrLenDigits = utils.getNumberDigitCount(u8, codegen.Instr.maxInstrSize());

    const bytes = bytecode[instrStart..];
    var printUtil = InstrPrintUtil.init(bytes);

    while (printUtil.hasNext()) {
        try printBytecodeInstr(
            &printUtil,
            numDigits,
            numInstrLenDigits,
            printUtil.currentInstr,
            writer,
        );
        printUtil.nextInstr();
    }
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

fn printBytecodeInstr(
    printUtil: *InstrPrintUtil,
    numDigits: u64,
    numInstrLenDigits: u8,
    current: usize,
    writer: *Writer,
) !void {
    const instrByte = printUtil.take(1)[0];
    const instr = @as(codegen.InstructionVariants, @enumFromInt(instrByte));

    try writer.writeByte('[');
    try writer.printInt(current, 10, .lower, .{ .width = numDigits, .fill = '.' });
    try writer.writeAll("] (");
    try writer.printInt(
        instr.getInstrLen(),
        10,
        .lower,
        .{ .width = numInstrLenDigits, .fill = '.' },
    );
    try writer.writeAll(") ");

    try writer.writeAll(instr.toString());
    try writer.writeByte(' ');

    switch (instr) {
        .MovSpNegOffsetAny,
        .PrePushRegNegOffsetAny,
        .PostPopRegNegOffsetAny,
        .PrePushLRNegOffsetAny,
        .PostPopLRNegOffsetAny,
        => unreachable,

        .Label, .NoOp, .Ret, .End => {},

        .SetReg64 => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),
        .SetReg32 => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .SetReg16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .SetReg8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),

        .Add => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .Sub => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .Mult => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),

        .Add8 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate8 }, writer),
        .Sub8 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate8 }, writer),

        .Add16 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),
        .Sub16 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),

        .Jump => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpEQ => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpNE => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpGT => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpLT => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpGTE => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpLTE => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBack => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackEQ => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackNE => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackGT => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackLT => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackGTE => try printSegments(printUtil, .{.Immediate32}, writer),
        .JumpBackLTE => try printSegments(printUtil, .{.Immediate32}, writer),

        .Cmp => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .CmpSetRegEQ => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpSetRegNE => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpSetRegGT => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpSetRegLT => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpSetRegGTE => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpSetRegLTE => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpConst8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),

        .IncConst8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),
        .DecConst8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),

        .Mov => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .MovSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .MovSpNegOffset32 => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .MovSpNegOffset64 => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),

        .Xor => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .XorConst8 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate8 }, writer),

        .AddSp8 => try printSegments(printUtil, .{.Immediate8}, writer),
        .SubSp8 => try printSegments(printUtil, .{.Immediate8}, writer),
        .AddSp16 => try printSegments(printUtil, .{.Immediate16}, writer),
        .SubSp16 => try printSegments(printUtil, .{.Immediate16}, writer),
        .AddSp32 => try printSegments(printUtil, .{.Immediate32}, writer),
        .SubSp32 => try printSegments(printUtil, .{.Immediate32}, writer),
        .AddSp64 => try printSegments(printUtil, .{.Immediate64}, writer),
        .SubSp64 => try printSegments(printUtil, .{.Immediate64}, writer),

        .Store64AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Store32AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Store16AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Store8AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .Store64AtRegPostInc16 => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Immediate16 },
            writer,
        ),
        .Store32AtRegPostInc16 => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Immediate16 },
            writer,
        ),
        .Store16AtRegPostInc16 => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Immediate16 },
            writer,
        ),
        .Store8AtRegPostInc16 => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Immediate16 },
            writer,
        ),

        .Store64AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Store32AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Store16AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Store8AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),

        .Load64AtRegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load32AtRegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load16AtRegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load8AtRegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),

        .Load64AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load32AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load16AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .Load8AtSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),

        .Load64AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Load32AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Load16AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Load8AtReg => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .MulReg16AddReg => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Reg, .Immediate16 },
            writer,
        ),

        .DbgReg => try printSegments(printUtil, .{.Reg}, writer),

        .BitAnd => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .BitOr => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .And => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .Or => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .AndSetReg => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .OrSetReg => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),

        .PrePushRegNegOffset8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),
        .PrePushRegNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .PrePushRegNegOffset32 => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .PrePushRegNegOffset64 => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),

        .PostPopRegNegOffset8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),
        .PostPopRegNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .PostPopRegNegOffset32 => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .PostPopRegNegOffset64 => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),

        .PrePushLRNegOffset8 => try printSegments(printUtil, .{.Immediate8}, writer),
        .PrePushLRNegOffset16 => try printSegments(printUtil, .{.Immediate16}, writer),
        .PrePushLRNegOffset32 => try printSegments(printUtil, .{.Immediate32}, writer),
        .PrePushLRNegOffset64 => try printSegments(printUtil, .{.Immediate64}, writer),

        .PostPopLRNegOffset8 => try printSegments(printUtil, .{.Immediate8}, writer),
        .PostPopLRNegOffset16 => try printSegments(printUtil, .{.Immediate16}, writer),
        .PostPopLRNegOffset32 => try printSegments(printUtil, .{.Immediate32}, writer),
        .PostPopLRNegOffset64 => try printSegments(printUtil, .{.Immediate64}, writer),

        .BranchLink => try printSegments(printUtil, .{.Immediate32}, writer),
        .BranchLinkBack => try printSegments(printUtil, .{.Immediate32}, writer),
    }

    try writer.writeByte('\n');
}

fn printSegments(
    printUtil: *InstrPrintUtil,
    comptime segments: anytype,
    writer: *Writer,
) !void {
    inline for (segments) |segment| {
        switch (segment) {
            .Reg => {
                const regByte = printUtil.take(1)[0];
                try writer.writeByte('r');
                try writer.printInt(regByte, 10, .lower, .{});
            },
            .Immediate8 => {
                const str = printUtil.take(1);
                try formatHexDecNumber(u8, str, writer);
            },
            .Immediate16 => {
                const str = printUtil.take(2);
                try formatHexDecNumber(u16, str, writer);
            },
            .Immediate32 => {
                const str = printUtil.take(4);
                try formatHexDecNumber(u32, str, writer);
            },
            .Immediate64 => {
                const str = printUtil.take(8);
                try formatHexDecNumber(u64, str, writer);
            },
            else => @compileError("Unexpected enum"),
        }
        try writer.writeByte(' ');
    }
}

fn writeHexDecNumberSlice(constStr: []const u8, writer: *Writer) !void {
    switch (constStr.len) {
        1 => try formatHexDecNumber(u8, constStr, writer),
        2 => try formatHexDecNumber(u16, constStr, writer),
        4 => try formatHexDecNumber(u32, constStr, writer),
        8 => try formatHexDecNumber(u64, constStr, writer),
        else => utils.unimplemented(),
    }
}

fn formatHexDecNumber(comptime T: type, str: []const u8, writer: *Writer) !void {
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
