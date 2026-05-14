const std = @import("std");
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const blitz = @import("blitz");
const utils = blitz.utils;
const codegen = blitz.codegen;
const vmInfo = blitz.vmInfo;
const print = blitz.print;

const InstrPrintUtil = struct {
    const Self = @This();

    bytes: []const u8,
    currentByte: usize = 0,
    currentInstr: usize = 0,
    currentInstrByte: usize = 0,

    pub fn init(
        bytes: []const u8,
        instrOffset: usize,
    ) InstrPrintUtil {
        return .{
            .bytes = bytes,
            .currentInstrByte = instrOffset,
            .currentByte = instrOffset,
        };
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
    try printVMStartInfo(bytecode[0..vmInfo.BYTECODE_HEADER_LEN], writer);
    const instrStart: u64 = std.mem.readInt(
        u32,
        bytecode[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
        .little,
    );

    try print.printHexViewer(
        bytecode[vmInfo.PADDED_BYTECODE_HEADER_LEN..instrStart],
        writer,
    );

    const byteCountFloat: f64 = @floatFromInt(bytecode.len);
    const numDigits: u64 = @intFromFloat(@floor(@log10(byteCountFloat)) + 1);
    const numInstrLenDigits = utils.getNumberDigitCount(u8, codegen.Instr.maxInstrSize());

    var printUtil = InstrPrintUtil.init(bytecode, instrStart);

    while (printUtil.hasNext()) {
        try printInstrFromSliceUtil(
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
    try writeHexDecNumberSlice(
        info[vmInfo.INSTR_START_PTR_LOCATION .. vmInfo.INSTR_START_PTR_LOCATION + 4],
        writer,
    );
    try writer.writeAll("\nInit stack size: ");
    try writeHexDecNumberSlice(
        info[vmInfo.STACK_START_LOCATION .. vmInfo.STACK_START_LOCATION + 4],
        writer,
    );
    try writer.writeByte('\n');
}

fn printInstrFromSliceUtil(
    printUtil: *InstrPrintUtil,
    numDigits: u64,
    numInstrLenDigits: u8,
    index: usize,
    writer: *Writer,
) !void {
    const instrByte = printUtil.take(1)[0];
    const instrPos = printUtil.currentInstrByte;
    const instr = @as(codegen.InstructionVariants, @enumFromInt(instrByte));

    try writer.printInt(index, 10, .lower, .{ .width = 3, .fill = ' ', .alignment = .right });
    try writer.writeAll(") [");
    try writer.printInt(instrPos, 10, .lower, .{ .width = numDigits, .fill = '.' });
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

        .Add, .Sub, .Mult => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),

        .Add8, .Sub8 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate8 }, writer),
        .Add16, .Sub16 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),
        .Add32, .Sub32 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate32 }, writer),
        .Add64, .Sub64 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate64 }, writer),

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
        => try printSegments(printUtil, .Immediate32, writer),

        .Cmp => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .CmpConst8 => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),

        .IncConst8,
        .DecConst8,
        => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),

        .Mov => try printSegments(printUtil, .{ .Reg, .Reg }, writer),
        .MovSpNegOffset16 => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .MovSpNegOffset32 => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .MovSpNegOffset64 => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),

        .Xor => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),
        .XorConst8 => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate8 }, writer),

        .AddSp8,
        .SubSp8,
        => try printSegments(printUtil, .Immediate8, writer),
        .AddSp16,
        .SubSp16,
        => try printSegments(printUtil, .Immediate16, writer),
        .AddSp32,
        .SubSp32,
        => try printSegments(printUtil, .Immediate32, writer),
        .AddSp64,
        .SubSp64,
        => try printSegments(printUtil, .Immediate64, writer),

        .Store64AtReg,
        .Store32AtReg,
        .Store16AtReg,
        .Store8AtReg,
        => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .Store64AtRegOffset16,
        .Store32AtRegOffset16,
        .Store16AtRegOffset16,
        .Store8AtRegOffset16,
        => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),

        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),

        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        .Load64AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load8AtSpNegOffset16,
        => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),

        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),

        .Load64AtReg,
        .Load32AtReg,
        .Load16AtReg,
        .Load8AtReg,
        => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .Load64AtRegPostInc16,
        .Load32AtRegPostInc16,
        .Load16AtRegPostInc16,
        .Load8AtRegPostInc16,
        => try printSegments(printUtil, .{ .Reg, .Reg, .Immediate16 }, writer),

        .MulReg8AddReg => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Reg, .Immediate8 },
            writer,
        ),
        .MulReg16AddReg => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Reg, .Immediate16 },
            writer,
        ),
        .MulReg32AddReg => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Reg, .Immediate32 },
            writer,
        ),
        .MulReg64AddReg => try printSegments(
            printUtil,
            .{ .Reg, .Reg, .Reg, .Immediate64 },
            writer,
        ),

        .DbgReg => try printSegments(printUtil, .Reg, writer),

        .BitAnd, .BitOr => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),

        .And, .Or => try printSegments(printUtil, .{ .Reg, .Reg }, writer),

        .AndSetReg, .OrSetReg => try printSegments(printUtil, .{ .Reg, .Reg, .Reg }, writer),

        .PrePushRegNegOffset8,
        .PostPopRegNegOffset8,
        => try printSegments(printUtil, .{ .Reg, .Immediate8 }, writer),
        .PrePushRegNegOffset16,
        .PostPopRegNegOffset16,
        => try printSegments(printUtil, .{ .Reg, .Immediate16 }, writer),
        .PrePushRegNegOffset32,
        .PostPopRegNegOffset32,
        => try printSegments(printUtil, .{ .Reg, .Immediate32 }, writer),
        .PrePushRegNegOffset64,
        .PostPopRegNegOffset64,
        => try printSegments(printUtil, .{ .Reg, .Immediate64 }, writer),

        .PrePushLRNegOffset8,
        .PostPopLRNegOffset8,
        => try printSegments(printUtil, .Immediate8, writer),
        .PrePushLRNegOffset16,
        .PostPopLRNegOffset16,
        => try printSegments(printUtil, .Immediate16, writer),
        .PrePushLRNegOffset32,
        .PostPopLRNegOffset32,
        => try printSegments(printUtil, .Immediate32, writer),
        .PrePushLRNegOffset64,
        .PostPopLRNegOffset64,
        => try printSegments(printUtil, .Immediate64, writer),

        .BranchLink, .BranchLinkBack => try printSegments(printUtil, .Immediate32, writer),
    }

    try writer.writeByte('\n');
}

fn printSegments(
    printUtil: *InstrPrintUtil,
    comptime segments: anytype,
    writer: *Writer,
) !void {
    const SegmentsType = @TypeOf(segments);
    const info = @typeInfo(SegmentsType);

    switch (info) {
        .enum_literal => try printSegment(printUtil, segments, writer),
        .@"struct" => {
            inline for (segments) |segment| {
                try printSegment(printUtil, segment, writer);
            }
        },
        else => @compileError("Unsupported type"),
    }
}

fn printSegment(printUtil: *InstrPrintUtil, comptime segment: anytype, writer: *Writer) !void {
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
    try print.writeHexDecNumber(T, num, writer);
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
    try print.writeHexNumber(T, num, writer);
}
