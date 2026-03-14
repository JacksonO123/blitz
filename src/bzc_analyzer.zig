const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const blitz = @import("blitz.zig");
const codegen = blitz.codegen;
const utils = blitz.utils;
const vmInfo = blitz.vmInfo;
const Context = blitz.context.Context;
const ArrayList = std.ArrayList;

const RegUsedPayload = struct {
    allocator: Allocator,
    list: *ArrayList(vmInfo.TempRegister),
};

const SPACING = "  ";
const NUM_FMT = "{d:<" ++ &[_]u8{(@as(u8, @intCast(SPACING.len)) + '0')} ++ "}";

pub fn analyze(childAllocator: Allocator, context: *Context, writer: *Writer) !void {
    var arena = std.heap.ArenaAllocator.init(childAllocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    context.genInfo.activeRegisters.items.len = @max(
        context.genInfo.registerLimits.preserved.end,
        context.genInfo.registers.items.len,
    );
    @memset(context.genInfo.activeRegisters.items, false);

    const numDigits = utils.getNumberDigitCount(u64, context.genInfo.byteCounter);
    const numInstrLenDigits = utils.getNumberDigitCount(u8, codegen.Instr.maxInstrSize());
    const fmtSettings = blitz.debug.ChunkPrintFmtSettings{
        .numDigits = numDigits,
        .numInstrLenDigits = numInstrLenDigits,
    };

    var str: ArrayList(u8) = .empty;
    var instrRegs: ArrayList(vmInfo.TempRegister) = .empty;
    var byteCounter: usize = vmInfo.VM_INFO_BYTECODE_LEN;
    var totalIndex: usize = 0;

    {
        var i: usize = 0;
        while (i < context.genInfo.registers.items.len) : (i += 1) {
            try str.appendSlice(allocator, "r");
            try str.print(allocator, NUM_FMT, .{i});
        }
        try str.append(allocator, '\n');
    }

    for (context.genInfo.instrList.items, 0..) |*instr, index| {
        if (instr.* == .Label and !context.settings.debug.printLabels) continue;
        if (instr.* == .NoOp and !context.settings.debug.printNoOps) continue;

        const skipped = context.genInfo.handleSkipInstruction(index);
        const printSkippedChunk = !skipped or context.settings.debug.printSkippedInstrs;
        if (printSkippedChunk) {
            const line = try fmtAnalysisLine(
                allocator,
                context,
                instr,
                &instrRegs,
                index,
                fmtSettings,
                &byteCounter,
                totalIndex,
                if (skipped) .Skip else null,
            );
            try str.appendSlice(allocator, line);
        }

        while (context.genInfo.handleInsertInstr(index)) |insertInstr| {
            const insertedLine = try fmtAnalysisLine(
                allocator,
                context,
                insertInstr,
                &instrRegs,
                index,
                fmtSettings,
                &byteCounter,
                totalIndex,
                .Insert,
            );
            try str.appendSlice(allocator, insertedLine);
        }

        if (printSkippedChunk) {
            totalIndex += 1;
        }
    }

    try writer.writeAll(str.items);
    context.genInfo.instrActions.resetPtrs();
}

fn setRegUsed(genInfo: *codegen.GenInfo, reg: vmInfo.TempRegister, payload: *RegUsedPayload) void {
    _ = genInfo;
    payload.list.append(payload.allocator, reg) catch {};
}

fn fmtAnalysisLine(
    allocator: Allocator,
    context: *Context,
    instr: *const codegen.Instr,
    instrRegs: *ArrayList(vmInfo.TempRegister),
    instrIndex: usize,
    fmtSettings: blitz.debug.ChunkPrintFmtSettings,
    byteCounter: *usize,
    totalIndex: usize,
    actionType: ?codegen.InstrActions.ActionTypes,
) ![]const u8 {
    defer instrRegs.clearRetainingCapacity();
    var line: ArrayList(u8) = .empty;

    context.genInfo.setInstrRegActiveStatus(instr, instrIndex);

    var payload = RegUsedPayload{
        .allocator = allocator,
        .list = instrRegs,
    };
    context.genInfo.applyRegIndexFnToInstr(*RegUsedPayload, instr, &payload, setRegUsed);

    var current: usize = 0;
    for (context.genInfo.registers.items, 0..) |regInfo, reg| {
        defer current += 1;
        const lastFound = regInfo.lastUsedIndex orelse continue;

        const found = std.mem.indexOfScalar(
            vmInfo.TempRegister,
            instrRegs.items,
            @intCast(reg),
        ) != null;

        if (context.genInfo.activeRegisters.items[reg] or
            (instrIndex == lastFound and found))
        {
            const slice = if (found) "X" ++ SPACING else "|" ++ SPACING;
            try line.appendSlice(allocator, slice);
        } else {
            try line.appendSlice(allocator, "." ++ SPACING);
        }
    }

    var arrWriter = line.writer(allocator).adaptToNewApi(&[_]u8{});
    const interface = &arrWriter.new_interface;

    byteCounter.* += try blitz.debug.printChunkDetailed(
        instr.*,
        totalIndex,
        byteCounter.*,
        fmtSettings,
        interface,
        actionType,
    );

    return line.items;
}
