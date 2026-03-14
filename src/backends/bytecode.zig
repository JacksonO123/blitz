const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("../blitz.zig");
const codegen = blitz.codegen;
const utils = blitz.utils;
const Context = blitz.context.Context;
const vmInfo = blitz.vmInfo;

pub const backend: codegen.BackendInterface = .{
    .initMetadata = initMetadata,
    .allocateRegisters = allocateRegisters,
};

const InsertionType = enum {
    Prepend,
    Append,
};

const SpillStateVariants = enum {
    UseReg,
    UseStack,
};

const AllocatedRegInfo = struct {
    reg: vmInfo.TempRegister,
    state: codegen.RegStateInfo,
};

fn initMetadata(allocator: Allocator, context: *Context) !void {
    // temporary and preserved registers split
    // remaining register space equally
    // (256 - 8) / 2 = 124

    // const a = true;
    const a = false;

    if (a) {
        context.genInfo.registerLimits.params = .{
            .start = 0,
            .end = 8,
        };
        context.genInfo.registerLimits.temporary = .{
            .start = 8,
            .end = 8 + 124,
        };
        context.genInfo.registerLimits.preserved = .{
            .start = 8 + 124,
            .end = 8 + 124 + 124,
        };
    } else {
        context.genInfo.registerLimits.params = .{
            .start = 0,
            .end = 8,
        };
        context.genInfo.registerLimits.temporary = .{
            .start = 8,
            .end = 13,
        };
        context.genInfo.registerLimits.preserved = .{
            .start = 13,
            .end = 15,
        };
    }

    try context.genInfo.registerStatus.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.registerStatus.items.len = context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.registerStatus.items, .{});

    try context.genInfo.regAllocateUtils.regNextUseIndex.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.regAllocateUtils.regNextUseIndex.items.len =
        context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.regAllocateUtils.regNextUseIndex.items, 0);
}

fn allocateRegisters(
    allocator: Allocator,
    context: *Context,
    instrs: []codegen.Instr,
    baseIndex: usize,
    sp: *u64,
) !void {
    var i: usize = 0;
    while (i < instrs.len) : (i += 1) {
        try remapInstr(allocator, context, i + baseIndex, sp);
    }
}

fn remapInstr(allocator: Allocator, context: *Context, instrIndex: usize, sp: *u64) !void {
    const instr = &context.genInfo.instrList.items[instrIndex];
    switch (instr.*) {
        .NoOp,
        .Label,
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
        .AddSp8,
        .SubSp8,
        .AddSp16,
        .SubSp16,
        .AddSp32,
        .SubSp32,
        .AddSp64,
        .SubSp64,
        .Ret,
        .End,
        .BranchLink,
        .BranchLinkBack,
        .PrePushLRNegOffsetAny,
        .PrePushLRNegOffset8,
        .PrePushLRNegOffset16,
        .PrePushLRNegOffset32,
        .PrePushLRNegOffset64,
        .PostPopLRNegOffsetAny,
        .PostPopLRNegOffset8,
        .PostPopLRNegOffset16,
        .PostPopLRNegOffset32,
        .PostPopLRNegOffset64,
        => {},

        .DbgReg => |*inner| try remapReg(allocator, context, inner, instrIndex, sp),

        .SetReg64 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .SetReg32 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .SetReg16 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .SetReg8 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .Add, .Sub, .Mult => |*inner| {
            try remapReg(allocator, context, &inner.reg1, instrIndex, sp);
            try remapReg(allocator, context, &inner.reg2, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Add8, .Sub8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Add16, .Sub16 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Cmp => |*inner| {
            try remapReg(allocator, context, &inner.reg1, instrIndex, sp);
            try remapReg(allocator, context, &inner.reg2, instrIndex, sp);
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        => |*inner| {
            try remapReg(allocator, context, &inner.reg1, instrIndex, sp);
            try remapReg(allocator, context, &inner.reg2, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .CmpConst8, .IncConst8, .DecConst8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
        },
        .Mov => |*inner| {
            try remapReg(allocator, context, &inner.src, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .MovSpNegOffset16 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .MovSpNegOffset32 => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .MovSpNegOffset64,
        .MovSpNegOffsetAny,
        => |*inner| try remapReg(allocator, context, &inner.reg, instrIndex, sp),
        .Xor, .BitAnd, .BitOr, .AndSetReg, .OrSetReg => |*inner| {
            try remapReg(allocator, context, &inner.reg1, instrIndex, sp);
            try remapReg(allocator, context, &inner.reg2, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .XorConst8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Store64AtReg, .Store32AtReg, .Store16AtReg, .Store8AtReg => |*inner| {
            try remapReg(allocator, context, &inner.fromReg, instrIndex, sp);
            try remapReg(allocator, context, &inner.toRegPtr, instrIndex, sp);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |*inner| {
            try remapReg(allocator, context, &inner.fromReg, instrIndex, sp);
            try remapReg(allocator, context, &inner.toRegPtr, instrIndex, sp);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        .Load64AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load8AtSpNegOffset16,
        => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => |*inner| {
            try remapReg(allocator, context, &inner.fromRegPtr, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |*inner| {
            try remapReg(allocator, context, &inner.fromRegPtr, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .MulReg16AddReg => |*inner| {
            try remapReg(allocator, context, &inner.addReg, instrIndex, sp);
            try remapReg(allocator, context, &inner.mulReg, instrIndex, sp);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .And, .Or => |*inner| {
            try remapReg(allocator, context, &inner.reg1, instrIndex, sp);
            try remapReg(allocator, context, &inner.reg2, instrIndex, sp);
        },
        .PrePushRegNegOffset8, .PostPopRegNegOffset8 => |*inner| {
            inner.reg += context.genInfo.registerLimits.preserved.start;
        },
        .PrePushRegNegOffset16, .PostPopRegNegOffset16 => |*inner| {
            inner.reg += context.genInfo.registerLimits.preserved.start;
        },
        .PrePushRegNegOffset32, .PostPopRegNegOffset32 => |*inner| {
            inner.reg += context.genInfo.registerLimits.preserved.start;
        },
        .PrePushRegNegOffset64,
        .PostPopRegNegOffset64,
        .PostPopRegNegOffsetAny,
        .PrePushRegNegOffsetAny,
        => |*inner| {
            inner.reg += context.genInfo.registerLimits.preserved.start;
        },
    }

    try handleMaybeSkipInstruction(context, allocator, instrIndex);
}

fn handleMaybeSkipInstruction(context: *Context, allocator: Allocator, instrIndex: usize) !void {
    const instr = context.genInfo.instrList.items[instrIndex];
    switch (instr) {
        .Mov => |inner| {
            if (inner.src == inner.dest) {
                try context.genInfo.instrActions.skipInstrInfo.action.append(
                    allocator,
                    @intCast(instrIndex),
                );
            }
        },
        else => {},
    }
}

fn remapReg(
    allocator: Allocator,
    context: *Context,
    regPtr: *vmInfo.TempRegister,
    instrIndex: usize,
    sp: *u64,
) !void {
    const regInfo = context.genInfo.registers.items[regPtr.*];

    if (regInfo.regRemap) |remap| {
        regPtr.* = remap;
    } else {
        const reg = try getRegFromUsage(allocator, context, instrIndex, regInfo, regPtr.*, sp);
        regPtr.* = reg;
        regInfo.regRemap = reg;
    }

    context.genInfo.regAllocateUtils.furthestInstrReach = @max(
        context.genInfo.regAllocateUtils.furthestInstrReach,
        regInfo.lastUsedIndex.?,
    );

    if (regInfo.lastUsedIndex.? == instrIndex and
        !(regInfo.usage == .Param or regInfo.usage == .ParamNext))
    {
        context.genInfo.registerStatus.items[regPtr.*].active = false;
    }
}

fn getRegFromUsage(
    allocator: Allocator,
    context: *Context,
    instrIndex: usize,
    regInfo: *codegen.RegInfo,
    vReg: vmInfo.TempRegister,
    sp: *u64,
) !vmInfo.TempRegister {
    return switch (regInfo.usage) {
        .Temporary => try inactiveRegFromLimits(
            allocator,
            context,
            instrIndex,
            context.genInfo.registerLimits.temporary,
            vReg,
            sp,
        ),
        .Preserved => try inactiveRegFromLimits(
            allocator,
            context,
            instrIndex,
            context.genInfo.registerLimits.preserved,
            vReg,
            sp,
        ),
        .Param, .Return => {
            const start = context.genInfo.registerLimits.params.start;
            const end = context.genInfo.registerLimits.params.end;
            @memset(context.genInfo.registerStatus.items[start + 1 .. end], .{});
            context.genInfo.registerStatus.items[0].active = true;
            return 0;
        },
        .ParamNext, .ReturnNext => try inactiveRegFromLimits(
            allocator,
            context,
            instrIndex,
            context.genInfo.registerLimits.params,
            vReg,
            sp,
        ),
    };
}

fn inactiveRegFromLimits(
    allocator: Allocator,
    context: *Context,
    instrIndex: usize,
    limits: codegen.RegisterRange,
    vReg: vmInfo.TempRegister,
    sp: *u64,
) !vmInfo.TempRegister {
    for (limits.start..limits.end) |index| {
        const status = context.genInfo.registerStatus.items[index];
        if (status.active) continue;

        context.genInfo.registerStatus.items[index].active = true;
        return @intCast(index);
    }

    const spillInfo = getIdealSpillReg(context, limits, instrIndex);

    const toLocation = sp.*;
    sp.* += vmInfo.POINTER_SIZE;

    const pushInstr = codegen.Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = spillInfo.reg,
            .offset = @intCast(toLocation),
        },
    };

    const startIndex = context.genInfo.registers.items[vReg].firstFoundIndex.?;
    try insertInsertAction(allocator, context, .{
        .instr = pushInstr,
        .pos = startIndex - 1,
    });

    return spillInfo.reg;
}

fn insertInsertAction(allocator: Allocator, context: *Context, action: codegen.InsertInfo) !void {
    try insertInsertActionUtil(allocator, context, action, .Append);
}

fn insertInsertActionUtil(
    allocator: Allocator,
    context: *Context,
    action: codegen.InsertInfo,
    insertionType: InsertionType,
) !void {
    const actions = context.genInfo.instrActions.insertInstrInfo.action.items;

    if (actions.len == 0 or actions[actions.len - 1].pos < action.pos) {
        try context.genInfo.instrActions.insertInstrInfo.action.append(allocator, action);
    } else {
        var i: usize = actions.len - 1;
        switch (insertionType) {
            .Append => {
                while (i > 0 and action.pos < actions[i].pos) : (i -= 1) {}
            },
            .Prepend => {
                while (i > 0 and action.pos <= actions[i].pos) : (i -= 1) {}
            },
        }
        i += 1;
        try context.genInfo.instrActions.insertInstrInfo.action.insert(
            allocator,
            i,
            action,
        );
    }
}

fn getIdealSpillReg(
    context: *Context,
    limits: codegen.RegisterRange,
    fromInstrIndex: usize,
) struct {
    reg: vmInfo.TempRegister,
    furthestUseIndex: u32,
} {
    @memset(context.genInfo.regAllocateUtils.regNextUseIndex.items, 0);

    const furthestInstrReach = context.genInfo.regAllocateUtils.furthestInstrReach;
    var i: usize = fromInstrIndex + 1;
    while (i <= furthestInstrReach) : (i += 1) {
        recordInstrRegUsages(context, i, limits);
    }

    const nextUseItems = context.genInfo.regAllocateUtils.regNextUseIndex.items;
    var furthestNextUse: u32 = 0;
    var furthestNextUseReg: vmInfo.TempRegister = 0;
    for (limits.start..limits.end) |index| {
        if (furthestNextUse < nextUseItems[index]) {
            furthestNextUse = nextUseItems[index];
            furthestNextUseReg = @intCast(index);
        }
    }

    return .{
        .reg = furthestNextUseReg,
        .furthestUseIndex = furthestNextUse,
    };
}

fn recordInstrRegUsages(context: *Context, instrIndex: usize, limits: codegen.RegisterRange) void {
    const instr = context.genInfo.instrList.items[instrIndex];

    switch (instr) {
        .NoOp,
        .Label,
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
        .AddSp8,
        .SubSp8,
        .AddSp16,
        .SubSp16,
        .AddSp32,
        .SubSp32,
        .AddSp64,
        .SubSp64,
        .DbgReg,
        .Ret,
        .End,
        .BranchLink,
        .BranchLinkBack,
        .PrePushLRNegOffsetAny,
        .PrePushLRNegOffset8,
        .PrePushLRNegOffset16,
        .PrePushLRNegOffset32,
        .PrePushLRNegOffset64,
        .PostPopLRNegOffsetAny,
        .PostPopLRNegOffset8,
        .PostPopLRNegOffset16,
        .PostPopLRNegOffset32,
        .PostPopLRNegOffset64,
        .PrePushRegNegOffset8,
        .PostPopRegNegOffset8,
        .PrePushRegNegOffset16,
        .PostPopRegNegOffset16,
        .PrePushRegNegOffset32,
        .PostPopRegNegOffset32,
        .PrePushRegNegOffset64,
        .PrePushRegNegOffsetAny,
        .PostPopRegNegOffset64,
        .PostPopRegNegOffsetAny,
        => {},

        .SetReg64 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .SetReg32 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .SetReg16 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .SetReg8 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .Add, .Sub, .Mult => |*inner| {
            recordNextUsage(context, inner.reg1, instrIndex, limits);
            recordNextUsage(context, inner.reg2, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .Add8, .Sub8 => |*inner| {
            recordNextUsage(context, inner.reg, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .Add16, .Sub16 => |*inner| {
            recordNextUsage(context, inner.reg, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .Cmp => |*inner| {
            recordNextUsage(context, inner.reg1, instrIndex, limits);
            recordNextUsage(context, inner.reg2, instrIndex, limits);
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        => |*inner| {
            recordNextUsage(context, inner.reg1, instrIndex, limits);
            recordNextUsage(context, inner.reg2, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .CmpConst8, .IncConst8, .DecConst8 => |*inner| {
            recordNextUsage(context, inner.reg, instrIndex, limits);
        },
        .Mov => |*inner| {
            recordNextUsage(context, inner.src, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .MovSpNegOffset16 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .MovSpNegOffset32 => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .MovSpNegOffset64,
        .MovSpNegOffsetAny,
        => |*inner| recordNextUsage(context, inner.reg, instrIndex, limits),
        .Xor, .BitAnd, .BitOr, .AndSetReg, .OrSetReg => |*inner| {
            recordNextUsage(context, inner.reg1, instrIndex, limits);
            recordNextUsage(context, inner.reg2, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .XorConst8 => |*inner| {
            recordNextUsage(context, inner.reg, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .Store64AtReg, .Store32AtReg, .Store16AtReg, .Store8AtReg => |*inner| {
            recordNextUsage(context, inner.fromReg, instrIndex, limits);
            recordNextUsage(context, inner.toRegPtr, instrIndex, limits);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |*inner| {
            recordNextUsage(context, inner.fromReg, instrIndex, limits);
            recordNextUsage(context, inner.toRegPtr, instrIndex, limits);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        .Load64AtSpNegOffset16,
        .Load32AtSpNegOffset16,
        .Load16AtSpNegOffset16,
        .Load8AtSpNegOffset16,
        => |*inner| {
            recordNextUsage(context, inner.reg, instrIndex, limits);
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => |*inner| {
            recordNextUsage(context, inner.fromRegPtr, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |*inner| {
            recordNextUsage(context, inner.fromRegPtr, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .MulReg16AddReg => |*inner| {
            recordNextUsage(context, inner.addReg, instrIndex, limits);
            recordNextUsage(context, inner.mulReg, instrIndex, limits);
            recordNextUsage(context, inner.dest, instrIndex, limits);
        },
        .And, .Or => |*inner| {
            recordNextUsage(context, inner.reg1, instrIndex, limits);
            recordNextUsage(context, inner.reg2, instrIndex, limits);
        },
    }
}

fn recordNextUsage(
    context: *Context,
    vReg: vmInfo.TempRegister,
    instrIndex: usize,
    limits: codegen.RegisterRange,
) void {
    const regInfo = context.genInfo.registers.items[vReg];
    const reg = regInfo.regRemap orelse return;

    if (reg >= limits.end or reg < limits.start) return;

    if (context.genInfo.regAllocateUtils.regNextUseIndex.items[reg] == 0) {
        context.genInfo.regAllocateUtils.regNextUseIndex.items[reg] = @intCast(instrIndex);
    }
}

test "remap reg" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = &[_]codegen.Instr{
        codegen.Instr{
            .SetReg32 = .{
                .reg = 0,
                .data = 1,
            },
        },
        codegen.Instr{
            .Mov = .{
                .dest = 1,
                .src = 0,
            },
        },
        codegen.Instr{
            .SetReg32 = .{
                .reg = 2,
                .data = 2,
            },
        },
        codegen.Instr{
            .Add = .{
                .dest = 3,
                .reg1 = 1,
                .reg2 = 2,
            },
        },
    };
    const instrsPtr = try blitz.utils.createMut(std.ArrayList(codegen.Instr), allocator, .empty);
    try instrsPtr.appendSlice(allocator, instrs);

    var stdout = std.fs.File.stdout().writer(&[_]u8{});
    defer stdout.end() catch {};
    const writer = &stdout.interface;
    defer writer.flush() catch {};

    var context = try Context.init(allocator, "", writer, .{});

    try initMetadata(allocator, &context);

    context.genInfo.instrList = instrsPtr;
    try context.genInfo.registers.ensureTotalCapacityPrecise(allocator, instrs.len);
    context.genInfo.registers.items.len = instrs.len;
    const info1 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .firstFoundIndex = 0,
        .lastUsedIndex = 1,
    });
    context.genInfo.registers.items[0] = info1;
    const info11 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .firstFoundIndex = 1,
        .lastUsedIndex = 3,
    });
    context.genInfo.registers.items[1] = info11;
    const info2 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .firstFoundIndex = 2,
        .lastUsedIndex = 3,
    });
    context.genInfo.registers.items[2] = info2;
    const info3 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .firstFoundIndex = 3,
        .lastUsedIndex = 3,
    });
    context.genInfo.registers.items[3] = info3;

    var sp: u64 = 0;

    var t0 = context.genInfo.instrList.items[0].SetReg32.reg;
    var t01 = context.genInfo.instrList.items[1].Mov.src;
    var t02 = context.genInfo.instrList.items[1].Mov.dest;
    var t1 = context.genInfo.instrList.items[2].SetReg32.reg;
    var t2 = context.genInfo.instrList.items[3].Add.reg2;
    var t3 = context.genInfo.instrList.items[3].Add.reg1;
    var t4 = context.genInfo.instrList.items[3].Add.dest;

    try remapReg(allocator, &context, &t0, 0, &sp);
    try remapReg(allocator, &context, &t01, 1, &sp);
    try remapReg(allocator, &context, &t02, 1, &sp);
    try remapReg(allocator, &context, &t1, 2, &sp);
    try remapReg(allocator, &context, &t2, 3, &sp);
    try remapReg(allocator, &context, &t3, 3, &sp);
    try remapReg(allocator, &context, &t4, 3, &sp);

    try std.testing.expectEqual(8, t0);
    try std.testing.expectEqual(8, t01);
    try std.testing.expectEqual(8, t02);
    try std.testing.expectEqual(9, t1);
    try std.testing.expectEqual(9, t2);
    try std.testing.expectEqual(8, t3);
    try std.testing.expectEqual(8, t4);
}
