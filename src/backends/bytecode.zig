const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("../blitz.zig");
const codegen = blitz.codegen;
const Context = blitz.context.Context;
const vmInfo = blitz.vmInfo;

pub const backend: codegen.BackendInterface = .{
    .initMetadata = initMetadata,
    .allocateRegisters = allocateRegisters,
};

fn initMetadata(allocator: Allocator, context: *Context) !void {
    // temporary and preserved registers split
    // remaining register space equally
    // (256 - 8) / 2 = 124

    const a = true;
    // const a = false;

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

    try context.genInfo.activeRegisters.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.activeRegisters.items.len = context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.activeRegisters.items, false);

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
        => {},

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

    if (regInfo.spilledUntil) |spilledUntil| {
        if (spilledUntil == instrIndex) {
            regInfo.spilledUntil = null;
            return;
        }
    }

    if (regInfo.lastUsedIndex.? == instrIndex and
        !(regInfo.usage == .Param or regInfo.usage == .ParamNext))
    {
        context.genInfo.activeRegisters.items[regPtr.*] = false;
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
            @memset(context.genInfo.activeRegisters.items[start + 1 .. end], false);
            context.genInfo.activeRegisters.items[0] = true;
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
        const isActive = context.genInfo.activeRegisters.items[index];
        const spillSafe = a: {
            const regInfo = context.genInfo.registers.items[index];
            const vRegInfo = context.genInfo.registers.items[vReg];
            const spilledUntil = regInfo.spilledUntil orelse break :a true;
            const lastUsed = vRegInfo.lastUsedIndex orelse break :a true;
            break :a lastUsed <= spilledUntil;
        };
        if (isActive or !spillSafe) continue;
        context.genInfo.activeRegisters.items[index] = true;
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

    const popInstr = codegen.Instr{
        .Load64AtSpNegOffset16 = .{
            .reg = spillInfo.reg,
            .offset = @intCast(toLocation),
        },
    };

    const startIndex = context.genInfo.registers.items[vReg].firstFoundIndex.?;
    try insertInsertAction(allocator, context, .{ .instr = pushInstr, .pos = startIndex - 1 });
    try insertInsertAction(
        allocator,
        context,
        .{ .instr = popInstr, .pos = spillInfo.furthestUseIndex - 1 },
    );

    context.genInfo.registers.items[spillInfo.reg].spilledUntil = spillInfo.furthestUseIndex - 1;

    return spillInfo.reg;
}

fn insertInsertAction(allocator: Allocator, context: *Context, action: codegen.InsertInfo) !void {
    const actions = context.genInfo.instrActions.insertInstrInfo.action.items;
    if (actions.len == 0 or actions[actions.len - 1].pos <= action.pos) {
        try context.genInfo.instrActions.insertInstrInfo.action.append(allocator, action);
    } else {
        var i: usize = actions.len - 1;
        while (i > 0 and action.pos < actions[i].pos) : (i -= 1) {}
        i += 1;
        try context.genInfo.instrActions.insertInstrInfo.action.insert(allocator, i, action);
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
    var i: usize = fromInstrIndex;
    while (i <= furthestInstrReach) : (i += 1) {
        recordInstrRegUsages(context, i, limits);
    }

    const nextUseItems = context.genInfo.regAllocateUtils.regNextUseIndex.items;
    var furthestUse: u32 = 0;
    var furthestReg: vmInfo.TempRegister = 0;
    for (limits.start..limits.end) |index| {
        if (furthestUse < nextUseItems[index]) {
            furthestUse = nextUseItems[index];
            furthestReg = @intCast(index);
        }
    }

    return .{
        .reg = furthestReg,
        .furthestUseIndex = furthestUse,
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
