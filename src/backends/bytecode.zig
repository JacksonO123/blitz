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
    // context.genInfo.registerLimits.params = .{
    //     .start = 0,
    //     .end = 8,
    // };
    // context.genInfo.registerLimits.temporary = .{
    //     .start = 8,
    //     .end = 8 + 124,
    // };
    // context.genInfo.registerLimits.preserved = .{
    //     .start = 8 + 124,
    //     .end = 8 + 124 + 124,
    // };

    context.genInfo.registerLimits.params = .{
        .start = 0,
        .end = 8,
    };
    context.genInfo.registerLimits.temporary = .{
        .start = 8,
        .end = 11,
    };
    context.genInfo.registerLimits.preserved = .{
        .start = 11,
        .end = 15,
    };

    try context.genInfo.activeRegisters.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.activeRegisters.items.len = context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.activeRegisters.items, false);
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
        .PostPopRegNegOffsetAny,
        .PrePushRegNegOffsetAny,
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
        .PrePushRegNegOffset64, .PostPopRegNegOffset64 => |*inner| {
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
        const reg = try getRegFromUsage(allocator, context, regInfo, regPtr.*, sp);
        regPtr.* = reg;
        regInfo.regRemap = reg;
    }

    if (regInfo.lastUsedInstrIndex.? == instrIndex and
        !(regInfo.usage == .Param or regInfo.usage == .ParamNext))
    {
        context.genInfo.activeRegisters.items[regPtr.*] = false;
    }
}

fn getRegFromUsage(
    allocator: Allocator,
    context: *Context,
    regInfo: *codegen.RegInfo,
    vReg: vmInfo.TempRegister,
    sp: *u64,
) !vmInfo.TempRegister {
    return switch (regInfo.usage) {
        .Temporary => try inactiveRegFromLimits(
            allocator,
            context,
            context.genInfo.registerLimits.temporary,
            vReg,
            sp,
        ),
        .Preserved => try inactiveRegFromLimits(
            allocator,
            context,
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
            context.genInfo.registerLimits.params,
            vReg,
            sp,
        ),
    };
}

fn inactiveRegFromLimits(
    allocator: Allocator,
    context: *Context,
    limits: codegen.RegisterRange,
    vReg: vmInfo.TempRegister,
    sp: *u64,
) !vmInfo.TempRegister {
    for (limits.start..limits.end) |index| {
        if (context.genInfo.activeRegisters.items[index]) continue;
        context.genInfo.activeRegisters.items[index] = true;
        return @intCast(index);
    }

    const regToSpill = getIdealSpillReg(context);

    const toLocation = sp.*;
    sp.* += vmInfo.POINTER_SIZE;

    const pushInstr = codegen.Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = regToSpill,
            .offset = @intCast(toLocation),
        },
    };

    const popInstr = codegen.Instr{
        .Load64AtSpNegOffset16 = .{
            .reg = regToSpill,
            .offset = @intCast(toLocation),
        },
    };

    const startIndex = context.genInfo.registers.items[vReg].firstFoundIndex.?;
    const endIndex = context.genInfo.registers.items[vReg].lastUsedInstrIndex.?;

    try context.genInfo.instrActions.insertInstrInfo.action.append(allocator, .{
        .instr = pushInstr,
        .pos = startIndex - 1,
    });

    try context.genInfo.instrActions.insertInstrInfo.action.append(allocator, .{
        .instr = popInstr,
        .pos = endIndex,
    });

    return regToSpill;
}

fn getIdealSpillReg(context: *Context) vmInfo.TempRegister {
    _ = context;
    return 5;
}
