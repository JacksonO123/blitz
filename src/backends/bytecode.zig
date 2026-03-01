const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("../blitz.zig");
const codegen = blitz.codegen;
const Context = blitz.context.Context;
const vmInfo = blitz.vmInfo;

pub fn begin(allocator: Allocator, context: *Context) !void {
    // temporary and preserved registers split
    // remaining register space equally
    // (256 - 8) / 2 = 124
    const paramLimits: codegen.RegisterRange = .{
        .start = 0,
        .end = 8,
    };
    context.genInfo.registerLimits.params = paramLimits;
    context.genInfo.registerLimits.temporary = .{
        .start = 8,
        .end = 8 + 124,
    };
    context.genInfo.registerLimits.preserved = .{
        .start = 8 + 124,
        .end = 8 + 124 + 124,
    };

    try context.genInfo.activeRegisters.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.activeRegisters.items.len = context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.activeRegisters.items, false);

    try allocateRegisters(allocator, context);
}

fn allocateRegisters(allocator: Allocator, context: *Context) !void {
    var i: usize = 0;
    while (i < context.genInfo.instrList.items.len) : (i += 1) {
        try remapInstr(allocator, context, i);
    }
}

fn remapInstr(allocator: Allocator, context: *Context, instrIndex: usize) !void {
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

        .SetReg64 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .SetReg32 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .SetReg16 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .SetReg8 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .Add, .Sub, .Mult => |*inner| {
            remapReg(context, &inner.reg1, instrIndex);
            remapReg(context, &inner.reg2, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .Add8, .Sub8 => |*inner| {
            remapReg(context, &inner.reg, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .Add16, .Sub16 => |*inner| {
            remapReg(context, &inner.reg, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .Cmp => |*inner| {
            remapReg(context, &inner.reg1, instrIndex);
            remapReg(context, &inner.reg2, instrIndex);
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        => |*inner| {
            remapReg(context, &inner.reg1, instrIndex);
            remapReg(context, &inner.reg2, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .CmpConst8, .IncConst8, .DecConst8 => |*inner| {
            remapReg(context, &inner.reg, instrIndex);
        },
        .Mov => |*inner| {
            remapReg(context, &inner.src, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .MovSpNegOffset16 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .MovSpNegOffset32 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .MovSpNegOffset64 => |*inner| remapReg(context, &inner.reg, instrIndex),
        .Xor, .BitAnd, .BitOr, .AndSetReg, .OrSetReg => |*inner| {
            remapReg(context, &inner.reg1, instrIndex);
            remapReg(context, &inner.reg2, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .XorConst8 => |*inner| {
            remapReg(context, &inner.reg, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .Store64AtReg, .Store32AtReg, .Store16AtReg, .Store8AtReg => |*inner| {
            remapReg(context, &inner.fromReg, instrIndex);
            remapReg(context, &inner.toRegPtr, instrIndex);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |*inner| {
            remapReg(context, &inner.fromReg, instrIndex);
            remapReg(context, &inner.toRegPtr, instrIndex);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        => |*inner| {
            remapReg(context, &inner.reg, instrIndex);
        },
        .Load64AtRegOffset16,
        .Load32AtRegOffset16,
        .Load16AtRegOffset16,
        .Load8AtRegOffset16,
        => |*inner| {
            remapReg(context, &inner.fromRegPtr, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |*inner| {
            remapReg(context, &inner.fromRegPtr, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .MulReg16AddReg => |*inner| {
            remapReg(context, &inner.addReg, instrIndex);
            remapReg(context, &inner.mulReg, instrIndex);
            remapReg(context, &inner.dest, instrIndex);
        },
        .And, .Or => |*inner| {
            remapReg(context, &inner.reg1, instrIndex);
            remapReg(context, &inner.reg2, instrIndex);
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

        .PostPopRegNegOffsetAny, .PrePushRegNegOffsetAny, .MovSpNegOffsetAny => unreachable,
    }

    try handleMaybeSkipInstruction(context, allocator, instrIndex);
}

fn handleMaybeSkipInstruction(context: *Context, allocator: Allocator, instrIndex: usize) !void {
    const instr = context.genInfo.instrList.items[instrIndex];
    switch (instr) {
        .Mov => |inner| {
            if (inner.src == inner.dest) {
                try context.genInfo.skipInstrInfo.action.append(allocator, @intCast(instrIndex));
            }
        },
        else => {},
    }
}

fn remapReg(context: *Context, regPtr: *vmInfo.TempRegister, instrIndex: usize) void {
    const regInfo = context.genInfo.registers.items[regPtr.*];
    if (regInfo.regRemap) |remap| {
        regPtr.* = remap;
    } else {
        const reg = getRegFromUsage(context, regInfo);
        regPtr.* = reg;
        regInfo.regRemap = reg;
    }

    if (regInfo.lastUsedInstrIndex.? == instrIndex) {
        context.genInfo.activeRegisters.items[regPtr.*] = false;
    }
}

fn getRegFromUsage(context: *Context, regInfo: *codegen.RegInfo) vmInfo.TempRegister {
    return switch (regInfo.usage) {
        .Temporary => inactiveRegFromLimits(context, context.genInfo.registerLimits.temporary),
        .Preserved => inactiveRegFromLimits(context, context.genInfo.registerLimits.preserved),
        .Param, .Return => {
            const start = context.genInfo.registerLimits.params.start;
            const end = context.genInfo.registerLimits.params.end;
            @memset(context.genInfo.activeRegisters.items[start + 1 .. end], false);
            context.genInfo.activeRegisters.items[0] = true;
            return 0;
        },
        .ParamNext, .ReturnNext => inactiveRegFromLimits(context, context.genInfo.registerLimits.params),
    };
}

fn inactiveRegFromLimits(context: *Context, limits: codegen.RegisterRange) vmInfo.TempRegister {
    for (limits.start..limits.end) |index| {
        if (context.genInfo.activeRegisters.items[index]) continue;
        context.genInfo.activeRegisters.items[index] = true;
        return @intCast(index);
    }
    unreachable;
}
