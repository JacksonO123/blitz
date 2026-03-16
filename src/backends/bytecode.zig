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

const SpillState = struct {
    reg: vmInfo.TempRegister,
    furthestUseIndex: u32,
};

const AllocatedRegInfo = struct {
    reg: vmInfo.TempRegister,
    state: codegen.RegStateInfo,
};

const InactiveRegResultVariants = enum {
    Normal,
    Spilled,
};

const InactiveRegResult = union(InactiveRegResultVariants) {
    Normal: vmInfo.TempRegister,
    Spilled: SpillState,
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
            .end = 11,
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

fn flushPendingDeactivations(context: *Context) void {
    for (context.genInfo.regAllocateUtils.pendingDeactivations.getSliceFromStart()) |reg| {
        context.genInfo.registerStatus.items[reg].active = false;
    }
    context.genInfo.regAllocateUtils.pendingDeactivations.clear();
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
}

fn remapInstr(allocator: Allocator, context: *Context, instrIndex: usize, sp: *u64) !void {
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
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
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Add8, .Sub8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Add16, .Sub16 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            flushPendingDeactivations(context);
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
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .CmpConst8, .IncConst8, .DecConst8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
        },
        .Mov => |*inner| {
            try remapReg(allocator, context, &inner.src, instrIndex, sp);
            flushPendingDeactivations(context);
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
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .XorConst8 => |*inner| {
            try remapReg(allocator, context, &inner.reg, instrIndex, sp);
            flushPendingDeactivations(context);
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
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |*inner| {
            try remapReg(allocator, context, &inner.fromRegPtr, instrIndex, sp);
            flushPendingDeactivations(context);
            try remapReg(allocator, context, &inner.dest, instrIndex, sp);
        },
        .MulReg16AddReg => |*inner| {
            try remapReg(allocator, context, &inner.addReg, instrIndex, sp);
            try remapReg(allocator, context, &inner.mulReg, instrIndex, sp);
            flushPendingDeactivations(context);
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

    flushPendingDeactivations(context);
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
    const currentVReg = regPtr.*;
    const regInfo = context.genInfo.registers.items[currentVReg];

    switch (regInfo.regRemap) {
        .Unused => {
            regPtr.* = try handleNewRegRemap(allocator, context, regInfo, regPtr, instrIndex, sp);
        },
        .Normal => |remap| {
            const regStatus = &context.genInfo.registerStatus.items[remap];
            var remapState = &regStatus.state;

            if (remapState.info == .Spilled and regStatus.active) {
                var stateIter: *codegen.RegState = remapState;
                while (stateIter.prevState) |prev| {
                    if (prev.info != .Spilled) break;
                    stateIter = prev;
                }

                const reg = try handleNewRegRemap(
                    allocator,
                    context,
                    regInfo,
                    regPtr,
                    instrIndex,
                    sp,
                );
                const spillLocation = stateIter.info.Spilled.location;

                const loadInstr = codegen.Instr{
                    .Load64AtSpNegOffset16 = .{
                        .reg = reg,
                        .offset = @intCast(spillLocation),
                    },
                };
                try insertInsertAction(allocator, context, .{
                    .instr = loadInstr,
                    .pos = @intCast(instrIndex - 1),
                });

                regPtr.* = reg;
            } else {
                while (remapState.info != .Normal) {
                    switch (remapState.info) {
                        .Unused, .Normal => break,
                        .Spilled => |*info| {
                            try handleStoreSpilledReg(
                                allocator,
                                context,
                                regInfo,
                                instrIndex,
                                info.*,
                                sp,
                            );

                            const popInstr = codegen.Instr{
                                .Load64AtSpNegOffset16 = .{
                                    .reg = info.reg,
                                    .offset = @intCast(info.location),
                                },
                            };
                            try insertInsertAction(allocator, context, .{
                                .instr = popInstr,
                                .pos = @intCast(instrIndex - 1),
                            });

                            context.genInfo.registerStatus.items[remap].state =
                                remapState.prevState.?.*;
                        },
                    }
                }

                regPtr.* = remap;
            }
        },
        .Spilled => |spillInfo| a: {
            const regStatus = &context.genInfo.registerStatus.items[spillInfo.reg];
            var remapState = &regStatus.state;

            if (remapState.info == .Spilled and regStatus.active and
                remapState.info.Spilled.by != spillInfo.byVReg)
            {
                var location: ?u64 = null;
                var stateIter: *codegen.RegState = remapState;
                while (stateIter.info == .Spilled) {
                    if (stateIter.info.Spilled.by == spillInfo.byVReg) {
                        break;
                    }
                    location = stateIter.info.Spilled.location;
                    stateIter = stateIter.prevState.?;
                }

                const newReg = if (location) |loc| b: {
                    const reg = try handleNewRegRemap(
                        allocator,
                        context,
                        regInfo,
                        regPtr,
                        instrIndex,
                        sp,
                    );

                    const loadInstr = codegen.Instr{
                        .Load64AtSpNegOffset16 = .{
                            .reg = reg,
                            .offset = @intCast(loc),
                        },
                    };
                    try insertInsertAction(allocator, context, .{
                        .instr = loadInstr,
                        .pos = @intCast(instrIndex - 1),
                    });

                    break :b reg;
                } else spillInfo.reg;

                regPtr.* = newReg;

                break :a;
            }

            while (remapState.info != .Normal) {
                switch (remapState.info) {
                    .Unused, .Normal => unreachable,
                    .Spilled => |*info| {
                        if (info.by != spillInfo.byVReg) {
                            try handleStoreSpilledReg(
                                allocator,
                                context,
                                regInfo,
                                instrIndex,
                                info.*,
                                sp,
                            );

                            const popInstr = codegen.Instr{
                                .Load64AtSpNegOffset16 = .{
                                    .reg = info.reg,
                                    .offset = @intCast(info.location),
                                },
                            };
                            try insertInsertAction(allocator, context, .{
                                .instr = popInstr,
                                .pos = @intCast(instrIndex - 1),
                            });

                            context.genInfo.registerStatus.items[spillInfo.reg].state =
                                remapState.prevState.?.*;
                        } else {
                            break;
                        }
                    },
                }
            }

            regPtr.* = spillInfo.reg;
        },
        .Stored => |storedInfo| {
            const reg = try handleNewRegRemap(allocator, context, regInfo, regPtr, instrIndex, sp);

            const loadInstr = codegen.Instr{
                .Load64AtSpNegOffset16 = .{
                    .reg = reg,
                    .offset = @intCast(storedInfo.location),
                },
            };

            try insertInsertAction(allocator, context, .{
                .instr = loadInstr,
                .pos = @intCast(instrIndex - 1),
            });

            regPtr.* = reg;
        },
    }

    context.genInfo.regAllocateUtils.protectedRegisters.push(regPtr.*);

    regInfo.useIndices.baseNext();
    const lastUseIndex = regInfo.useIndices.last();

    context.genInfo.regAllocateUtils.furthestInstrReach = @max(
        context.genInfo.regAllocateUtils.furthestInstrReach,
        lastUseIndex,
    );

    if (lastUseIndex == instrIndex and !regInfo.usage.isParam()) {
        context.genInfo.regAllocateUtils.pendingDeactivations.push(regPtr.*);
    }
}

fn handleStoreSpilledReg(
    allocator: Allocator,
    context: *Context,
    regInfo: *codegen.RegInfo,
    instrIndex: usize,
    spillInfo: codegen.RegStateSpilled,
    sp: *u64,
) !void {
    _ = regInfo;
    const spilledByRegInfo = context.genInfo.registers.items[spillInfo.by];
    const nextUseOrNull = spilledByRegInfo.useIndices.current();

    if (nextUseOrNull == null) return;

    const storeLocation = sp.*;
    sp.* += vmInfo.POINTER_SIZE;

    const storeInstr = codegen.Instr{
        .Store64AtSpNegOffset16 = .{
            .reg = spillInfo.reg,
            .offset = @intCast(storeLocation),
        },
    };
    try insertInsertAction(allocator, context, .{
        .instr = storeInstr,
        .pos = @intCast(instrIndex - 1),
    });

    spilledByRegInfo.regRemap = .{
        .Stored = .{
            .byVReg = spillInfo.by,
            .location = storeLocation,
        },
    };
}

fn handleNewRegRemap(
    allocator: Allocator,
    context: *Context,
    regInfo: *codegen.RegInfo,
    regPtr: *vmInfo.TempRegister,
    instrIndex: usize,
    sp: *u64,
) !vmInfo.TempRegister {
    const currentVReg = regPtr.*;
    const regResult = try getRegFromUsage(context, instrIndex, regInfo);

    switch (regResult) {
        .Normal => |reg| {
            const resRegState = &context.genInfo.registerStatus.items[reg].state.info;

            switch (resRegState.*) {
                .Unused => {
                    resRegState.* = .{
                        .Normal = reg,
                    };

                    context.genInfo.registers.items[currentVReg].regRemap = .{
                        .Normal = reg,
                    };
                },
                .Normal => {
                    context.genInfo.registers.items[currentVReg].regRemap = .{
                        .Normal = reg,
                    };
                },
                .Spilled => |*spillInfo| {
                    spillInfo.by = currentVReg;
                    context.genInfo.registers.items[currentVReg].regRemap = .{
                        .Spilled = .{
                            .reg = reg,
                            .byVReg = currentVReg,
                        },
                    };
                },
            }

            return reg;
        },
        .Spilled => |info| {
            const toLocation = sp.*;
            sp.* += vmInfo.POINTER_SIZE;

            const pushInstr = codegen.Instr{
                .Store64AtSpNegOffset16 = .{
                    .reg = info.reg,
                    .offset = @intCast(toLocation),
                },
            };

            try insertInsertAction(allocator, context, .{
                .instr = pushInstr,
                .pos = @intCast(instrIndex - 1),
            });

            const currentState = context.genInfo.registerStatus.items[info.reg].state;
            const newState = codegen.RegState{
                .prevState = try utils.createMut(codegen.RegState, allocator, currentState),
                .info = .{
                    .Spilled = .{
                        .by = currentVReg,
                        .reg = info.reg,
                        .until = info.furthestUseIndex,
                        .location = toLocation,
                    },
                },
            };
            context.genInfo.registerStatus.items[info.reg].state = newState;

            regInfo.regRemap = .{
                .Spilled = .{
                    .reg = info.reg,
                    .byVReg = currentVReg,
                },
            };

            return info.reg;
        },
    }
}

fn getRegLimitsFromUsage(context: *Context, regInfo: *codegen.RegInfo) codegen.RegisterRange {
    return switch (regInfo.usage) {
        .Temporary => context.genInfo.registerLimits.temporary,
        .Preserved => context.genInfo.registerLimits.preserved,
        .Param, .Return, .ParamNext, .ReturnNext => context.genInfo.registerLimits.params,
    };
}

fn getRegFromUsage(
    context: *Context,
    instrIndex: usize,
    regInfo: *codegen.RegInfo,
) !InactiveRegResult {
    switch (regInfo.usage) {
        .Param, .Return => {
            const start = context.genInfo.registerLimits.params.start;
            const end = context.genInfo.registerLimits.params.end;
            @memset(context.genInfo.registerStatus.items[start + 1 .. end], .{});
            context.genInfo.registerStatus.items[0].active = true;
            return .{ .Normal = 0 };
        },
        else => {
            const limits = getRegLimitsFromUsage(context, regInfo);
            return try inactiveRegFromLimits(
                context,
                instrIndex,
                limits,
            );
        },
    }
}

fn inactiveRegFromLimits(
    context: *Context,
    instrIndex: usize,
    limits: codegen.RegisterRange,
) !InactiveRegResult {
    var firstFoundAvailable: ?vmInfo.TempRegister = null;

    for (limits.start..limits.end) |index| {
        const status = context.genInfo.registerStatus.items[index];
        if (status.active) continue;

        if (firstFoundAvailable == null) {
            firstFoundAvailable = @intCast(index);
        }

        if (status.state.info == .Spilled) continue;

        context.genInfo.registerStatus.items[index].active = true;
        return .{ .Normal = @intCast(index) };
    }

    if (firstFoundAvailable) |available| {
        context.genInfo.registerStatus.items[available].active = true;
        return .{ .Normal = available };
    }

    return .{
        .Spilled = try getIdealSpillReg(context, limits, instrIndex),
    };
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
) !SpillState {
    recordInstrRegUsagesFromTo(
        context,
        limits,
        fromInstrIndex,
        context.genInfo.regAllocateUtils.furthestInstrReach,
    );

    const nextUseItems = context.genInfo.regAllocateUtils.regNextUseIndex.items;
    var furthestNextUse: u32 = 0;
    var furthestNextUseReg: vmInfo.TempRegister = 0;
    for (limits.start..limits.end) |index| {
        const isProtected = context.genInfo.regAllocateUtils.protectedRegisters.includes(
            @intCast(index),
        );
        if (isProtected) continue;

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

fn recordInstrRegUsagesFromTo(
    context: *Context,
    limits: codegen.RegisterRange,
    fromInstrIndex: usize,
    toInstrIndex: usize,
) void {
    @memset(context.genInfo.regAllocateUtils.regNextUseIndex.items, 0);

    var i: usize = fromInstrIndex + 1;
    while (i <= toInstrIndex) : (i += 1) {
        recordInstrRegUsages(context, i, limits);
    }
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
    const reg = switch (regInfo.regRemap) {
        .Unused, .Stored => return,
        .Spilled => |info| info.reg,
        .Normal => |reg| reg,
    };

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

    const uses1 = try codegen.RegUseIndices.init(allocator);
    try uses1.indices.append(allocator, 0);
    try uses1.indices.append(allocator, 1);
    const info1 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .useIndices = uses1,
    });
    context.genInfo.registers.items[0] = info1;

    const uses11 = try codegen.RegUseIndices.init(allocator);
    try uses11.indices.append(allocator, 1);
    try uses11.indices.append(allocator, 3);
    const info11 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .useIndices = uses11,
    });
    context.genInfo.registers.items[1] = info11;

    const uses2 = try codegen.RegUseIndices.init(allocator);
    try uses2.indices.append(allocator, 2);
    try uses2.indices.append(allocator, 3);
    const info2 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .useIndices = uses2,
    });
    context.genInfo.registers.items[2] = info2;

    const uses3 = try codegen.RegUseIndices.init(allocator);
    try uses3.indices.append(allocator, 3);
    try uses3.indices.append(allocator, 3);
    const info3 = try blitz.utils.createMut(codegen.RegInfo, allocator, .{
        .useIndices = uses3,
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

    try std.testing.expectEqual(0, context.genInfo.registers.items[0].useIndices.current());
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
    try remapReg(allocator, &context, &t0, 0, &sp);
    try std.testing.expectEqual(1, context.genInfo.registers.items[0].useIndices.current());
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
    try remapReg(allocator, &context, &t01, 1, &sp);
    flushPendingDeactivations(&context);
    try remapReg(allocator, &context, &t02, 1, &sp);
    flushPendingDeactivations(&context);
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
    try remapReg(allocator, &context, &t1, 2, &sp);
    flushPendingDeactivations(&context);
    context.genInfo.regAllocateUtils.protectedRegisters.clear();
    try remapReg(allocator, &context, &t2, 3, &sp);
    try remapReg(allocator, &context, &t3, 3, &sp);
    flushPendingDeactivations(&context);
    try remapReg(allocator, &context, &t4, 3, &sp);
    flushPendingDeactivations(&context);
    try std.testing.expectEqual(null, context.genInfo.registers.items[0].useIndices.current());

    try std.testing.expectEqual(8, t0);
    try std.testing.expectEqual(8, t01);
    try std.testing.expectEqual(8, t02);
    try std.testing.expectEqual(9, t1);
    try std.testing.expectEqual(9, t2);
    try std.testing.expectEqual(8, t3);
    try std.testing.expectEqual(8, t4);
}
