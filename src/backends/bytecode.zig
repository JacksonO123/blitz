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

    context.genInfo.registerLimits = vmInfo.bytecodeRegLimits;

    // // test limits for register allocation
    // context.genInfo.registerLimits.params = .{
    //     .start = 0,
    //     .end = 8,
    // };
    // context.genInfo.registerLimits.temporary = .{
    //     .start = 8,
    //     .end = 11,
    // };
    // context.genInfo.registerLimits.preserved = .{
    //     .start = 13,
    //     .end = 15,
    // };

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
    defer flushPendingDeactivations(context);

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
            @branchHint(.likely);
            regPtr.* = try handleNewRegRemap(allocator, context, regInfo, regPtr, instrIndex, sp);
        },
        .Normal => |remap| a: {
            @branchHint(.likely);
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
                break :a;
            }

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
            @branchHint(.likely);
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
    return switch (regInfo.usage) {
        .Temporary => try inactiveRegFromLimits(
            context,
            instrIndex,
            context.genInfo.registerLimits.temporary,
        ),
        .Preserved => try inactiveRegFromLimits(
            context,
            instrIndex,
            context.genInfo.registerLimits.preserved,
        ),
        .Param, .Return => {
            const start = context.genInfo.registerLimits.params.start;
            const end = context.genInfo.registerLimits.params.end;
            @memset(context.genInfo.registerStatus.items[start + 1 .. end], .{});
            context.genInfo.registerStatus.items[0].active = true;
            return .{ .Normal = 0 };
        },
        .ParamNext, .ReturnNext => try inactiveRegFromLimits(
            context,
            instrIndex,
            context.genInfo.registerLimits.params,
        ),
    };
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

// Tests

const TestHelper = struct {
    const Self = @This();

    allocator: Allocator,
    context: Context,
    sp: u64,

    fn init(allocator: Allocator, instrs: []const codegen.Instr, regCount: usize) !TestHelper {
        // default bytecode register settings
        return initWithLimits(allocator, instrs, regCount, .{
            .params = .{ .start = 0, .end = 8 },
            .temporary = .{ .start = 8, .end = 8 + 124 },
            .preserved = .{ .start = 8 + 124, .end = 8 + 124 + 124 },
        });
    }

    fn initWithLimits(
        allocator: std.mem.Allocator,
        instrs: []const codegen.Instr,
        regCount: usize,
        limits: codegen.BackendRegLimits,
    ) !Self {
        var stdout = std.fs.File.stdout().writer(&[_]u8{});
        defer stdout.end() catch {};
        const writer = &stdout.interface;
        defer writer.flush() catch {};

        var context = try Context.init(allocator, "", writer, .{});

        context.genInfo.registerLimits = limits;

        const totalRegs = limits.preserved.end;

        try context.genInfo.registerStatus.ensureTotalCapacityPrecise(allocator, totalRegs);
        context.genInfo.registerStatus.items.len = totalRegs;
        @memset(context.genInfo.registerStatus.items, .{});

        try context.genInfo.regAllocateUtils.regNextUseIndex.ensureTotalCapacityPrecise(
            allocator,
            totalRegs,
        );
        context.genInfo.regAllocateUtils.regNextUseIndex.items.len = totalRegs;
        @memset(context.genInfo.regAllocateUtils.regNextUseIndex.items, 0);

        const instrsPtr = try utils.createMut(std.ArrayList(codegen.Instr), allocator, .empty);
        try instrsPtr.appendSlice(allocator, instrs);
        context.genInfo.instrList = instrsPtr;

        try context.genInfo.registers.ensureTotalCapacityPrecise(allocator, regCount);
        context.genInfo.registers.items.len = regCount;

        return .{
            .allocator = allocator,
            .context = context,
            .sp = 0,
        };
    }

    fn addVReg(self: *TestHelper, regIndex: usize, useIndicesList: []const u32) !void {
        try self.addVRegUtil(regIndex, useIndicesList, .Temporary);
    }

    fn addVRegUtil(
        self: *TestHelper,
        regIndex: usize,
        useIndicesList: []const u32,
        usage: codegen.RegisterUsage,
    ) !void {
        const uses = try codegen.RegUseIndices.init(self.allocator);
        try uses.indices.appendSlice(self.allocator, useIndicesList);

        const info = try utils.createMut(codegen.RegInfo, self.allocator, .{
            .useIndices = uses,
            .usage = usage,
        });
        self.context.genInfo.registers.items[regIndex] = info;
    }

    fn runRemapInstr(self: *TestHelper, instrIndex: usize) !void {
        try remapInstr(self.allocator, &self.context, instrIndex, &self.sp);
    }

    fn runAllocateRegisters(self: *TestHelper) !void {
        try allocateRegisters(
            self.allocator,
            &self.context,
            self.context.genInfo.instrList.items,
            0,
            &self.sp,
        );
    }

    fn getInstr(self: *TestHelper, index: usize) codegen.Instr {
        return self.context.genInfo.instrList.items[index];
    }

    fn getRegRemap(self: *TestHelper, vReg: usize) codegen.RegRemap {
        return self.context.genInfo.registers.items[vReg].regRemap;
    }

    fn getRegStatus(self: *TestHelper, realReg: usize) codegen.RegStatus {
        return self.context.genInfo.registerStatus.items[realReg];
    }

    fn getInsertActions(self: *TestHelper) []const codegen.InsertInfo {
        return self.context.genInfo.instrActions.insertInstrInfo.action.items;
    }

    fn getSkipActions(self: *TestHelper) []const u32 {
        return self.context.genInfo.instrActions.skipInstrInfo.action.items;
    }
};

test "basic register mapping" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .Mov = .{ .dest = 1, .src = 0 } },
        .{ .SetReg32 = .{ .reg = 2, .data = 101 } },
        .{ .Add = .{ .dest = 3, .reg1 = 1, .reg2 = 2 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 4);
    try helper.addVReg(0, &.{ 0, 1 });
    try helper.addVReg(1, &.{ 1, 3 });
    try helper.addVReg(2, &.{ 2, 3 });
    try helper.addVReg(3, &.{ 3, 3 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(1).Mov.src);
    try std.testing.expectEqual(8, helper.getInstr(1).Mov.dest);
    try std.testing.expectEqual(9, helper.getInstr(2).SetReg32.reg);

    const addInstr = helper.getInstr(3).Add;
    try std.testing.expectEqual(8, addInstr.reg1);
    try std.testing.expectEqual(9, addInstr.reg2);
    try std.testing.expectEqual(8, addInstr.dest);

    try std.testing.expectEqual(@as(u64, 0), helper.sp);
    try std.testing.expectEqual(@as(usize, 0), helper.getInsertActions().len);
}

test "register reuse after last use" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 2);
    try helper.addVReg(0, &.{ 0, 0 });
    try helper.addVReg(1, &.{ 1, 1 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(1).SetReg32.reg);
}

test "register lifetimes" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 0, .data = 101 } },
        .{ .SetReg32 = .{ .reg = 0, .data = 102 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 1);
    try helper.addVReg(0, &.{ 0, 1, 2 });

    try std.testing.expectEqual(
        @as(u32, 0),
        helper.context.genInfo.registers.items[0].useIndices.current().?,
    );

    try helper.runRemapInstr(0);
    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(
        @as(u32, 1),
        helper.context.genInfo.registers.items[0].useIndices.current().?,
    );
    try std.testing.expect(helper.getRegStatus(8).active);

    try helper.runRemapInstr(1);
    try std.testing.expectEqual(
        @as(u32, 2),
        helper.context.genInfo.registers.items[0].useIndices.current().?,
    );

    try helper.runRemapInstr(2);
    try std.testing.expectEqual(
        null,
        helper.context.genInfo.registers.items[0].useIndices.current(),
    );
    try std.testing.expect(!helper.getRegStatus(8).active);

    try std.testing.expectEqual(8, helper.getInstr(1).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(2).SetReg32.reg);
}

test "register usage types map to correct ranges" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // temp, preserved, param, paramNext usage
    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } }, // temp
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } }, // preserved
        .{ .SetReg32 = .{ .reg = 2, .data = 102 } }, // param
        .{ .SetReg32 = .{ .reg = 3, .data = 103 } }, // paramNext
    };

    var helper = try TestHelper.init(allocator, &instrs, 4);
    try helper.addVRegUtil(0, &.{ 0, 0 }, .Temporary);
    try helper.addVRegUtil(1, &.{ 1, 1 }, .Preserved);
    try helper.addVRegUtil(2, &.{ 2, 2 }, .Param);
    try helper.addVRegUtil(3, &.{ 3, 3 }, .ParamNext);

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(132, helper.getInstr(1).SetReg32.reg);

    const paramReg = helper.getInstr(2).SetReg32.reg;
    try std.testing.expectEqual(0, paramReg);

    const paramNextReg = helper.getInstr(3).SetReg32.reg;
    try std.testing.expect(paramNextReg == 1);
}

test "spilling: store, load back, sp inc" {
    // 2 temp regs, 3 live registers, requires spill
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } }, // stores until add
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
        .{ .SetReg32 = .{ .reg = 2, .data = 102 } }, // spills a reg and takes it
        .{ .Add = .{ .dest = 3, .reg1 = 0, .reg2 = 1 } }, // loads to get reg back
    };

    var helper = try TestHelper.initWithLimits(allocator, &instrs, 4, .{
        .params = .{ .start = 0, .end = 8 },
        .temporary = .{ .start = 8, .end = 10 },
        .preserved = .{ .start = 10, .end = 12 },
    });
    try helper.addVReg(0, &.{ 0, 3 });
    try helper.addVReg(1, &.{ 1, 3 });
    try helper.addVReg(2, &.{ 2, 2 });
    try helper.addVReg(3, &.{ 3, 3 });

    try helper.runAllocateRegisters();

    // spill v0 at offset 0, store v2 at offset 8
    try std.testing.expectEqual(2 * vmInfo.POINTER_SIZE, helper.sp);

    const actions = helper.getInsertActions();
    try std.testing.expectEqual(@as(usize, 3), actions.len);
    // spill store at pos 1
    try std.testing.expectEqual(
        codegen.InstructionVariants.Store64AtSpNegOffset16,
        std.meta.activeTag(actions[0].instr),
    );
    try std.testing.expectEqual(@as(u32, 1), actions[0].pos);
    // store the taken reg at pos 2
    try std.testing.expectEqual(
        codegen.InstructionVariants.Store64AtSpNegOffset16,
        std.meta.activeTag(actions[1].instr),
    );
    try std.testing.expectEqual(@as(u32, 2), actions[1].pos);
    // load from pos 2
    try std.testing.expectEqual(
        codegen.InstructionVariants.Load64AtSpNegOffset16,
        std.meta.activeTag(actions[2].instr),
    );
    try std.testing.expectEqual(@as(u32, 2), actions[2].pos);
}

test "spill positioning and multiple spills" {
    // should pick furthest next use (v0)
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
        .{ .SetReg32 = .{ .reg = 2, .data = 102 } }, // spill at pos 1
        .{ .SetReg32 = .{ .reg = 1, .data = 103 } },
        .{ .SetReg32 = .{ .reg = 2, .data = 104 } },
        .{ .SetReg32 = .{ .reg = 0, .data = 105 } }, // load
    };

    var helper = try TestHelper.initWithLimits(allocator, &instrs, 3, .{
        .params = .{ .start = 0, .end = 8 },
        .temporary = .{ .start = 8, .end = 10 },
        .preserved = .{ .start = 10, .end = 12 },
    });
    try helper.addVReg(0, &.{ 0, 5 });
    try helper.addVReg(1, &.{ 1, 3 });
    try helper.addVReg(2, &.{ 2, 4 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(vmInfo.POINTER_SIZE, helper.sp);

    const actions = helper.getInsertActions();
    try std.testing.expectEqual(@as(usize, 2), actions.len);
    // v0 spilled (furthest next use at 5 vs v1 at 3)
    try std.testing.expectEqual(
        codegen.InstructionVariants.Store64AtSpNegOffset16,
        std.meta.activeTag(actions[0].instr),
    );
    try std.testing.expectEqual(@as(u32, 1), actions[0].pos);
    try std.testing.expectEqual(
        codegen.InstructionVariants.Load64AtSpNegOffset16,
        std.meta.activeTag(actions[1].instr),
    );
    try std.testing.expectEqual(@as(u32, 4), actions[1].pos);
}

test "mov same src to dest creates skip action" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .Mov = .{ .dest = 0, .src = 0 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 1);
    try helper.addVReg(0, &.{ 0, 1, 1, 1 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(8, helper.getInstr(1).Mov.src);
    try std.testing.expectEqual(8, helper.getInstr(1).Mov.dest);

    const skipActions = helper.getSkipActions();
    try std.testing.expectEqual(@as(usize, 1), skipActions.len);
    try std.testing.expectEqual(@as(u32, 1), skipActions[0]);
}

test "store and Load at reg remap" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg64 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
        .{ .Store64AtReg = .{ .fromReg = 1, .toRegPtr = 0 } },
        .{ .Load64AtReg = .{ .dest = 2, .fromRegPtr = 0 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 3);
    try helper.addVReg(0, &.{ 0, 2, 3 });
    try helper.addVReg(1, &.{ 1, 2 });
    try helper.addVReg(2, &.{ 3, 3 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(9, helper.getInstr(2).Store64AtReg.fromReg);
    try std.testing.expectEqual(8, helper.getInstr(2).Store64AtReg.toRegPtr);
    try std.testing.expectEqual(8, helper.getInstr(3).Load64AtReg.fromRegPtr);
    try std.testing.expectEqual(8, helper.getInstr(3).Load64AtReg.dest);
}

test "register state changes after allocation" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 1 } },
        .{ .SetReg32 = .{ .reg = 0, .data = 2 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 1);
    try helper.addVReg(0, &.{ 0, 1 });

    try std.testing.expectEqual(
        codegen.RegRemapStateVariants.Unused,
        std.meta.activeTag(helper.getRegRemap(0)),
    );

    try helper.runRemapInstr(0);

    try std.testing.expectEqual(
        codegen.RegRemapStateVariants.Normal,
        std.meta.activeTag(helper.getRegRemap(0)),
    );
    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(
        codegen.RegStateVariants.Normal,
        std.meta.activeTag(helper.getRegStatus(8).state.info),
    );
}

test "init metadata" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var stdout = std.fs.File.stdout().writer(&[_]u8{});
    defer stdout.end() catch {};
    const writer = &stdout.interface;
    defer writer.flush() catch {};

    var context = try Context.init(allocator, "", writer, .{});
    try initMetadata(allocator, &context);

    try std.testing.expectEqual(@as(u16, 0), context.genInfo.registerLimits.params.start);
    try std.testing.expectEqual(@as(u16, 8), context.genInfo.registerLimits.params.end);
    try std.testing.expectEqual(@as(u16, 8), context.genInfo.registerLimits.temporary.start);
    try std.testing.expectEqual(@as(u16, 132), context.genInfo.registerLimits.temporary.end);
    try std.testing.expectEqual(@as(u16, 132), context.genInfo.registerLimits.preserved.start);
    try std.testing.expectEqual(@as(u16, 256), context.genInfo.registerLimits.preserved.end);

    // All 256 statuses initialized inactive with Unused state
    try std.testing.expectEqual(@as(usize, 256), context.genInfo.registerStatus.items.len);
    try std.testing.expectEqual(
        @as(usize, 256),
        context.genInfo.regAllocateUtils.regNextUseIndex.items.len,
    );
    for (context.genInfo.registerStatus.items) |status| {
        try std.testing.expect(!status.active);
        try std.testing.expectEqual(codegen.RegStateInfo.Unused, status.state.info);
    }
}

test "chained register reuse" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
        .{ .Add = .{ .dest = 2, .reg1 = 0, .reg2 = 1 } },
        .{ .SetReg32 = .{ .reg = 3, .data = 102 } },
        .{ .Add = .{ .dest = 4, .reg1 = 2, .reg2 = 3 } },
        .{ .SetReg32 = .{ .reg = 5, .data = 103 } },
        .{ .Add = .{ .dest = 6, .reg1 = 4, .reg2 = 5 } },
    };

    var helper = try TestHelper.init(allocator, &instrs, 7);
    try helper.addVReg(0, &.{ 0, 2 });
    try helper.addVReg(1, &.{ 1, 2 });
    try helper.addVReg(2, &.{ 2, 4 });
    try helper.addVReg(3, &.{ 3, 4 });
    try helper.addVReg(4, &.{ 4, 6 });
    try helper.addVReg(5, &.{ 5, 6 });
    try helper.addVReg(6, &.{ 6, 6 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(@as(u64, 0), helper.sp);

    try std.testing.expectEqual(8, helper.getInstr(0).SetReg32.reg);
    try std.testing.expectEqual(9, helper.getInstr(1).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(2).Add.dest);
    try std.testing.expectEqual(8, helper.getInstr(2).Add.reg1);
    try std.testing.expectEqual(9, helper.getInstr(2).Add.reg2);
    try std.testing.expectEqual(9, helper.getInstr(3).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(4).Add.dest);
    try std.testing.expectEqual(8, helper.getInstr(4).Add.reg1);
    try std.testing.expectEqual(9, helper.getInstr(4).Add.reg2);
    try std.testing.expectEqual(9, helper.getInstr(5).SetReg32.reg);
    try std.testing.expectEqual(8, helper.getInstr(6).Add.dest);
    try std.testing.expectEqual(8, helper.getInstr(6).Add.reg1);
    try std.testing.expectEqual(9, helper.getInstr(6).Add.reg2);
}

test "tight register pressure and no spill" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const instrs = [_]codegen.Instr{
        .{ .SetReg32 = .{ .reg = 0, .data = 100 } },
        .{ .SetReg32 = .{ .reg = 1, .data = 101 } },
        .{ .SetReg32 = .{ .reg = 2, .data = 102 } },
        .{ .Add = .{ .dest = 3, .reg1 = 0, .reg2 = 1 } },
        .{ .Add = .{ .dest = 4, .reg1 = 2, .reg2 = 3 } },
    };

    var helper = try TestHelper.initWithLimits(allocator, &instrs, 5, .{
        .params = .{ .start = 0, .end = 8 },
        .temporary = .{ .start = 8, .end = 11 },
        .preserved = .{ .start = 11, .end = 13 },
    });
    try helper.addVReg(0, &.{ 0, 3 });
    try helper.addVReg(1, &.{ 1, 3 });
    try helper.addVReg(2, &.{ 2, 4 });
    try helper.addVReg(3, &.{ 3, 4 });
    try helper.addVReg(4, &.{ 4, 4 });

    try helper.runAllocateRegisters();

    try std.testing.expectEqual(@as(u64, 0), helper.sp);
}
