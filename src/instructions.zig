const blitz = @import("blitz.zig");
const vmInfo = blitz.vmInfo;
const codegen = blitz.codegen;

pub fn storeRegAtPtrPostInc(
    reg: vmInfo.TempRegister,
    ptrReg: vmInfo.TempRegister,
    inc: u16,
    size: u8,
) codegen.Instr {
    return switch (size) {
        1 => codegen.Instr{
            .Store8AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        2 => codegen.Instr{
            .Store16AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        3, 4 => codegen.Instr{
            .Store32AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        5, 6, 7, 8 => codegen.Instr{
            .Store64AtRegPostInc16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .inc = inc,
            },
        },
        else => unreachable,
    };
}

pub fn loadRegAtPtrOffset(
    reg: vmInfo.TempRegister,
    outReg: vmInfo.TempRegister,
    readOffset: u64,
    size: u64,
) codegen.Instr {
    const offset: u16 = @intCast(readOffset);
    if (offset == 0) return loadRegAtPtr(reg, outReg, size);

    return switch (size) {
        1 => codegen.Instr{
            .Load8AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        2 => codegen.Instr{
            .Load16AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        3, 4 => codegen.Instr{
            .Load32AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        5...8 => codegen.Instr{
            .Load64AtRegOffset16 = .{
                .dest = outReg,
                .fromRegPtr = reg,
                .offset = offset,
            },
        },
        else => unreachable,
    };
}

pub fn loadRegAtPtr(
    ptrReg: vmInfo.TempRegister,
    reg: vmInfo.TempRegister,
    size: u64,
) codegen.Instr {
    return switch (size) {
        1 => codegen.Instr{
            .Load8AtReg = .{
                .dest = reg,
                .fromRegPtr = ptrReg,
            },
        },
        2 => codegen.Instr{
            .Load16AtReg = .{
                .dest = reg,
                .fromRegPtr = ptrReg,
            },
        },
        3, 4 => codegen.Instr{
            .Load32AtReg = .{
                .dest = reg,
                .fromRegPtr = ptrReg,
            },
        },
        5...8 => codegen.Instr{
            .Load64AtReg = .{
                .dest = reg,
                .fromRegPtr = ptrReg,
            },
        },
        else => unreachable,
    };
}

pub fn loadRegAtSpNegOffset(outReg: vmInfo.TempRegister, offset: u64, size: u64) codegen.Instr {
    return switch (size) {
        1 => codegen.Instr{
            .Load8AtSpNegOffset16 = .{
                .reg = outReg,
                .offset = @intCast(offset),
            },
        },
        2 => codegen.Instr{
            .Load16AtSpNegOffset16 = .{
                .reg = outReg,
                .offset = @intCast(offset),
            },
        },
        3, 4 => codegen.Instr{
            .Load32AtSpNegOffset16 = .{
                .reg = outReg,
                .offset = @intCast(offset),
            },
        },
        5...8 => codegen.Instr{
            .Load64AtSpNegOffset16 = .{
                .reg = outReg,
                .offset = @intCast(offset),
            },
        },
        else => unreachable,
    };
}

pub fn storeRegAtSpNegOffset(
    reg: vmInfo.TempRegister,
    size: u64,
    loc: u64,
) codegen.Instr {
    return switch (size) {
        1 => codegen.Instr{
            .Store8AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        2 => codegen.Instr{
            .Store16AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        3, 4 => codegen.Instr{
            .Store32AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        5...8 => codegen.Instr{
            .Store64AtSpNegOffset16 = .{
                .reg = reg,
                .offset = @intCast(loc),
            },
        },
        else => unreachable,
    };
}

pub fn storeRegAtPtr(
    reg: vmInfo.TempRegister,
    ptrReg: vmInfo.TempRegister,
    size: u64,
) codegen.Instr {
    return switch (size) {
        1 => codegen.Instr{
            .Store8AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        2 => codegen.Instr{
            .Store16AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        3, 4 => codegen.Instr{
            .Store32AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        5...8 => codegen.Instr{
            .Store64AtReg = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
            },
        },
        else => unreachable,
    };
}

pub fn storeRegAtPtrOffset(
    reg: vmInfo.TempRegister,
    ptrReg: vmInfo.TempRegister,
    offset: u16,
    size: u8,
) codegen.Instr {
    if (offset == 0) return storeRegAtPtr(reg, ptrReg, size);

    return switch (size) {
        1 => codegen.Instr{
            .Store8AtRegOffset16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .offset = offset,
            },
        },
        2 => codegen.Instr{
            .Store16AtRegOffset16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .offset = offset,
            },
        },
        3, 4 => codegen.Instr{
            .Store32AtRegOffset16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .offset = offset,
            },
        },
        5, 6, 7, 8 => codegen.Instr{
            .Store64AtRegOffset16 = .{
                .fromReg = reg,
                .toRegPtr = ptrReg,
                .offset = offset,
            },
        },
        else => unreachable,
    };
}

pub fn getSpIncInstructions(size: u64) struct {
    add: codegen.Instr,
    sub: codegen.Instr,
} {
    const spOpSize = codegen.getOpSizeFromNum(size);

    return switch (spOpSize) {
        .U8 => .{
            .add = codegen.Instr{ .AddSp8 = @intCast(size) },
            .sub = codegen.Instr{ .SubSp8 = @intCast(size) },
        },
        .U16 => .{
            .add = codegen.Instr{ .AddSp16 = @intCast(size) },
            .sub = codegen.Instr{ .SubSp16 = @intCast(size) },
        },
        .U32 => .{
            .add = codegen.Instr{ .AddSp32 = @intCast(size) },
            .sub = codegen.Instr{ .SubSp32 = @intCast(size) },
        },
        .U64 => .{
            .add = codegen.Instr{ .AddSp64 = @intCast(size) },
            .sub = codegen.Instr{ .SubSp64 = @intCast(size) },
        },
    };
}

pub fn addConst(outReg: vmInfo.TempRegister, opReg: vmInfo.TempRegister, data: u64) codegen.Instr {
    const opSize = codegen.getOpSizeFromNum(data);

    return switch (opSize) {
        .U8 => codegen.Instr{
            .Add8 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U16 => codegen.Instr{
            .Add16 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U32 => codegen.Instr{
            .Add32 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U64 => codegen.Instr{
            .Add64 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
    };
}

pub fn subConst(outReg: vmInfo.TempRegister, opReg: vmInfo.TempRegister, data: u64) codegen.Instr {
    const opSize = codegen.getOpSizeFromNum(data);

    return switch (opSize) {
        .U8 => codegen.Instr{
            .Sub8 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U16 => codegen.Instr{
            .Sub16 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U32 => codegen.Instr{
            .Sub32 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
        .U64 => codegen.Instr{
            .Sub64 = .{
                .dest = outReg,
                .reg = opReg,
                .data = @intCast(data),
            },
        },
    };
}

pub fn mulRegAddReg(
    dest: vmInfo.TempRegister,
    addReg: vmInfo.TempRegister,
    mulReg: vmInfo.TempRegister,
    data: u64,
) codegen.Instr {
    const opSize = codegen.getOpSizeFromNum(data);

    return switch (opSize) {
        .U8 => codegen.Instr{
            .MulReg8AddReg = .{
                .dest = dest,
                .addReg = addReg,
                .mulReg = mulReg,
                .data = @intCast(data),
            },
        },
        .U16 => codegen.Instr{
            .MulReg16AddReg = .{
                .dest = dest,
                .addReg = addReg,
                .mulReg = mulReg,
                .data = @intCast(data),
            },
        },
        .U32 => codegen.Instr{
            .MulReg32AddReg = .{
                .dest = dest,
                .addReg = addReg,
                .mulReg = mulReg,
                .data = @intCast(data),
            },
        },
        .U64 => codegen.Instr{
            .MulReg64AddReg = .{
                .dest = dest,
                .addReg = addReg,
                .mulReg = mulReg,
                .data = @intCast(data),
            },
        },
    };
}
