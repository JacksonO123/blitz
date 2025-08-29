const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const version = blitz.version;
const settings = blitz.settings;
const GenInfo = utils.GenInfo;
const Allocator = std.mem.Allocator;

pub const RegisterNumber = u8;
pub const NUM_REGISTERS = 256;
pub const REGISTER_SIZE = 8; // bytes

const CodeGenError = error{
    RawNumberIsTooBig,
    NoAvailableRegisters,
};

pub const Instructions = enum(u8) {
    const Self = @This();

    Store,
    SetReg,
    SetRegHalf,

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }
};

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    try writeStartVMInfo(allocator, genInfo);
    _ = try genBytecode(allocator, genInfo, ast.root);
}

fn writeStartVMInfo(allocator: Allocator, genInfo: *GenInfo) !void {
    const startStackTypeSize = @sizeOf(@TypeOf(genInfo.stackStartSize));
    var buf = try allocator.alloc(u8, startStackTypeSize + 1);

    buf[0] = version.VERSION;
    std.mem.writeInt(@TypeOf(genInfo.stackStartSize), @ptrCast(buf[1..]), genInfo.stackStartSize, .big);

    try genInfo.appendChunk(buf);
}

fn writeIntSliceToInstr(
    comptime T: type,
    buf: []u8,
    offset: usize,
    num: []u8,
) !void {
    const size = @sizeOf(T);
    std.mem.writeInt(T, @ptrCast(buf[offset .. size + offset]), try std.fmt.parseInt(T, num, 10), .big);
}

pub fn genBytecode(
    allocator: Allocator,
    genInfo: *GenInfo,
    node: *const blitzAst.AstNode,
) !?RegisterNumber {
    switch (node.*) {
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                _ = try genBytecode(allocator, genInfo, seqNode);
            }
        },
        .VarDec => |dec| {
            const reg = try genBytecode(allocator, genInfo, dec.setNode);
            if (reg) |num| {
                try genInfo.setVariableRegister(dec.name, num);
            }
        },
        .Value => |value| {
            switch (value) {
                .RawNumber => |num| {
                    const reg = genInfo.getAvailableRegPushSpill();
                    genInfo.reserveRegister(reg.?);

                    const inst = Instructions.SetRegHalf.getInstrByte();
                    const buf = try allocator.alloc(u8, 6);
                    buf[0] = inst;
                    buf[1] = reg;
                    try writeIntSliceToInstr(u32, buf, 2, num);

                    try genInfo.appendChunk(buf);

                    return reg;
                },
                else => {},
            }
        },
        else => {},
    }

    return null;
}
