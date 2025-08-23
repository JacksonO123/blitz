const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const settings = blitz.settings;
const GenInfo = utils.GenInfo;
const Allocator = std.mem.Allocator;

const CodeGenError = error{
    RawNumberIsTooBig,
};

pub const Instructions = enum(u8) {
    const Self = @This();

    StoreConst,
    StoreRegConst,

    pub fn getInstrByte(self: Self) u8 {
        return @as(u8, @intCast(@intFromEnum(self)));
    }
};

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    try writeStartStackSize(allocator, genInfo);
    try codegenNode(allocator, genInfo, ast.root);
}

fn writeStartStackSize(allocator: Allocator, genInfo: *GenInfo) !void {
    const startStackTypeSize = @sizeOf(@TypeOf(genInfo.stackStartSize));
    var buf = try allocator.alloc(u8, startStackTypeSize);
    std.mem.writeInt(@TypeOf(genInfo.stackStartSize), @ptrCast(buf[0..]), genInfo.stackStartSize, .big);
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

pub fn codegenNode(allocator: Allocator, genInfo: *GenInfo, node: *const blitzAst.AstNode) !void {
    switch (node.*) {
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                try codegenNode(allocator, genInfo, seqNode);
            }
        },
        .VarDec => |dec| {
            switch (dec.setNode.*) {
                .Value => |val| {
                    const inst = Instructions.StoreRegConst;
                    var buf: []u8 = undefined;

                    switch (val) {
                        .RawNumber => |num| {
                            buf = try allocator.alloc(u8, 7);
                            buf[1] = getOpenRegister(genInfo);
                            buf[2] = @sizeOf(u32);
                            try writeIntSliceToInstr(u32, buf, 3, num);
                        },
                        else => {},
                    }

                    buf[0] = inst.getInstrByte();
                    try genInfo.appendChunk(buf);
                },
                else => {},
            }
        },
        else => {},
    }
}

fn getOpenRegister(genInfo: *GenInfo) u8 {
    const openRegisters = genInfo.availableRegisters;
    var i: u8 = 0;
    while (i < @sizeOf(u8)) : (i += 1) {
        const mask = @as(u8, 1) << @intCast(i);
        const openBit = (openRegisters & mask) >> @intCast(i);

        if (openBit == 1) {
            return i;
        }
    }

    return 0;
}

// const MaxTypeRel = struct {
//     str: []const u8,
//     type: blitzAst.AstNumberVariants,
// };

// fn getBestNumberType(num: []u8) !blitzAst.AstNumberVariants {
//     const u32Max = getStrTypeRel(.U32, "4294967295");
//     const u64Max = getStrTypeRel(.U64, "18446744073709551615");
//     const u128Max = getStrTypeRel(.U128, "340282366920938463463374607431768211455");
//     const uNumMaxes = &[_]MaxTypeRel{ u32Max, u64Max, u128Max };

//     for (uNumMaxes) |max| {
//         if (num.len < max.str.len) {
//             return max.type;
//         }

//         if (num.len > max.str.len) {
//             continue;
//         }

//         var i: u32 = 0;
//         while (i < max.str.len) : (i += 1) {
//             const index = max.str.len - i - 1;
//             if (max.str[index] < num[index]) continue;
//         }

//         return max.type;
//     }

//     return CodeGenError.RawNumberIsTooBig;
// }

// fn getStrTypeRel(numType: blitzAst.AstNumberVariants, str: []const u8) MaxTypeRel {
//     return .{
//         .str = str,
//         .type = numType,
//     };
// }
