const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const settings = blitz.settings;
const GenInfo = utils.GenInfo;
const Allocator = std.mem.Allocator;

const Instructions = enum(u8) {
    const Self = @This();

    Store,

    pub fn toString(self: Self) []u8 {
        if (!settings.codeGenSettings.DebugOut) {
            return &[_]u8{@intFromEnum(self)};
        }

        return switch (self) {
            .Store => "store",
        };
    }
};

pub fn codegenAst(allocator: Allocator, genInfo: *GenInfo, ast: blitzAst.Ast) !void {
    if (settings.codeGenSettings.DebugOut) {
        const buf = try std.fmt.allocPrint(allocator, "make stack: {d}\n", .{genInfo.stackStartSize});
        try genInfo.instructionList.appendSlice(buf);
        allocator.free(buf);
    } else {
        var stackSizeBuf: [@sizeOf(@TypeOf(genInfo.stackStartSize))]u8 = undefined;
        std.mem.writeInt(@TypeOf(genInfo.stackStartSize), &stackSizeBuf, genInfo.stackStartSize, .big);
        try genInfo.instructionList.appendSlice(&stackSizeBuf);
    }

    try codegenNode(allocator, genInfo, ast.root);
}

pub fn codegenNode(allocator: Allocator, genInfo: *GenInfo, node: *const blitzAst.AstNode) !void {
    _ = allocator;
    _ = genInfo;
    switch (node.*) {
        else => {},
    }
}
