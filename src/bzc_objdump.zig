const std = @import("std");
const builtin = @import("builtin");
pub const blitz = @import("blitz.zig");
const string = blitz.string;
const debug = blitz.debug;
const utils = blitz.utils;

const BzcObjDumpError = error{
    InvalidArgCount,
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        return BzcObjDumpError.InvalidArgCount;
    }

    const filename = args[1];
    const bzcFilename = try allocator.alloc(u8, filename.len + 4);
    defer allocator.free(bzcFilename);
    @memcpy(bzcFilename[0..filename.len], filename);
    @memcpy(bzcFilename[filename.len .. filename.len + 4], ".bzc");

    const bytecode = try utils.readRelativeFile(allocator, bzcFilename);
    defer allocator.free(bytecode);

    var buf = utils.getBufferedWriter();
    const writer = buf.writer();
    defer buf.flush() catch {};
    try debug.printBytecode(bytecode, writer);
}
