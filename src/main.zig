const std = @import("std");
const compile = @import("compiler.zig").compile;

const RuntimeError = error{NoInputFile};

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return RuntimeError.NoInputFile;
    }

    const testSrc: []const u8 = "bz-src/";
    var path = try allocator.alloc(u8, testSrc.len + args[1].len);
    std.mem.copyForwards(u8, path[0..], testSrc);
    std.mem.copyForwards(u8, path[testSrc.len..], args[1]);
    defer allocator.free(path);

    try compile(allocator, path);
}
