const std = @import("std");
pub const blitz = @import("blitz.zig");
const compiler = blitz.compiler;

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

    var path = try allocator.alloc(u8, args[1].len);
    std.mem.copyForwards(u8, path[0..], args[1]);
    defer allocator.free(path);

    std.debug.print("opening: {s}", .{path});

    try compiler.compile(allocator, path);
}
