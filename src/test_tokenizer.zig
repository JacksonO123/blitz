const std = @import("std");
pub const blitz = @import("blitz.zig");
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const debug = blitz.debug;

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const code = try utils.readRelativeFile(allocator, "input/token-test.blitz");
    defer allocator.free(code);

    const tokens = try tokenizer.tokenize(allocator, code);
    defer allocator.free(tokens);
    debug.printTokens(tokens, code);
}
