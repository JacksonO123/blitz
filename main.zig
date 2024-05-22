const std = @import("std");
const tokenizer = @import("./tokenizer.zig");
const tokenize = tokenizer.tokenize;
const freeTokens = tokenizer.freeTokens;
const allocator = std.heap.page_allocator;

pub fn main() !void {
    const str = "const thing = 2;";

    const tokens = try tokenize(str, allocator);

    std.debug.print("({})\n", .{tokens.len});

    for (tokens) |token| {
        std.debug.print("{}", .{token.type});
        if (token.string != null) {
            std.debug.print(" : ", .{});
            tokenizer.printChars(token.string.?);
        } else {
            std.debug.print("\n", .{});
        }
    }

    freeTokens(tokens, allocator);
}
