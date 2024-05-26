const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const Allocator = std.mem.Allocator;

pub fn findChar(items: []const u8, start: usize, item: u8) ?usize {
    var i = start;

    while (i < items.len) : (i += 1) {
        if (items[i] == item) return i;
    }

    return null;
}

pub fn findToken(tokens: []Token, start: usize, tokenType: TokenType) ?usize {
    var i = start;

    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type == tokenType) return i;
    }

    return null;
}

pub fn create(comptime T: type, allocator: Allocator, obj: anytype) !*const T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}
