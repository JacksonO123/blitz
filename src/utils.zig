const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn findChar(items: []const u8, start: usize, item: u8) ?usize {
    var i = start;

    while (i < items.len) : (i += 1) {
        if (items[i] == item) return i;
    }

    return null;
}

pub fn create(comptime T: type, allocator: Allocator, obj: anytype) Allocator.Error!*const T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}

pub fn toSlice(comptime T: type, allocator: Allocator, data: anytype) ![]T {
    var list = ArrayList(T).init(allocator);
    defer list.deinit();
    try list.resize(data.len);
    std.mem.copyForwards(T, list.items, data);
    const res = try allocator.dupe(T, list.items);
    return res;
}
