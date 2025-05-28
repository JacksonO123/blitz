const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn findChar(items: []const u8, start: usize, item: u8) ?usize {
    var i = start;

    while (i < items.len) : (i += 1) {
        if (items[i] == item) return i;
    }

    return null;
}

pub inline fn cloneString(allocator: Allocator, string: []u8) ![]u8 {
    return try allocator.dupe(u8, string);
}

pub inline fn compString(str1: []const u8, str2: []const u8) bool {
    return std.mem.eql(u8, str1, str2);
}

pub fn cloneStringArray(allocator: Allocator, arr: [][]u8) ![][]u8 {
    var newStrs = try ArrayList([]u8).initCapacity(allocator, arr.len);
    defer newStrs.deinit();

    for (arr) |str| {
        try newStrs.append(try cloneString(allocator, str));
    }

    return newStrs.toOwnedSlice();
}
