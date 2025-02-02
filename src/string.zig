const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn findChar(items: []const u8, start: usize, item: u8) ?usize {
    var i = start;

    while (i < items.len) : (i += 1) {
        if (items[i] == item) return i;
    }

    return null;
}

pub fn cloneString(allocator: Allocator, string: []u8) ![]u8 {
    return try allocator.dupe(u8, string);
}

pub inline fn compString(str1: []const u8, str2: []const u8) bool {
    return std.mem.eql(u8, str1, str2);
}
