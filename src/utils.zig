const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

pub const BUFFERED_WRITER_SIZE = 1024 * 32;

pub inline fn create(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*const T {
    return createMut(T, allocator, obj);
}

pub inline fn createMut(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}

pub fn readRelativeFile(allocator: Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

pub fn initMutPtrT(comptime T: type, allocator: Allocator) !*T {
    const data = T.init(allocator);
    return try createMut(T, allocator, data);
}

/// IMPORTANT: supports values 0 - 16
pub fn intToHex(num: usize) u8 {
    return if (num < 10) ('0' + @as(u8, @intCast(num))) else ('a' + @as(u8, @intCast(num - 10)));
}

pub fn getNumberDigitCount(comptime T: type, int: T) u32 {
    const asFloat: f64 = @floatFromInt(int);
    const numDigits: u32 = @intFromFloat(@floor(@log10(asFloat)) + 1);
    return numDigits;
}

pub inline fn unimplemented() void {
    unreachable;
}

pub inline fn compString(str1: []const u8, str2: []const u8) bool {
    return std.mem.eql(u8, str1, str2);
}

pub fn inStringArr(arr: []const []const u8, str: []const u8) bool {
    for (arr) |item| {
        if (compString(item, str)) return true;
    }

    return false;
}

pub fn calculatePadding(stackLocation: u64, alignment: u8) u8 {
    if (alignment == 0) return 0;

    const missAlign = stackLocation % alignment;
    const padding: u8 = if (missAlign == 0)
        0
    else
        @intCast(alignment - missAlign);

    return padding;
}

test "Calculate padding" {
    const p1 = calculatePadding(0, 1);
    try std.testing.expectEqual(0, p1.padding);

    const p2 = calculatePadding(1, 1);
    try std.testing.expectEqual(0, p2.padding);

    const p3 = calculatePadding(1, 2);
    try std.testing.expectEqual(1, p3.padding);

    const p4 = calculatePadding(2, 2);
    try std.testing.expectEqual(0, p4.padding);
}

const FlagInfo = struct { []const u8, usize };

fn formatFlagStructure(comptime structure: anytype) [structure.len]FlagInfo {
    var thing: [structure.len]FlagInfo = undefined;

    inline for (structure, 0..) |tuple, index| {
        thing[index] = .{ tuple[0], tuple.len - 1 };
    }

    return thing;
}

/// returns flag if flag is unknown
pub fn scanUnknownFlags(
    flags: [][:0]u8,
    flagMap: *const std.StaticStringMap(usize),
    baseFlagStart: u32,
) ?[]const u8 {
    if (flags.len < 1 + baseFlagStart) return null;

    for (flags[1 + baseFlagStart ..]) |flag| {
        if (!flagMap.has(flag)) return flag;
    }

    return null;
}

pub inline fn createFlagMap(
    args: [][:0]u8,
    comptime flagStructure: anytype,
    writer: *Writer,
    baseFlagStart: u32,
) !std.StaticStringMap(usize) {
    const flagMap = std.StaticStringMap(usize).initComptime(formatFlagStructure(flagStructure));

    if (scanUnknownFlags(args, &flagMap, baseFlagStart)) |unknownFlag| {
        try writer.writeAll("Unknown flag :: ");
        try writer.writeAll(unknownFlag);
        try writer.writeByte('\n');
        return error.UnknownFlag;
    }

    return flagMap;
}

pub fn searchFlagMap(
    arr: [][:0]u8,
    value: []const u8,
    flagMap: *const std.StaticStringMap(usize),
) ?usize {
    var i: usize = 0;
    while (i < arr.len) : (i += 1) {
        if (std.mem.eql(u8, arr[i], value)) return i;
        if (flagMap.get(arr[i])) |width| i += width;
    }
    return null;
}
