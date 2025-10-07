const std = @import("std");
const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const string = blitz.string;
const free = blitz.free;
const scanner = blitz.scanner;
const clone = blitz.clone;
const builtins = blitz.builtins;
const codegen = blitz.codegen;
const Allocator = std.mem.Allocator;
const AutoHashMap = std.AutoHashMap;
const AstError = blitzAst.AstError;
const ScanError = scanner.ScanError;
const ArrayList = std.ArrayList;

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

// TODO - remove
pub inline fn dbgWriter() *std.Io.Writer {
    var stdout = std.fs.File.stdout().writer(&[_]u8{});
    const writer = &stdout.interface;
    return writer;
}
