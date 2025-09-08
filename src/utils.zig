const std = @import("std");
const blitz = @import("root").blitz;
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

const bufferedWriterSize = 4096;
pub const BufferedWriterType = std.io.BufferedWriter(bufferedWriterSize, std.fs.File.Writer);

pub fn getBufferedWriter() BufferedWriterType {
    const stdout = std.io.getStdOut();
    const stdoutWriter = stdout.writer();
    return std.io.BufferedWriter(bufferedWriterSize, @TypeOf(stdoutWriter)){
        .unbuffered_writer = stdoutWriter,
    };
}

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

pub inline fn astTypesPtrToInfo(astType: *blitzAst.AstTypes, isConst: bool) blitzAst.AstTypeInfo {
    return .{
        .isConst = isConst,
        .astType = astType,
    };
}

pub inline fn astTypesToInfo(allocator: Allocator, astType: blitzAst.AstTypes, isConst: bool) !blitzAst.AstTypeInfo {
    const ptr = try createMut(blitzAst.AstTypes, allocator, astType);
    return .{
        .isConst = isConst,
        .astType = ptr,
    };
}

/// IMPORTANT: supports values 0 - 16
pub fn intToHex(num: usize) u8 {
    return if (num < 10) ('0' + @as(u8, @intCast(num))) else ('a' + @as(u8, @intCast(num - 10)));
}

pub fn unimplemented() void {
    unreachable;
}
