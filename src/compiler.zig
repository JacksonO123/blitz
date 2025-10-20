const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const scanner = blitz.scanner;
const utils = blitz.utils;
const free = blitz.free;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const debug = blitz.debug;
const logger = blitz.logger;
const allocPools = blitz.allocPools;
const blitzContext = blitz.context;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const CompInfo = blitzCompInfo.CompInfo;
const TokenUtil = tokenizer.TokenUtil;
const GenInfo = codegen.GenInfo;
const Context = blitzContext.Context;

const RuntimeError = error{NoInputFile};

pub fn main() !void {
    // const dbg = builtin.mode == .Debug;
    // var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = false }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    // TODO - extend later
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return RuntimeError.NoInputFile;
    }

    const path = args[1];

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    var writer = &stdout.interface;

    try writer.writeAll("opening ");
    try writer.writeAll(path);
    try writer.writeAll("\n");

    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    var context = try Context.init(allocator, code, writer);
    defer {
        context.deinit();
        allocator.destroy(context);
    }

    const structsAndErrors = try blitzAst.registerStructsAndErrors(allocator, context);
    defer allocator.free(structsAndErrors.structs);
    defer allocator.free(structsAndErrors.errors);
    try context.compInfo.setStructDecs(structsAndErrors.structs);
    try context.compInfo.setErrorDecs(structsAndErrors.errors);

    try debug.printRegisteredStructs(context, structsAndErrors.structs, writer);
    try debug.printRegisteredErrors(structsAndErrors.errors, writer);

    try context.compInfo.prepareForAst(context);

    for (structsAndErrors.structs) |s| {
        const res = try scanner.scanNode(allocator, context, s, true);
        scanner.releaseIfAllocated(context, res);
    }

    var ast = try blitzAst.createAst(allocator, context);

    try writer.writeAll("--- code ---\n");
    try writer.writeAll(code);
    try writer.writeAll("\n------------\n\n");
    try debug.printAst(context, ast, writer);

    try scanner.typeScan(allocator, ast, context);

    defer {
        ast.deinit();
        context.clear();
        free.freeStructsAndErrors(allocator, context, structsAndErrors);
    }

    try writer.writeAll("\n------------\n");
    try context.pools.writeStats(writer);
    try writer.writeAll("\n");

    try codegen.codegenAst(allocator, context, ast);
    try writer.writeAll("--- bytecode out ---\n");
    try debug.printBytecodeChunks(context, writer);
    try writer.writeAll("\n------------\n");

    const outFile = try std.fs.cwd().createFile("out.bzc", .{});
    defer outFile.close();
    var fileBuffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var fileBufferedWriter = outFile.writer(&fileBuffer);
    defer fileBufferedWriter.end() catch {};
    const fileWriter = &fileBufferedWriter.interface;
    try context.genInfo.writeChunks(fileWriter);
}
