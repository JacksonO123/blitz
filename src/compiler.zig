const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const ast = blitz.ast;
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
const Writer = std.Io.Writer;

const DebugPrintState = enum {
    All,
    None,
};

const BytecodeFormat = enum {
    Binary,
    PlainText,
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    // TODO - extend later
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return error.NoInputFile;
    }

    const path = args[1];

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    const outFile = try std.fs.cwd().createFile("out.bzc", .{});
    defer outFile.close();
    var fileBuffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var fileBufferedWriter = outFile.writer(&fileBuffer);
    defer fileBufferedWriter.end() catch {};
    const fileWriter = &fileBufferedWriter.interface;

    try writer.writeAll("opening ");
    try writer.writeAll(path);
    try writer.writeAll("\n");

    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    try compile(allocator, code, writer, fileWriter, .All, .Binary);
}

pub fn compile(
    allocator: Allocator,
    code: []u8,
    printWriter: *Writer,
    fileWriter: ?*Writer,
    printState: DebugPrintState,
    format: BytecodeFormat,
) !void {
    var context = try Context.init(allocator, code, printWriter, .{});
    defer {
        context.deinit();
        allocator.destroy(context);
    }

    const structsAndErrors = try ast.registerStructsAndErrors(allocator, context);
    defer allocator.free(structsAndErrors.structs);
    defer allocator.free(structsAndErrors.errors);
    try context.compInfo.setStructDecs(structsAndErrors.structs);
    try context.compInfo.setErrorDecs(structsAndErrors.errors);

    if (printState == .All) {
        try debug.printRegisteredStructs(context, structsAndErrors.structs, printWriter);
        try debug.printRegisteredErrors(structsAndErrors.errors, printWriter);
    }

    try context.compInfo.prepareForAst(context);

    for (structsAndErrors.structs) |s| {
        const res = try scanner.scanNode(allocator, context, s, true);
        scanner.releaseIfAllocated(context, res);
    }

    {
        var tree = try ast.createAst(allocator, context);
        defer {
            tree.deinit();
            context.clear();
            free.freeStructsAndErrors(context, structsAndErrors);
        }

        if (printState == .All) {
            try printWriter.writeAll("--- code ---\n");
            try printWriter.writeAll(code);
            try printWriter.writeAll("\n------------\n\n");
            try debug.printAst(context, tree, printWriter);
        }

        try scanner.typeScan(allocator, tree, context);
        try codegen.codegenAst(allocator, context, tree);

        if (printState == .All) {
            try printWriter.writeAll("--- bytecode out ---\n");
            try debug.printBytecodeChunks(context, printWriter);
        }

        if (fileWriter) |fWriter| {
            if (format == .Binary) {
                try context.genInfo.writeChunks(fWriter);
            } else {
                try debug.printBytecodeChunks(context, fWriter);
            }
        }
    }

    if (printState == .All) {
        try printWriter.writeAll("\n------------\n\n");
        try context.pools.writeStats(true, printWriter);
        try printWriter.writeByte('\n');
    }
}
