const std = @import("std");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const blitz = @import("blitz");
const tokenizer = blitz.tokenizer;
const ast = blitz.ast;
const scanner = blitz.scanner;
const utils = blitz.utils;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const debug = blitz.debug;
const logger = blitz.logger;
const allocPools = blitz.allocPools;
const CompInfo = blitzCompInfo.CompInfo;
const TokenUtil = tokenizer.TokenUtil;
const GenInfo = codegen.GenInfo;
const Context = blitz.context.Context;

const DebugPrintState = enum {
    All,
    None,
};

const BytecodeFormat = enum {
    Binary,
    PlainText,
};

const compilerPrintState: DebugPrintState = .All;
// const compilerPrintState: DebugPrintState = .None;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    // TODO - extend later
    const args = try init.minimal.args.toSlice(allocator);
    if (args.len < 2) {
        return error.NoInputFile;
    }

    const path = args[1];

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(init.io, &buffer);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    const outFile = try std.Io.Dir.cwd().createFile(init.io, "out.bzc", .{});
    defer outFile.close(init.io);
    var fileBuffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var fileBufferedWriter = outFile.writer(init.io, &fileBuffer);
    defer fileBufferedWriter.end() catch {};
    const fileWriter = &fileBufferedWriter.interface;

    try writer.writeAll("opening ");
    try writer.writeAll(path);
    try writer.writeAll("\n");

    const code = try utils.readRelativeFile(allocator, init.io, path);
    try compile(allocator, code, writer, fileWriter, compilerPrintState, .Binary);
}

pub fn compile(
    baseAllocator: Allocator,
    code: []u8,
    printWriter: *Writer,
    fileWriter: ?*Writer,
    printState: DebugPrintState,
    format: BytecodeFormat,
) !void {
    var arena = std.heap.ArenaAllocator.init(baseAllocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var context = try Context.init(allocator, code, printWriter, .{});

    const hoistedNodes = ast.hoistRelevantNodes(allocator, &context) catch |e| {
        logger.logParseError(&context, e, printWriter);
        return e;
    };
    try context.compInfo.setHoistedNodes(hoistedNodes);

    if (printState == .All) {
        try debug.printRegisteredStructs(&context, hoistedNodes.structs, printWriter);
        try debug.printRegisteredErrors(hoistedNodes.errors, printWriter);
    }

    try context.compInfo.prepareForAst(allocator, &context, printWriter);

    for (hoistedNodes.structs) |structs| {
        const res = try scanner.scanNode(allocator, &context, structs, true);
        scanner.releaseIfAllocated(&context, res);
    }

    {
        var tree = try ast.createAst(allocator, &context, printWriter);
        defer {
            tree.deinit();
            context.clearPoolMem();
            allocPools.releaseHoistedNodes(&context, hoistedNodes);
        }

        if (printState == .All) {
            try printWriter.writeAll("--- code ---\n");
            try printWriter.writeAll(code);
            try printWriter.writeAll("\n------------\n\n");
            // try debug.printAst(&context, tree, printWriter);
        }

        try scanner.typeScan(allocator, &context, tree);
        // _ = format;
        // _ = fileWriter;
        try codegen.codegenAst(allocator, &context, .Bytecode);

        // if (printState == .All) {
        //     try blitz.analyzer.analyze(allocator, &context, printWriter);
        // }

        if (printState == .All) {
            try printWriter.writeAll("\n------------\n\n");
            try debug.printBytecodeChunks(&context, printWriter);
        }

        if (fileWriter) |fWriter| {
            if (format == .Binary) {
                try context.genInfo.writeChunks(fWriter);
            } else {
                try debug.printBytecodeChunks(&context, fWriter);
            }
        }
    }

    if (printState == .All) {
        try printWriter.writeAll("\n------------\n\n");
        try context.pools.writeStats(&context, true, printWriter);
        try printWriter.writeByte('\n');
    }
}
