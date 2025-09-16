const std = @import("std");
const builtin = @import("builtin");
pub const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const scanner = blitz.scanner;
const utils = blitz.utils;
const free = blitz.free;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const debug = blitz.debug;
const create = utils.create;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const CompInfo = blitzCompInfo.CompInfo;
const TokenUtil = tokenizer.TokenUtil;
const GenInfo = codegen.GenInfo;

const RuntimeError = error{NoInputFile};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    // TODO - extend later
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return RuntimeError.NoInputFile;
    }

    const path = args[1];

    var bufferedWriter = utils.getBufferedWriter();
    defer bufferedWriter.flush() catch {};
    const writer = bufferedWriter.writer();

    try writer.writeAll("opening ");
    try writer.writeAll(path);
    try writer.writeByte('\n');

    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    const tokens = try tokenizer.tokenize(allocator, code);
    defer allocator.free(tokens);

    const names = try blitzAst.findStructsAndErrors(allocator, tokens, code);
    try debug.printStructAndErrorNames(names, writer);

    var compInfo = try CompInfo.init(allocator, tokens, names, code);
    defer compInfo.deinit();

    const structsAndErrors = try blitzAst.registerStructsAndErrors(allocator, &compInfo);
    defer allocator.free(structsAndErrors.structs);
    defer allocator.free(structsAndErrors.errors);
    try compInfo.setStructDecs(structsAndErrors.structs);
    try compInfo.setErrorDecs(structsAndErrors.errors);

    try compInfo.prepareForAst();

    try debug.printRegisteredStructs(&compInfo, structsAndErrors.structs, writer);
    try debug.printRegisteredErrors(structsAndErrors.errors, writer);

    for (structsAndErrors.structs) |s| {
        var node: blitzAst.AstNode = .{
            .StructDec = s,
        };
        const nodeType = try scanner.scanNode(allocator, &compInfo, &node, true);
        free.freeAstTypeInfo(allocator, nodeType);
    }

    var ast = try blitzAst.createAst(allocator, &compInfo);
    defer ast.deinit();

    try writer.writeAll("--- code ---\n");
    try writer.writeAll(code);
    try writer.writeAll("\n------------\n\n");
    try debug.printAst(&compInfo, ast, writer);

    try scanner.typeScan(allocator, ast, &compInfo);

    var genInfo = try GenInfo.init(allocator);
    defer genInfo.deinit();
    genInfo.vmInfo.stackStartSize = compInfo.stackSizeEstimate;

    try codegen.codegenAst(allocator, &genInfo, ast);
    try writer.writeAll("--- bytecode out ---\n");
    try debug.printBytecodeChunks(&genInfo, writer);
    try writer.writeAll("\n------------\n");

    const outFile = try std.fs.cwd().createFile("out.bzc", .{});
    defer outFile.close();
    var fileBufferedWriter = std.io.bufferedWriter(outFile.writer());
    defer fileBufferedWriter.flush() catch {};
    const fileWriter = fileBufferedWriter.writer();
    try genInfo.writeChunks(fileWriter);
}
