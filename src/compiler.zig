const std = @import("std");
pub const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const scanner = blitz.scanner;
const utils = blitz.utils;
const free = blitz.free;
const create = utils.create;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;

// debug
const debug = @import("debug.zig");
const printRegisteredStructs = debug.printRegisteredStructs;
const printAst = debug.printAst;
const printStructNames = debug.printStructNames;

const RuntimeError = error{NoInputFile};

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    // TODO - extend later
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return RuntimeError.NoInputFile;
    }

    const path = args[1];

    std.debug.print("opening: {s}\n", .{path});

    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    const tokens = try tokenizer.tokenize(allocator, code);
    defer free.freeTokens(allocator, tokens);

    const structNames = try blitzAst.findStructNames(allocator, tokens);
    printStructNames(structNames);

    var compInfo = try CompInfo.init(allocator, structNames);
    defer compInfo.deinit();

    const registeredStructs = try blitzAst.registerStructs(allocator, &compInfo, tokens);
    defer allocator.free(registeredStructs);
    try compInfo.setStructDecs(registeredStructs);

    printRegisteredStructs(&compInfo, registeredStructs);

    try compInfo.prepareForAst();

    for (registeredStructs) |s| {
        const node: blitzAst.AstNode = .{
            .StructDec = s,
        };
        _ = try scanner.scanNode(allocator, &compInfo, &node, true);
    }

    var ast = try blitzAst.createAst(allocator, &compInfo, tokens);
    defer ast.deinit();

    std.debug.print("--- code ---\n{s}\n------------\n", .{code});
    printAst(&compInfo, ast);

    try scanner.typeScan(allocator, ast, &compInfo);
    std.debug.print("\n", .{});
}
