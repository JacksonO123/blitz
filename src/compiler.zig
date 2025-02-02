const std = @import("std");
const blitz = @import("root").blitz;
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

pub fn compile(allocator: Allocator, path: []const u8) !void {
    const maxFileSize = 1028 * 4; // arbitrary
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const code = try file.readToEndAlloc(allocator, maxFileSize);
    defer allocator.free(code);

    const tokens = try tokenizer.tokenize(allocator, code);
    defer free.freeTokens(allocator, tokens);

    const structNames = try blitzAst.findStructNames(allocator, tokens);
    printStructNames(structNames);

    var genericsList = ArrayList([]u8).init(allocator);
    var currentStructs = ArrayList([]u8).init(allocator);
    var distFromStructMethod = ArrayList(u32).init(allocator);
    var functions = StringHashMap(*const blitzAst.FuncDecNode).init(allocator);
    var variableTypes = StringHashMap(*const blitzAst.AstTypes).init(allocator);
    var structs = StringHashMap(*const blitzAst.StructDecNode).init(allocator);

    var compInfo: CompInfo = .{
        .structNames = structNames,
        .generics = &genericsList,
        .variableTypes = &variableTypes,
        .functions = &functions,
        .structs = &structs,
        .currentStructs = &currentStructs,
        .distFromStructMethod = &distFromStructMethod,
        .preAst = true,
    };
    defer free.freeCompInfo(allocator, &compInfo);

    const registeredStructs = try blitzAst.registerStructs(allocator, &compInfo, tokens);
    defer allocator.free(registeredStructs);
    try compInfo.setStructDecs(registeredStructs);

    printRegisteredStructs(&compInfo, registeredStructs);

    {
        var registeredStructNodes = ArrayList(*const blitzAst.AstNode).init(allocator);
        defer registeredStructNodes.deinit();

        for (registeredStructs) |s| {
            const node = try create(blitzAst.AstNode, allocator, .{
                .StructDec = s,
            });
            try registeredStructNodes.append(node);
        }

        try scanner.scanNodes(allocator, &compInfo, registeredStructNodes.items);

        for (registeredStructNodes.items) |node| {
            allocator.destroy(node);
        }
    }

    compInfo.prepareForAst();

    const ast = try blitzAst.createAst(allocator, &compInfo, tokens);
    defer free.freeAst(allocator, ast);

    std.debug.print("--- code ---\n{s}\n------------\n", .{code});
    printAst(&compInfo, ast);

    try scanner.typeScan(allocator, ast, &compInfo);
    std.debug.print("\n", .{});
}
