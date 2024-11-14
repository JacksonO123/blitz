const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const astMod = @import("ast.zig");
const scan = @import("scan.zig");
const utils = @import("utils.zig");
const free = @import("free.zig");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;
const AstTypes = astMod.AstTypes;
const tokenize = tokenizer.tokenize;
const freeTokens = free.freeTokens;
const registerStructs = astMod.registerStructs;
const CompInfo = utils.CompInfo;
const createAst = astMod.createAst;
const freeAst = free.freeAst;
const freeCompInfo = free.freeCompInfo;
const typeScan = scan.typeScan;
const findStructNames = astMod.findStructNames;
const FuncDecNode = astMod.FuncDecNode;
const StructDecNode = astMod.StructDecNode;

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

    const tokens = try tokenize(allocator, code);
    defer freeTokens(allocator, tokens);

    const structNames = try findStructNames(allocator, tokens);
    printStructNames(structNames);

    var genericsList = ArrayList([]u8).init(allocator);
    var functions = StringHashMap(*const FuncDecNode).init(allocator);
    var variableTypes = StringHashMap(*const AstTypes).init(allocator);
    var structs = StringHashMap(*const StructDecNode).init(allocator);
    var compInfo = CompInfo{
        .structNames = structNames,
        .generics = &genericsList,
        .variableTypes = &variableTypes,
        .functions = &functions,
        .structs = &structs,
        .preAst = true,
    };
    defer freeCompInfo(allocator, &compInfo);

    const registeredStructs = try registerStructs(allocator, &compInfo, tokens);
    defer allocator.free(registeredStructs);
    try compInfo.setStructDecs(registeredStructs);

    printRegisteredStructs(registeredStructs);

    compInfo.prepareForAst();

    const ast = try createAst(allocator, &compInfo, tokens);
    defer freeAst(allocator, ast);

    std.debug.print("--- code ---\n{s}\n------------\n", .{code});
    printAst(ast);

    try typeScan(allocator, ast, &compInfo);
    std.debug.print("\n", .{});
}
