const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const astUtils = @import("ast.zig");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const tokenize = tokenizer.tokenize;
const freeTokens = tokenizer.freeTokens;
const registerStructs = astUtils.registerStructs;
const freeRegisteredStructs = astUtils.freeRegisteredStructs;
const CompInfo = astUtils.CompInfo;
const createAst = astUtils.createAst;
const freeAst = astUtils.freeAst;
const freeCompInfo = astUtils.freeCompInfo;

// debug
const debug = @import("debug.zig");
const printRegisteredStructs = debug.printRegisteredStructs;
const printAst = debug.printAst;

pub fn compile(allocator: Allocator, path: []const u8) !void {
    const maxFileSize = 1028 * 4; // arbitrary
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const code = try file.readToEndAlloc(allocator, maxFileSize);
    defer allocator.free(code);

    const tokens = try tokenize(allocator, code);
    defer freeTokens(allocator, tokens);

    const structs = try registerStructs(allocator, tokens);
    defer freeRegisteredStructs(allocator, structs);

    printRegisteredStructs(structs);

    var genericsList = ArrayList([]u8).init(allocator);
    var compInfo = CompInfo{
        .registeredStructs = structs,
        .generics = &genericsList,
    };
    defer freeCompInfo(&compInfo);

    const ast = try createAst(allocator, &compInfo, tokens);
    defer freeAst(allocator, ast);

    std.debug.print("--- code ---\n{s}\n------------\n", .{code});
    printAst(ast);
}
