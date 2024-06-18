const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const astUtils = @import("ast.zig");
const Allocator = std.mem.Allocator;
const tokenize = tokenizer.tokenize;
const freeTokens = tokenizer.freeTokens;
const registerStructs = astUtils.registerStructs;
const freeRegisteredStructs = astUtils.freeRegisteredStructs;
const CompInfo = astUtils.CompInfo;
const createAst = astUtils.createAst;
const freeAst = astUtils.freeAst;

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

    // std.debug.print("{any}", .{tokens});

    const structs = try registerStructs(allocator, tokens);
    defer freeRegisteredStructs(allocator, structs);

    printRegisteredStructs(structs);

    const compInfo = CompInfo{
        .registeredStructs = structs,
    };

    const ast = try createAst(allocator, compInfo, tokens);
    defer freeAst(allocator, ast);

    std.debug.print("--- code ---\n{s}\n------------\n", .{code});
    printAst(ast);
}
