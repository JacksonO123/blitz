const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const astUtils = @import("ast.zig");
const createAst = astUtils.createAst;
const tokenize = tokenizer.tokenize;
const freeTokens = tokenizer.freeTokens;
const Ast = astUtils.Ast;
const freeAst = astUtils.freeAst;

const RuntimeError = error{NoInputFile};

pub fn main() !void {
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return RuntimeError.NoInputFile;
    }

    const testSrc: []const u8 = "bz-src/";
    var path = try allocator.alloc(u8, testSrc.len + args[1].len);
    std.mem.copyForwards(u8, path[0..], testSrc);
    std.mem.copyForwards(u8, path[testSrc.len..], args[1]);
    defer allocator.free(path);

    const maxFileSize = 1028 * 4; // arbitrary
    const file = try std.fs.cwd().openFile(path, .{});
    const code = try file.readToEndAlloc(allocator, maxFileSize);
    defer allocator.free(code);

    const tokens = try tokenize(allocator, code);
    defer freeTokens(allocator, tokens);

    var ast = Ast().init(allocator);
    defer ast.deinit();
    try ast.fromTokens(tokens);

    std.debug.print("{s}", .{code});
    std.debug.print("@@ {any}\n", .{ast.root.nodes[0].VarDec.annotation});

    file.close();

    std.debug.print("({})\n", .{tokens.len});

    for (tokens) |token| {
        std.debug.print("{}", .{token.type});
        if (token.string != null) {
            std.debug.print(" : ", .{});
            tokenizer.printChars(token.string.?);
        } else {
            std.debug.print("\n", .{});
        }
    }
}
