const std = @import("std");
const expect = std.testing.expect;
const expectEqualDeep = std.testing.expectEqualDeep;
const tokenizer = @import("./tokenizer.zig");
const tokenize = tokenizer.tokenize;
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const TokenizeError = tokenizer.TokenizeError;
const ArrayList = std.ArrayList;
const allocator = std.heap.page_allocator;
const verbose = true;

fn toArr(comptime T: type, data: anytype) ![]T {
    var list = ArrayList(T).init(allocator);
    try list.resize(data.len);
    std.mem.copyForwards(T, list.items, data);
    const res = try allocator.dupe(T, list.items);
    return res;
}

fn printTokens(tokens: anytype) void {
    for (tokens) |token| {
        std.debug.print("{any}", .{token.type});
        if (token.string != null) {
            std.debug.print(" : {s}\n", .{token.string.?});
        } else {
            std.debug.print("\n", .{});
        }
    }
}

fn testTokens(code: []const u8, tokens: anytype) !void {
    const expectedTokens = try toArr(Token, &tokens);

    const resTokens = try tokenize(code, allocator);

    if (verbose) {
        std.debug.print("\n-- expected --\n", .{});
        printTokens(expectedTokens);

        std.debug.print("-- got --\n", .{});
        printTokens(resTokens);
    }

    try expectEqualDeep(resTokens, expectedTokens);
}

test "numbers" {
    const number1 = "1235321;";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Number, .string = try toArr(u8, "1235321") },
        Token{ .type = TokenType.Semicolon, .string = null },
    };

    try testTokens(number1, tokensArr1);

    const number2 = "1235.321;";
    const tokensArr2 = [_]Token{
        Token{ .type = TokenType.Number, .string = try toArr(u8, "1235.321") },
        Token{ .type = TokenType.Semicolon, .string = null },
    };

    try testTokens(number2, tokensArr2);

    const number3 = "12.35.321;";
    const tokens3 = tokenize(number3, allocator);
    try expect(tokens3 == TokenizeError.NumberHasTwoPeriods);

    const number4 = "23word;";
    const tokens4 = tokenize(number4, allocator);
    try expect(tokens4 == TokenizeError.IdentifierWithStartingNumber);
}

test "variables" {
    const code1 = "const thing = 2;";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Const, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "thing") },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.Number, .string = try toArr(u8, "2") },
        Token{ .type = TokenType.Semicolon, .string = null },
    };

    try testTokens(code1, tokensArr1);

    const code2 = "var thing: string = \"something\";";
    const tokensArr2 = [_]Token{
        Token{ .type = TokenType.Var, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "thing") },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.String, .string = null },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.DoubleQuote, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "something") },
        Token{ .type = TokenType.DoubleQuote, .string = null },
        Token{ .type = TokenType.Semicolon, .string = null },
    };

    try testTokens(code2, tokensArr2);
}

test "if statements" {
    const code1 = "if (a == 2) {}";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.If, .string = null },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "a") },
        Token{ .type = TokenType.EqComp, .string = null },
        Token{ .type = TokenType.Number, .string = try toArr(u8, "2") },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };

    try testTokens(code1, tokensArr1);
}

test "loops" {
    const code1 = "for (var i = 0; i < 10; i++) {}";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.For, .string = null },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Var, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "i") },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.Number, .string = try toArr(u8, "0") },
        Token{ .type = TokenType.Semicolon, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "i") },
        Token{ .type = TokenType.LAngle, .string = null },
        Token{ .type = TokenType.Number, .string = try toArr(u8, "10") },
        Token{ .type = TokenType.Semicolon, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "i") },
        Token{ .type = TokenType.Inc, .string = null },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };

    try testTokens(code1, tokensArr1);

    const code2 = "while (i < 10) { i++; }";
    const tokensArr2 = [_]Token{
        Token{ .type = TokenType.While, .string = null },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "i") },
        Token{ .type = TokenType.LAngle, .string = null },
        Token{ .type = TokenType.Number, .string = try toArr(u8, "10") },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "i") },
        Token{ .type = TokenType.Inc, .string = null },
        Token{ .type = TokenType.Semicolon, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };

    try testTokens(code2, tokensArr2);
}

test "post eq symbols" {
    const symbol1 = "==";
    const expected1 = [_]Token{Token{ .type = TokenType.EqComp, .string = null }};
    try testTokens(symbol1, expected1);

    const symbol2 = "<=";
    const expected2 = [_]Token{Token{ .type = TokenType.LAngleEq, .string = null }};
    try testTokens(symbol2, expected2);

    const symbol3 = ">=";
    const expected3 = [_]Token{Token{ .type = TokenType.RAngleEq, .string = null }};
    try testTokens(symbol3, expected3);

    const symbol4 = "-=";
    const expected4 = [_]Token{Token{ .type = TokenType.SubEq, .string = null }};
    try testTokens(symbol4, expected4);

    const symbol5 = "+=";
    const expected5 = [_]Token{Token{ .type = TokenType.AddEq, .string = null }};
    try testTokens(symbol5, expected5);

    const symbol6 = "*=";
    const expected6 = [_]Token{Token{ .type = TokenType.MultEq, .string = null }};
    try testTokens(symbol6, expected6);

    const symbol7 = "/=";
    const expected7 = [_]Token{Token{ .type = TokenType.DivEq, .string = null }};
    try testTokens(symbol7, expected7);
}

test "functions" {
    const code1 = "fn [T: Rectangle | Car] name(param: T) {}";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Fn, .string = null },
        Token{ .type = TokenType.LBracket, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "T") },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "Rectangle") },
        Token{ .type = TokenType.Union, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "Car") },
        Token{ .type = TokenType.RBracket, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "name") },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "param") },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.Identifier, .string = try toArr(u8, "T") },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };

    try testTokens(code1, tokensArr1);
}
