const std = @import("std");
const tokenizer = @import("./tokenizer.zig");
const utils = @import("utils.zig");
const expect = std.testing.expect;
const expectEqualDeep = std.testing.expectEqualDeep;
const tokenize = tokenizer.tokenize;
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const TokenizeError = tokenizer.TokenizeError;
const ArrayList = std.ArrayList;
const allocator = std.testing.allocator;
const freeTokens = tokenizer.freeTokens;
const freeTokenArr = tokenizer.freeTokenArr;
const toSlice = utils.toSlice;

// const verbose = true;
const verbose = false;

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
    const expectedTokens = try toSlice(Token, allocator, &tokens);
    defer allocator.free(expectedTokens);

    const resTokens = try tokenize(allocator, code);
    defer freeTokens(allocator, resTokens);

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
    const numStr1 = try toSlice(u8, allocator, "1235321");
    defer allocator.free(numStr1);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Number, .string = numStr1 },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(number1, tokensArr1);

    const number2 = "1235.321;";
    const numStr2 = try toSlice(u8, allocator, "1235.321");
    defer allocator.free(numStr2);
    const tokensArr2 = [_]Token{
        Token{ .type = TokenType.Number, .string = numStr2 },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(number2, tokensArr2);

    const number3 = "12.35.321;";
    const tokens3 = tokenize(allocator, number3);
    try expect(tokens3 == TokenizeError.NumberHasTwoPeriods);

    const number4 = "23word;";
    const tokens4 = tokenize(allocator, number4);
    try expect(tokens4 == TokenizeError.IdentifierWithStartingNumber);
}

test "variables" {
    const code1 = "const thing = 2;";
    const str1 = try toSlice(u8, allocator, "thing");
    defer allocator.free(str1);
    const str2 = try toSlice(u8, allocator, "2");
    defer allocator.free(str2);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Const, .string = null },
        Token{ .type = TokenType.Identifier, .string = str1 },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.Number, .string = str2 },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code1, tokensArr1);

    const code2 = "var thing: string = \"something\";";
    const str3 = try toSlice(u8, allocator, "thing");
    defer allocator.free(str3);
    const str4 = try toSlice(u8, allocator, "\"something\"");
    defer allocator.free(str4);
    const tokensArr2 = [_]Token{
        Token{ .type = TokenType.Var, .string = null },
        Token{ .type = TokenType.Identifier, .string = str3 },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.String, .string = null },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.String, .string = str4 },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code2, tokensArr2);
}

test "if statements" {
    const code1 = "if (a == 2) {}";
    const str1 = try toSlice(u8, allocator, "a");
    defer allocator.free(str1);
    const str2 = try toSlice(u8, allocator, "2");
    defer allocator.free(str2);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.If, .string = null },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Identifier, .string = str1 },
        Token{ .type = TokenType.EqComp, .string = null },
        Token{ .type = TokenType.Number, .string = str2 },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };
    try testTokens(code1, tokensArr1);
}

test "loops" {
    const code1 = "for (var i = 0; i < 10; i++) {}";
    const iStr = try toSlice(u8, allocator, "i");
    defer allocator.free(iStr);
    const str1 = try toSlice(u8, allocator, "0");
    defer allocator.free(str1);
    const str2 = try toSlice(u8, allocator, "10");
    defer allocator.free(str2);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.For, .string = null },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Var, .string = null },
        Token{ .type = TokenType.Identifier, .string = iStr },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.Number, .string = str1 },
        Token{ .type = TokenType.Semicolon, .string = null },
        Token{ .type = TokenType.Identifier, .string = iStr },
        Token{ .type = TokenType.LAngle, .string = null },
        Token{ .type = TokenType.Number, .string = str2 },
        Token{ .type = TokenType.Semicolon, .string = null },
        Token{ .type = TokenType.Identifier, .string = iStr },
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
        Token{ .type = TokenType.Identifier, .string = iStr },
        Token{ .type = TokenType.LAngle, .string = null },
        Token{ .type = TokenType.Number, .string = str2 },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.Identifier, .string = iStr },
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
    const code1 = "fn [T: Rectangle | Car, K: string[]] name(param: T) {}";
    const str1 = try toSlice(u8, allocator, "T");
    defer allocator.free(str1);
    const str2 = try toSlice(u8, allocator, "Rectangle");
    defer allocator.free(str2);
    const str3 = try toSlice(u8, allocator, "Car");
    defer allocator.free(str3);
    const str4 = try toSlice(u8, allocator, "K");
    defer allocator.free(str4);
    const str5 = try toSlice(u8, allocator, "name");
    defer allocator.free(str5);
    const str6 = try toSlice(u8, allocator, "param");
    defer allocator.free(str6);
    const str7 = try toSlice(u8, allocator, "T");
    defer allocator.free(str7);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Fn, .string = null },
        Token{ .type = TokenType.LBracket, .string = null },
        Token{ .type = TokenType.Identifier, .string = str1 },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.Identifier, .string = str2 },
        Token{ .type = TokenType.Union, .string = null },
        Token{ .type = TokenType.Identifier, .string = str3 },
        Token{ .type = TokenType.Comma, .string = null },
        Token{ .type = TokenType.Identifier, .string = str4 },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.String, .string = null },
        Token{ .type = TokenType.LBracket, .string = null },
        Token{ .type = TokenType.RBracket, .string = null },
        Token{ .type = TokenType.RBracket, .string = null },
        Token{ .type = TokenType.Identifier, .string = str5 },
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.Identifier, .string = str6 },
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.Identifier, .string = str7 },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
    };
    try testTokens(code1, tokensArr1);
}
