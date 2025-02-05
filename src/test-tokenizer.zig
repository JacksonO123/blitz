const std = @import("std");
const tokenizer = @import("tokenizer.zig");
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

// debug
const debug = @import("debug.zig");
const printTokens = debug.printTokens;

const verbose = true;
// const verbose = false;

pub fn toSlice(comptime T: type, data: anytype) ![]T {
    var list = ArrayList(T).init(allocator);
    defer list.deinit();
    try list.resize(data.len);
    std.mem.copyForwards(T, list.items, data);
    const res = try allocator.dupe(T, list.items);
    return res;
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

    try expectEqualDeep(expectedTokens, resTokens);
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

test "methods" {
    const token1 = Token{
        .type = TokenType.LBracket,
        .string = null,
    };
    const token2 = Token{
        .type = TokenType.LParen,
        .string = null,
    };
    const token3 = Token{
        .type = TokenType.LBrace,
        .string = null,
    };
    const token4 = Token{
        .type = TokenType.LAngle,
        .string = null,
    };

    try expect(token1.type.isOpenToken(false) == true);
    try expect(token2.type.isOpenToken(false) == true);
    try expect(token3.type.isOpenToken(false) == true);
    try expect(token4.type.isOpenToken(false) == false);
    try expect(token4.type.isOpenToken(true) == true);

    const token5 = Token{
        .type = TokenType.RBracket,
        .string = null,
    };
    const token6 = Token{
        .type = TokenType.RParen,
        .string = null,
    };
    const token7 = Token{
        .type = TokenType.RBrace,
        .string = null,
    };
    const token8 = Token{
        .type = TokenType.RAngle,
        .string = null,
    };

    try expect(token5.type.isCloseToken(false) == true);
    try expect(token6.type.isCloseToken(false) == true);
    try expect(token7.type.isCloseToken(false) == true);
    try expect(token8.type.isCloseToken(false) == false);
    try expect(token8.type.isCloseToken(true) == true);
}

test "booleans" {
    const code1 = "true;";
    const tokens1 = [_]Token{
        Token{ .type = TokenType.True, .string = null },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code1, tokens1);

    const code2 = "false;";
    const tokens2 = [_]Token{
        Token{ .type = TokenType.False, .string = null },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code2, tokens2);
}

test "keywords" {
    const code1 = "const var fn struct if for while continue break pub prot static return;";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Const, .string = null },
        Token{ .type = TokenType.Var, .string = null },
        Token{ .type = TokenType.Fn, .string = null },
        Token{ .type = TokenType.Struct, .string = null },
        Token{ .type = TokenType.If, .string = null },
        Token{ .type = TokenType.For, .string = null },
        Token{ .type = TokenType.While, .string = null },
        Token{ .type = TokenType.Continue, .string = null },
        Token{ .type = TokenType.Break, .string = null },
        Token{ .type = TokenType.Pub, .string = null },
        Token{ .type = TokenType.Prot, .string = null },
        Token{ .type = TokenType.Static, .string = null },
        Token{ .type = TokenType.Return, .string = null },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code1, tokensArr1);
}

test "string/char" {
    const code1 = "\"this is a string\" 'c';";
    const str1 = try toSlice(u8, allocator, "this is a string");
    defer allocator.free(str1);
    const str2 = try toSlice(u8, allocator, "c");
    defer allocator.free(str2);
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.StringToken, .string = str1 },
        Token{ .type = TokenType.CharToken, .string = str2 },
        Token{ .type = TokenType.Semicolon, .string = null },
    };
    try testTokens(code1, tokensArr1);
}

test "parens" {
    const code1 = "()[]{}<>";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.LParen, .string = null },
        Token{ .type = TokenType.RParen, .string = null },
        Token{ .type = TokenType.LBracket, .string = null },
        Token{ .type = TokenType.RBracket, .string = null },
        Token{ .type = TokenType.LBrace, .string = null },
        Token{ .type = TokenType.RBrace, .string = null },
        Token{ .type = TokenType.LAngle, .string = null },
        Token{ .type = TokenType.RAngle, .string = null },
    };
    try testTokens(code1, tokensArr1);
}

test "other chars" {
    const code1 = ": & = - + * / % += -= *= /= ! . , ? == <= >= ++ --";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.Colon, .string = null },
        Token{ .type = TokenType.Ampersand, .string = null },
        Token{ .type = TokenType.EqSet, .string = null },
        Token{ .type = TokenType.Sub, .string = null },
        Token{ .type = TokenType.Add, .string = null },
        Token{ .type = TokenType.Mult, .string = null },
        Token{ .type = TokenType.Div, .string = null },
        Token{ .type = TokenType.Mod, .string = null },
        Token{ .type = TokenType.AddEq, .string = null },
        Token{ .type = TokenType.SubEq, .string = null },
        Token{ .type = TokenType.MultEq, .string = null },
        Token{ .type = TokenType.DivEq, .string = null },
        Token{ .type = TokenType.Bang, .string = null },
        Token{ .type = TokenType.Period, .string = null },
        Token{ .type = TokenType.Comma, .string = null },
        Token{ .type = TokenType.QuestionMark, .string = null },
        Token{ .type = TokenType.EqComp, .string = null },
        Token{ .type = TokenType.LAngleEq, .string = null },
        Token{ .type = TokenType.RAngleEq, .string = null },
        Token{ .type = TokenType.Inc, .string = null },
        Token{ .type = TokenType.Dec, .string = null },
    };
    try testTokens(code1, tokensArr1);
}

test "types" {
    const code1 = "char u8 u16 u32 u64 u128 i8 i32 i64 i128 f8 f16 f32 f64 f128 usize string bool";
    const tokensArr1 = [_]Token{
        Token{ .type = TokenType.CharType, .string = null },
        Token{ .type = TokenType.U8, .string = null },
        Token{ .type = TokenType.U16, .string = null },
        Token{ .type = TokenType.U32, .string = null },
        Token{ .type = TokenType.U64, .string = null },
        Token{ .type = TokenType.U128, .string = null },
        Token{ .type = TokenType.I8, .string = null },
        Token{ .type = TokenType.I16, .string = null },
        Token{ .type = TokenType.I32, .string = null },
        Token{ .type = TokenType.I64, .string = null },
        Token{ .type = TokenType.I128, .string = null },
        Token{ .type = TokenType.F8, .string = null },
        Token{ .type = TokenType.F16, .string = null },
        Token{ .type = TokenType.F32, .string = null },
        Token{ .type = TokenType.F64, .string = null },
        Token{ .type = TokenType.F128, .string = null },
        Token{ .type = TokenType.USize, .string = null },
        Token{ .type = TokenType.StringType, .string = null },
        Token{ .type = TokenType.Bool, .string = null },
    };
    try testTokens(code1, tokensArr1);
}
