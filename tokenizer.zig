const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const TokenizeError = error{ IdentifierWithStartingNumber, NumberHasTwoPeriods };

pub const TokenType = enum {
    // keywords
    Const,
    Var,
    Fn,
    Struct,
    If,
    For,
    While,
    Continue,
    Break,

    // symbols
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Ampersand,
    Union,
    EqSet,
    Sub,
    Add,
    Mult,
    Div,
    Mod,
    Bang,
    Period,
    SingleQuote,
    DoubleQuote,
    Comma,

    // datatypes
    Char,
    U16,
    U32,
    U64,
    U128, //possibly add bigger numbers
    String,
    Bool,

    // other
    Identifier,
    Number,
    EqComp,
    LAngleEq,
    RAngleEq,
    SubEq,
    AddEq,
    MultEq,
    DivEq,
    Inc,
    Dec,
};

const TokenTypeMap = struct {
    string: []const u8,
    token: TokenType,
};

const SymbolMap = struct {
    symbol: u8,
    token: TokenType,
};

pub const Token = struct { type: TokenType, string: ?[]u8 };

pub fn printChars(chars: []u8) void {
    for (chars) |char| {
        std.debug.print("{c}", .{char});
    }

    std.debug.print("\n", .{});
}

pub fn tokenize(input: []const u8, allocator: Allocator) ![]Token {
    var chars = ArrayList(u8).init(allocator);
    defer chars.deinit();

    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var i: u32 = 0;
    outer: while (i < input.len) : (i += 1) {
        const char = input[i];

        if (i < input.len - 1 and char == '/') {
            const next = input[i + 1];
            if (next == '/') {
                const index = findChar(input, i, '\n');
                if (index == null) {
                    break;
                } else {
                    i = index.? + 1;
                }
            } else if (next == '*') {
                var index = findChar(input, i, '/');
                if (index == null) break;

                while (chars.items[index.? - 1] != '*') {
                    index = findChar(input, index.? + 1, '/');
                    if (index == null) break :outer;
                }
            }
        }

        if (i < input.len - 1) {
            var tokenType: ?TokenType = null;

            if (char == '+' and input[i + 1] == '+') {
                tokenType = TokenType.Inc;
            } else if (char == '-' and input[i + 1] == '-') {
                tokenType = TokenType.Dec;
            }

            if (tokenType != null) {
                const firstToken = try charsToToken(chars.items, allocator);

                if (firstToken != null) {
                    try tokens.append(firstToken.?);
                    chars.clearRetainingCapacity();
                }

                const token = Token{ .type = tokenType.?, .string = null };
                try tokens.append(token);
                i += 1;

                continue;
            }
        }

        const postEq = isPostEqSymbol(input, i);
        if (postEq != null) {
            const firstToken = try charsToToken(chars.items, allocator);

            if (firstToken != null) {
                try tokens.append(firstToken.?);
                chars.clearRetainingCapacity();
            }

            i += 1;
            try tokens.append(postEq.?);

            continue;
        }

        if (char == '.') {
            if (isNumber(chars.items)) {
                if (findChar(chars.items, 0, '.') != null) {
                    return TokenizeError.NumberHasTwoPeriods;
                }

                try chars.append(char);
                continue;
            } else if (std.ascii.isDigit(chars.items[0])) {
                return TokenizeError.IdentifierWithStartingNumber;
            }
        }

        const symbol = isSymbol(char);
        if (symbol != null) {
            const firstToken = try charsToToken(chars.items, allocator);

            if (firstToken != null) {
                try tokens.append(firstToken.?);
                chars.clearRetainingCapacity();
            }

            const secondToken = Token{ .string = null, .type = symbol.? };
            try tokens.append(secondToken);

            continue;
        }

        if (std.ascii.isWhitespace(char)) {
            const token = try charsToToken(chars.items, allocator);

            if (token != null) {
                try tokens.append(token.?);
                chars.clearRetainingCapacity();
            }
        }

        if (std.ascii.isAlphanumeric(char)) {
            try chars.append(char);
        }
    }

    const resSlice = allocator.dupe(Token, tokens.items);

    return resSlice;
}

fn isPostEqSymbol(chars: []const u8, start: u32) ?Token {
    if (chars.len == 0) return null;

    if (start < chars.len - 1 and chars[start + 1] == '=') {
        const res = switch (chars[start]) {
            '=' => TokenType.EqComp,
            '<' => TokenType.LAngleEq,
            '>' => TokenType.RAngleEq,
            '-' => TokenType.SubEq,
            '+' => TokenType.AddEq,
            '*' => TokenType.MultEq,
            '/' => TokenType.DivEq,
            else => null,
        };

        if (res == null) return null;

        return Token{ .type = res.?, .string = null };
    }

    return null;
}

fn findChar(chars: []const u8, start: u32, char: u8) ?u32 {
    var i: u32 = start;

    while (i < chars.len) : (i += 1) {
        if (chars[i] == char) return i;
    }

    return null;
}

fn isNumber(chars: []u8) bool {
    for (chars) |char| {
        if (!std.ascii.isDigit(char) and char != '.') return false;
    }

    return true;
}

fn charsToToken(chars: []u8, allocator: Allocator) !?Token {
    if (chars.len == 0) return null;

    var number = false;

    if (std.ascii.isDigit(chars[0])) {
        if (!isNumber(chars)) return TokenizeError.IdentifierWithStartingNumber;
        number = true;
    }

    if (number) {
        const str = try allocator.dupe(u8, chars);
        return Token{ .string = str, .type = TokenType.Number };
    }

    const datatype = isDatatype(chars);
    if (datatype != null) {
        return Token{ .string = null, .type = datatype.? };
    }

    const keyword = isKeyword(chars);
    if (keyword != null) {
        return Token{ .string = null, .type = keyword.? };
    } else {
        const str = try allocator.dupe(u8, chars);
        return Token{ .string = str, .type = TokenType.Identifier };
    }
}

fn isDatatype(chars: []const u8) ?TokenType {
    const datatypes = [_]TokenTypeMap{
        TokenTypeMap{ .string = "char", .token = TokenType.Char },
        TokenTypeMap{ .string = "u16", .token = TokenType.U16 },
        TokenTypeMap{ .string = "u32", .token = TokenType.U32 },
        TokenTypeMap{ .string = "u64", .token = TokenType.U64 },
        TokenTypeMap{ .string = "u128", .token = TokenType.U128 },
        TokenTypeMap{ .string = "string", .token = TokenType.String },
        TokenTypeMap{ .string = "bool", .token = TokenType.Bool },
    };

    return getTypeFromMap(chars, datatypes);
}

fn isKeyword(chars: []const u8) ?TokenType {
    const keywords = [_]TokenTypeMap{
        TokenTypeMap{ .string = "var", .token = TokenType.Var },
        TokenTypeMap{ .string = "const", .token = TokenType.Const },
        TokenTypeMap{ .string = "fn", .token = TokenType.Fn },
        TokenTypeMap{ .string = "struct", .token = TokenType.Struct },
        TokenTypeMap{ .string = "if", .token = TokenType.If },
        TokenTypeMap{ .string = "for", .token = TokenType.For },
        TokenTypeMap{ .string = "while", .token = TokenType.While },
        TokenTypeMap{ .string = "continue", .token = TokenType.Continue },
        TokenTypeMap{ .string = "break", .token = TokenType.Break },
    };

    return getTypeFromMap(chars, keywords);
}

fn getTypeFromMap(chars: []const u8, map: anytype) ?TokenType {
    for (map) |mapItem| {
        if (std.mem.eql(u8, chars, mapItem.string)) {
            return mapItem.token;
        }
    }

    return null;
}

fn isSymbol(char: u8) ?TokenType {
    const symbols = [_]SymbolMap{
        SymbolMap{ .symbol = ':', .token = TokenType.Colon },
        SymbolMap{ .symbol = ';', .token = TokenType.Semicolon },
        SymbolMap{ .symbol = '(', .token = TokenType.LParen },
        SymbolMap{ .symbol = ')', .token = TokenType.RParen },
        SymbolMap{ .symbol = '[', .token = TokenType.LBracket },
        SymbolMap{ .symbol = ']', .token = TokenType.RBracket },
        SymbolMap{ .symbol = '{', .token = TokenType.LBrace },
        SymbolMap{ .symbol = '}', .token = TokenType.RBrace },
        SymbolMap{ .symbol = '<', .token = TokenType.LAngle },
        SymbolMap{ .symbol = '>', .token = TokenType.RAngle },
        SymbolMap{ .symbol = '&', .token = TokenType.Ampersand },
        SymbolMap{ .symbol = '|', .token = TokenType.Union },
        SymbolMap{ .symbol = '=', .token = TokenType.EqSet },
        SymbolMap{ .symbol = '-', .token = TokenType.Sub },
        SymbolMap{ .symbol = '+', .token = TokenType.Add },
        SymbolMap{ .symbol = '*', .token = TokenType.Mult },
        SymbolMap{ .symbol = '/', .token = TokenType.Div },
        SymbolMap{ .symbol = '%', .token = TokenType.Mod },
        SymbolMap{ .symbol = '!', .token = TokenType.Bang },
        SymbolMap{ .symbol = '\'', .token = TokenType.SingleQuote },
        SymbolMap{ .symbol = '"', .token = TokenType.DoubleQuote },
        SymbolMap{ .symbol = '.', .token = TokenType.Period },
        SymbolMap{ .symbol = ',', .token = TokenType.Comma },
    };

    for (symbols) |symbol| {
        if (char == symbol.symbol) {
            return symbol.token;
        }
    }

    return null;
}

pub fn freeTokens(tokens: anytype, allocator: Allocator) void {
    for (tokens) |token| {
        if (token.string != null) {
            allocator.free(token.string.?);
        }
    }
}
