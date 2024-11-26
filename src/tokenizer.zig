const std = @import("std");
const utils = @import("utils.zig");
const findChar = utils.findChar;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const TokenizeError = error{
    IdentifierWithStartingNumber,
    NumberHasTwoPeriods,
    NoClosingQuote,
    CharTokenTooLong,
};

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
    Pub,
    Prot,
    Static,
    Return,

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
    EqSet,
    Sub,
    Add,
    Mult,
    Div,
    Mod,
    Bang,
    Period,
    Comma,
    QuestionMark,
    True,
    False,
    StringToken,
    CharToken,

    // datatypes
    CharType,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F8,
    F16,
    F32,
    F64,
    F128,
    USize,
    StringType,
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

    pub fn toString(self: *const TokenType) []const u8 {
        return switch (self.*) {
            .Const => "const",
            .Var => "var",
            .Pub => "pub",
            .Prot => "prot",
            .Fn => "fn",
            .Struct => "struct",
            .If => "if",
            .For => "for",
            .While => "while",
            .Continue => "continue",
            .Break => "break",
            .Colon => ":",
            .Semicolon => ";",
            .LParen => "(",
            .RParen => ")",
            .LBracket => "[",
            .RBracket => "]",
            .LBrace => "{",
            .RBrace => "}",
            .LAngle => "<",
            .RAngle => ">",
            .Ampersand => "&",
            .EqSet => "=",
            .Sub => "-",
            .Add => "+",
            .Mult => "*",
            .Div => "/",
            .Mod => "%",
            .Bang => "!",
            .Period => ".",
            .Comma => ",",
            .CharType => "char",
            .CharToken => "(char data...)",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .I128 => "i128",
            .F8 => "f8",
            .F16 => "f16",
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
            .USize => "usize",
            .StringType => "string",
            .StringToken => "(string data...)",
            .Bool => "bool",
            .Identifier => "identifier",
            .Number => "number",
            .EqComp => "==",
            .LAngleEq => "<=",
            .RAngleEq => ">=",
            .SubEq => "-=",
            .AddEq => "+=",
            .MultEq => "*=",
            .DivEq => "/=",
            .Inc => "++",
            .Dec => "--",
            .QuestionMark => "?",
            .True => "true",
            .False => "false",
            .Static => "static",
            .Return => "return",
        };
    }

    pub fn isOpenToken(self: @This(), includeAngle: bool) bool {
        const temp = switch (self) {
            .LParen, .LBrace, .LBracket => true,
            else => false,
        };

        return if (includeAngle) temp or self == TokenType.LAngle else temp;
    }

    pub fn isCloseToken(self: @This(), includeAngle: bool) bool {
        const temp = switch (self) {
            .RParen, .RBrace, .RBracket => true,
            else => false,
        };

        return if (includeAngle) temp or self == TokenType.RAngle else temp;
    }
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

pub fn tokenize(allocator: Allocator, input: []const u8) ![]Token {
    var chars = ArrayList(u8).init(allocator);
    defer chars.deinit();

    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var i: usize = 0;
    outer: while (i < input.len) : (i += 1) {
        const char = input[i];

        if (char == '"') {
            const strEnd = findChar(input, i + 1, '\"');

            if (strEnd) |end| {
                const str = try allocator.dupe(u8, input[i + 1 .. end]);
                const token = Token{ .type = TokenType.StringToken, .string = str };
                try tokens.append(token);

                i += str.len + 1;

                continue;
            }

            return TokenizeError.NoClosingQuote;
        }

        if (char == '\'') {
            if (input[i + 2] != '\'') {
                return TokenizeError.CharTokenTooLong;
            }

            const str = try allocator.dupe(u8, &[_]u8{input[i + 1]});
            const token = Token{ .type = TokenType.CharToken, .string = str };
            try tokens.append(token);
            i += 2;

            continue;
        }

        if (i < input.len - 1 and char == '/') {
            const next = input[i + 1];
            if (next == '/') {
                const index = findChar(input, i, '\n');

                if (index) |idx| {
                    i = idx;
                    continue;
                }

                break;
            } else if (next == '*') {
                var index = findChar(input, i, '/');
                if (index != null) break;

                while (input[index.? - 1] != '*') {
                    index = findChar(input, index.? + 1, '/');
                    if (index) |newIndex| {
                        i = newIndex;
                    } else {
                        continue :outer;
                    }
                }

                continue;
            }
        }

        if (i < input.len - 1) {
            var tokenType: ?TokenType = null;

            if (char == '+' and input[i + 1] == '+') {
                tokenType = TokenType.Inc;
            } else if (char == '-' and input[i + 1] == '-') {
                tokenType = TokenType.Dec;
            }

            if (tokenType) |tokType| {
                const firstToken = try charsToToken(chars.items, allocator);

                if (firstToken) |firstTok| {
                    try tokens.append(firstTok);
                    chars.clearRetainingCapacity();
                }

                const token = Token{ .type = tokType, .string = null };
                try tokens.append(token);
                i += 1;

                continue;
            }
        }

        const postEq = isPostEqSymbol(input, i);
        if (postEq) |tok| {
            const firstToken = try charsToToken(chars.items, allocator);

            if (firstToken) |firstTok| {
                try tokens.append(firstTok);
                chars.clearRetainingCapacity();
            }

            i += 1;
            try tokens.append(tok);

            continue;
        }

        if (char == '.') {
            if (isNumber(chars.items)) {
                if (findChar(chars.items, 0, '.') != null) {
                    return TokenizeError.NumberHasTwoPeriods;
                }

                try chars.append(char);
                continue;
            } else if (chars.items.len > 0 and std.ascii.isDigit(chars.items[0])) {
                return TokenizeError.IdentifierWithStartingNumber;
            }
        }

        const symbol = isSymbol(char);
        if (symbol) |sym| {
            const firstToken = try charsToToken(chars.items, allocator);

            if (firstToken) |firstTok| {
                try tokens.append(firstTok);
                chars.clearRetainingCapacity();
            }

            const secondToken = Token{ .string = null, .type = sym };
            try tokens.append(secondToken);

            continue;
        }

        if (std.ascii.isWhitespace(char)) {
            const token = try charsToToken(chars.items, allocator);

            if (token) |tok| {
                try tokens.append(tok);
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

fn isPostEqSymbol(chars: []const u8, start: usize) ?Token {
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

        if (res) |tok| {
            return Token{ .type = tok, .string = null };
        }

        return null;
    }

    return null;
}

fn isNumber(chars: []u8) bool {
    if (chars.len == 0) return false;

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
    if (datatype) |dt| {
        return Token{ .string = null, .type = dt };
    }

    const keyword = isKeyword(chars);
    if (keyword) |k| {
        return Token{ .string = null, .type = k };
    } else {
        const str = try allocator.dupe(u8, chars);
        return Token{ .string = str, .type = TokenType.Identifier };
    }
}

fn isDatatype(chars: []const u8) ?TokenType {
    const datatypes = [_]TokenTypeMap{
        TokenTypeMap{ .string = "char", .token = TokenType.CharType },
        TokenTypeMap{ .string = "string", .token = TokenType.StringType },
        TokenTypeMap{ .string = "bool", .token = TokenType.Bool },
        TokenTypeMap{ .string = "usize", .token = TokenType.USize },
        // numbers
        TokenTypeMap{ .string = "u16", .token = TokenType.U16 },
        TokenTypeMap{ .string = "u32", .token = TokenType.U32 },
        TokenTypeMap{ .string = "u64", .token = TokenType.U64 },
        TokenTypeMap{ .string = "u128", .token = TokenType.U128 },
        TokenTypeMap{ .string = "i16", .token = TokenType.I16 },
        TokenTypeMap{ .string = "i32", .token = TokenType.I32 },
        TokenTypeMap{ .string = "i64", .token = TokenType.I64 },
        TokenTypeMap{ .string = "i128", .token = TokenType.I128 },
        TokenTypeMap{ .string = "f16", .token = TokenType.F16 },
        TokenTypeMap{ .string = "f32", .token = TokenType.F32 },
        TokenTypeMap{ .string = "f64", .token = TokenType.F64 },
        TokenTypeMap{ .string = "f128", .token = TokenType.F128 },
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
        TokenTypeMap{ .string = "true", .token = TokenType.True },
        TokenTypeMap{ .string = "false", .token = TokenType.False },
        TokenTypeMap{ .string = "pub", .token = TokenType.Pub },
        TokenTypeMap{ .string = "prot", .token = TokenType.Prot },
        TokenTypeMap{ .string = "static", .token = TokenType.Static },
        TokenTypeMap{ .string = "return", .token = TokenType.Return },
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
        SymbolMap{ .symbol = '=', .token = TokenType.EqSet },
        SymbolMap{ .symbol = '-', .token = TokenType.Sub },
        SymbolMap{ .symbol = '+', .token = TokenType.Add },
        SymbolMap{ .symbol = '*', .token = TokenType.Mult },
        SymbolMap{ .symbol = '/', .token = TokenType.Div },
        SymbolMap{ .symbol = '%', .token = TokenType.Mod },
        SymbolMap{ .symbol = '!', .token = TokenType.Bang },
        SymbolMap{ .symbol = '.', .token = TokenType.Period },
        SymbolMap{ .symbol = ',', .token = TokenType.Comma },
        SymbolMap{ .symbol = '?', .token = TokenType.QuestionMark },
    };

    for (symbols) |symbol| {
        if (char == symbol.symbol) {
            return symbol.token;
        }
    }

    return null;
}
