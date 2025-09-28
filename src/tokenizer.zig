const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const string = blitz.string;
const utils = blitz.utils;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Logger = blitz.logger.Logger;
const Writer = std.Io.Writer;

const INIT_TOK_CAPACITY = 1024 * 10;

pub const TokenizeError = error{
    NumberHasTwoPeriods,
    NoClosingQuote,
    ExpectedCharacterFoundNothing,
    UnexpectedCharacter,
    CharTokenTooLong,
    CharTokenTooShort,
};

pub const TokenError = error{
    ExpectedTokenFoundNothing,
    UnexpectedToken,
};

const TokenVariants = enum {
    const Self = @This();

    // keywords
    Let,
    Fn,
    Struct,
    If,
    Else,
    For,
    While,
    Continue,
    Break,
    Pub,
    Prot,
    Static,
    Return,
    Error,
    Mut,
    New,
    With,
    Delete,
    Cast,

    // symbols
    Colon,
    Semicolon,
    Ampersand,
    Asterisk,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    BitOr,
    And,
    Or,
    Sub,
    Add,
    Div,
    Mod,
    BitAndEq,
    BitOrEq,
    AndEq,
    OrEq,
    EqSet,
    Bang,
    Period,
    Comma,
    QuestionMark,
    StringToken,
    CharToken,
    EqComp,
    LAngleEq,
    RAngleEq,
    SubEq,
    AddEq,
    MultEq,
    DivEq,
    Inc,
    Dec,

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
    F32,
    F64,
    F128,
    StringType,
    Bool,
    Null,
    True,
    False,

    // other
    Identifier,
    Number,
    NegNumber,
    NewLine,

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            // keywords + operators
            .Let => "let",
            .Mut => "mut",
            .Pub => "pub",
            .Prot => "prot",
            .Fn => "fn",
            .Struct => "struct",
            .If => "if",
            .Else => "else",
            .For => "for",
            .While => "while",
            .Continue => "continue",
            .Break => "break",
            .Cast => "cast",
            .Static => "static",
            .Return => "return",
            .Error => "error",
            .New => "new",
            .With => "with",
            .Delete => "delete",
            .CharType => "char",
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
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
            .StringType => "string",
            .Bool => "bool",
            .Null => "null",
            .True => "true",
            .False => "false",
            .Colon => ":",
            .Semicolon => ";",
            .Ampersand => "&",
            .Asterisk => "*",
            .LParen => "(",
            .RParen => ")",
            .LBracket => "[",
            .RBracket => "]",
            .LBrace => "{",
            .RBrace => "}",
            .LAngle => "<",
            .RAngle => ">",
            .BitOr => "|",
            .BitAndEq => "&=",
            .BitOrEq => "|=",
            .AndEq => "&=",
            .OrEq => "|=",
            .And => "&&",
            .Or => "||",
            .EqSet => "=",
            .Sub => "-",
            .Add => "+",
            .Div => "/",
            .Mod => "%",
            .Bang => "!",
            .Period => ".",
            .Comma => ",",
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

            // misc
            .NewLine => "newline",
            .CharToken => "(char data...)",
            .StringToken => "(string data...)",
            .Identifier => "identifier",
            .Number => "number",
            .NegNumber => "[-]number",
        };
    }
};

pub const TokenType = union(TokenVariants) {
    const Self = @This();

    // keywords
    Let,
    Fn,
    Struct,
    If,
    Else,
    For,
    While,
    Continue,
    Break,
    Pub,
    Prot,
    Static,
    Return,
    Error,
    Mut,
    New,
    With,
    Delete,
    Cast,

    // symbols
    Colon,
    Semicolon,
    Ampersand,
    Asterisk,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    BitOr,
    And,
    Or,
    Sub,
    Add,
    Div,
    Mod,
    BitAndEq,
    BitOrEq,
    AndEq,
    OrEq,
    EqSet,
    Bang,
    Period,
    Comma,
    QuestionMark,
    StringToken,
    CharToken,
    EqComp,
    LAngleEq,
    RAngleEq,
    SubEq,
    AddEq,
    MultEq,
    DivEq,
    Inc,
    Dec,

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
    F32,
    F64,
    F128,
    StringType,
    Bool,
    Null,
    True,
    False,

    // other
    Identifier,
    Number: blitzAst.AstNumberVariants,
    NegNumber: blitzAst.AstNumberVariants,
    NewLine,

    pub fn toString(self: Self) []const u8 {
        const active = std.meta.activeTag(self);
        return active.toString();
    }
};

pub const Token = struct {
    const Self = @This();

    type: TokenType,
    start: usize,
    end: usize,

    pub fn init(tokenType: TokenType, index: usize) Self {
        return Self{
            .type = tokenType,
            .start = index,
            .end = index + 1,
        };
    }

    pub fn initBounds(tokenType: TokenType, start: usize, end: usize) Self {
        return Self{
            .type = tokenType,
            .start = start,
            .end = end,
        };
    }

    pub fn isOpenToken(self: Self, includeAngle: bool) bool {
        const temp = switch (self.type) {
            .LParen, .LBrace, .LBracket => true,
            else => false,
        };

        return if (includeAngle) temp or self.type == .LAngle else temp;
    }

    pub fn isCloseToken(self: Self, includeAngle: bool) bool {
        const temp = switch (self.type) {
            .RParen, .RBrace, .RBracket => true,
            else => false,
        };

        return if (includeAngle) temp or self.type == .RAngle else temp;
    }

    pub fn strFromCode(self: Self, code: []u8) []u8 {
        return code[self.start..self.end];
    }
};

const TokenTypeMap = struct {
    string: []const u8,
    token: TokenType,
};

const LineBounds = struct {
    start: usize,
    end: usize,
};

const CharUtil = struct {
    const Self = @This();

    writer: *Writer,
    index: usize,
    chars: []const u8,

    pub fn init(chars: []const u8, writer: *Writer) Self {
        return Self{
            .writer = writer,
            .index = 0,
            .chars = chars,
        };
    }

    pub fn getSlice(self: Self) []const u8 {
        return self.chars[self.index..];
    }

    pub fn hasNext(self: Self) bool {
        return self.index < self.chars.len;
    }

    pub fn advance(self: *Self, amount: usize) !void {
        if (self.index + amount > self.chars.len) {
            return TokenizeError.ExpectedCharacterFoundNothing;
        }

        self.index += amount;
    }

    pub fn take(self: *Self) !u8 {
        if (self.index == self.chars.len) {
            return TokenizeError.ExpectedCharacterFoundNothing;
        }

        const char = self.chars[self.index];
        self.index += 1;
        return char;
    }

    pub fn peak(self: Self) !u8 {
        if (self.index >= self.chars.len) {
            return TokenizeError.ExpectedCharacterFoundNothing;
        }

        return self.chars[self.index];
    }

    pub fn returnChar(self: *Self) void {
        self.index -= 1;
    }

    pub fn getChars(self: Self) []const u8 {
        return self.chars;
    }

    pub fn logError(self: *Self, err: TokenizeError) void {
        const writer = self.writer;
        const errStr = tokenizeErrorToString(err);

        const index = self.index - 1;
        const bounds = getLineBounds(self.chars, index);
        const charIndex = index - bounds.start;
        const line = self.chars[bounds.start..bounds.end];

        writer.writeAll("Error: ") catch {};
        writer.writeAll(errStr) catch {};
        writer.writeByte('\n') catch {};
        writer.writeAll(line) catch {};
        writer.writeByte('\n') catch {};

        var i: usize = 0;
        while (i < charIndex) : (i += 1) {
            writer.writeByte(' ') catch {};
        }

        writer.writeAll("^\n") catch {};
    }
};

pub fn tokenize(allocator: Allocator, input: []const u8, writer: *Writer) ![]Token {
    var tokens = try ArrayList(Token).initCapacity(allocator, INIT_TOK_CAPACITY);
    var charUtil = CharUtil.init(input, writer);

    while (charUtil.hasNext()) {
        const token = parseNextToken(&charUtil) catch |e| {
            charUtil.logError(e);
            return e;
        };
        if (token) |tok| {
            try tokens.append(allocator, tok);
        }
    }

    return try tokens.toOwnedSlice(allocator);
}

fn parseNextToken(chars: *CharUtil) !?Token {
    if (!chars.hasNext()) return null;
    const startIndex = chars.index;
    const first = try chars.take();

    switch (first) {
        '\n' => return Token.init(.NewLine, startIndex),
        ' ' => {
            return null;
        },
        '+' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.AddEq, startIndex);
            } else if ((try chars.peak()) == '+') {
                _ = try chars.take();
                return Token.init(.Inc, startIndex);
            }

            return Token.init(.Add, startIndex);
        },
        '-' => {
            const nextPeak = try chars.peak();
            if (std.ascii.isDigit(nextPeak)) {
                const numberInfo = try parseNumber(chars);
                chars.returnChar();
                return Token.initBounds(
                    .{ .NegNumber = numberInfo.numType orelse .I32 },
                    numberInfo.start - 1,
                    numberInfo.end,
                );
            } else if (nextPeak == '=') {
                _ = try chars.take();
                return Token.init(.SubEq, startIndex);
            } else if (nextPeak == '-') {
                _ = try chars.take();
                return Token.init(.Dec, startIndex);
            }

            return Token.init(.Sub, startIndex);
        },
        '*' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.MultEq, startIndex);
            }

            return Token.init(.Asterisk, startIndex);
        },
        '/' => {
            const nextPeak = try chars.peak();
            if (nextPeak == '=') {
                _ = try chars.take();
                return Token.init(.DivEq, startIndex);
            } else if (nextPeak == '/') {
                var next = try chars.take();
                while (next != '\n') {
                    if (!chars.hasNext()) return null;

                    next = try chars.take();
                }

                chars.returnChar();
                return null;
            }
            return Token.init(.Div, startIndex);
        },
        ';' => return Token.init(.Semicolon, startIndex),
        '{' => return Token.init(.LBrace, startIndex),
        '}' => return Token.init(.RBrace, startIndex),
        '[' => return Token.init(.LBracket, startIndex),
        ']' => return Token.init(.RBracket, startIndex),
        '(' => return Token.init(.LParen, startIndex),
        ')' => {
            return Token.init(.RParen, startIndex);
        },
        ':' => return Token.init(.Colon, startIndex),
        ',' => return Token.init(.Comma, startIndex),
        '<' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.LAngleEq, startIndex);
            }

            return Token.init(.LAngle, startIndex);
        },
        '>' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.RAngleEq, startIndex);
            }

            return Token.init(.RAngle, startIndex);
        },
        '=' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.EqComp, startIndex);
            }

            return Token.init(.EqSet, startIndex);
        },
        '!' => return Token.init(.Bang, startIndex),
        '.' => {
            const next = try chars.peak();
            if (!std.ascii.isAlphabetic(next) and !isValidNameChar(next)) {
                _ = try chars.take();
                return TokenizeError.UnexpectedCharacter;
            }

            return Token.init(.Period, startIndex);
        },
        '&' => {
            const peakChar = try chars.peak();

            if (peakChar == '&') {
                _ = try chars.take();

                if ((try chars.peak()) == '=') {
                    _ = try chars.take();
                    return Token.init(.AndEq, startIndex);
                }

                return Token.init(.And, startIndex);
            } else if (peakChar == '=') {
                _ = try chars.take();
                return Token.init(.BitAndEq, startIndex);
            }

            return Token.init(.Ampersand, startIndex);
        },
        '|' => {
            const peakChar = try chars.peak();

            if (peakChar == '|') {
                _ = try chars.take();

                if ((try chars.peak()) == '=') {
                    _ = try chars.take();
                    return Token.init(.OrEq, startIndex);
                }

                return Token.init(.Or, startIndex);
            } else if (peakChar == '=') {
                _ = try chars.take();
                return Token.init(.BitOrEq, startIndex);
            }

            return Token.init(.BitOr, startIndex);
        },
        '%' => return Token.init(.Mod, startIndex),
        '?' => return Token.init(.QuestionMark, startIndex),
        '\'' => {
            var next = try chars.take();
            if (next == '\\') {
                next = try chars.take();
            } else if (next == '\'') {
                return TokenizeError.CharTokenTooShort;
            }

            const endTick = try chars.take();
            if (endTick != '\'') return TokenizeError.CharTokenTooLong;

            return Token.initBounds(.CharToken, startIndex, chars.index);
        },
        '"' => {
            var current = first;
            var next = try chars.take();

            while (next != '"' or current == '\\') {
                current = next;

                if (!chars.hasNext()) {
                    return TokenizeError.NoClosingQuote;
                }
                next = try chars.take();
            }

            return Token.initBounds(.StringToken, startIndex, chars.index);
        },
        else => {
            if (std.ascii.isDigit(first)) {
                chars.returnChar();
                const numberInfo = try parseNumber(chars);
                chars.returnChar();
                return Token.initBounds(
                    .{ .Number = numberInfo.numType orelse .U32 },
                    numberInfo.start,
                    numberInfo.end,
                );
            }

            var char = first;
            var isIdent = false;
            while (std.ascii.isAlphanumeric(char) or isValidNameChar(char)) {
                isIdent = true;
                if (!chars.hasNext()) break;
                char = try chars.take();
            }
            const endIndex = chars.index - 1;

            if (isIdent) {
                chars.returnChar();

                if (string.compString(chars.chars[startIndex..endIndex], "null")) {
                    return Token.init(.Null, startIndex);
                }

                if (isKeyword(chars.chars[startIndex..endIndex])) |keywordType| {
                    return Token.init(keywordType, startIndex);
                }

                if (isDatatype(chars.chars[startIndex..endIndex])) |dataType| {
                    return Token.init(dataType, startIndex);
                }

                return Token.initBounds(.Identifier, startIndex, endIndex);
            }

            return TokenizeError.UnexpectedCharacter;
        },
    }
}

const ParsedNumberInfo = struct {
    start: usize,
    end: usize,
    numType: ?blitzAst.AstNumberVariants,
};

fn parseNumber(chars: *CharUtil) !ParsedNumberInfo {
    const startIndex: usize = chars.index;

    var char = try chars.take();
    if (char == '.') {
        return TokenizeError.UnexpectedCharacter;
    }

    var foundPeriod = false;
    while (std.ascii.isDigit(char) or char == '.') {
        if (char == '.') {
            if (foundPeriod) {
                return TokenizeError.NumberHasTwoPeriods;
            }

            foundPeriod = true;
        }

        char = try chars.take();
    }

    if (std.ascii.isAlphabetic(char)) {
        chars.returnChar();
        const endIndex = chars.index;

        const typeStrings = &[_][]const u8{
            "char",
            "u8",
            "u16",
            "u32",
            "u64",
            "u128",
            "i8",
            "i16",
            "i32",
            "i64",
            "i128",
            "f32",
            "f64",
            "f128",
        };

        for (typeStrings) |str| {
            if (chars.index + str.len >= chars.chars.len) continue;
            const charSlice = chars.chars[chars.index .. chars.index + str.len];

            if (string.compString(str, charSlice)) {
                try chars.advance(str.len + 1);
                const variant = blitzAst.AstNumberVariants.fromStr(str).?;
                return .{
                    .start = startIndex,
                    .end = endIndex,
                    .numType = variant,
                };
            }
        }

        _ = try chars.take();
    }

    return .{
        .start = startIndex,
        .end = chars.index - 1,
        .numType = null,
    };
}

fn getLineBounds(chars: []const u8, index: usize) LineBounds {
    var lineStart: usize = 0;
    var lineEnd: usize = 0;

    for (chars, 0..) |char, charIndex| {
        if (char == '\n') {
            if (charIndex < index) {
                lineStart = charIndex + 1;
            } else {
                lineEnd = charIndex;

                return .{
                    .start = lineStart,
                    .end = lineEnd,
                };
            }
        }
    }

    if (lineEnd < lineStart) lineEnd = chars.len;

    return .{
        .start = lineStart,
        .end = lineEnd,
    };
}

fn isValidNameChar(char: u8) bool {
    return switch (char) {
        '_' => true,
        else => false,
    };
}

fn isDatatype(chars: []const u8) ?TokenType {
    const datatypes = .{
        .CharType,
        .StringType,
        .Bool,
        .U8,
        .U16,
        .U32,
        .U64,
        .U128,
        .I8,
        .I16,
        .I32,
        .I64,
        .I128,
        .F32,
        .F64,
        .F128,
    };

    return getTypeFromTuple(chars, datatypes);
}

fn isKeyword(chars: []const u8) ?TokenType {
    const keywords = .{
        .Let,
        .Mut,
        .Fn,
        .Struct,
        .If,
        .Else,
        .For,
        .While,
        .Continue,
        .Break,
        .True,
        .False,
        .Pub,
        .Prot,
        .Static,
        .Return,
        .Error,
        .New,
        .With,
        .Delete,
        .Cast,
    };

    return getTypeFromTuple(chars, keywords);
}

fn getTypeFromTuple(chars: []const u8, tuple: anytype) ?TokenType {
    inline for (tuple) |item| {
        if (string.compString(chars, @as(TokenType, item).toString())) {
            return item;
        }
    }

    return null;
}

fn tokenizeErrorToString(err: TokenizeError) []const u8 {
    return switch (err) {
        TokenizeError.CharTokenTooLong => "char token too long",
        TokenizeError.CharTokenTooShort => "char token too short",
        TokenizeError.ExpectedCharacterFoundNothing => "expected character found nothing",
        TokenizeError.NoClosingQuote => "no closing quote",
        TokenizeError.NumberHasTwoPeriods => "number has two periods",
        TokenizeError.UnexpectedCharacter => "unexpected character",
    };
}

const TokenPosition = struct {
    index: usize,
    currentLine: usize,
};

pub const TokenUtil = struct {
    const Self = @This();

    pos: TokenPosition,
    tokens: []Token,

    pub fn init(tokens: []Token) Self {
        return Self{
            .pos = .{
                .index = 0,
                .currentLine = 0,
            },
            .tokens = tokens,
        };
    }

    pub fn reset(self: *Self) void {
        self.pos = .{
            .index = 0,
            .currentLine = 0,
        };
    }

    pub fn take(self: *Self) !Token {
        const res = try self.takeFixed();

        if (res.type == .NewLine) {
            return try self.take();
        }

        return res;
    }

    pub fn takeFixed(self: *Self) !Token {
        if (self.pos.index >= self.tokens.len) {
            return TokenError.ExpectedTokenFoundNothing;
        }

        const res = self.tokens[self.pos.index];
        self.pos.index += 1;

        if (res.type == .NewLine) {
            self.pos.currentLine += 1;
        }

        return res;
    }

    pub fn peakFixed(self: Self) !Token {
        if (self.pos.index >= self.tokens.len) {
            return TokenError.ExpectedTokenFoundNothing;
        }

        return self.tokens[self.pos.index];
    }

    pub fn peak(self: *Self) !Token {
        const res = try self.peakFixed();

        if (res.type == .NewLine) {
            self.pos.index += 1;
            self.pos.currentLine += 1;

            const newRes = self.peak();

            self.pos.index -= 1;
            self.pos.currentLine -= 1;

            return newRes;
        }

        return res;
    }

    pub fn returnToken(self: *Self) void {
        self.pos.index -= 1;
        while (self.tokens[self.pos.index].type == .NewLine) : (self.pos.index -= 1) {}
    }

    pub fn expectToken(self: *Self, tokenType: TokenType) !void {
        const token = try self.take();
        if (std.meta.activeTag(token.type) != std.meta.activeTag(tokenType)) {
            return TokenError.UnexpectedToken;
        }
    }

    pub fn hasNextFixed(self: Self) bool {
        if (self.pos.index < self.tokens.len) return true;
        return false;
    }

    pub fn hasNext(self: *Self) bool {
        _ = self.peak() catch {
            return false;
        };

        return true;
    }
};
