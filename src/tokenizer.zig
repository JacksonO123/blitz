const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const string = blitz.string;
const utils = blitz.utils;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Logger = blitz.logger.Logger;

pub const TokenizeError = error{
    NumberHasTwoPeriods,
    NoClosingQuote,
    ExpectedCharacterFoundNothing,
    UnexpectedCharacter,
    CharTokenTooLong,
    CharTokenTooShort,
};

const TokenVariants = enum {
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
    USize,
    ISize,
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
    USize,
    ISize,
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
        return switch (self) {
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
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
            .USize => "usize",
            .ISize => "isize",
            .StringType => "string",
            .StringToken => "(string data...)",
            .Bool => "bool",
            .Null => "null",
            .Identifier => "identifier",
            .Number => "number",
            .NegNumber => "[-]number",
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
            .Error => "error",
            .New => "new",
            .NewLine => "newline",
            .With => "with",
        };
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

    index: usize,
    chars: []const u8,

    pub fn init(chars: []const u8) Self {
        return Self{
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
            return self.logError(TokenizeError.ExpectedCharacterFoundNothing);
        }

        self.index += amount;
    }

    pub fn take(self: *Self) !u8 {
        if (self.index == self.chars.len) {
            return self.logError(TokenizeError.ExpectedCharacterFoundNothing);
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

    pub fn logError(self: *Self, err: TokenizeError) TokenizeError {
        const writer = std.io.getStdOut().writer();
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

        writer.writeAll(&[_]u8{ '^', '\n' }) catch {};

        return err;
    }
};

pub fn tokenize(allocator: Allocator, input: []const u8) ![]Token {
    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();
    var charUtil = CharUtil.init(input);

    while (charUtil.hasNext()) {
        const token = try parseNextToken(&charUtil);
        if (token) |tok| {
            try tokens.append(tok);
        }
    }

    return tokens.toOwnedSlice();
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
                    numberInfo.start,
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
                return chars.logError(TokenizeError.UnexpectedCharacter);
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
                return chars.logError(TokenizeError.CharTokenTooShort);
            }

            const endTick = try chars.take();
            if (endTick != '\'') return chars.logError(TokenizeError.CharTokenTooLong);

            return Token.initBounds(.CharToken, startIndex, chars.index);
        },
        '"' => {
            var current = first;
            var next = try chars.take();

            while (next != '"' or current == '\\') {
                current = next;

                if (!chars.hasNext()) {
                    return chars.logError(TokenizeError.NoClosingQuote);
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

            return chars.logError(TokenizeError.UnexpectedCharacter);
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
        return chars.logError(TokenizeError.UnexpectedCharacter);
    }

    var foundPeriod = false;
    while (std.ascii.isDigit(char) or char == '.') {
        if (char == '.') {
            if (foundPeriod) {
                return chars.logError(TokenizeError.NumberHasTwoPeriods);
            }

            foundPeriod = true;
        }

        char = try chars.take();
    }

    if (std.ascii.isAlphabetic(char)) {
        chars.returnChar();

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
            "usize",
            "isize",
        };

        for (typeStrings) |str| {
            if (chars.index + str.len >= chars.chars.len) continue;
            const charSlice = chars.chars[chars.index .. chars.index + str.len];

            if (string.compString(str, charSlice)) {
                try chars.advance(str.len + 1);
                const variant = blitzAst.AstNumberVariants.fromStr(str).?;
                return .{
                    .start = startIndex,
                    .end = chars.index - 1,
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
    const datatypes = [_]TokenTypeMap{
        .{ .string = "char", .token = .CharType },
        .{ .string = "string", .token = .StringType },
        .{ .string = "bool", .token = .Bool },
        // numbers
        .{ .string = "usize", .token = .USize },
        .{ .string = "u8", .token = .U8 },
        .{ .string = "u16", .token = .U16 },
        .{ .string = "u32", .token = .U32 },
        .{ .string = "u64", .token = .U64 },
        .{ .string = "u128", .token = .U128 },
        .{ .string = "i8", .token = .I8 },
        .{ .string = "i16", .token = .I16 },
        .{ .string = "i32", .token = .I32 },
        .{ .string = "i64", .token = .I64 },
        .{ .string = "i128", .token = .I128 },
        .{ .string = "f32", .token = .F32 },
        .{ .string = "f64", .token = .F64 },
        .{ .string = "f128", .token = .F128 },
    };

    return getTypeFromMap(chars, datatypes);
}

fn isKeyword(chars: []const u8) ?TokenType {
    const keywords = [_]TokenTypeMap{
        .{ .string = "let", .token = .Let },
        .{ .string = "mut", .token = .Mut },
        .{ .string = "fn", .token = .Fn },
        .{ .string = "struct", .token = .Struct },
        .{ .string = "if", .token = .If },
        .{ .string = "else", .token = .Else },
        .{ .string = "for", .token = .For },
        .{ .string = "while", .token = .While },
        .{ .string = "continue", .token = .Continue },
        .{ .string = "break", .token = .Break },
        .{ .string = "true", .token = .True },
        .{ .string = "false", .token = .False },
        .{ .string = "pub", .token = .Pub },
        .{ .string = "prot", .token = .Prot },
        .{ .string = "static", .token = .Static },
        .{ .string = "return", .token = .Return },
        .{ .string = "error", .token = .Error },
        .{ .string = "new", .token = .New },
        .{ .string = "with", .token = .With },
    };

    return getTypeFromMap(chars, keywords);
}

fn getTypeFromMap(chars: []const u8, map: anytype) ?TokenType {
    for (map) |mapItem| {
        if (string.compString(chars, mapItem.string)) {
            return mapItem.token;
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

    allocator: Allocator,
    pos: TokenPosition,
    tokens: []Token,
    windows: *ArrayList(usize),
    logger: *Logger,

    pub fn init(allocator: Allocator, logger: *Logger, tokens: []Token) !Self {
        const windows = try utils.initMutPtrT(ArrayList(usize), allocator);

        return Self{
            .allocator = allocator,
            .pos = .{
                .index = 0,
                .currentLine = 0,
            },
            .tokens = tokens,
            .windows = windows,
            .logger = logger,
        };
    }

    pub fn deinit(self: *Self) void {
        self.windows.deinit();
        self.allocator.destroy(self.windows);
    }

    pub fn reset(self: *Self) void {
        self.pos = .{
            .index = 0,
            .currentLine = 0,
        };
        self.windows.clearRetainingCapacity();
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
            return self.logger.logError(blitzAst.AstError.ExpectedTokenFoundNothing);
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
            return blitzAst.AstError.ExpectedTokenFoundNothing;
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
            return self.logger.logError(blitzAst.AstError.UnexpectedToken);
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
