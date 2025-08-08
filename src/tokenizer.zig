const std = @import("std");
const blitz = @import("blitz.zig");
const string = blitz.string;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const logger = blitz.logger;

pub const TokenizeError = error{
    NumberHasTwoPeriods,
    NoClosingQuote,
    ExpectedCharacterFoundNothing,
    UnexpectedCharacter,
    CharTokenTooLong,
    CharTokenTooShort,
};

pub const TokenType = enum(u16) {
    const Self = @This();

    // keywords
    Let,
    Mut,
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
    BitAnd,
    BitOr,
    And,
    Or,
    Sub,
    Add,
    Mult,
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
            .LParen => "(",
            .RParen => ")",
            .LBracket => "[",
            .RBracket => "]",
            .LBrace => "{",
            .RBrace => "}",
            .LAngle => "<",
            .RAngle => ">",
            .BitAnd => "&",
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
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
            .USize => "usize",
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
            .NewLine => "newline",
        };
    }
};

const TokenTypeMap = struct {
    string: []const u8,
    token: TokenType,
};

pub const Token = struct {
    const Self = @This();

    type: TokenType,
    string: ?[]u8,

    pub fn init(tokenType: TokenType) Self {
        return Self{
            .type = tokenType,
            .string = null,
        };
    }

    pub fn initStr(tokenType: TokenType, str: []u8) Self {
        return Self{
            .type = tokenType,
            .string = str,
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
};

const TokenizerOut = struct {
    tokens: []Token,
    skippedWhitespace: usize,
};

const LineBounds = struct {
    start: usize,
    end: usize,
};

const CharUtil = struct {
    const Self = @This();

    index: usize,
    chars: []const u8,
    skippedWhitespace: usize,
    buf: logger.BufferedWriterType,
    allowPeriod: bool,

    pub fn init(chars: []const u8) Self {
        const buf = logger.getBufferedWriter();

        return Self{
            .index = 0,
            .chars = chars,
            .skippedWhitespace = 0,
            .buf = buf,
            .allowPeriod = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.buf.flush() catch {};
    }

    pub fn getSlice(self: Self) []const u8 {
        return self.chars[self.index..];
    }

    pub fn hasNext(self: Self) bool {
        return self.index < self.chars.len;
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
        const writer = self.buf.writer();
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
    defer charUtil.deinit();

    while (charUtil.hasNext()) {
        const token = try parseNextToken(allocator, &charUtil);
        if (token) |tok| {
            try tokens.append(tok);
        }
    }

    return tokens.toOwnedSlice();
}

pub fn tokenizeNumTokens(allocator: Allocator, input: []const u8, numTokens: usize) !TokenizerOut {
    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();
    var charUtil = CharUtil.init(input);
    defer charUtil.deinit();

    var i: usize = 0;
    while (i < numTokens and charUtil.hasNext()) {
        const token = try parseNextToken(allocator, &charUtil);
        if (token) |tok| {
            try tokens.append(tok);
            i += 1;
        }
    }

    return .{
        .tokens = try tokens.toOwnedSlice(),
        .skippedWhitespace = charUtil.skippedWhitespace,
    };
}

fn parseNextToken(allocator: Allocator, chars: *CharUtil) !?Token {
    if (!chars.hasNext()) return null;
    const first = try chars.take();
    var charStr = ArrayList(u8).init(allocator);
    defer charStr.deinit();

    if (first != '.' and chars.allowPeriod) {
        chars.allowPeriod = false;
    }

    switch (first) {
        '\n' => return Token.init(.NewLine),
        ' ' => {
            chars.skippedWhitespace += 1;
            return null;
        },
        '+' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.AddEq);
            } else if ((try chars.peak()) == '+') {
                _ = try chars.take();
                return Token.init(.Inc);
            }

            return Token.init(.Add);
        },
        '-' => {
            const nextPeak = try chars.peak();
            if (std.ascii.isDigit(nextPeak)) {
                const number = try parseNumber(allocator, chars);
                chars.returnChar();
                return Token.initStr(.NegNumber, number);
            } else if (nextPeak == '=') {
                _ = try chars.take();
                return Token.init(.SubEq);
            } else if (nextPeak == '-') {
                _ = try chars.take();
                return Token.init(.Dec);
            }

            return Token.init(.Sub);
        },
        '*' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.MultEq);
            }

            return Token.init(.Mult);
        },
        '/' => {
            const nextPeak = try chars.peak();
            if (nextPeak == '=') {
                _ = try chars.take();
                return Token.init(.DivEq);
            } else if (nextPeak == '/') {
                var next = try chars.take();
                while (next != '\n') {
                    if (!chars.hasNext()) return null;

                    next = try chars.take();
                }

                chars.returnChar();
                return null;
            }
            return Token.init(.Div);
        },
        ';' => return Token.init(.Semicolon),
        '{' => return Token.init(.LBrace),
        '}' => return Token.init(.RBrace),
        '[' => return Token.init(.LBracket),
        ']' => return Token.init(.RBracket),
        '(' => return Token.init(.LParen),
        ')' => {
            chars.allowPeriod = true;
            return Token.init(.RParen);
        },
        ':' => return Token.init(.Colon),
        ',' => return Token.init(.Comma),
        '<' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.LAngleEq);
            }

            return Token.init(.LAngle);
        },
        '>' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.RAngleEq);
            }

            return Token.init(.RAngle);
        },
        '=' => {
            if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.EqComp);
            }

            return Token.init(.EqSet);
        },
        '!' => return Token.init(.Bang),
        '.' => {
            if (!chars.allowPeriod) {
                return chars.logError(TokenizeError.UnexpectedCharacter);
            }

            const next = try chars.peak();
            if (!std.ascii.isAlphabetic(next) and !isValidNameChar(next)) {
                _ = try chars.take();
                return chars.logError(TokenizeError.UnexpectedCharacter);
            }

            chars.allowPeriod = false;
            return Token.init(.Period);
        },
        '&' => {
            if ((try chars.peak()) == '&') {
                _ = try chars.take();

                if ((try chars.peak()) == '=') {
                    _ = try chars.take();
                    return Token.init(.AndEq);
                }

                return Token.init(.And);
            } else if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.BitAndEq);
            }
            return Token.init(.BitAnd);
        },
        '|' => {
            if ((try chars.peak()) == '|') {
                _ = try chars.take();

                if ((try chars.peak()) == '=') {
                    _ = try chars.take();
                    return Token.init(.OrEq);
                }

                return Token.init(.Or);
            } else if ((try chars.peak()) == '=') {
                _ = try chars.take();
                return Token.init(.BitOrEq);
            }

            return Token.init(.BitOr);
        },
        '%' => return Token.init(.Mod),
        '?' => return Token.init(.QuestionMark),
        '\'' => {
            var next = try chars.take();
            if (next == '\\') {
                next = try chars.take();
            } else if (next == '\'') {
                return chars.logError(TokenizeError.CharTokenTooShort);
            }

            const endTick = try chars.take();
            if (endTick != '\'') return chars.logError(TokenizeError.CharTokenTooLong);

            const char = try allocator.dupe(u8, &[_]u8{next});
            return Token.initStr(.CharToken, char);
        },
        '"' => {
            var current = first;
            var next = try chars.take();

            while (next != '"' or current == '\\') {
                try charStr.append(next);
                current = next;

                if (!chars.hasNext()) {
                    return chars.logError(TokenizeError.NoClosingQuote);
                }
                next = try chars.take();
            }

            chars.allowPeriod = true;

            return Token.initStr(.StringToken, try charStr.toOwnedSlice());
        },
        else => {
            if (std.ascii.isDigit(first)) {
                chars.returnChar();
                const number = try parseNumber(allocator, chars);
                chars.returnChar();
                return Token.initStr(.Number, number);
            }

            var char = first;
            var isIdent = false;
            while (std.ascii.isAlphanumeric(char) or isValidNameChar(char)) {
                isIdent = true;
                chars.allowPeriod = true;
                try charStr.append(char);

                if (!chars.hasNext()) break;
                char = try chars.take();
            }

            if (isIdent) {
                chars.returnChar();

                if (string.compString(charStr.items, "null")) {
                    return Token.init(.Null);
                }

                if (isKeyword(charStr.items)) |keywordType| {
                    return Token.init(keywordType);
                }

                if (isDatatype(charStr.items)) |dataType| {
                    return Token.init(dataType);
                }

                return Token.initStr(.Identifier, try charStr.toOwnedSlice());
            }

            return chars.logError(TokenizeError.UnexpectedCharacter);
        },
    }
}

fn parseNumber(allocator: Allocator, chars: *CharUtil) ![]u8 {
    var number = ArrayList(u8).init(allocator);
    defer number.deinit();

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

        try number.append(char);
        char = try chars.take();
    }

    return number.toOwnedSlice();
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
