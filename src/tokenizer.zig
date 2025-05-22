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

pub const TokenType = enum {
    const Self = @This();

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
    NewLine,

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
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
            .BitAnd => "&",
            .BitOr => "|",
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

    pub fn init(chars: []const u8) Self {
        const buf = logger.getBufferedWriter();

        return Self{
            .index = 0,
            .chars = chars,
            .skippedWhitespace = 0,
            .buf = buf,
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

    pub fn peak(self: Self) u8 {
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

    switch (first) {
        '\n' => return Token.init(.NewLine),
        ' ' => {
            chars.skippedWhitespace += 1;
            return null;
        },
        '+' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.AddEq);
            } else if (chars.peak() == '+') {
                _ = try chars.take();
                return Token.init(.Inc);
            }

            return Token.init(.Add);
        },
        '-' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.SubEq);
            } else if (chars.peak() == '-') {
                _ = try chars.take();
                return Token.init(.Dec);
            }

            return Token.init(.Sub);
        },
        '*' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.MultEq);
            }

            return Token.init(.Mult);
        },
        '/' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.DivEq);
            } else if (chars.peak() == '/') {
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
        ')' => return Token.init(.RParen),
        ':' => return Token.init(.Colon),
        ',' => return Token.init(.Comma),
        '<' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.LAngleEq);
            }

            return Token.init(.LAngle);
        },
        '>' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.RAngleEq);
            }

            return Token.init(.RAngle);
        },
        '=' => {
            if (chars.peak() == '=') {
                _ = try chars.take();
                return Token.init(.EqComp);
            }

            return Token.init(.EqSet);
        },
        '!' => return Token.init(.Bang),
        '.' => return Token.init(.Period),
        '&' => {
            if (chars.peak() == '&') {
                _ = try chars.take();
                return Token.init(.And);
            }
            return Token.init(.BitAnd);
        },
        '|' => {
            if (chars.peak() == '|') {
                _ = try chars.take();
                return Token.init(.Or);
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

            return Token.initStr(.StringToken, try charStr.toOwnedSlice());
        },
        else => {
            var char = first;

            var isNumber = false;
            var foundPeriod = false;
            while (std.ascii.isDigit(char) or char == '.') {
                isNumber = true;
                if (char == '.') {
                    if (foundPeriod) {
                        return chars.logError(TokenizeError.NumberHasTwoPeriods);
                    }

                    foundPeriod = true;
                }

                try charStr.append(char);
                char = try chars.take();
            }

            if (isNumber) {
                chars.returnChar();
                return Token.initStr(.Number, try charStr.toOwnedSlice());
            }

            var isIdent = false;
            while (std.ascii.isAlphanumeric(char) or isValidNameChar(char)) {
                isIdent = true;
                try charStr.append(char);

                if (!chars.hasNext()) break;
                char = try chars.take();
            }

            if (isIdent) {
                chars.returnChar();

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
        .{ .string = "f8", .token = .F8 },
        .{ .string = "f16", .token = .F16 },
        .{ .string = "f32", .token = .F32 },
        .{ .string = "f64", .token = .F64 },
        .{ .string = "f128", .token = .F128 },
    };

    return getTypeFromMap(chars, datatypes);
}

fn isKeyword(chars: []const u8) ?TokenType {
    const keywords = [_]TokenTypeMap{
        .{ .string = "var", .token = .Var },
        .{ .string = "const", .token = .Const },
        .{ .string = "fn", .token = .Fn },
        .{ .string = "struct", .token = .Struct },
        .{ .string = "if", .token = .If },
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
