const std = @import("std");
const blitz = @import("root").blitz;
const utils = blitz.utils;
const TokenUtil = utils.TokenUtil;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const File = std.fs.File;
const Allocator = std.mem.Allocator;
const AstError = blitzAst.AstError;

const LineBounds = struct {
    start: usize,
    end: usize,
};

const SurroundingBounds = struct {
    before: LineBounds,
    after: LineBounds,
};

const size = 4096;
pub const BufferedWriterType = std.io.BufferedWriter(size, std.fs.File.Writer);

pub fn getBufferedWriter() BufferedWriterType {
    const stdout = std.io.getStdOut();
    const stdoutWriter = stdout.writer();
    return std.io.BufferedWriter(size, @TypeOf(stdoutWriter)){
        .unbuffered_writer = stdoutWriter,
    };
}

pub const Logger = struct {
    const Self = @This();

    allocator: Allocator,
    tokens: *TokenUtil,
    buf: BufferedWriterType,
    code: []const u8,

    pub fn init(allocator: Allocator, tokens: *TokenUtil, code: []const u8) Self {
        const buf = getBufferedWriter();

        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .buf = buf,
            .code = code,
        };
    }

    pub fn deinit(self: *Self) void {
        self.buf.flush() catch {};
    }

    pub fn logError(self: *Self, err: blitzAst.AstError) blitzAst.AstError {
        const writer = self.buf.writer();
        const errStr = astErrorToString(err);

        const numSurroundingLines = 1;
        const contextBlock = findSurroundingLines(self.code, self.tokens.currentLine, numSurroundingLines);
        const beforeLines = self.code[contextBlock.before.start..contextBlock.before.end];
        const afterLines = self.code[contextBlock.after.start..contextBlock.after.end];

        const lineBounds = findLineBounds(self.code, self.tokens.currentLine);
        const line = self.code[lineBounds.start..lineBounds.end];

        const tokenizeRes = tokenizer.tokenizeNumTokens(self.allocator, line, self.tokens.currentLineToken) catch {
            return err;
        };
        const tokenOffset = calculateTokenOffset(tokenizeRes.tokens, tokenizeRes.skippedWhitespace);

        writer.writeAll("Error: ") catch {};
        writer.writeAll(errStr) catch {};
        writer.writeByte('\n') catch {};

        if (beforeLines.len > 0) {
            writer.writeAll(beforeLines) catch {};
            writer.writeByte('\n') catch {};
        }

        writer.writeAll(line) catch {};
        writer.writeByte('\n') catch {};

        var i: usize = 0;
        while (i < tokenOffset) : (i += 1) {
            writer.writeByte(' ') catch {};
        }
        writer.writeAll(&[_]u8{ '^', '\n' }) catch {};

        if (afterLines.len > 0) {
            writer.writeAll(afterLines) catch {};
            writer.writeByte('\n') catch {};
        }

        return err;
    }
};

fn calculateTokenOffset(tokens: []tokenizer.Token, skippedWhitespace: usize) usize {
    if (tokens.len == 0) return skippedWhitespace;

    var res: usize = 0;
    const temp = tokens[0 .. tokens.len - 1];

    for (temp) |token| {
        const str = if (token.string != null) token.string.? else token.type.toString();
        if (token.type == .StringToken) res += 2;
        res += str.len;
    }

    return res + skippedWhitespace;
}

fn findSurroundingLines(code: []const u8, line: usize, numSurroundingLines: usize) SurroundingBounds {
    var surroundingBefore = numSurroundingLines;

    if (line < numSurroundingLines) {
        surroundingBefore = line;
    }

    var currentLine: usize = 0;
    var beforeStart: ?usize = null;
    var beforeEnd: usize = 0;
    var afterStart: ?usize = null;
    var afterEnd: usize = 0;

    for (code, 0..) |char, index| {
        if (index == code.len - 1) {
            afterEnd = index;
            break;
        }

        if (currentLine + surroundingBefore + 1 == line and beforeStart == null) {
            beforeStart = index;
        } else if (currentLine + 1 == line and char == '\n') {
            beforeEnd = index;
        } else if (currentLine == line + 1 and afterStart == null) {
            afterStart = index;
        } else if (line + numSurroundingLines == currentLine and char == '\n') {
            afterEnd = index;
            break;
        }

        if (char == '\n') currentLine += 1;
    }

    if (beforeStart != null) {
        while (code[beforeStart.?] == '\n') beforeStart = beforeStart.? + 1;
    }

    if (afterStart) |start| {
        while (code[afterStart.?] == '\n') afterStart = afterStart.? + 1;

        if (start + 1 == afterEnd) {
            afterEnd = start;
        }
    }

    return .{
        .before = .{
            .start = if (beforeStart) |start| start else 0,
            .end = beforeEnd,
        },
        .after = .{
            .start = if (afterStart) |start| start else 0,
            .end = if (afterStart == null) 0 else afterEnd,
        },
    };
}

fn findLineBounds(code: []const u8, line: usize) LineBounds {
    var start: ?usize = null;
    var end: usize = 0;
    var currentLine: usize = 0;

    for (code, 0..) |char, index| {
        if (index == code.len - 1) {
            end = index;
            break;
        }

        if (currentLine == line) {
            if (start == null) {
                start = index;
            } else if (char == '\n') {
                end = index;
                break;
            }
        }

        if (char == '\n') currentLine += 1;
    }

    return .{
        .start = if (start) |s| s else 0,
        .end = end,
    };
}

fn astErrorToString(errorType: AstError) []const u8 {
    return switch (errorType) {
        AstError.UnexpectedToken => "unexpected token",
        AstError.ExpectedNameForStruct => "expected name for struct",
        AstError.ExpectedIdentifierPropertyAccessSource => "expected identifier for property access source",
        AstError.ExpectedStructDeriveType => "expected struct derive type",
        AstError.ExpectedIdentifierForDeriveType => "expected identifier for derive type",
        AstError.ExpectedIdentifierForErrorName => "expected identifier for struct type",
        AstError.ExpectedNameForError => "expected name for error",
        AstError.ExpectedIdentifierForVariableName => "expected identifier for variable name",
        AstError.InvalidExprOperand => "invalid expression operand",
        AstError.ExpectedTokenFoundNothing => "expected token found nothing",
        AstError.ExpectedExpression => "expected expression",
        AstError.ExpectedIdentifierForFunctionName => "expected identifier for function name",
        AstError.ExpectedIdentifierForParameterName => "expected identifier for parameter name",
        AstError.ExpectedIdentifierForGenericType => "expected identifier for generic type",
        AstError.ExpectedIdentifierForPropertyAccess => "expected identifier for struct property access",
        AstError.ExpectedIdentifierForErrorVariant => "expected identifier for error variant",
        AstError.ExpectedIdentifierForStructName => "expected identifier for struct name",
        AstError.ExpectedSizeForStaticArray => "expected size for static array",
        AstError.ExpectedIdentifierForStructProperty => "expected identifier for struct property",
        AstError.ExpectedValueForStructProperty => "expected value for struct property",
        AstError.UnexpectedGenericOnErrorType => "unexpected generic type",
        AstError.ExpectedTypeExpression => "expected type expression",
        AstError.ErrorPayloadMayNotBeError => "error payload may not be error",
        AstError.ExpectedNameForFunction => "expected name for function",
        AstError.UnexpectedGeneric => "unexpected generic",
    };
}
