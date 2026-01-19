const std = @import("std");
const blitz = @import("blitz.zig");
const utils = blitz.utils;
const ast = blitz.ast;
const tokenizer = blitz.tokenizer;
const blitzContext = blitz.context;
const TokenUtil = tokenizer.TokenUtil;
const File = std.fs.File;
const AstError = ast.AstError;
const Writer = std.Io.Writer;
const Context = blitzContext.Context;
const TokenError = tokenizer.TokenError;

const LineBounds = struct {
    start: usize,
    end: usize,
};

const SurroundingBounds = struct {
    before: LineBounds,
    after: LineBounds,
};

pub const Logger = struct {
    const Self = @This();

    writer: *Writer,
    tokens: *TokenUtil,
    code: []const u8,

    pub fn init(tokens: *TokenUtil, code: []const u8, writer: *Writer) Self {
        return Self{
            .writer = writer,
            .tokens = tokens,
            .code = code,
        };
    }

    pub fn logError(self: *Self, errStr: []const u8) void {
        const writer = self.writer;

        const numSurroundingLines = 1;
        const contextBlock = findSurroundingLines(
            self.code,
            self.tokens.pos.currentLine,
            numSurroundingLines,
        );
        const beforeLines = self.code[contextBlock.before.start..contextBlock.before.end];
        const afterLines = self.code[contextBlock.after.start..contextBlock.after.end];

        const lineBounds = findLineBounds(self.code, self.tokens.pos.currentLine);
        const line = self.code[lineBounds.start..lineBounds.end];
        const startOffset = getStartOffset(
            self.tokens.tokens[self.tokens.pos.index - 1].start,
            self.code,
        );

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
        while (i < startOffset) : (i += 1) {
            writer.writeByte(' ') catch {};
        }
        writer.writeAll(&[_]u8{ '^', '\n' }) catch {};

        if (afterLines.len > 0) {
            writer.writeAll(afterLines) catch {};
            writer.writeByte('\n') catch {};
        }
    }
};

fn getStartOffset(loc: usize, code: []const u8) usize {
    var offset: usize = 0;

    for (code, 0..) |char, index| {
        if (index == loc) return offset;
        if (char == '\n') {
            offset = 0;
        } else {
            offset += 1;
        }
    }

    return offset;
}

fn findSurroundingLines(
    code: []const u8,
    line: usize,
    numSurroundingLines: usize,
) SurroundingBounds {
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

pub fn logParseError(context: *Context, err: ast.ParseError) void {
    const errString = switch (err) {
        error.ExpectedNameForStruct => "expected name for struct",
        error.ExpectedIdentifierPropertyAccessSource => "expected identifier for property access source",
        error.ExpectedStructDeriveType => "expected struct derive type",
        error.ExpectedIdentifierForDeriveType => "expected identifier for derive type",
        error.ExpectedIdentifierForErrorName => "expected identifier for struct type",
        error.ExpectedNameForError => "expected name for error",
        error.ExpectedIdentifierForVariableName => "expected identifier for variable name",
        error.InvalidExprOperand => "invalid expression operand",
        error.ExpectedExpression => "expected expression",
        error.ExpectedIdentifierForFunctionName => "expected identifier for function name",
        error.ExpectedIdentifierForParameterName => "expected identifier for parameter name",
        error.ExpectedIdentifierForGenericType => "expected identifier for generic type",
        error.ExpectedIdentifierForPropertyAccess => "expected identifier for struct property access",
        error.ExpectedIdentifierForErrorVariant => "expected identifier for error variant",
        error.ExpectedIdentifierForStructName => "expected identifier for struct name",
        error.ExpectedSizeForArrayDec => "expected size for array dec",
        error.ExpectedIdentifierForStructProperty => "expected identifier for struct property",
        error.ExpectedValueForStructProperty => "expected value for struct property",
        error.UnexpectedGenericOnErrorType => "unexpected generic type",
        error.ExpectedTypeExpression => "expected type expression",
        error.ErrorPayloadMayNotBeError => "error payload may not be error",
        error.ExpectedNameForFunction => "expected name for function",
        error.UnexpectedGeneric => "unexpected generic",
        error.UnexpectedMutSpecifierOnGeneric => "unexpected mut specifier on generic",
        error.ExpectedU64ForArraySize => "expected u64 for array size",
        error.StructDefinedInLowerScope => "struct defined in lower scope",
        error.ErrorDefinedInLowerScope => "struct defined in lower scope",
        error.FunctionDefinedInLowerScope => "function defined in lower scope",
        error.UnexpectedDeriveType => "unexpected derive type",
        error.NegativeNumberWithUnsignedTypeConflict => "negative number with unsigned type conflict",
        error.ExpectedIdentifierForArrayInitIndex => "expected identifier for array init index",
        error.ExpectedIdentifierForArrayInitPtr => "expected identifier for array init ptr",
        error.ExpectedTokenFoundNothing => "expected token found nothing",
        error.UnexpectedToken => "unexpected token",
        else => @errorName(err),
    };

    context.logger.logError(errString);
}
