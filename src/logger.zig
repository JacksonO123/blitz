const std = @import("std");
const blitz = @import("blitz.zig");
const utils = blitz.utils;
const ast = blitz.ast;
const tokenizer = blitz.tokenizer;
const blitzContext = blitz.context;
const TokenUtil = tokenizer.TokenUtil;
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

    tokens: *TokenUtil,
    code: []const u8,

    pub fn init(tokens: *TokenUtil, code: []const u8) Self {
        return Self{
            .tokens = tokens,
            .code = code,
        };
    }

    pub fn logError(self: *Self, errStr: []const u8, writer: *Writer) void {
        self.tokens.returnToken();
        const currentToken = self.tokens.tokens[self.tokens.pos];
        const tokenStart = currentToken.start;
        const tokenLen = currentToken.end - tokenStart - 1;

        const numSurroundingLines: u32 = 1;
        const contextBlock = findSurroundingLines(
            self.code,
            tokenStart,
            numSurroundingLines,
        );
        const beforeLines = self.code[contextBlock.before.start..contextBlock.before.end];
        const afterLines = self.code[contextBlock.after.start..contextBlock.after.end];

        const line = self.code[contextBlock.before.end + 1 .. contextBlock.after.start - 1];
        const startOffset = tokenStart - contextBlock.before.end - 1;

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
        writer.writeByte('^') catch {};
        i = 0;
        while (i < tokenLen) : (i += 1) {
            writer.writeByte('~') catch {};
        }
        writer.writeByte('\n') catch {};

        if (afterLines.len > 0) {
            writer.writeAll(afterLines) catch {};
            writer.writeByte('\n') catch {};
        }
    }
};

fn findSurroundingLines(
    code: []const u8,
    charIndex: usize,
    numSurroundingLines: u32,
) SurroundingBounds {
    var output = SurroundingBounds{
        .before = .{
            .start = 0,
            .end = 0,
        },
        .after = .{
            .start = 0,
            .end = 0,
        },
    };

    var current = charIndex;
    while (current > 0 and code[current - 1] != '\n') : (current -= 1) {}
    output.before.end = current - 1;

    var lineCount: u32 = 0;
    current -= 1;
    while (current > 0) : (current -= 1) {
        if (code[current - 1] == '\n') lineCount += 1;
        if (lineCount == numSurroundingLines) break;
    }
    output.before.start = current;

    current = charIndex + 1;
    while (current < code.len and code[current] != '\n') : (current += 1) {}
    output.after.start = current + 1;

    lineCount = 0;
    current += 1;
    while (current < code.len) : (current += 1) {
        if (code[current] == '\n') lineCount += 1;
        if (lineCount == numSurroundingLines) break;
    }
    output.after.end = current;

    return output;
}

pub fn logParseError(context: *Context, err: ast.ParseError, writer: *Writer) void {
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

    context.logger.logError(errString, writer);
}
