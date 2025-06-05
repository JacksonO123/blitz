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

        const lineBounds = findLineBounds(self.code, self.tokens.currentLine);
        const line = self.code[lineBounds.start..lineBounds.end];
        const res = tokenizer.tokenizeNumTokens(self.allocator, line, self.tokens.currentLineToken) catch {
            return err;
        };
        const tokenOffset = calculateTokenOffset(res.tokens, res.skippedWhitespace);

        writer.writeAll("Error: ") catch {};
        writer.writeAll(errStr) catch {};
        writer.writeByte('\n') catch {};
        writer.writeAll(line) catch {};
        writer.writeByte('\n') catch {};

        var i: usize = 0;
        while (i < tokenOffset) : (i += 1) {
            writer.writeByte(' ') catch {};
        }
        writer.writeAll(&[_]u8{ '^', '\n' }) catch {};

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

fn findLineBounds(code: []const u8, line: usize) LineBounds {
    var start: usize = 0;
    var end: usize = 0;
    var currentLine: usize = 0;

    for (code, 0..) |char, index| {
        if (currentLine == line) {
            if (char == '\n' or index == code.len - 1) {
                end = index;
                break;
            }
        }

        if (char == '\n') {
            currentLine += 1;
            start = index + 1;
        }
    }

    if (start > end) end = start;

    return .{
        .start = start,
        .end = end,
    };
}

fn astErrorToString(errorType: AstError) []const u8 {
    return switch (errorType) {
        AstError.UnexpectedToken => "unexpected token",
        AstError.TokenNotFound => "token not found",
        AstError.InvalidType => "invalid type",
        AstError.UnknownType => "unknown type",
        AstError.ExpectedGenericArgument => "expected generic argument",
        AstError.InvalidStructKey => "invalid struct key",
        AstError.FunctionNotFound => "function not found",
        AstError.ExpectedNameForStruct => "expected name for struct",
        AstError.ExpectedIdentifierPropertyAccessSource => "expected identifier for property access source",
        AstError.ExpectedStructDeriveType => "expected struct derive type",
        AstError.ExpectedIdentifierForDeriveType => "expected identifier for derive type",
        AstError.UndefinedStruct => "undefined struct",
        AstError.VariableNameExistsAsStruct => "variable name exists as struct",
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
    };
}
