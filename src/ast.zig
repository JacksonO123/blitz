const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const utils = @import("utils.zig");
const findChar = utils.findChar;
const create = utils.create;
const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const ArrayList = std.ArrayList;
const TokenizeError = tokenizer.TokenizeError;

pub const RegisteredStruct = struct {
    name: []u8,
    isGeneric: bool,
};

const SeqNode = struct {
    nodes: []*const AstNode,
};

pub const AstNumberVariants = enum {
    U16,
    U32,
    U64,
    U128,
    I16,
    I32,
    I64,
    I128,
    F16,
    F32,
    F64,
    F128,
};
const AstNumber = struct {
    type: AstNumberVariants,
    value: []u8,
};

const AstDynamicArrayType = struct { type: *const AstTypes };
const AstStaticArrayType = struct {
    type: *const AstTypes,
    size: *const AstNode,
};

const AstNullableType = struct { type: *const AstTypes };

const Types = enum {
    String,
    Bool,
    Char,
    Number,
    DynamicArray,
    StaticArray,
    Nullable,
};
pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Number: AstNumberVariants,
    DynamicArray: AstDynamicArrayType,
    StaticArray: AstStaticArrayType,
    Nullable: AstNullableType,
};
const StaticTypes = enum {
    String,
    Bool,
    Char,
    Number,
    StaticArray,
};
pub const AstValues = union(StaticTypes) {
    String: []u8,
    Bool: bool,
    Char: u8,
    Number: AstNumber,
    StaticArray: []AstValues,
};

const VarDecNode = struct {
    name: []u8,
    isConst: bool,
    setNode: *const AstNode,
    annotation: ?*const AstTypes,
};

const AstNodeVariants = enum {
    Seq,
    VarDec,
    Type,
    Value,
};
pub const AstNode = union(AstNodeVariants) {
    Seq: SeqNode,
    VarDec: VarDecNode,
    Type: AstTypes,
    Value: AstValues,
};

const AstError = error{
    UnexpectedToken,
    TokenNotFound,
    InvalidType,
    UnknownType,
};

const AstNodeOffsetData = struct {
    node: *const AstNode,
    offset: usize,
};

const AstTypeOffsetData = struct {
    type: *const AstTypes,
    offset: usize,
};

pub const Ast = struct {
    root: SeqNode,
};

pub const CompInfo = struct {
    const Self = @This();

    registeredStructs: []RegisteredStruct,

    pub fn hasRegisteredStruct(self: Self, structName: []u8) bool {
        for (self.registeredStructs) |s| {
            if (std.mem.eql(u8, s.name, structName)) return true;
        }

        return false;
    }
};

pub fn createAst(allocator: Allocator, compInfo: CompInfo, tokens: []Token) !Ast {
    var currentToken: usize = 0;
    var seq = ArrayList(*const AstNode).init(allocator);
    defer seq.deinit();

    while (currentToken < tokens.len) {
        if (tokens[currentToken].type == TokenType.Semicolon) {
            currentToken += 1;
            continue;
        }

        const node = try createAstNode(allocator, compInfo, tokens, currentToken);
        currentToken += node.offset;
        try seq.append(node.node);
    }

    const astNodes = try allocator.dupe(*const AstNode, seq.items);

    return Ast{
        .root = SeqNode{ .nodes = astNodes },
    };
}

pub fn createAstNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) !AstNodeOffsetData {
    const token = tokens[start];

    switch (token.type) {
        .Const => {
            return try createVarDecNode(allocator, compInfo, tokens, start, true);
        },
        .Var => {
            return try createVarDecNode(allocator, compInfo, tokens, start, false);
        },
        .Number => {
            const numType = if (findChar(token.string.?, 0, '.') != null) AstNumberVariants.U32 else AstNumberVariants.F32;
            const node = try create(AstNode, allocator, .{
                .Value = .{
                    .Number = .{
                        .type = numType,
                        .value = token.string.?,
                    },
                },
            });

            return .{
                .node = node,
                .offset = 1,
            };
        },
        .LBracket => {
            const end = tokenDelimiterIndex(tokens, compInfo, start + 1, TokenType.RBracket) catch |e| return astError(e, "]");

            std.debug.print("end: {d}", .{end});

            return AstError.UnexpectedToken;
        },
        .Struct => {
            // TODO: litterally the rest of this
            const end = try findToken(tokens, start + 3, TokenType.RBrace);
            return AstNodeOffsetData{
                .offset = end - start + 1,
                .node = try create(AstNode, allocator, .{
                    .Value = AstValues{ .Char = 'i' },
                }),
            };
        },
        else => {
            std.debug.print("\n## {any}\n", .{token.type});
            return AstError.UnexpectedToken;
        },
    }
}

fn tokenDelimiterIndex(tokens: []Token, compInfo: CompInfo, start: usize, delimiter: TokenType) !usize {
    var current: usize = start;
    var parens: u32 = 0;
    var inGeneric = false;

    while (current < tokens.len) : (current += 1) {
        if (parens == 0 and tokens[current].type == delimiter) return current;

        if (tokens[current].type == TokenType.RAngle and inGeneric) {
            parens -= 1;
            inGeneric = false;
            continue;
        }

        if (tokens[current].type == TokenType.LAngle and current > 0) {
            const prevToken = tokens[current - 1];
            if (prevToken.type == TokenType.Identifier and compInfo.hasRegisteredStruct(prevToken.string.?)) {
                parens += 1;
                inGeneric = true;
                continue;
            }
        }

        switch (tokens[current].type) {
            .LParen, .LBrace, .LBracket => {
                parens += 1;
            },
            .RParen, .RBrace, .RBracket => {
                parens -= 1;
            },
            else => {},
        }
    }

    return AstError.TokenNotFound;
}

fn findToken(tokens: []Token, start: usize, tokenType: TokenType) !usize {
    var i = start;

    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type == tokenType) return i;
    }

    return AstError.TokenNotFound;
}

fn createVarDecNode(
    allocator: Allocator,
    compInfo: CompInfo,
    tokens: []Token,
    start: usize,
    isConst: bool,
) (AstError || Allocator.Error)!AstNodeOffsetData {
    const name = tokens[start + 1];
    const hasType = switch (tokens[start + 2].type) {
        .EqSet => false,
        .Colon => true,
        else => return AstError.UnexpectedToken,
    };
    var annotation: ?*const AstTypes = null;
    var offset: usize = 3;

    if (hasType) {
        const searchStart = start + offset;
        const index = findToken(tokens, searchStart, TokenType.EqSet) catch |e| return astError(e, "=");

        annotation = try createTypeNode(allocator, compInfo, tokens, searchStart, index);

        offset += index - searchStart + 1;
    }

    const setNode = try createAstNode(allocator, compInfo, tokens, start + offset);
    const tokenOffset = offset + setNode.offset;

    if (name.type != TokenType.Identifier) return AstError.UnexpectedToken;
    const node = try create(AstNode, allocator, .{
        .VarDec = VarDecNode{
            .name = name.string.?,
            .isConst = isConst,
            .setNode = setNode.node,
            .annotation = annotation,
        },
    });

    return .{ .node = node, .offset = tokenOffset };
}

fn createTypeNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize, end: usize) (AstError || Allocator.Error)!*const AstTypes {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == TokenType.QuestionMark;
    var current = start + if (nullable) @as(u32, 1) else @as(u32, 0);

    // TODO: extends for generics
    var res: *const AstTypes = try createAstType(allocator, tokens[current]);

    while (tokens[current + 1].type == TokenType.LBracket and current < end) : (current += 1) {
        if (tokens[current + 2].type != TokenType.RBracket) {
            const sizeNode = try createAstNode(allocator, compInfo, tokens, current + 2);
            res = try create(AstTypes, allocator, .{
                .StaticArray = .{
                    .type = res,
                    .size = sizeNode.node,
                },
            });
        } else {
            res = try create(AstTypes, allocator, AstTypes{
                .DynamicArray = .{
                    .type = res,
                },
            });
        }
    }

    return res;
}

fn createAstType(allocator: Allocator, token: Token) !*const AstTypes {
    const val = switch (token.type) {
        .Bool => AstTypes.Bool,
        .String => AstTypes.String,
        .U16 => AstTypes{ .Number = AstNumberVariants.U16 },
        .U32 => AstTypes{ .Number = AstNumberVariants.U32 },
        .U64 => AstTypes{ .Number = AstNumberVariants.U64 },
        .U128 => AstTypes{ .Number = AstNumberVariants.U128 },
        .I16 => AstTypes{ .Number = AstNumberVariants.I16 },
        .I32 => AstTypes{ .Number = AstNumberVariants.I32 },
        .I64 => AstTypes{ .Number = AstNumberVariants.I64 },
        .I128 => AstTypes{ .Number = AstNumberVariants.I128 },
        .F16 => AstTypes{ .Number = AstNumberVariants.F16 },
        .F32 => AstTypes{ .Number = AstNumberVariants.F32 },
        .F64 => AstTypes{ .Number = AstNumberVariants.F64 },
        .F128 => AstTypes{ .Number = AstNumberVariants.F128 },
        .Char => AstTypes.Char,
        else => return AstError.UnknownType,
    };

    return create(AstTypes, allocator, val);
}

pub fn registerStructNames(allocator: Allocator, tokens: []Token) ![]RegisteredStruct {
    var arr = ArrayList(RegisteredStruct).init(allocator);
    defer arr.deinit();

    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type != TokenType.Struct) continue;

        const isGeneric = tokens[i + 1].type == TokenType.LBracket;

        if (isGeneric) {
            const pos = findToken(tokens, i, TokenType.RBracket) catch |e| return astError(e, "]");

            i = pos + 1;
        } else {
            i += 1;
        }

        if (tokens[i].type != TokenType.Identifier) return AstError.UnexpectedToken;
        const name = tokens[i].string.?;

        const registeredStruct = RegisteredStruct{
            .isGeneric = isGeneric,
            .name = name,
        };

        try arr.append(registeredStruct);
    }

    const slice = allocator.dupe(RegisteredStruct, arr.items);
    return slice;
}

fn astErrorToString(errorType: AstError) []const u8 {
    return switch (errorType) {
        AstError.UnexpectedToken => "unexpected token",
        AstError.TokenNotFound => "token not found",
        AstError.InvalidType => "invalid type",
        AstError.UnknownType => "unknown type",
    };
}

fn astError(errorType: AstError, str: []const u8) AstError {
    const stdout = std.io.getStdOut().writer();
    stdout.print("{s} {s}", .{
        astErrorToString(errorType),
        str,
    }) catch {};

    return errorType;
}

// |-----------------|
// | free structures |
// |-----------------|

pub fn freeAst(allocator: Allocator, ast: Ast) void {
    for (ast.root.nodes) |node| {
        freeNode(allocator, node);
    }

    allocator.free(ast.root.nodes);
}

fn freeType(allocator: Allocator, node: *const AstTypes) void {
    switch (node.*) {
        .DynamicArray => |*arr| {
            freeType(allocator, arr.type);
        },
        .StaticArray => |*arr| {
            freeType(allocator, arr.type);
            freeNode(allocator, arr.size);
        },
        .Nullable => |*nullable| {
            freeType(allocator, nullable.type);
        },
        else => {},
    }

    allocator.destroy(node);
}

pub fn freeNode(allocator: Allocator, node: *const AstNode) void {
    switch (node.*) {
        .VarDec => |*dec| {
            freeNode(allocator, dec.setNode);
            if (dec.annotation != null) {
                freeType(allocator, dec.annotation.?);
            }
        },
        .Seq => |*seq| {
            for (seq.nodes) |seqNode| {
                freeNode(allocator, seqNode);
            }
        },
        .Type => |*t| {
            freeType(allocator, t);
        },
        .Value => {},
    }

    allocator.destroy(node);
}

pub fn freeRegisteredStructs(allocator: Allocator, structs: []RegisteredStruct) void {
    allocator.free(structs);
}
