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
    typeCode: usize,
    numGenerics: usize,
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

const AstDynamicArrayType = struct {
    type: *const AstTypes,
};
const AstStaticArrayType = struct {
    type: *const AstTypes,
    size: *const AstNode,
};

const AstNullableType = struct {
    type: *const AstTypes,
};

const CustomType = struct {
    structCode: usize,
    name: []u8,
    generics: []*const AstTypes,
};

const Types = enum {
    String,
    Bool,
    Char,
    Number,
    DynamicArray,
    StaticArray,
    Nullable,
    Custom,
};
pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Number: AstNumberVariants,
    DynamicArray: AstDynamicArrayType,
    StaticArray: AstStaticArrayType,
    Nullable: AstNullableType,
    Custom: CustomType,
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
    StaticArray: []*const AstNode,
};

const VarDecNode = struct {
    name: []u8,
    isConst: bool,
    setNode: *const AstNode,
    annotation: ?*const AstTypes,
};

const CastNode = struct {
    node: *const AstNode,
    toType: *const AstTypes,
};

const VariableNode = struct {
    name: []u8,
};

const AstNodeVariants = enum {
    Seq,
    VarDec,
    Type,
    Value,
    Cast,
    Variable,
};
pub const AstNode = union(AstNodeVariants) {
    Seq: SeqNode,
    VarDec: VarDecNode,
    Type: AstTypes,
    Value: AstValues,
    Cast: CastNode,
    Variable: VariableNode,
};

const AstError = error{
    UnexpectedToken,
    TokenNotFound,
    InvalidType,
    UnknownType,
    ExpectedGenericArgument,
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

    pub fn getRegisteredStruct(self: Self, structName: []u8) ?RegisteredStruct {
        for (self.registeredStructs) |s| {
            if (std.mem.eql(u8, s.name, structName)) return s;
        }

        return null;
    }

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
            const numType = if (findChar(token.string.?, 0, '.') != null) AstNumberVariants.F32 else AstNumberVariants.U32;
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
            var nodeItems = ArrayList(*const AstNode).init(allocator);
            defer nodeItems.deinit();

            const end = smartDelimiterIndex(tokens, compInfo, start + 1, TokenType.RBracket) catch |e| return astError(e, "]");
            var prev = start + 1;
            var comma = smartDelimiterIndex(tokens, compInfo, start + 1, TokenType.Comma) catch end;

            while (prev < end) {
                const tempTokens = tokens[prev..comma];
                const node = try createAstNode(allocator, compInfo, tempTokens, 0);
                try nodeItems.append(node.node);

                prev = comma + 1;
                comma = smartDelimiterIndex(tokens, compInfo, comma + 1, TokenType.Comma) catch end;
            }

            const itemsSlice = try allocator.dupe(*const AstNode, nodeItems.items);

            return .{
                .node = try create(AstNode, allocator, .{
                    .Value = .{
                        .StaticArray = itemsSlice,
                    },
                }),
                .offset = end - start + 1,
            };
        },
        .Struct => {
            // TODO: litterally the rest of this
            const end = try nextTokenOccurrence(tokens, start + 3, TokenType.RBrace);
            return .{
                .offset = end - start + 1,
                .node = try create(AstNode, allocator, .{
                    .Value = .{ .Char = 'i' },
                }),
            };
        },
        .Identifier => {
            if (compInfo.hasRegisteredStruct(token.string.?)) {
                const lParenIndex = try smartDelimiterIndex(tokens, compInfo, start + 1, TokenType.LParen);
                const typeTokens = tokens[start..lParenIndex];

                const typeNode = try createTypeNode(allocator, compInfo, typeTokens, 0);
                const node = try createAstNode(allocator, compInfo, tokens, lParenIndex + 1);

                const castNode = try create(AstNode, allocator, .{
                    .Cast = .{
                        .node = node.node,
                        .toType = typeNode,
                    },
                });

                return .{
                    .node = castNode,
                    .offset = typeTokens.len + node.offset + 2,
                };
            }

            return .{
                .offset = 1,
                .node = try create(AstNode, allocator, .{
                    .Variable = VariableNode{
                        .name = token.string.?,
                    },
                }),
            };
        },
        else => {
            std.debug.print("\n## {any} {s}\n", .{ token.type, if (token.string != null) token.string.? else "" });
            return AstError.UnexpectedToken;
        },
    }
}

fn delimiterIndex(tokens: []Token, start: usize, delimiter: TokenType) !usize {
    var parens: u32 = 0;

    var i = start;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type == delimiter and parens == 0) return i;

        if (isOpenToken(tokens[i].type, true)) {
            parens += 1;
        } else if (isCloseToken(tokens[i].type, true)) {
            parens -= 1;
        }
    }

    return AstError.TokenNotFound;
}

fn smartDelimiterIndex(tokens: []Token, compInfo: CompInfo, start: usize, delimiter: TokenType) !usize {
    var current = start;
    var parens: u32 = 0;
    var inGeneric = false;
    var genericStart: u32 = 0;

    while (current < tokens.len) : (current += 1) {
        if (parens == 0 and tokens[current].type == delimiter) return current;

        if (tokens[current].type == TokenType.RAngle and inGeneric) {
            parens -= 1;
            if (parens == genericStart) {
                genericStart = 0;
                inGeneric = false;
            }
            continue;
        }

        if (tokens[current].type == TokenType.LAngle and current > 0) {
            const prevToken = tokens[current - 1];
            if (prevToken.type == TokenType.Identifier and compInfo.hasRegisteredStruct(prevToken.string.?)) {
                if (!inGeneric) {
                    genericStart = parens;
                    inGeneric = true;
                }

                parens += 1;
                continue;
            }
        }

        if (isOpenToken(tokens[current].type, false)) {
            parens += 1;
        } else if (isCloseToken(tokens[current].type, false)) {
            if (parens == 0) return AstError.TokenNotFound;
            parens -= 1;
        }
    }

    return AstError.TokenNotFound;
}

fn isOpenToken(tokenType: TokenType, includeAngle: bool) bool {
    const temp = switch (tokenType) {
        .LParen, .LBrace, .LBracket => true,
        else => false,
    };

    return if (includeAngle) temp and tokenType == TokenType.LAngle else temp;
}

fn isCloseToken(tokenType: TokenType, includeAngle: bool) bool {
    const temp = switch (tokenType) {
        .RParen, .RBrace, .RBracket => true,
        else => false,
    };

    return if (includeAngle) temp and tokenType == TokenType.RAngle else temp;
}

fn nextTokenOccurrence(tokens: []Token, start: usize, tokenType: TokenType) !usize {
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
        const index = nextTokenOccurrence(tokens, searchStart, TokenType.EqSet) catch |e| return astError(e, "=");

        const typeTokens = tokens[searchStart..index];
        annotation = try createTypeNode(allocator, compInfo, typeTokens, 0);

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

fn createTypeNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) (AstError || Allocator.Error)!*const AstTypes {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == TokenType.QuestionMark;
    var current = start + @as(u32, if (nullable) 1 else 0);

    var res: *const AstTypes = undefined;

    if (tokens[current].type == TokenType.Identifier) {
        const registeredStruct = compInfo.getRegisteredStruct(tokens[current].string.?);
        if (registeredStruct == null) return AstError.UnknownType;

        var typeGenericsArr = ArrayList(*const AstTypes).init(allocator);
        defer typeGenericsArr.deinit();

        if (registeredStruct.?.numGenerics > 0) {
            // min tokens: some_generic_type, <, some_type, >
            const minTokensForGeneric = 4;
            if (current + minTokensForGeneric < tokens.len and tokens[current + 1].type == TokenType.LAngle) {
                const tokensToRAngle = smartDelimiterIndex(tokens, compInfo, current + 2, TokenType.RAngle) catch |e| return astError(e, ">");
                var typeEnd = smartDelimiterIndex(tokens, compInfo, current + 2, TokenType.Comma) catch tokensToRAngle;
                var prev = current + 2;

                std.debug.print("{d} < {d}", .{ prev, tokensToRAngle });

                while (prev < tokensToRAngle) {
                    const typeTokens = tokens[prev..typeEnd];
                    const typeNode = try createTypeNode(allocator, compInfo, typeTokens, 0);
                    try typeGenericsArr.append(typeNode);

                    prev = typeEnd + 1;
                    typeEnd = smartDelimiterIndex(tokens, compInfo, typeEnd + 1, TokenType.Comma) catch tokensToRAngle;
                }
            } else {
                return AstError.ExpectedGenericArgument;
            }
        }

        const typeGenerics = try allocator.dupe(*const AstTypes, typeGenericsArr.items);

        res = try create(AstTypes, allocator, AstTypes{
            .Custom = CustomType{
                .generics = typeGenerics,
                .structCode = registeredStruct.?.typeCode,
                .name = registeredStruct.?.name,
            },
        });
    } else {
        res = try createAstType(allocator, tokens[current]);
    }

    while (current + 1 < tokens.len and tokens[current + 1].type == TokenType.LBracket) : (current += 1) {
        if (tokens[current + 2].type != TokenType.RBracket) {
            const sizeNode = try createAstNode(allocator, compInfo, tokens, current + 2);
            res = try create(AstTypes, allocator, .{
                .StaticArray = .{
                    .type = res,
                    .size = sizeNode.node,
                },
            });
        } else {
            res = try create(AstTypes, allocator, .{
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

pub fn registerStructs(allocator: Allocator, tokens: []Token) ![]RegisteredStruct {
    var arr = ArrayList(RegisteredStruct).init(allocator);
    defer arr.deinit();

    var typeCode: u32 = 0;
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type != TokenType.Struct) continue;

        const isGeneric = tokens[i + 1].type == TokenType.LBracket;
        var numGenerics: u32 = 0;

        if (isGeneric) {
            const pos = nextTokenOccurrence(tokens, i, TokenType.RBracket) catch |e| return astError(e, "]");

            var currentIndex = i + 2;
            while (currentIndex < pos) {
                currentIndex = delimiterIndex(tokens, currentIndex, TokenType.Comma) catch break;

                currentIndex += 1;
                numGenerics += 1;
            }

            i = pos + 1;
        } else {
            i += 1;
        }

        if (tokens[i].type != TokenType.Identifier) return AstError.UnexpectedToken;
        const name = tokens[i].string.?;

        const registeredStruct = RegisteredStruct{
            .numGenerics = numGenerics,
            .name = name,
            .typeCode = typeCode,
        };

        typeCode += 1;

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
        AstError.ExpectedGenericArgument => "expected generic argument",
    };
}

fn astError(errorType: AstError, str: []const u8) AstError {
    const stdout = std.io.getStdOut().writer();
    stdout.print("{s} {s}\n", .{
        astErrorToString(errorType),
        str,
    }) catch {};

    return errorType;
}

// |-----------------|
// | free structures |
// |-----------------|

pub fn freeAst(allocator: Allocator, ast: Ast) void {
    freeNodes(allocator, ast.root.nodes);
    allocator.free(ast.root.nodes);
}

fn freeNodes(allocator: Allocator, nodes: []*const AstNode) void {
    for (nodes) |node| {
        freeNode(allocator, node);
    }
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
        .Custom => |*custom| {
            for (custom.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(custom.generics);
        },
        else => {},
    }

    allocator.destroy(node);
}

fn freeValueNode(allocator: Allocator, node: *const AstValues) void {
    switch (node.*) {
        .StaticArray => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        else => {},
    }
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
        .Value => |*val| {
            freeValueNode(allocator, val);
        },
        .Cast => |*cast| {
            freeNode(allocator, cast.node);
            freeType(allocator, cast.toType);
        },
        .Variable => {},
    }

    allocator.destroy(node);
}

pub fn freeRegisteredStructs(allocator: Allocator, structs: []RegisteredStruct) void {
    allocator.free(structs);
}
