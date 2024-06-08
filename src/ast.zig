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
const cloneString = utils.cloneString;

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
    Union,
};
pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Number: AstNumberVariants,
    DynamicArray: AstDynamicArrayType,
    StaticArray: AstStaticArrayType,
    Nullable: *const AstTypes,
    Custom: CustomType,
    Union: []*const AstTypes,
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

const StructDecNode = struct {
    typeCode: usize,
    name: []u8,
    generics: []StructGeneric,
};

const StructGeneric = struct {
    name: []u8,
    restriction: ?*const AstTypes,
};

const IfStatementNode = struct {
    condition: *const AstNode,
    body: *const AstNode,
};

const AstNodeVariants = enum {
    Seq,
    VarDec,
    Type,
    Value,
    Cast,
    Variable,
    StructDec,
    IfStatement,
    NoOp,
};
pub const AstNode = union(AstNodeVariants) {
    Seq: SeqNode,
    VarDec: VarDecNode,
    Type: AstTypes,
    Value: AstValues,
    Cast: CastNode,
    Variable: VariableNode,
    StructDec: StructDecNode,
    IfStatement: IfStatementNode,
    NoOp,
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
    const seq = try createSeqNode(allocator, compInfo, tokens);
    return Ast{ .root = seq };
}

fn createSeqAstNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token) !*const AstNode {
    const seq = try createSeqNode(allocator, compInfo, tokens);
    const node = try create(AstNode, allocator, .{ .Seq = seq });
    return node;
}

fn createSeqNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token) !SeqNode {
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
    return SeqNode{ .nodes = astNodes };
}

fn createAstNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) (AstError || Allocator.Error)!AstNodeOffsetData {
    if (tokens.len == 0) {
        return .{
            .offset = 0,
            .node = try create(AstNode, allocator, @as(AstNode, AstNode.NoOp)),
        };
    }

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
                        .value = try cloneString(allocator, token.string.?),
                    },
                },
            });

            return .{
                .node = node,
                .offset = 1,
            };
        },
        .True => {
            const node = try createBoolNode(allocator, true);
            return .{
                .node = node,
                .offset = 1,
            };
        },
        .False => {
            const node = try createBoolNode(allocator, false);
            return .{
                .node = node,
                .offset = 1,
            };
        },
        .LBracket => {
            var nodeItems = ArrayList(*const AstNode).init(allocator);
            defer nodeItems.deinit();

            const end = smartDelimiterIndex(tokens, compInfo, start + 1, TokenType.RBracket) catch |e| return astError(e, "]");
            var comma = smartDelimiterIndex(tokens, compInfo, start + 1, TokenType.Comma) catch end;
            var prev = start + 1;

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
            const isGeneric = tokens[start + 1].type == TokenType.LBracket;
            const nameIndex = (if (isGeneric) try delimiterIndex(tokens, start + 2, TokenType.RBracket) else start) + 1;
            const end = try smartDelimiterIndex(tokens, compInfo, nameIndex + 2, TokenType.RBrace);

            if (tokens[nameIndex].type != TokenType.Identifier) {
                return astError(AstError.TokenNotFound, "struct name");
            }

            const name = try cloneString(allocator, tokens[nameIndex].string.?);
            const registeredStruct = compInfo.getRegisteredStruct(name);

            var structDecNode = StructDecNode{
                .typeCode = registeredStruct.?.typeCode,
                .name = name,
                .generics = &[_]StructGeneric{},
            };

            if (isGeneric) {
                const genericsTokens = tokens[start + 2 .. nameIndex - 1];
                const genericTypes = try parseStructGenerics(allocator, compInfo, genericsTokens);
                structDecNode.generics = genericTypes;
            }

            const structNode = try create(AstNode, allocator, .{ .StructDec = structDecNode });

            return .{
                .offset = end - start + 1,
                .node = structNode,
            };
        },
        .Identifier => {
            // type cast to struct
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
                    .Variable = .{
                        .name = try cloneString(allocator, token.string.?),
                    },
                }),
            };
        },
        .If => {
            const closeParen = smartDelimiterIndex(tokens, compInfo, start + 2, TokenType.RParen) catch |e| return astError(e, ")");
            const conditionTokens = tokens[start + 2 .. closeParen];
            const conditionNode = try createAstNode(allocator, compInfo, conditionTokens, 0);

            const endBrace = smartDelimiterIndex(tokens, compInfo, closeParen + 2, TokenType.RBrace) catch |e| return astError(e, "]");
            const bodyTokens = tokens[closeParen + 2 .. endBrace];
            const bodyNode = try createSeqAstNode(allocator, compInfo, bodyTokens);

            const ifStatement = try create(AstNode, allocator, .{
                .IfStatement = .{
                    .condition = conditionNode.node,
                    .body = bodyNode,
                },
            });

            return .{
                .offset = endBrace - start + 1,
                .node = ifStatement,
            };
        },
        else => {
            return AstError.UnexpectedToken;
        },
    }
}

fn createBoolNode(allocator: Allocator, value: bool) !*const AstNode {
    const node = try create(AstNode, allocator, .{
        .Value = .{ .Bool = value },
    });
    return node;
}

fn parseStructGenerics(allocator: Allocator, compInfo: CompInfo, tokens: []Token) ![]StructGeneric {
    var genericList = ArrayList(StructGeneric).init(allocator);
    defer genericList.deinit();

    var to = delimiterIndex(tokens, 0, TokenType.Comma) catch tokens.len;
    var prev: usize = 0;

    while (prev < tokens.len) {
        const typeTokens = tokens[prev..to];
        var hasRestriction = true;
        const colonIndex: usize = delimiterIndex(typeTokens, 1, TokenType.Colon) catch blk: {
            hasRestriction = false;
            break :blk to;
        };

        var structGeneric = StructGeneric{
            .name = &[_]u8{},
            .restriction = null,
        };

        const tokenDiff = colonIndex - prev;
        if (tokenDiff == 0) {
            return astError(AstError.TokenNotFound, "name for generic argument in struct");
        } else if (tokenDiff > 1) {
            return astError(AstError.UnexpectedToken, typeTokens[1].type.toString());
        } else if (typeTokens[0].type != TokenType.Identifier) {
            return astError(AstError.UnexpectedToken, typeTokens[0].type.toString());
        } else {
            structGeneric.name = try cloneString(allocator, typeTokens[0].string.?);
        }

        if (hasRestriction) {
            const restrictionTokens = tokens[colonIndex + 1 .. to];
            const typeNode = try createTypeNode(allocator, compInfo, restrictionTokens, 0);
            structGeneric.restriction = typeNode;
        }

        try genericList.append(structGeneric);

        prev = to + 1;
        to = delimiterIndex(tokens, to + 1, TokenType.Comma) catch tokens.len;
    }

    const slice = allocator.dupe(StructGeneric, genericList.items);
    return slice;
}

fn delimiterIndex(tokens: []Token, start: usize, delimiter: TokenType) !usize {
    var parens: u32 = 0;

    var i = start;
    while (i < tokens.len) : (i += 1) {
        if (parens == 0 and tokens[i].type == delimiter) return i;

        if (tokens[i].type.isOpenToken(true)) {
            parens += 1;
        } else if (tokens[i].type.isCloseToken(true)) {
            if (parens > 0) {
                parens -= 1;
            } else if (tokens[i].type != delimiter) return AstError.TokenNotFound;
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

        if (TokenType.isOpenToken(tokens[current].type, false)) {
            parens += 1;
        } else if (TokenType.isCloseToken(tokens[current].type, false)) {
            if (parens == 0) return AstError.TokenNotFound;
            parens -= 1;
        }
    }

    return AstError.TokenNotFound;
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
            .name = try cloneString(allocator, name.string.?),
            .isConst = isConst,
            .setNode = setNode.node,
            .annotation = annotation,
        },
    });

    return .{ .node = node, .offset = tokenOffset };
}

fn parseGenericArgs(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) ![]*const AstTypes {
    var typeGenericsArr = ArrayList(*const AstTypes).init(allocator);
    defer typeGenericsArr.deinit();

    if (start + 2 < tokens.len) {
        const tokensToRAngle = smartDelimiterIndex(tokens, compInfo, start + 2, TokenType.RAngle) catch |e| return astError(e, ">");
        var typeEnd = smartDelimiterIndex(tokens, compInfo, start + 2, TokenType.Comma) catch tokensToRAngle;
        var prev = start + 2;

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

    return try allocator.dupe(*const AstTypes, typeGenericsArr.items);
}

fn createTypeNodeItem(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) (AstError || Allocator.Error)!AstTypeOffsetData {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == TokenType.QuestionMark;
    var current = start + @as(u32, if (nullable) 1 else 0);

    var res: *const AstTypes = undefined;

    if (tokens[current].type == TokenType.Identifier) {
        const registeredStruct = compInfo.getRegisteredStruct(tokens[current].string.?);
        if (registeredStruct == null) return astError(AstError.UnknownType, tokens[current].string.?);

        var customType = CustomType{
            .generics = &[_]*const AstTypes{},
            .structCode = registeredStruct.?.typeCode,
            .name = try cloneString(allocator, registeredStruct.?.name),
        };

        if (registeredStruct.?.numGenerics > 0) {
            customType.generics = try parseGenericArgs(allocator, compInfo, tokens, current);
        }

        res = try create(AstTypes, allocator, .{ .Custom = customType });
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

    if (nullable) {
        res = try create(AstTypes, allocator, .{ .Nullable = res });
    }

    return .{
        .type = res,
        .offset = current - start,
    };
}

fn createTypeNode(allocator: Allocator, compInfo: CompInfo, tokens: []Token, start: usize) (AstError || Allocator.Error)!*const AstTypes {
    var unionTypes = ArrayList(*const AstTypes).init(allocator);
    defer unionTypes.deinit();

    var isUnion = false;
    const data = try createTypeNodeItem(allocator, compInfo, tokens, start);
    var offset = data.offset;
    var node = data.type;

    while (start + offset + 1 < tokens.len and tokens[start + offset + 1].type == TokenType.Union) {
        if (!isUnion) {
            try unionTypes.append(node);
            isUnion = true;
        }

        const newNode = try createTypeNodeItem(allocator, compInfo, tokens, start + offset + 2);
        try unionTypes.append(newNode.type);

        offset += newNode.offset + 1;
    }

    if (isUnion) {
        const copy = try allocator.dupe(*const AstTypes, unionTypes.items);
        node = try create(AstTypes, allocator, .{
            .Union = copy,
        });
    }

    return node;
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
            const pos = delimiterIndex(tokens, i + 2, TokenType.RBracket) catch |e| return astError(e, "]");

            var currentIndex = i + 2;
            while (currentIndex < pos) {
                currentIndex = delimiterIndex(tokens, currentIndex, TokenType.Comma) catch break;

                currentIndex += 1;
                numGenerics += 1;
            }

            numGenerics += 1;
            i = pos + 1;
        } else {
            i += 1;
        }

        if (tokens[i].type != TokenType.Identifier) return AstError.UnexpectedToken;

        const registeredStruct = RegisteredStruct{
            .numGenerics = numGenerics,
            .name = try cloneString(allocator, tokens[i].string.?),
            .typeCode = typeCode,
        };

        typeCode += 1;

        try arr.append(registeredStruct);
    }

    const slice = try allocator.dupe(RegisteredStruct, arr.items);
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
        .Nullable => |nullable| {
            freeType(allocator, nullable);
        },
        .Custom => |*custom| {
            for (custom.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(custom.generics);
            allocator.free(custom.name);
        },
        .Union => |typeUnion| {
            for (typeUnion) |t| {
                freeType(allocator, t);
            }

            allocator.free(typeUnion);
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
        .Number => |*num| {
            allocator.free(num.value);
        },
        .String => |string| {
            allocator.free(string);
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

            allocator.free(dec.name);
        },
        .Seq => |*seq| {
            for (seq.nodes) |seqNode| {
                freeNode(allocator, seqNode);
            }
            allocator.free(seq.nodes);
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
        .Variable => |*variable| {
            allocator.free(variable.name);
        },
        .StructDec => |*dec| {
            allocator.free(dec.name);

            for (dec.generics) |generic| {
                if (generic.restriction != null) {
                    freeType(allocator, generic.restriction.?);
                }
                allocator.free(generic.name);
            }

            allocator.free(dec.generics);
        },
        .IfStatement => |*statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
        },
        .NoOp => {},
    }

    allocator.destroy(node);
}

pub fn freeRegisteredStructs(allocator: Allocator, structs: []RegisteredStruct) void {
    for (structs) |s| {
        allocator.free(s.name);
    }

    allocator.free(structs);
}
