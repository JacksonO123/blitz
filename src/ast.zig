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

// debug
const debug = @import("./debug.zig");
const printTokens = debug.printTokens;

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
    Void,
    Number,
    DynamicArray,
    StaticArray,
    Nullable,
    Custom,
    Generic,
};
pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Void,
    Number: AstNumberVariants,
    DynamicArray: AstDynamicArrayType,
    StaticArray: AstStaticArrayType,
    Nullable: *const AstTypes,
    Custom: CustomType,
    Generic: []u8,
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

const MemberVisibility = enum {
    const Self = @This();

    Private,
    Public,
    Protected,

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .Private => "private",
            .Public => "public",
            .Protected => "protected",
        };
    }
};

const StructMember = struct {
    type: *const AstTypes,
    visibility: MemberVisibility,
    name: []u8,
};

const StructFunction = struct {
    visibility: MemberVisibility,
    func: FuncDecNode,
    name: []u8,
};

const StructAttributeVariants = enum {
    Member,
    Function,
};

const StructAttributeUnion = union(StructAttributeVariants) {
    Member: StructMember,
    Function: StructFunction,
};

pub const StructAttribute = struct {
    attr: StructAttributeUnion,
    static: bool,
};

const StructAttributesOffsetData = struct {
    attrs: []StructAttribute,
    offset: usize,
};

const StructAttributeUnionOffsetData = struct {
    attr: StructAttributeUnion,
    offset: usize,
};

const StructAttributeOffsetData = struct {
    attr: StructAttribute,
    offset: usize,
};

const StructDecNode = struct {
    typeCode: usize,
    name: []u8,
    generics: []GenericType,
    attributes: []StructAttribute,
};

const AttributeDefinition = struct {
    name: []u8,
    value: *const AstNode,
};

const StructInitNode = struct {
    name: []u8,
    attributes: []AttributeDefinition,
    generics: []*const AstTypes,
};

pub const GenericType = struct {
    name: []u8,
    restriction: ?*const AstTypes,
};

const IfStatementNode = struct {
    condition: *const AstNode,
    body: *const AstNode,
};

pub const Parameter = struct {
    name: []u8,
    type: *const AstTypes,
};

pub const FuncDecNode = struct {
    name: []u8,
    generics: ?[]GenericType,
    params: []Parameter,
    body: *const AstNode,
    returnType: *const AstTypes,
};

const AstNodeVariants = enum {
    NoOp,
    Seq,
    VarDec,
    Type,
    Value,
    Cast,
    Variable,
    StructDec,
    IfStatement,
    FuncDec,
    ReturnNode,
    StructInit,
    Bang,
};
pub const AstNode = union(AstNodeVariants) {
    NoOp,
    Seq: SeqNode,
    VarDec: VarDecNode,
    Type: AstTypes,
    Value: AstValues,
    Cast: CastNode,
    Variable: VariableNode,
    StructDec: StructDecNode,
    IfStatement: IfStatementNode,
    FuncDec: FuncDecNode,
    ReturnNode: *const AstNode,
    StructInit: StructInitNode,
    Bang: *const AstNode,
};

const AstError = error{ UnexpectedToken, TokenNotFound, InvalidType, UnknownType, ExpectedGenericArgument, InvalidStructKey };

const AstNodeOffsetData = struct {
    node: *const AstNode,
    offset: usize,
};

const AstTypeOffsetData = struct {
    type: *const AstTypes,
    offset: usize,
};

const FuncOffsetData = struct {
    func: FuncDecNode,
    offset: usize,
};

pub const Ast = struct {
    root: SeqNode,
};

pub const CompInfo = struct {
    const Self = @This();

    registeredStructs: []RegisteredStruct,
    generics: *ArrayList([]u8),

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

    pub fn addGeneric(self: *Self, name: []u8) !void {
        try self.generics.append(name);
    }

    pub fn removeGeneric(self: *Self, name: []u8) void {
        for (self.generics.items, 0..) |item, index| {
            if (std.mem.eql(u8, item, name)) {
                _ = self.generics.swapRemove(index);
            }
        }
    }

    pub fn hasGeneric(self: Self, name: []u8) bool {
        for (self.generics.items) |item| {
            if (std.mem.eql(u8, item, name)) {
                return true;
            }
        }

        return false;
    }
};

pub fn createAst(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) !Ast {
    const seq = try createSeqNode(allocator, compInfo, tokens);
    return Ast{ .root = seq };
}

fn createSeqAstNode(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) !*const AstNode {
    const seq = try createSeqNode(allocator, compInfo, tokens);
    const node = try create(AstNode, allocator, .{ .Seq = seq });
    return node;
}

fn createSeqNode(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) !SeqNode {
    var currentToken: usize = 0;
    var seq = ArrayList(*const AstNode).init(allocator);
    defer seq.deinit();

    while (currentToken < tokens.len) {
        if (tokens[currentToken].type == TokenType.Semicolon) {
            currentToken += 1;
            continue;
        }

        const seqTokens = tokens[currentToken..];
        const node = try createAstNode(allocator, compInfo, seqTokens);
        currentToken += node.offset;
        try seq.append(node.node);
    }

    const astNodes = try allocator.dupe(*const AstNode, seq.items);
    return SeqNode{ .nodes = astNodes };
}

fn createAstNode(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) (AstError || Allocator.Error)!AstNodeOffsetData {
    if (tokens.len == 0) {
        return .{
            .offset = 0,
            .node = try create(AstNode, allocator, @as(AstNode, AstNode.NoOp)),
        };
    }

    const token = tokens[0];

    switch (token.type) {
        .Const => {
            return try createVarDecNode(allocator, compInfo, tokens, 0, true);
        },
        .Var => {
            return try createVarDecNode(allocator, compInfo, tokens, 0, false);
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

            const end = smartDelimiterIndex(tokens, compInfo, 1, TokenType.RBracket) catch |e| return astError(e, "]");
            var comma = smartDelimiterIndex(tokens, compInfo, 1, TokenType.Comma) catch end;
            var prev: usize = 1;

            while (prev < end) {
                const tempTokens = tokens[prev..comma];
                const node = try createAstNode(allocator, compInfo, tempTokens);
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
                .offset = end + 1,
            };
        },
        .Struct => {
            const isGeneric = tokens[1].type == TokenType.LBracket;
            const nameIndex = (if (isGeneric) try delimiterIndex(tokens, 2, TokenType.RBracket) else 0) + 1;
            const end = try smartDelimiterIndex(tokens, compInfo, nameIndex + 2, TokenType.RBrace);
            const defTokens = tokens[nameIndex + 2 .. end];
            var genericTypes: []GenericType = &[_]GenericType{};

            if (isGeneric) {
                const genericsTokens = tokens[2 .. nameIndex - 1];
                genericTypes = try parseGenerics(allocator, compInfo, genericsTokens);
            }

            for (genericTypes) |gType| {
                try compInfo.addGeneric(gType.name);
            }

            const attrData = try createStructAttributes(allocator, compInfo, defTokens);

            if (tokens[nameIndex].type != TokenType.Identifier) {
                return astError(AstError.TokenNotFound, "struct name");
            }

            const name = try cloneString(allocator, tokens[nameIndex].string.?);
            const registeredStruct = compInfo.getRegisteredStruct(name);

            const structDecNode = StructDecNode{
                .typeCode = registeredStruct.?.typeCode,
                .name = name,
                .generics = genericTypes,
                .attributes = attrData.attrs,
            };

            const structNode = try create(AstNode, allocator, .{ .StructDec = structDecNode });

            for (genericTypes) |gType| {
                compInfo.removeGeneric(gType.name);
            }

            return .{
                .offset = end + 1,
                .node = structNode,
            };
        },
        .Identifier => {
            if (compInfo.hasRegisteredStruct(token.string.?)) {
                if (tokens[1].type == TokenType.LBrace or tokens[1].type == TokenType.LAngle) {
                    const openBraceIndex = try smartDelimiterIndex(tokens, compInfo, 1, TokenType.LBrace);
                    const typeTokens = tokens[0..openBraceIndex];
                    const typeNodes = try parseGenericArgs(allocator, compInfo, typeTokens, 0);
                    const initTokens = tokens[openBraceIndex..];
                    const structDef = try createStructDef(allocator, compInfo, token.string.?, typeNodes, initTokens);
                    const initNode = try create(AstNode, allocator, .{ .StructInit = structDef });

                    return .{
                        .node = initNode,
                        .offset = tokens.len,
                    };
                } else {
                    // type cast to struct

                    const lParenIndex = try smartDelimiterIndex(tokens, compInfo, 1, TokenType.LParen);
                    const typeTokens = tokens[0..lParenIndex];

                    const typeNode = try createTypeNode(allocator, compInfo, typeTokens);
                    const castTokens = tokens[lParenIndex + 1 ..];
                    const node = try createAstNode(allocator, compInfo, castTokens);

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
            const closeParen = smartDelimiterIndex(tokens, compInfo, 2, TokenType.RParen) catch |e| return astError(e, ")");
            const conditionTokens = tokens[2..closeParen];
            const conditionNode = try createAstNode(allocator, compInfo, conditionTokens);

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
                .offset = endBrace + 1,
                .node = ifStatement,
            };
        },
        .Fn => {
            const funcTokens = tokens[1..];
            const data = try createFuncDecNode(allocator, compInfo, funcTokens);

            const func = try create(AstNode, allocator, .{
                .FuncDec = data.func,
            });

            return .{
                .node = func,
                .offset = data.offset + 1,
            };
        },
        .Return => {
            const semiIndex = try smartDelimiterIndex(tokens, compInfo, 1, TokenType.Semicolon);
            const node = try createAstNode(allocator, compInfo, tokens[1..semiIndex]);
            const returnNode = try create(AstNode, allocator, .{ .ReturnNode = node.node });

            return .{
                .node = returnNode,
                .offset = node.offset + 1,
            };
        },
        .Bang => {
            const otherTokens = tokens[1..];
            const otherNode = try createAstNode(allocator, compInfo, otherTokens);
            const node = try create(AstNode, allocator, .{
                .Bang = otherNode.node,
            });

            return .{
                .node = node,
                .offset = otherNode.offset + 1,
            };
        },
        else => {
            return astError(AstError.UnexpectedToken, tokens[0].type.toString());
        },
    }
}

fn createStructDef(allocator: Allocator, compInfo: *CompInfo, structName: []u8, generics: []*const AstTypes, tokens: []Token) !StructInitNode {
    var attributes = ArrayList(AttributeDefinition).init(allocator);
    defer attributes.deinit();

    var i: usize = 1;
    while (i < tokens.len) {
        const commaIndex = smartDelimiterIndex(tokens, compInfo, i, TokenType.Comma) catch tokens.len;

        const value: *const AstNode = switch (tokens[i + 1].type) {
            .EqSet => a: {
                const valueTokens = tokens[i + 2 .. commaIndex];
                const valueNode = try createAstNode(allocator, compInfo, valueTokens);
                break :a valueNode.node;
            },
            .Comma => a: {
                const varNode = try create(AstNode, allocator, .{
                    .Variable = .{
                        .name = try cloneString(
                            allocator,
                            tokens[i].string.?,
                        ),
                    },
                });
                break :a varNode;
            },
            .RBrace => a: {
                const varNode = try create(AstNode, allocator, .{
                    .Variable = .{
                        .name = try cloneString(
                            allocator,
                            tokens[i].string.?,
                        ),
                    },
                });
                break :a varNode;
            },
            else => return astError(AstError.UnexpectedToken, if (tokens[i + 2].string != null) tokens[i + 2].string.? else ""),
        };

        const attr = AttributeDefinition{
            .name = try cloneString(allocator, tokens[i].string.?),
            .value = value,
        };

        try attributes.append(attr);

        i = commaIndex + 1;
    }

    const attributesSlices = try allocator.dupe(AttributeDefinition, attributes.items);

    return StructInitNode{
        .name = try cloneString(allocator, structName),
        .attributes = attributesSlices,
        .generics = generics,
    };
}

/// not including keyword token `fn`
fn createFuncDecNode(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) !FuncOffsetData {
    var offset: usize = 0;
    var generics: ?[]GenericType = null;

    if (tokens[0].type == TokenType.LBracket) {
        const rBracketIndex = delimiterIndex(tokens, 2, TokenType.RBracket) catch |e| return astError(e, "]");
        const genericTokens = tokens[2..rBracketIndex];
        offset += genericTokens.len + 2;
        generics = try parseGenerics(allocator, compInfo, genericTokens);
    }

    const name = tokens[offset].string.?;

    const rParenIndex = delimiterIndex(tokens, offset + 2, TokenType.RParen) catch |e| return astError(e, ")");
    const parameterTokens = tokens[offset + 2 .. rParenIndex];
    const parameters = try parseParameters(allocator, compInfo, parameterTokens);

    const lBraceIndex = smartDelimiterIndex(tokens, compInfo, rParenIndex + 1, TokenType.LBrace) catch |e| return astError(e, "{");
    const rBraceIndex = smartDelimiterIndex(tokens, compInfo, lBraceIndex + 1, TokenType.RBrace) catch |e| return astError(e, "}");
    const bodyTokens = tokens[lBraceIndex + 1 .. rBraceIndex];
    const bodyNode = try createSeqAstNode(allocator, compInfo, bodyTokens);

    const returnType = if (tokens[rParenIndex + 1].type == TokenType.Colon) val: {
        const returnTypeTokens = tokens[rParenIndex + 2 .. lBraceIndex];
        break :val try createTypeNode(allocator, compInfo, returnTypeTokens);
    } else val: {
        const tempType: AstTypes = AstTypes.Void;
        break :val try create(AstTypes, allocator, tempType);
    };

    return FuncOffsetData{
        .func = .{
            .name = try cloneString(allocator, name),
            .generics = generics,
            .params = parameters,
            .body = bodyNode,
            .returnType = returnType,
        },
        .offset = rBraceIndex + 1,
    };
}

fn createStructAttributes(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) !StructAttributesOffsetData {
    var attributes = ArrayList(StructAttribute).init(allocator);
    defer attributes.deinit();

    var current: usize = 0;

    while (current < tokens.len) {
        switch (tokens[current].type) {
            .Pub => {
                const attr = try createStructAttribute(allocator, compInfo, tokens[current + 1 ..], MemberVisibility.Public);
                try attributes.append(attr.attr);
                current += attr.offset;
            },
            .Prot => {
                const attr = try createStructAttribute(allocator, compInfo, tokens[current + 1 ..], MemberVisibility.Protected);
                try attributes.append(attr.attr);
                current += attr.offset;
            },
            .Identifier => {
                const attr = try createStructAttribute(allocator, compInfo, tokens[current..], MemberVisibility.Private);
                try attributes.append(attr.attr);
                current += attr.offset;
            },
            .Semicolon => {
                current += 1;
            },
            else => {
                std.debug.print("stuck {any}\n", .{tokens[current]});
            },
        }
    }

    const slice = try allocator.dupe(StructAttribute, attributes.items);
    return .{
        .attrs = slice,
        .offset = current,
    };
}

fn createStructAttribute(allocator: Allocator, compInfo: *CompInfo, tokens: []Token, visibility: MemberVisibility) !StructAttributeOffsetData {
    if (switch (tokens[0].type) {
        .Identifier => false,
        .Static => false,
        .Prot => false,
        .Pub => false,
        .Fn => false,
        else => true,
    }) {
        return astError(AstError.TokenNotFound, "struct attribute name");
    }

    var offset: usize = 0;
    var attr: StructAttribute = undefined;
    const isStatic = tokens[0].type == TokenType.Static;
    const attrTokens = if (isStatic) tokens[1..] else tokens;
    const tempAttr = try createStructAttributeData(allocator, compInfo, attrTokens, visibility);
    offset = tempAttr.offset + @as(usize, if (isStatic) 1 else 0);
    attr = StructAttribute{
        .static = isStatic,
        .attr = tempAttr.attr,
    };

    return .{
        .attr = attr,
        .offset = offset,
    };
}

fn createStructAttributeData(allocator: Allocator, compInfo: *CompInfo, tokens: []Token, visibility: MemberVisibility) !StructAttributeUnionOffsetData {
    printTokens(tokens[0..1]);
    const nameIndex: u32 = if (tokens[0].type == TokenType.Fn) 1 else 0;
    const name = try cloneString(allocator, tokens[nameIndex].string.?);
    var offset: usize = 0;

    const attr = switch (tokens[0].type) {
        .Fn => val: {
            const data = try createFuncDecNode(allocator, compInfo, tokens[1..]);
            offset += data.offset + 2;

            break :val StructAttributeUnion{
                .Function = .{
                    .name = name,
                    .func = data.func,
                    .visibility = visibility,
                },
            };
        },
        .Identifier => val: {
            const typeTokens = tokens[2..];
            const typeNode = try createTypeNode(allocator, compInfo, typeTokens);
            offset = delimiterIndex(tokens, 2, TokenType.Semicolon) catch |e| return astError(e, ";");

            break :val StructAttributeUnion{
                .Member = .{
                    .name = name,
                    .visibility = visibility,
                    .type = typeNode,
                },
            };
        },
        else => {
            return astError(AstError.TokenNotFound, "( or :");
        },
    };

    return .{
        .attr = attr,
        .offset = offset,
    };
}

fn parseParameters(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) ![]Parameter {
    var parameters = ArrayList(Parameter).init(allocator);
    defer parameters.deinit();

    var to = delimiterIndex(tokens, 0, TokenType.Comma) catch tokens.len;
    var prev: usize = 0;

    while (prev < tokens.len) {
        const paramTokens = tokens[prev..to];
        const nameToken = paramTokens[0];

        if (nameToken.type != TokenType.Identifier) return astError(AstError.TokenNotFound, "identifier");

        const typeTokens = tokens[prev + 2 .. to];
        const parameterType = try createTypeNode(allocator, compInfo, typeTokens);

        try parameters.append(.{
            .name = try cloneString(allocator, nameToken.string.?),
            .type = parameterType,
        });

        prev = to + 1;
        to = delimiterIndex(tokens, to + 1, TokenType.Comma) catch tokens.len;
    }

    const slice = try allocator.dupe(Parameter, parameters.items);
    return slice;
}

fn createBoolNode(allocator: Allocator, value: bool) !*const AstNode {
    const node = try create(AstNode, allocator, .{
        .Value = .{ .Bool = value },
    });
    return node;
}

fn parseGenerics(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) ![]GenericType {
    var genericList = ArrayList(GenericType).init(allocator);
    defer genericList.deinit();

    var to = delimiterIndex(tokens, 0, TokenType.Comma) catch tokens.len;
    var prev: usize = 0;

    while (prev < tokens.len) {
        const typeTokens = tokens[prev..to];
        const hasRestriction = typeTokens.len > 1;
        const colonIndex = if (hasRestriction) prev + 1 else to;

        var structGeneric = GenericType{
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
            const typeNode = try createTypeNode(allocator, compInfo, restrictionTokens);
            structGeneric.restriction = typeNode;
        }

        try genericList.append(structGeneric);

        prev = to + 1;
        to = delimiterIndex(tokens, to + 1, TokenType.Comma) catch tokens.len;
    }

    const slice = allocator.dupe(GenericType, genericList.items);
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

fn smartDelimiterIndex(tokens: []Token, compInfo: *CompInfo, start: usize, delimiter: TokenType) !usize {
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
    compInfo: *CompInfo,
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
        annotation = try createTypeNode(allocator, compInfo, typeTokens);

        offset += index - searchStart + 1;
    }

    const setTokens = tokens[start + offset ..];
    const setNode = try createAstNode(allocator, compInfo, setTokens);
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

fn parseGenericArgs(allocator: Allocator, compInfo: *CompInfo, tokens: []Token, start: usize) ![]*const AstTypes {
    var typeGenericsArr = ArrayList(*const AstTypes).init(allocator);
    defer typeGenericsArr.deinit();

    if (start + 2 < tokens.len) {
        const tokensToRAngle = smartDelimiterIndex(tokens, compInfo, start + 2, TokenType.RAngle) catch |e| return astError(e, ">");
        var typeEnd = smartDelimiterIndex(tokens, compInfo, start + 2, TokenType.Comma) catch tokensToRAngle;
        var prev = start + 2;

        while (prev < tokensToRAngle) {
            const typeTokens = tokens[prev..typeEnd];
            const typeNode = try createTypeNode(allocator, compInfo, typeTokens);
            try typeGenericsArr.append(typeNode);

            prev = typeEnd + 1;
            typeEnd = smartDelimiterIndex(tokens, compInfo, typeEnd + 1, TokenType.Comma) catch tokensToRAngle;
        }
    } else {
        return AstError.ExpectedGenericArgument;
    }

    return try allocator.dupe(*const AstTypes, typeGenericsArr.items);
}

fn createTypeNode(allocator: Allocator, compInfo: *CompInfo, tokens: []Token) (AstError || Allocator.Error)!*const AstTypes {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == TokenType.QuestionMark;
    var current = @as(u32, if (nullable) 1 else 0);

    var res: *const AstTypes = undefined;

    if (tokens[current].type == TokenType.Identifier) {
        const tokenString = tokens[current].string.?;
        if (compInfo.hasGeneric(tokenString)) {
            res = try create(AstTypes, allocator, AstTypes{ .Generic = tokenString });
        } else if (compInfo.getRegisteredStruct(tokenString)) |registeredStruct| {
            var customType = CustomType{
                .generics = &[_]*const AstTypes{},
                .structCode = registeredStruct.typeCode,
                .name = try cloneString(allocator, registeredStruct.name),
            };

            if (registeredStruct.numGenerics > 0) {
                customType.generics = try parseGenericArgs(allocator, compInfo, tokens, current);
            }

            res = try create(AstTypes, allocator, .{ .Custom = customType });
        } else {
            return astError(AstError.UnknownType, tokenString);
        }
    } else {
        res = try createAstType(allocator, compInfo, tokens[current]);
    }

    while (current + 1 < tokens.len and tokens[current + 1].type == TokenType.LBracket) : (current += 1) {
        // check for (type)[]
        if (tokens[current + 2].type == TokenType.RBracket) {
            res = try create(AstTypes, allocator, .{
                .DynamicArray = .{
                    .type = res,
                },
            });
        } else {
            const sizeTokens = tokens[current + 2 ..];
            const sizeNode = try createAstNode(allocator, compInfo, sizeTokens);
            res = try create(AstTypes, allocator, .{
                .StaticArray = .{
                    .type = res,
                    .size = sizeNode.node,
                },
            });
        }
    }

    if (nullable) {
        res = try create(AstTypes, allocator, .{ .Nullable = res });
    }

    return res;
}

fn createAstType(allocator: Allocator, compInfo: *CompInfo, token: Token) !*const AstTypes {
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
        else => a: {
            if (token.string) |tokenString| {
                if (token.type == TokenType.Identifier and compInfo.hasGeneric(tokenString)) {
                    break :a AstTypes{
                        .Generic = tokenString,
                    };
                }
            }

            return AstError.UnknownType;
        },
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
        AstError.InvalidStructKey => "invalid struct key",
    };
}

fn astError(errorType: AstError, str: []const u8) AstError {
    const stdout = std.io.getStdOut().writer();
    stdout.print("{s} \"{s}\"\n", .{
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

            if (dec.annotation) |annotation| {
                freeType(allocator, annotation);
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
                if (generic.restriction) |restriction| {
                    freeType(allocator, restriction);
                }
                allocator.free(generic.name);
            }

            for (dec.attributes) |attr| {
                freeAttr(allocator, attr);
            }

            allocator.free(dec.attributes);
            allocator.free(dec.generics);
        },
        .IfStatement => |*statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
        },
        .NoOp => {},
        .FuncDec => |func| {
            freeFuncDec(allocator, func);
        },
        .ReturnNode => |ret| {
            freeNode(allocator, ret);
        },
        .StructInit => |init| {
            allocator.free(init.name);

            for (init.attributes) |attr| {
                allocator.free(attr.name);
                freeNode(allocator, attr.value);
            }

            for (init.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(init.attributes);
            allocator.free(init.generics);
        },
        .Bang => |bang| {
            freeNode(allocator, bang);
        },
    }

    allocator.destroy(node);
}

fn freeFuncDec(allocator: Allocator, func: FuncDecNode) void {
    allocator.free(func.name);

    for (func.params) |param| {
        freeType(allocator, param.type);
        allocator.free(param.name);
    }
    allocator.free(func.params);

    if (func.generics) |generics| {
        allocator.free(generics);
    }
    freeNode(allocator, func.body);
    freeType(allocator, func.returnType);
}

fn freeAttr(allocator: Allocator, attr: StructAttribute) void {
    switch (attr.attr) {
        .Member => {
            allocator.free(attr.attr.Member.name);
            freeType(allocator, attr.attr.Member.type);
        },
        .Function => {
            allocator.free(attr.attr.Function.name);
            freeFuncDec(allocator, attr.attr.Function.func);
        },
    }
}

pub fn freeRegisteredStructs(allocator: Allocator, structs: []RegisteredStruct) void {
    for (structs) |s| {
        allocator.free(s.name);
    }

    allocator.free(structs);
}

pub fn freeCompInfo(compInfo: *CompInfo) void {
    compInfo.generics.deinit();
}
