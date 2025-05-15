const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const free = blitz.free;
const string = blitz.string;
const CompInfo = utils.CompInfo;
const create = utils.create;
const createMut = utils.createMut;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const TokenizeError = tokenizer.TokenizeError;

// debug
const debug = @import("debug.zig");
const printTokens = debug.printTokens;
const printNode = debug.printNode;
const printType = debug.printType;
const printFuncDec = debug.printFuncDec;
const printGenerics = debug.printGenerics;

const SeqNode = struct {
    nodes: []*const AstNode,
};

pub const AstNumberVariants = enum {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
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
};

const AstNumber = struct {
    type: AstNumberVariants,
    value: []u8,
};

const AstStaticArrayType = struct {
    type: *const AstTypes,
    size: *const AstNode,
};

pub const CustomType = struct {
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
    Function,
    StaticStructInstance,
};

pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Void,
    Number: AstNumberVariants,
    DynamicArray: *const AstTypes,
    StaticArray: AstStaticArrayType,
    Nullable: *const AstTypes,
    Custom: CustomType,
    Generic: []u8,
    Function: *const FuncDecNode,
    StaticStructInstance: []u8,
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

pub const StructAttributeVariants = enum {
    Member,
    Function,
};

pub const StructAttributeUnion = union(StructAttributeVariants) {
    Member: *const AstTypes,
    Function: *FuncDecNode,
};

pub const StructAttribute = struct {
    name: []u8,
    attr: StructAttributeUnion,
    visibility: MemberVisibility,
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

pub const StructDecNode = struct {
    const Self = @This();

    name: []u8,
    generics: []GenericType,
    attributes: []StructAttribute,
    totalMemberList: []StructAttribute,
    deriveType: ?*const AstTypes,
};

pub const AttributeDefinition = struct {
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
    bodyTokens: []tokenizer.Token,
    returnType: *const AstTypes,
};

const FuncCallNode = struct {
    func: *const AstNode,
    params: []*const AstNode,
};

const PropertyAccess = struct {
    value: *const AstNode,
    property: []u8,
};

pub const MathOps = enum {
    Add,
    Sub,
    Mult,
    Div,
};

const MathOp = struct {
    type: MathOps,
    left: *const AstNode,
    right: *const AstNode,
};

const IndexValueNode = struct {
    index: *const AstNode,
    value: *const AstNode,
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
    FuncCall,
    ReturnNode,
    StructInit,
    Bang,
    PropertyAccess,
    StaticStructInstance,
    FuncReference,
    MathOp,
    IndexValue,
};
pub const AstNode = union(AstNodeVariants) {
    NoOp,
    Seq: SeqNode,
    VarDec: VarDecNode,
    Type: AstTypes,
    Value: AstValues,
    Cast: CastNode,
    Variable: VariableNode,
    StructDec: *StructDecNode,
    IfStatement: IfStatementNode,
    FuncDec: []u8,
    FuncCall: FuncCallNode,
    ReturnNode: *const AstNode,
    StructInit: StructInitNode,
    Bang: *const AstNode,
    PropertyAccess: PropertyAccess,
    StaticStructInstance: []u8,
    FuncReference: []u8,
    MathOp: MathOp,
    IndexValue: IndexValueNode,
};

pub const AstError = error{
    UnexpectedToken,
    TokenNotFound,
    InvalidType,
    UnknownType,
    ExpectedGenericArgument,
    InvalidStructKey,
    FunctionNotFound,
    ExpectedNameForStruct,
    ExpectedIdentifierPropertyAccess,
    ExpectedIdentifierPropertyAccessSource,
    ExpectedStructDeriveType,
    ExpectedIdentifierForDerivedType,
    UndefinedStruct,
};

const AstNodeOffsetData = struct {
    node: *const AstNode,
    offset: usize,
};

const AstTypeOffsetData = struct {
    type: *const AstTypes,
    offset: usize,
};

const FuncOffsetData = struct {
    func: *FuncDecNode,
    offset: usize,
};

const MathOpTokenMap = struct {
    type: MathOps,
    token: tokenizer.TokenType,
};

pub const Ast = struct {
    const Self = @This();

    root: SeqNode,
    allocator: Allocator,

    pub fn init(allocator: Allocator, seq: SeqNode) Self {
        return Self{
            .root = seq,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        free.freeNodes(self.allocator, self.root.nodes);
        self.allocator.free(self.root.nodes);
    }
};

pub fn createAst(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) !Ast {
    const seq = try createSeqNode(allocator, compInfo, tokens);
    return Ast.init(allocator, seq);
}

fn createSeqAstNode(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) !*const AstNode {
    const seq = try createSeqNode(allocator, compInfo, tokens);
    const node = try create(AstNode, allocator, .{ .Seq = seq });
    return node;
}

fn createSeqNode(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) !SeqNode {
    var currentToken: usize = 0;
    var seq = ArrayList(*const AstNode).init(allocator);
    defer seq.deinit();

    while (currentToken < tokens.len) {
        if (tokens[currentToken].type == .Semicolon) {
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

pub fn createAstNode(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) (AstError || Allocator.Error)!AstNodeOffsetData {
    if (tokens.len == 0) {
        return .{
            .offset = 0,
            .node = try create(AstNode, allocator, @as(AstNode, .NoOp)),
        };
    }

    if (isMathExpr(compInfo, tokens)) {
        const node = try parseMathExpr(allocator, compInfo, tokens);
        return .{
            .node = node,
            .offset = tokens.len,
        };
    }

    var propAccessNode: ?*const AstNode = null;
    // holds index of closest period to create prop access node
    // ends up holding final index of expression - 2 just because
    var closestPeriod = utils.smartDelimiterIndex(tokens, compInfo, 0, .Period) catch null;
    if (closestPeriod) |periodIndex| {
        const sourceTokens = tokens[0..periodIndex];
        const singleNode = try isSingleNode(compInfo, sourceTokens);
        if (singleNode) {
            const data = try createAstNode(allocator, compInfo, sourceTokens);
            propAccessNode = data.node;
        }
    }

    if (propAccessNode != null) {
        var currentChar = closestPeriod.?;

        while (currentChar < tokens.len) {
            if (tokens[currentChar + 1].type != .Identifier) return AstError.ExpectedIdentifierPropertyAccess;

            propAccessNode = try create(AstNode, allocator, .{
                .PropertyAccess = .{
                    .value = propAccessNode.?,
                    .property = tokens[currentChar + 1].string.?,
                },
            });

            var current = currentChar + 2;

            while (current < tokens.len and (tokens[current].type == .LParen or tokens[current].type == .LBracket)) {
                if (tokens[current].type == .LParen) {
                    const rParenIndex = utils.smartDelimiterIndex(tokens, compInfo, current + 1, .RParen) catch |e| return astError(e, ")");
                    const paramTokens = tokens[current + 1 .. rParenIndex];
                    const params = try parseParams(allocator, compInfo, paramTokens);

                    propAccessNode = try create(AstNode, allocator, .{
                        .FuncCall = .{
                            .func = propAccessNode.?,
                            .params = params,
                        },
                    });

                    current += paramTokens.len + 2;
                } else if (tokens[current].type == .LBracket) {
                    const rBracketIndex = utils.smartDelimiterIndex(tokens, compInfo, current + 1, .RBracket) catch |e| return astError(e, "]");
                    const indexTokens = tokens[current + 1 .. rBracketIndex];
                    const indexNode = try createAstNode(allocator, compInfo, indexTokens);

                    propAccessNode = try create(AstNode, allocator, .{
                        .IndexValue = .{
                            .index = indexNode.node,
                            .value = propAccessNode.?,
                        },
                    });

                    current += indexTokens.len + 2;
                }
            }

            if (current < tokens.len) {
                switch (tokens[current].type) {
                    .Period => currentChar = current,
                    .LParen => {},
                    else => {
                        closestPeriod = current;
                        break;
                    },
                }
            } else {
                closestPeriod = current;
                break;
            }
        }
    }

    if (propAccessNode) |accessNode| {
        return .{
            .node = accessNode,
            .offset = closestPeriod.?,
        };
    }

    const token = tokens[0];

    switch (token.type) {
        .Const => {
            return try createVarDecNode(allocator, compInfo, tokens, true);
        },
        .Var => {
            return try createVarDecNode(allocator, compInfo, tokens, false);
        },
        .Number => {
            const numType = if (string.findChar(token.string.?, 0, '.') != null) AstNumberVariants.F32 else AstNumberVariants.U32;
            const node = try create(AstNode, allocator, .{
                .Value = .{
                    .Number = .{
                        .type = numType,
                        .value = try string.cloneString(allocator, token.string.?),
                    },
                },
            });

            return .{
                .node = node,
                .offset = 1,
            };
        },
        .StringToken => {
            const str = tokens[0].string.?;
            const node = try create(AstNode, allocator, .{
                .Value = .{
                    .String = try string.cloneString(allocator, str),
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

            const end = utils.smartDelimiterIndex(tokens, compInfo, 1, .RBracket) catch |e| return astError(e, "]");
            var comma = utils.smartDelimiterIndex(tokens, compInfo, 1, .Comma) catch end;
            var prev: usize = 1;

            while (prev < end) {
                const tempTokens = tokens[prev..comma];
                const node = try createAstNode(allocator, compInfo, tempTokens);
                try nodeItems.append(node.node);

                prev = comma + 1;
                comma = utils.smartDelimiterIndex(tokens, compInfo, comma + 1, .Comma) catch end;
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
            const isGeneric = tokens[1].type == .LBracket;
            const nameIndex = (if (isGeneric) try utils.delimiterIndex(tokens, 2, .RBracket) else 0) + 1;
            const lBraceIndex = try utils.delimiterIndex(tokens, nameIndex + 1, .LBrace);
            const end = try utils.smartDelimiterIndex(tokens, compInfo, lBraceIndex + 1, .RBrace);

            if (!compInfo.preAst) {
                return .{
                    .offset = end + 1,
                    .node = try create(AstNode, allocator, @as(AstNode, AstNode.NoOp)),
                };
            }

            var deriveType: ?*const AstTypes = null;
            if (tokens[nameIndex + 1].type == .Colon) {
                const deriveTokens = tokens[nameIndex + 2 .. lBraceIndex];

                if (deriveTokens.len == 0) {
                    return AstError.ExpectedStructDeriveType;
                } else if (deriveTokens[0].type != .Identifier) {
                    return AstError.ExpectedIdentifierForDerivedType;
                }

                deriveType = try createTypeNode(allocator, compInfo, deriveTokens);
            }

            const defTokens = tokens[lBraceIndex + 1 .. end];
            var genericTypes: []GenericType = &[_]GenericType{};

            if (isGeneric) {
                const genericsTokens = tokens[2 .. nameIndex - 1];
                genericTypes = try parseGenerics(allocator, compInfo, genericsTokens);
            }

            for (genericTypes) |gType| {
                try compInfo.addAvailableGeneric(gType.name);
            }

            const attrData = try createStructAttributes(allocator, compInfo, defTokens);

            if (tokens[nameIndex].type != .Identifier) {
                return astError(AstError.TokenNotFound, "struct name");
            }

            const name = try string.cloneString(allocator, tokens[nameIndex].string.?);

            const structDecNode = try createMut(StructDecNode, allocator, .{
                .name = name,
                .generics = genericTypes,
                .attributes = attrData.attrs,
                .deriveType = deriveType,
                .totalMemberList = &[_]StructAttribute{},
            });

            const structNode = try create(AstNode, allocator, .{
                .StructDec = structDecNode,
            });

            for (genericTypes) |gType| {
                compInfo.removeAvailableGeneric(gType.name);
            }

            return .{
                .offset = end + 1,
                .node = structNode,
            };
        },
        .Identifier => {
            if (tokens.len > 1) {
                if (tokens[1].type == .LParen and compInfo.hasFunctionName(token.string.?)) {
                    const rParenIndex = utils.smartDelimiterIndex(tokens, compInfo, 2, .RParen) catch |e| return astError(e, ")");
                    const paramTokens = tokens[2..rParenIndex];
                    const params = try parseParams(allocator, compInfo, paramTokens);

                    if (compInfo.hasFunctionName(token.string.?)) {
                        const node = try create(AstNode, allocator, .{
                            .FuncCall = .{
                                .func = try create(AstNode, allocator, .{
                                    .FuncReference = try string.cloneString(allocator, token.string.?),
                                }),
                                .params = params,
                            },
                        });

                        return .{
                            .offset = 3 + paramTokens.len,
                            .node = node,
                        };
                    } else return astError(AstError.FunctionNotFound, token.string.?);
                }

                if (tokens[1].type == .LBracket) {
                    const rBracketIndex = utils.smartDelimiterIndex(tokens, compInfo, 2, .RBracket) catch |e| return astError(e, "]");
                    const indexTokens = tokens[2..rBracketIndex];
                    const indexNode = try createAstNode(allocator, compInfo, indexTokens);

                    const valueNode = try createAstNode(allocator, compInfo, tokens[0..1]);

                    const node = try create(AstNode, allocator, .{
                        .IndexValue = .{
                            .index = indexNode.node,
                            .value = valueNode.node,
                        },
                    });

                    return .{
                        .offset = 3 + indexTokens.len,
                        .node = node,
                    };
                }
            }

            if (compInfo.hasStruct(token.string.?)) {
                if (tokens.len == 1) {
                    return .{
                        .offset = 1,
                        .node = try create(AstNode, allocator, .{
                            .StaticStructInstance = try string.cloneString(allocator, token.string.?),
                        }),
                    };
                }

                var offset: usize = 0;
                var structGenerics: []*const AstTypes = &[_]*const AstTypes{};

                if (tokens.len > 1 and tokens[1].type == .LAngle) {
                    const rAngleIndex = utils.delimiterIndex(tokens, 2, .RAngle) catch |e| return astError(e, ">");
                    const typeTokens = tokens[0 .. rAngleIndex + 1];

                    if (typeTokens.len > 0) {
                        offset += typeTokens.len - 1;
                    }

                    structGenerics = try parseGenericArgs(allocator, compInfo, typeTokens, 0);
                }

                if (tokens.len > 1 + offset and tokens[1 + offset].type == .LBrace) {
                    const lBraceIndex = try utils.smartDelimiterIndex(tokens, compInfo, 1, .LBrace);
                    const rBraceIndex = try utils.smartDelimiterIndex(tokens, compInfo, lBraceIndex + 1, .RBrace);
                    const initTokens = tokens[lBraceIndex..rBraceIndex];
                    const structInit = try createStructInit(allocator, compInfo, token.string.?, structGenerics, initTokens);
                    const initNode = try create(AstNode, allocator, .{
                        .StructInit = structInit,
                    });

                    return .{
                        .node = initNode,
                        .offset = rBraceIndex + 1,
                    };
                } else {
                    // TODO - type cast to struct PROBABLY

                    const lParenIndex = try utils.smartDelimiterIndex(tokens, compInfo, 1, .LParen);
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
                        .name = try string.cloneString(allocator, token.string.?),
                    },
                }),
            };
        },
        .If => {
            const closeParen = utils.smartDelimiterIndex(tokens, compInfo, 2, .RParen) catch |e| return astError(e, ")");
            const conditionTokens = tokens[2..closeParen];
            const conditionNode = try createAstNode(allocator, compInfo, conditionTokens);

            const endBrace = utils.smartDelimiterIndex(tokens, compInfo, closeParen + 2, .RBrace) catch |e| return astError(e, "]");
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
            const data = try createFuncDecNode(allocator, compInfo, tokens, true, true);

            const func = try create(AstNode, allocator, .{
                .FuncDec = try string.cloneString(allocator, data.func.name),
            });

            try compInfo.addFunction(data.func.name, data.func);

            return .{
                .node = func,
                .offset = data.offset + 1,
            };
        },
        .Return => {
            const semiIndex = try utils.smartDelimiterIndex(tokens, compInfo, 1, .Semicolon);
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
        .CharToken => {
            const node = try create(AstNode, allocator, .{
                .Value = .{ .Char = token.string.?[0] },
            });

            return .{
                .node = node,
                .offset = 1,
            };
        },
        .U8,
        .U16,
        .U32,
        .U64,
        .U128,
        .USize,
        .I8,
        .I16,
        .I32,
        .I64,
        .I128,
        .F8,
        .F16,
        .F32,
        .F64,
        .F128,
        .StringType,
        .Bool,
        .CharType,
        => {
            if (tokens.len == 1) return astError(AstError.TokenNotFound, "(");

            if (tokens[1].type != .LParen) {
                return astError(AstError.UnexpectedToken, tokens[0].type.toString());
            }

            const rParentIndex = utils.smartDelimiterIndex(tokens, compInfo, 2, .RParen) catch |e| return astError(e, ")");
            const castTokens = tokens[2..rParentIndex];
            const castNode = try createAstNode(allocator, compInfo, castTokens);

            const toType = try createAstType(allocator, compInfo, tokens[0]);

            const node = try create(AstNode, allocator, .{
                .Cast = .{
                    .node = castNode.node,
                    .toType = toType,
                },
            });

            return .{
                .node = node,
                .offset = castNode.offset + 4,
            };
        },
        else => {
            return astError(AstError.UnexpectedToken, tokens[0].type.toString());
        },
    }
}

pub fn mergeMembers(allocator: Allocator, compInfo: *CompInfo, attrs: []StructAttribute, derived: *const AstTypes) ![]StructAttribute {
    var res = try ArrayList(StructAttribute).initCapacity(allocator, attrs.len);
    const derivedDec = compInfo.getStructDec(derived.Custom.name);

    for (attrs) |attr| {
        if (attr.attr != .Member) continue;
        try res.append(attr);
    }

    if (derivedDec) |dec| {
        if (dec.deriveType) |decDerived| {
            const arr = try mergeMembers(allocator, compInfo, dec.attributes, decDerived);
            try res.appendSlice(arr);
        } else {
            for (dec.attributes) |attr| {
                if (attr.attr != .Member) continue;
                try res.append(attr);
            }
        }
    }

    return res.toOwnedSlice();
}

fn isMathExpr(compInfo: *CompInfo, tokens: []tokenizer.Token) bool {
    const addIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, .Add) catch null;
    const subIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, .Sub) catch null;
    const mulIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, .Mult) catch null;
    const divIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, .Div) catch null;
    const eqIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, .EqSet) catch null;

    if (eqIndex != null) return false;
    if (addIndex == null and subIndex == null and mulIndex == null and divIndex == null) return false;
    return true;
}

fn parseMathHelper(allocator: Allocator, compInfo: *CompInfo, opIndex: usize, tokens: []tokenizer.Token, opType: MathOps) !MathOp {
    const leftTokens = tokens[0..opIndex];
    const leftExpr = try parseMathExpr(allocator, compInfo, leftTokens);
    const rightTokens = tokens[opIndex + 1 ..];
    const rightExpr = try parseMathExpr(allocator, compInfo, rightTokens);

    return .{
        .type = opType,
        .left = leftExpr,
        .right = rightExpr,
    };
}

fn parseMathExpr(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) (AstError || Allocator.Error)!*const AstNode {
    const ops = &[_]MathOpTokenMap{
        .{ .type = .Add, .token = .Add },
        .{ .type = .Sub, .token = .Sub },
        .{ .type = .Mult, .token = .Mult },
        .{ .type = .Div, .token = .Div },
    };

    inline for (ops) |op| {
        const tokIndex = utils.smartDelimiterIndex(tokens, compInfo, 0, op.token) catch null;

        if (tokIndex) |index| {
            const opSides = try parseMathHelper(allocator, compInfo, index, tokens, op.type);
            return try create(AstNode, allocator, .{
                .MathOp = .{
                    .type = op.type,
                    .left = opSides.left,
                    .right = opSides.right,
                },
            });
        }
    }

    return (try createAstNode(allocator, compInfo, tokens)).node;
}

fn isSingleNode(compInfo: *CompInfo, tokens: []tokenizer.Token) !bool {
    if (tokens.len == 0) return false;

    if (tokens.len > 1) {
        if (tokens[0].type == .LParen and tokens[tokens.len - 1].type == .RParen) return true;
        if (tokens.len > 3 and tokens[0].type == .Identifier and tokens[1].type == .LBracket) {
            const rBracketIndex = try utils.smartDelimiterIndex(tokens, compInfo, 2, .RBracket);
            if (rBracketIndex == tokens.len - 1) return true;
        }
    } else if (tokens[0].type == .Identifier) return true;

    return false;
}

fn createPropertyAccess(allocator: Allocator, value: *const AstNode, str: []u8) !*const AstNode {
    return try create(AstNode, allocator, .{
        .PropertyAccess = .{
            .value = value,
            .property = str,
        },
    });
}

fn parseParams(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) ![]*const AstNode {
    var params = ArrayList(*const AstNode).init(allocator);
    defer params.deinit();

    var i: usize = 0;
    while (i < tokens.len) {
        const commaIndex = utils.smartDelimiterIndex(tokens, compInfo, i, .Comma) catch tokens.len;
        const paramTokens = tokens[i..commaIndex];

        const node = try createAstNode(allocator, compInfo, paramTokens);
        try params.append(node.node);

        i = commaIndex + 1;
    }

    return try allocator.dupe(*const AstNode, params.items);
}

fn createStructInit(allocator: Allocator, compInfo: *CompInfo, structName: []u8, generics: []*const AstTypes, tokens: []tokenizer.Token) !StructInitNode {
    var attributes = ArrayList(AttributeDefinition).init(allocator);
    defer attributes.deinit();

    var i: usize = 1;
    while (i < tokens.len) {
        const commaIndex = utils.smartDelimiterIndex(tokens, compInfo, i, .Comma) catch tokens.len;

        const value: *const AstNode = if (i == tokens.len - 1) a: {
            const varNode = try create(AstNode, allocator, .{
                .Variable = .{
                    .name = try string.cloneString(allocator, tokens[i].string.?),
                },
            });
            break :a varNode;
        } else switch (tokens[i + 1].type) {
            .EqSet => a: {
                const valueTokens = tokens[i + 2 .. commaIndex];
                const valueNode = try createAstNode(allocator, compInfo, valueTokens);
                break :a valueNode.node;
            },
            .Comma => a: {
                const varNode = try create(AstNode, allocator, .{
                    .Variable = .{
                        .name = try string.cloneString(allocator, tokens[i].string.?),
                    },
                });
                break :a varNode;
            },
            .RBrace => a: {
                const varNode = try create(AstNode, allocator, .{
                    .Variable = .{
                        .name = try string.cloneString(allocator, tokens[i].string.?),
                    },
                });
                break :a varNode;
            },
            else => return astError(
                AstError.UnexpectedToken,
                if (tokens[i + 1].string != null) tokens[i + 1].string.? else "(no str)",
            ),
        };

        const attr: AttributeDefinition = .{
            .name = try string.cloneString(allocator, tokens[i].string.?),
            .value = value,
        };

        try attributes.append(attr);
        i = commaIndex + 1;

        if (commaIndex == tokens.len) break;
    }

    const attributesSlices = try allocator.dupe(AttributeDefinition, attributes.items);

    return .{
        .name = try string.cloneString(allocator, structName),
        .attributes = attributesSlices,
        .generics = generics,
    };
}

fn createFuncDecNode(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token, parseBody: bool, pushRegGenScope: bool) !FuncOffsetData {
    var offset: usize = 1;
    var generics: ?[]GenericType = null;

    if (pushRegGenScope) {
        try compInfo.pushRegGenScope();
    }

    if (tokens[1].type == .LBracket) {
        const rBracketIndex = utils.delimiterIndex(tokens, 3, .RBracket) catch |e| return astError(e, "]");
        const genericTokens = tokens[2..rBracketIndex];
        offset += genericTokens.len + 2;
        generics = try parseGenerics(allocator, compInfo, genericTokens);

        if (generics) |gens| {
            for (gens) |generic| {
                try compInfo.addAvailableGeneric(generic.name);
            }
        }
    }

    const name = tokens[offset].string.?;

    const rParenIndex = utils.delimiterIndex(tokens, offset + 2, .RParen) catch |e| return astError(e, ")");
    const parameterTokens = tokens[offset + 2 .. rParenIndex];
    const parameters = try parseParameters(allocator, compInfo, parameterTokens);

    const lBraceIndex = utils.smartDelimiterIndex(tokens, compInfo, rParenIndex + 1, .LBrace) catch |e| return astError(e, "{");
    const rBraceIndex = utils.smartDelimiterIndex(tokens, compInfo, lBraceIndex + 1, .RBrace) catch |e| return astError(e, "}");
    const bodyTokens = tokens[lBraceIndex + 1 .. rBraceIndex];
    const bodyNode = if (parseBody) a: {
        break :a try createSeqAstNode(allocator, compInfo, bodyTokens);
    } else try create(AstNode, allocator, .NoOp);

    const returnType = if (tokens[rParenIndex + 1].type == .Colon) val: {
        const returnTypeTokens = tokens[rParenIndex + 2 .. lBraceIndex];
        break :val try createTypeNode(allocator, compInfo, returnTypeTokens);
    } else val: {
        break :val try create(AstTypes, allocator, @as(AstTypes, AstTypes.Void));
    };

    if (pushRegGenScope) {
        compInfo.popRegGenScope();
    }

    return .{
        .func = try createMut(FuncDecNode, allocator, .{
            .name = try string.cloneString(allocator, name),
            .generics = generics,
            .params = parameters,
            .body = bodyNode,
            .bodyTokens = bodyTokens,
            .returnType = returnType,
        }),
        .offset = rBraceIndex,
    };
}

fn createStructAttributes(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) !StructAttributesOffsetData {
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
            .Fn => {
                const attr = try createStructAttribute(allocator, compInfo, tokens[current..], MemberVisibility.Private);
                try attributes.append(attr.attr);
                current += attr.offset;
            },
            .Semicolon => {
                current += 1;
            },
            else => {
                std.debug.print("stuck {any}\n", .{tokens[current]});
                return AstError.UnexpectedToken;
            },
        }
    }

    const slice = try allocator.dupe(StructAttribute, attributes.items);
    return .{
        .attrs = slice,
        .offset = current,
    };
}

fn createStructAttribute(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token, visibility: MemberVisibility) !StructAttributeOffsetData {
    if (switch (tokens[0].type) {
        .Identifier, .Static, .Prot, .Pub, .Fn => false,
        else => true,
    }) {
        return astError(AstError.TokenNotFound, "struct attribute name");
    }

    var offset: usize = 0;
    var attr: StructAttribute = undefined;
    const isStatic = tokens[0].type == .Static;
    const attrTokens = if (isStatic) tokens[1..] else tokens;
    const tempAttr = try createStructAttributeData(allocator, compInfo, attrTokens);
    offset = tempAttr.offset + @as(usize, if (isStatic) 1 else 0);

    const nameIndex: u32 = if (attrTokens[0].type == .Fn) 1 else 0;
    const name = try string.cloneString(allocator, attrTokens[nameIndex].string.?);

    attr = .{
        .static = isStatic,
        .attr = tempAttr.attr,
        .visibility = visibility,
        .name = name,
    };

    return .{
        .attr = attr,
        .offset = offset,
    };
}

fn createStructAttributeData(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) !StructAttributeUnionOffsetData {
    var offset: usize = 0;

    const attr: StructAttributeUnion = switch (tokens[0].type) {
        .Fn => val: {
            const data = try createFuncDecNode(allocator, compInfo, tokens, !compInfo.preAst, false);
            offset += data.offset + 2;

            break :val .{
                .Function = data.func,
            };
        },
        .Identifier => val: {
            const typeTokens = tokens[2..];
            const typeNode = try createTypeNode(allocator, compInfo, typeTokens);
            offset = (utils.delimiterIndex(tokens, 2, .Semicolon) catch |e| return astError(e, ";")) + 1;

            break :val .{ .Member = typeNode };
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

fn parseParameters(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) ![]Parameter {
    var parameters = ArrayList(Parameter).init(allocator);
    defer parameters.deinit();

    var to = utils.delimiterIndex(tokens, 0, .Comma) catch tokens.len;
    var prev: usize = 0;

    while (prev < tokens.len) {
        const paramTokens = tokens[prev..to];
        const nameToken = paramTokens[0];

        if (nameToken.type != .Identifier) return astError(AstError.TokenNotFound, "identifier");

        const typeTokens = tokens[prev + 2 .. to];
        const parameterType = try createTypeNode(allocator, compInfo, typeTokens);

        try parameters.append(.{
            .name = try string.cloneString(allocator, nameToken.string.?),
            .type = parameterType,
        });

        prev = to + 1;
        to = utils.delimiterIndex(tokens, to + 1, .Comma) catch tokens.len;
    }

    const slice = try allocator.dupe(Parameter, parameters.items);
    return slice;
}

fn createBoolNode(allocator: Allocator, value: bool) !*const AstNode {
    const node = try create(AstNode, allocator, .{
        .Value = .{
            .Bool = value,
        },
    });
    return node;
}

fn parseGenerics(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) ![]GenericType {
    var genericList = ArrayList(GenericType).init(allocator);
    defer genericList.deinit();

    var to = utils.delimiterIndex(tokens, 0, .Comma) catch tokens.len;
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
        } else if (typeTokens[0].type != .Identifier) {
            return astError(AstError.UnexpectedToken, typeTokens[0].type.toString());
        } else {
            structGeneric.name = try string.cloneString(allocator, typeTokens[0].string.?);
        }

        if (hasRestriction) {
            const restrictionTokens = tokens[colonIndex + 1 .. to];
            const typeNode = try createTypeNode(allocator, compInfo, restrictionTokens);
            structGeneric.restriction = typeNode;
        }

        try genericList.append(structGeneric);

        prev = to + 1;
        to = utils.delimiterIndex(tokens, to + 1, .Comma) catch tokens.len;
    }

    const slice = allocator.dupe(GenericType, genericList.items);
    return slice;
}

fn createVarDecNode(
    allocator: Allocator,
    compInfo: *CompInfo,
    tokens: []tokenizer.Token,
    isConst: bool,
) (AstError || Allocator.Error)!AstNodeOffsetData {
    const name = tokens[1];
    const hasType = switch (tokens[2].type) {
        .EqSet => false,
        .Colon => true,
        else => return AstError.UnexpectedToken,
    };
    var annotation: ?*const AstTypes = null;
    var offset: usize = 3;

    if (hasType) {
        const searchStart = offset;
        const index = utils.delimiterIndex(tokens, searchStart, .EqSet) catch |e| return astError(e, "=");

        const typeTokens = tokens[searchStart..index];
        annotation = try createTypeNode(allocator, compInfo, typeTokens);

        offset += index - searchStart + 1;
    }

    const setTokens = tokens[offset..];
    const setNode = try createAstNode(allocator, compInfo, setTokens);
    const tokenOffset = offset + setNode.offset;

    if (name.type != .Identifier) return AstError.UnexpectedToken;
    const node = try create(AstNode, allocator, .{
        .VarDec = VarDecNode{
            .name = try string.cloneString(allocator, name.string.?),
            .isConst = isConst,
            .setNode = setNode.node,
            .annotation = annotation,
        },
    });

    return .{ .node = node, .offset = tokenOffset };
}

fn parseGenericArgs(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token, start: usize) ![]*const AstTypes {
    var typeGenericsArr = ArrayList(*const AstTypes).init(allocator);
    defer typeGenericsArr.deinit();

    if (start + 2 < tokens.len) {
        const tokensToRAngle = utils.smartDelimiterIndex(tokens, compInfo, start + 2, .RAngle) catch |e| return astError(e, ">");
        var typeEnd = utils.smartDelimiterIndex(tokens, compInfo, start + 2, .Comma) catch tokensToRAngle;
        var prev = start + 2;

        while (prev < tokensToRAngle) {
            const typeTokens = tokens[prev..typeEnd];
            const typeNode = try createTypeNode(allocator, compInfo, typeTokens);
            try typeGenericsArr.append(typeNode);

            prev = typeEnd + 1;
            typeEnd = utils.smartDelimiterIndex(tokens, compInfo, typeEnd + 1, .Comma) catch tokensToRAngle;
        }
    } else {
        return AstError.ExpectedGenericArgument;
    }

    return try allocator.dupe(*const AstTypes, typeGenericsArr.items);
}

fn createTypeNode(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) (AstError || Allocator.Error)!*const AstTypes {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == .QuestionMark;
    var current = @as(u32, if (nullable) 1 else 0);
    var res: *const AstTypes = undefined;

    if (tokens[current].type == .Identifier) {
        const tokenString = tokens[current].string.?;

        if (compInfo.hasRegGeneric(tokenString)) {
            res = try create(AstTypes, allocator, AstTypes{
                .Generic = try string.cloneString(allocator, tokenString),
            });
        } else if (compInfo.hasStruct(tokenString)) {
            var customType = CustomType{
                .generics = &[_]*const AstTypes{},
                .name = try string.cloneString(allocator, tokenString),
            };

            if (tokens.len > 1 and tokens[1].type == .LAngle) {
                customType.generics = try parseGenericArgs(allocator, compInfo, tokens, current);
            }

            res = try create(AstTypes, allocator, .{
                .Custom = customType,
            });
        } else {
            return astError(AstError.UnknownType, tokenString);
        }
    } else {
        res = try createAstType(allocator, compInfo, tokens[current]);
    }

    while (current + 1 < tokens.len and tokens[current + 1].type == .LBracket) : (current += 1) {
        // check for (type)[]
        if (tokens[current + 2].type == .RBracket) {
            res = try create(AstTypes, allocator, .{
                .DynamicArray = res,
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
        res = try create(AstTypes, allocator, .{
            .Nullable = res,
        });
    }

    return res;
}

fn createAstType(allocator: Allocator, compInfo: *CompInfo, token: tokenizer.Token) !*const AstTypes {
    const val = switch (token.type) {
        .Bool => AstTypes.Bool,
        .StringType => AstTypes.String,
        .U8 => AstTypes{ .Number = AstNumberVariants.U8 },
        .U16 => AstTypes{ .Number = AstNumberVariants.U16 },
        .U32 => AstTypes{ .Number = AstNumberVariants.U32 },
        .U64 => AstTypes{ .Number = AstNumberVariants.U64 },
        .U128 => AstTypes{ .Number = AstNumberVariants.U128 },
        .I8 => AstTypes{ .Number = AstNumberVariants.I8 },
        .I16 => AstTypes{ .Number = AstNumberVariants.I16 },
        .I32 => AstTypes{ .Number = AstNumberVariants.I32 },
        .I64 => AstTypes{ .Number = AstNumberVariants.I64 },
        .I128 => AstTypes{ .Number = AstNumberVariants.I128 },
        .F8 => AstTypes{ .Number = AstNumberVariants.F8 },
        .F16 => AstTypes{ .Number = AstNumberVariants.F16 },
        .F32 => AstTypes{ .Number = AstNumberVariants.F32 },
        .F64 => AstTypes{ .Number = AstNumberVariants.F64 },
        .F128 => AstTypes{ .Number = AstNumberVariants.F128 },
        .USize => AstTypes{ .Number = AstNumberVariants.USize },
        .CharType => AstTypes.Char,
        else => a: {
            if (token.string) |tokenString| {
                if (token.type == .Identifier and compInfo.hasRegGeneric(tokenString)) {
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

pub fn findStructNames(allocator: Allocator, tokens: []tokenizer.Token) ![][]u8 {
    var names = ArrayList([]u8).init(allocator);
    defer names.deinit();

    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].type != .Struct) continue;

        if (tokens[i + 1].type == .LBracket) {
            const rBracket = try utils.delimiterIndex(tokens, i + 2, .RBracket);
            i = rBracket;
        }

        if (tokens[i + 1].type == .Identifier) {
            const str = try string.cloneString(allocator, tokens[i + 1].string.?);
            try names.append(str);
        } else return AstError.ExpectedNameForStruct;
    }

    return allocator.dupe([]u8, names.items);
}

pub fn registerStructs(allocator: Allocator, compInfo: *CompInfo, tokens: []tokenizer.Token) ![](*StructDecNode) {
    var arr = ArrayList(*StructDecNode).init(allocator);
    defer arr.deinit();

    var i: usize = 0;
    while (i < tokens.len) {
        if (tokens[i].type != .Struct) {
            i += 1;
            continue;
        }

        const structTokens = tokens[i..];
        const node = try createAstNode(allocator, compInfo, structTokens);
        try arr.append(node.node.*.StructDec);
        allocator.destroy(node.node);

        i += node.offset;
    }

    return try arr.toOwnedSlice();
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
        AstError.ExpectedIdentifierPropertyAccess => "expected identifier for struct property access",
        AstError.ExpectedIdentifierPropertyAccessSource => "expected identifier for property access source",
        AstError.ExpectedStructDeriveType => "expected struct derive type",
        AstError.ExpectedIdentifierForDerivedType => "expected identifier for derive type",
        AstError.UndefinedStruct => "undefined struct",
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
