const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const free = blitz.free;
const string = blitz.string;
const scanner = blitz.scanner;
const CompInfo = utils.CompInfo;
const create = utils.create;
const createMut = utils.createMut;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const TokenUtil = utils.TokenUtil;

// debug
const debug = @import("debug.zig");
const printTokens = debug.printTokens;
const printToken = debug.printToken;
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
    F32,
    F64,
    F128,
};

pub const AstNumber = union(AstNumberVariants) {
    const Self = @This();

    U8: u8,
    U16: u16,
    U32: u32,
    U64: u64,
    U128: u128,
    USize: usize,
    I8: i8,
    I16: i16,
    I32: i32,
    I64: i64,
    I128: i128,
    F32: f32,
    F64: f64,
    F128: f128,

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
            .USize => "usize",
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .I128 => "i128",
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
        };
    }

    pub fn toType(self: Self) AstNumberVariants {
        return switch (self) {
            .U8 => .U8,
            .U16 => .U16,
            .U32 => .U32,
            .U64 => .U64,
            .U128 => .U128,
            .USize => .USize,
            .I8 => .I8,
            .I16 => .I16,
            .I32 => .I32,
            .I64 => .I64,
            .I128 => .I128,
            .F32 => .F32,
            .F64 => .F64,
            .F128 => .F128,
        };
    }
};

pub const AstStaticArrayType = struct {
    type: *const AstTypes,
    size: *const AstNode,
};

pub const CustomType = struct {
    name: []u8,
    generics: []*const AstTypes,
};

const ErrorVariantType = struct {
    from: []u8,
    variant: []u8,
};

const ErrorAstType = struct {
    name: []u8,
    payload: ?*const AstTypes,
};

const Types = enum {
    String,
    Bool,
    Char,
    Void,
    Null,
    Number,
    RawNumber,
    DynamicArray,
    StaticArray,
    GeneralArray,
    Nullable,
    Custom,
    Generic,
    Function,
    StaticStructInstance,
    Error,
    ErrorVariant,
};

pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Void,
    Null,
    Number: AstNumberVariants,
    RawNumber,
    DynamicArray: *const AstTypes,
    StaticArray: AstStaticArrayType,
    GeneralArray: AstStaticArrayType,
    Nullable: *const AstTypes,
    Custom: CustomType,
    Generic: []u8,
    Function: *FuncDecNode,
    StaticStructInstance: []u8,
    Error: ErrorAstType,
    ErrorVariant: ErrorVariantType,
};

const StaticTypes = enum {
    String,
    Bool,
    Char,
    Number,
    RawNumber,
    GeneralArray,
    Null,
};

pub const AstValues = union(StaticTypes) {
    String: []u8,
    Bool: bool,
    Char: u8,
    Number: AstNumber,
    RawNumber: []u8,
    GeneralArray: []*const AstNode,
    Null,
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

pub const IfFallback = struct {
    condition: ?*const AstNode,
    body: *const AstNode,
    fallback: ?*const IfFallback,
};

const IfStatementNode = struct {
    condition: *const AstNode,
    body: *const AstNode,
    fallback: ?*const IfFallback,
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
    capturedValues: ?*utils.CaptureScope,
};

const FuncCallNode = struct {
    func: *const AstNode,
    params: []*AstNode,
};

const PropertyAccess = struct {
    value: *AstNode,
    property: []u8,
};

pub const OpExprTypes = enum {
    BitAnd,
    BitOr,
    And,
    Or,
    Add,
    Sub,
    Mult,
    Div,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
};

const OpExpr = struct {
    type: OpExprTypes,
    left: *AstNode,
    right: *AstNode,
};

const IndexValueNode = struct {
    index: *const AstNode,
    value: *const AstNode,
};

pub const ErrorDecNode = struct {
    name: []u8,
    variants: ?[][]u8,
};

const VarSetNode = struct {
    variable: []u8,
    setNode: *AstNode,
};

const EqOpTypes = enum {
    AddEq,
    SubEq,
    MultEq,
    DivEq,
    BitAndEq,
    BitOrEq,
    AndEq,
    OrEq,
};

const VarEqOpNode = struct {
    opType: EqOpTypes,
    value: *AstNode,
    variable: []u8,
};

const ForLoopNode = struct {
    initNode: ?*const AstNode,
    condition: *const AstNode,
    incNode: *const AstNode,
    body: *const AstNode,
};

const WhileLoopNode = struct {
    condition: *const AstNode,
    body: *const AstNode,
};

const AstNodeVariants = enum {
    NoOp,
    StructPlaceholder,
    Seq,
    VarDec,
    VarSet,
    VarEqOp,
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
    OpExpr,
    IndexValue,
    ErrorDec,
    Error,
    Group,
    Scope,
    IncOne,
    DecOne,
    ForLoop,
    WhileLoop,
};
pub const AstNode = union(AstNodeVariants) {
    NoOp,
    StructPlaceholder,
    Seq: SeqNode,
    VarDec: VarDecNode,
    VarSet: VarSetNode,
    VarEqOp: VarEqOpNode,
    Type: AstTypes,
    Value: AstValues,
    Cast: CastNode,
    Variable: []u8,
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
    OpExpr: OpExpr,
    IndexValue: IndexValueNode,
    ErrorDec: *const ErrorDecNode,
    Error: []u8,
    Group: *const AstNode,
    Scope: *const AstNode,
    IncOne: *const AstNode,
    DecOne: *const AstNode,
    ForLoop: ForLoopNode,
    WhileLoop: WhileLoopNode,
};

pub const AstError = error{
    UnexpectedToken,
    InvalidExprOperand,
    ExpectedTokenFoundNothing,
    ExpectedExpression,
    ExpectedIdentifierForVariableName,
    ExpectedIdentifierForFunctionName,
    ExpectedIdentifierForParameterName,
    ExpectedIdentifierForGenericType,
    ExpectedIdentifierForErrorName,
    ExpectedIdentifierForPropertyAccess,
    ExpectedIdentifierForErrorVariant,
    ExpectedIdentifierForStructName,
    ExpectedStructDeriveType,
    ExpectedIdentifierForDeriveType,
    ExpectedNameForError,
    ExpectedNameForStruct,
    ExpectedNameForFunction,
    ExpectedSizeForStaticArray,
    ExpectedIdentifierForStructProperty,
    ExpectedValueForStructProperty,
    ExpectedIdentifierPropertyAccessSource,
    UnexpectedGenericOnErrorType,
    ExpectedTypeExpression,
    ErrorPayloadMayNotBeError,
};

const RegisterStructsAndErrorsResult = struct {
    structs: []*StructDecNode,
    errors: []*const ErrorDecNode,
};

pub const HoistedNames = struct {
    structNames: [][]u8,
    errorNames: [][]u8,
};

const OpExprTokenMap = struct {
    type: OpExprTypes,
    token: tokenizer.TokenType,
};

pub const Ast = struct {
    const Self = @This();

    root: *const AstNode,
    allocator: Allocator,

    pub fn init(allocator: Allocator, root: *const AstNode) Self {
        return Self{
            .root = root,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        free.freeNode(self.allocator, self.root);
    }
};

const opPrecedence = [_]OpExprTypes{
    .BitAnd,
    .BitOr,
    .And,
    .Or,
    .Mult,
    .Div,
    .Add,
    .Sub,
};

pub fn createAst(allocator: Allocator, compInfo: *CompInfo) !Ast {
    const seq = try parseSequence(allocator, compInfo);
    return Ast.init(allocator, seq);
}

pub fn parseSequence(allocator: Allocator, compInfo: *CompInfo) (AstError || Allocator.Error)!*const AstNode {
    var seq = ArrayList(*const AstNode).init(allocator);
    defer seq.deinit();

    while (compInfo.tokens.hasNext()) {
        const peakToken = try compInfo.tokens.peakFixed();
        if (peakToken.type == .Semicolon or peakToken.type == .NewLine) {
            _ = try compInfo.tokens.takeFixed();
            continue;
        }

        if (peakToken.type == .RBrace) {
            break;
        }

        const node = try parseStatement(allocator, compInfo);
        if (node == null) break;

        try seq.append(node.?);
    }

    return try create(AstNode, allocator, .{
        .Seq = .{
            .nodes = try seq.toOwnedSlice(),
        },
    });
}

fn parseStatement(allocator: Allocator, compInfo: *CompInfo) (AstError || Allocator.Error)!?*AstNode {
    const first = try compInfo.tokens.take();
    switch (first.type) {
        .Const => return try createVarDecNode(allocator, compInfo, true),
        .Var => return try createVarDecNode(allocator, compInfo, false),
        .If => {
            try compInfo.tokens.expectToken(.LParen);
            const condition = try parseExpression(allocator, compInfo);
            if (condition == null) return compInfo.logger.logError(AstError.ExpectedExpression);
            try compInfo.tokens.expectToken(.RParen);

            try compInfo.tokens.expectToken(.LBrace);
            const seq = try parseSequence(allocator, compInfo);

            const fallback = try parseIfChain(allocator, compInfo);

            return try createMut(AstNode, allocator, .{
                .IfStatement = .{
                    .condition = condition.?,
                    .body = seq,
                    .fallback = fallback,
                },
            });
        },
        .Fn => {
            try compInfo.pushRegGenScope();
            defer compInfo.popRegGenScope();

            const func = try parseFuncDef(allocator, compInfo);
            try compInfo.addFunction(func.name, func);

            return try createMut(AstNode, allocator, .{
                .FuncDec = try string.cloneString(allocator, func.name),
            });
        },
        .For => {
            try compInfo.tokens.expectToken(.LParen);

            var initNode: ?*const AstNode = null;

            const next = try compInfo.tokens.peak();
            if (next.type != .Semicolon) {
                initNode = try parseStatement(allocator, compInfo);
            }

            try compInfo.tokens.expectToken(.Semicolon);

            const condition = try parseExpression(allocator, compInfo);
            if (condition == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            try compInfo.tokens.expectToken(.Semicolon);

            const incNode = try parseExpression(allocator, compInfo);
            if (incNode == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            try compInfo.tokens.expectToken(.RParen);
            try compInfo.tokens.expectToken(.LBrace);

            const body = try parseSequence(allocator, compInfo);

            return try createMut(AstNode, allocator, .{
                .ForLoop = .{
                    .initNode = initNode,
                    .condition = condition.?,
                    .incNode = incNode.?,
                    .body = body,
                },
            });
        },
        .While => {
            try compInfo.tokens.expectToken(.LParen);

            const condition = try parseExpression(allocator, compInfo);
            if (condition == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            try compInfo.tokens.expectToken(.RParen);
            try compInfo.tokens.expectToken(.LBrace);

            const body = try parseSequence(allocator, compInfo);

            return try createMut(AstNode, allocator, .{
                .WhileLoop = .{
                    .condition = condition.?,
                    .body = body,
                },
            });
        },
        .Identifier => {
            const next = try compInfo.tokens.take();
            switch (next.type) {
                .EqSet => {
                    const setNode = try parseExpression(allocator, compInfo);
                    if (setNode == null) return compInfo.logger.logError(AstError.ExpectedExpression);

                    return try createMut(AstNode, allocator, .{
                        .VarSet = .{
                            .setNode = setNode.?,
                            .variable = try string.cloneString(allocator, first.string.?),
                        },
                    });
                },
                .AddEq => {
                    const incNode = try parseExpression(allocator, compInfo);
                    if (incNode == null) return compInfo.logger.logError(AstError.ExpectedExpression);

                    return try createMut(AstNode, allocator, .{
                        .VarEqOp = .{
                            .opType = .AddEq,
                            .value = incNode.?,
                            .variable = try string.cloneString(allocator, first.string.?),
                        },
                    });
                },
                .LParen => {
                    return try parseFuncCall(allocator, compInfo, first.string.?);
                },
                .Period => {
                    compInfo.tokens.returnToken();
                    compInfo.tokens.returnToken();
                    return parsePropertyAccess(allocator, compInfo);
                },
                else => return try createMut(AstNode, allocator, .{
                    .Variable = try string.cloneString(allocator, first.string.?),
                }),
            }
        },
        .Return => {
            const value = try parseExpression(allocator, compInfo);
            if (value == null) return compInfo.logger.logError(AstError.ExpectedExpression);
            try compInfo.tokens.expectToken(.Semicolon);

            return try createMut(AstNode, allocator, .{
                .ReturnNode = value.?,
            });
        },
        .Error => {
            if (!compInfo.preAst) {
                _ = try compInfo.tokens.take();
                var next = try compInfo.tokens.take();
                if (next.type == .LBrace) {
                    while (next.type != .RBrace) {
                        next = try compInfo.tokens.take();
                    }
                } else if (next.type != .Semicolon) {
                    return compInfo.logger.logError(AstError.UnexpectedToken);
                }

                return try createMut(AstNode, allocator, .NoOp);
            }

            const errNode = try parseError(allocator, compInfo);

            return try createMut(AstNode, allocator, .{
                .ErrorDec = errNode,
            });
        },
        .Struct => {
            if (!compInfo.preAst) {
                var parens: usize = 0;

                var current = try compInfo.tokens.take();
                while (parens > 1 or current.type != .RBrace) {
                    if (current.isOpenToken(false)) {
                        parens += 1;
                    } else if (current.isCloseToken(false)) {
                        parens -= 1;
                    }

                    current = try compInfo.tokens.take();
                }

                return try createMut(AstNode, allocator, .StructPlaceholder);
            }

            return try parseStruct(allocator, compInfo);
        },
        .LBrace => {
            const seq = try parseSequence(allocator, compInfo);
            try compInfo.tokens.expectToken(.RBrace);
            return try createMut(AstNode, allocator, .{
                .Scope = seq,
            });
        },
        else => return compInfo.logger.logError(AstError.UnexpectedToken),
    }
}

fn parseIfChain(allocator: Allocator, compInfo: *CompInfo) !?*const IfFallback {
    if (!compInfo.tokens.hasNext()) return null;

    const next = try compInfo.tokens.peak();
    if (next.type != .Else) return null;
    _ = try compInfo.tokens.take();

    var condition: ?*const AstNode = null;

    const nextNext = try compInfo.tokens.peak();
    if (nextNext.type == .If) {
        _ = try compInfo.tokens.take();
        try compInfo.tokens.expectToken(.LParen);

        condition = try parseExpression(allocator, compInfo);
        if (condition == null) {
            return compInfo.logger.logError(AstError.ExpectedExpression);
        }

        try compInfo.tokens.expectToken(.RParen);
    }

    try compInfo.tokens.expectToken(.LBrace);
    const body = try parseSequence(allocator, compInfo);
    const fallback = try parseIfChain(allocator, compInfo);

    return try create(IfFallback, allocator, .{
        .condition = condition,
        .body = body,
        .fallback = fallback,
    });
}

fn parseStruct(allocator: Allocator, compInfo: *CompInfo) !?*AstNode {
    var deriveType: ?*const AstTypes = null;
    var generics: []GenericType = &[_]GenericType{};

    var first = try compInfo.tokens.take();
    if (first.type == .LBracket) {
        generics = try parseGenerics(allocator, compInfo);
        first = try compInfo.tokens.take();
    } else if (first.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForStructName);
    }

    for (generics) |generic| {
        try compInfo.addAvailableGeneric(generic.name);
    }

    var next = try compInfo.tokens.take();

    if (next.type == .Colon) {
        next = try compInfo.tokens.peak();

        if (next.type != .Identifier) {
            return compInfo.logger.logError(AstError.ExpectedIdentifierForDeriveType);
        }

        if (!compInfo.hasStruct(next.string.?)) {
            return compInfo.logger.logError(AstError.ExpectedStructDeriveType);
        }

        deriveType = try parseType(allocator, compInfo);
        try compInfo.tokens.expectToken(.LBrace);
    } else if (next.type != .LBrace) {
        return compInfo.logger.logError(AstError.UnexpectedToken);
    }

    const attributes = try parseStructAttributes(allocator, compInfo);

    for (generics) |generic| {
        compInfo.removeAvailableGeneric(generic.name);
    }

    return try createMut(AstNode, allocator, .{
        .StructDec = try createMut(StructDecNode, allocator, .{
            .name = try string.cloneString(allocator, first.string.?),
            .generics = generics,
            .attributes = attributes,
            .totalMemberList = &[_]StructAttribute{},
            .deriveType = deriveType,
        }),
    });
}

fn parseStructAttributes(allocator: Allocator, compInfo: *CompInfo) ![]StructAttribute {
    var attributes = ArrayList(StructAttribute).init(allocator);
    defer attributes.deinit();

    while ((try compInfo.tokens.peak()).type != .RBrace) {
        const attr = try parseStructAttribute(allocator, compInfo);
        try attributes.append(attr);

        if (attr.attr == .Member) {
            try compInfo.tokens.expectToken(.Semicolon);
        }
    }

    _ = try compInfo.tokens.take();

    return attributes.toOwnedSlice();
}

fn parseStructAttribute(allocator: Allocator, compInfo: *CompInfo) !StructAttribute {
    const first = try compInfo.tokens.take();
    switch (first.type) {
        .Identifier, .Fn => {
            compInfo.tokens.returnToken();
            return parseStructAttributeUtil(allocator, compInfo, .Private);
        },
        .Prot => return parseStructAttributeUtil(allocator, compInfo, .Protected),
        .Pub => return parseStructAttributeUtil(allocator, compInfo, .Public),
        else => {
            return compInfo.logger.logError(AstError.UnexpectedToken);
        },
    }
}

fn parseStructAttributeUtil(allocator: Allocator, compInfo: *CompInfo, visibility: MemberVisibility) !StructAttribute {
    var first = try compInfo.tokens.take();
    var static = false;

    if (first.type == .Static) {
        static = true;
        first = try compInfo.tokens.take();
    }

    switch (first.type) {
        .Identifier => {
            try compInfo.tokens.expectToken(.Colon);
            const attrType = try parseType(allocator, compInfo);

            return .{
                .name = try string.cloneString(allocator, first.string.?),
                .attr = .{
                    .Member = attrType,
                },
                .visibility = visibility,
                .static = static,
            };
        },
        .Fn => {
            const def = try parseFuncDef(allocator, compInfo);

            return .{
                .name = try string.cloneString(allocator, def.name),
                .attr = .{
                    .Function = def,
                },
                .visibility = visibility,
                .static = static,
            };
        },
        else => {
            return compInfo.logger.logError(AstError.UnexpectedToken);
        },
    }
}

fn parseError(allocator: Allocator, compInfo: *CompInfo) !*const ErrorDecNode {
    const name = try compInfo.tokens.take();
    if (name.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForErrorName);
    }

    var variants: ?[][]u8 = null;

    const next = try compInfo.tokens.take();
    if (next.type == .LBrace) {
        variants = try parseVariants(allocator, compInfo);
    } else if (next.type != .Semicolon) {
        return compInfo.logger.logError(AstError.UnexpectedToken);
    }

    return try create(ErrorDecNode, allocator, .{
        .name = try string.cloneString(allocator, name.string.?),
        .variants = variants,
    });
}

fn parseVariants(allocator: Allocator, compInfo: *CompInfo) ![][]u8 {
    var variants = ArrayList([]u8).init(allocator);
    defer variants.deinit();

    var variant = try compInfo.tokens.take();
    while (variant.type == .Identifier) {
        const comma = try compInfo.tokens.take();
        if (comma.type != .RBrace and comma.type != .Comma) {
            return compInfo.logger.logError(AstError.UnexpectedToken);
        }

        const variantClone = try string.cloneString(allocator, variant.string.?);
        try variants.append(variantClone);

        if (comma.type == .RBrace) break;
        variant = try compInfo.tokens.take();
    }

    return variants.toOwnedSlice();
}

fn parseExpression(allocator: Allocator, compInfo: *CompInfo) !?*AstNode {
    const expr = try parseExpressionUtil(allocator, compInfo);
    const next = try compInfo.tokens.peak();
    switch (next.type) {
        .BitAnd,
        .BitOr,
        .And,
        .Or,
        .Add,
        .Sub,
        .Mult,
        .Div,
        .Mod,
        .LAngle,
        .RAngle,
        => {
            if (expr == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            _ = try compInfo.tokens.take();

            const after = try parseExpression(allocator, compInfo);
            if (after == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            return try createMut(AstNode, allocator, .{
                .OpExpr = .{
                    .type = tokenTypeToOpType(next.type),
                    .left = expr.?,
                    .right = after.?,
                },
            });
        },
        .Inc => {
            _ = try compInfo.tokens.take();
            return try createMut(AstNode, allocator, .{
                .IncOne = expr.?,
            });
        },
        .Dec => {
            _ = try compInfo.tokens.take();
            return try createMut(AstNode, allocator, .{
                .DecOne = expr.?,
            });
        },
        else => {},
    }

    if (expr) |node| {
        if (node.* == .OpExpr) {
            return rotatePrecedence(node);
        }
    }

    return expr;
}

fn parseExpressionUtil(allocator: Allocator, compInfo: *CompInfo) (Allocator.Error || AstError)!?*AstNode {
    const first = try compInfo.tokens.take();
    switch (first.type) {
        .Null => return try createMut(AstNode, allocator, .{
            .Value = .Null,
        }),
        .Number, .NegNumber => return try createMut(AstNode, allocator, .{
            .Value = .{
                .RawNumber = try string.cloneString(allocator, first.string.?),
            },
        }),
        .StringToken => {
            const str = first.string.?;
            return try createMut(AstNode, allocator, .{
                .Value = .{
                    .String = try string.cloneString(allocator, str),
                },
            });
        },
        .CharToken => return try createMut(AstNode, allocator, .{
            .Value = .{
                .Char = first.string.?[0],
            },
        }),
        .Bang => {
            const expr = try parseExpression(allocator, compInfo);
            if (expr == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            return try createMut(AstNode, allocator, .{
                .Bang = expr.?,
            });
        },
        .LParen => {
            const expr = try parseExpression(allocator, compInfo);
            if (expr == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }

            try compInfo.tokens.expectToken(.RParen);

            return try createMut(AstNode, allocator, .{
                .Group = expr.?,
            });
        },
        .Identifier => {
            const next = try compInfo.tokens.peak();
            switch (next.type) {
                .LParen => {
                    _ = try compInfo.tokens.take();

                    return try parseFuncCall(allocator, compInfo, first.string.?);
                },
                .Period => {
                    compInfo.tokens.returnToken();
                    return try parsePropertyAccess(allocator, compInfo);
                },
                .LBrace, .LAngle => {
                    if (compInfo.hasStruct(first.string.?)) {
                        return try parseStructInit(allocator, compInfo, first.string.?);
                    }
                },
                .LBracket => {
                    _ = try compInfo.tokens.take();
                    const index = try parseExpression(allocator, compInfo);
                    if (index == null) {
                        return compInfo.logger.logError(AstError.ExpectedExpression);
                    }
                    try compInfo.tokens.expectToken(.RBracket);

                    return try createMut(AstNode, allocator, .{
                        .IndexValue = .{
                            .index = index.?,
                            .value = try getIdentNode(allocator, compInfo, first.string.?),
                        },
                    });
                },
                else => {},
            }

            return try getIdentNode(allocator, compInfo, first.string.?);
        },
        .LBracket => return parseArray(allocator, compInfo),
        .True => return try createBoolNode(allocator, true),
        .False => return try createBoolNode(allocator, false),
        .U8,
        .U16,
        .U32,
        .U64,
        .U128,
        .F32,
        .F64,
        .F128,
        .I8,
        .I16,
        .I32,
        .I64,
        .I128,
        .USize,
        .StringType,
        .CharType,
        .Bool,
        => {
            compInfo.tokens.returnToken();
            const toType = try parseType(allocator, compInfo);
            try compInfo.tokens.expectToken(.LParen);
            const inner = try parseExpression(allocator, compInfo);
            if (inner == null) {
                return compInfo.logger.logError(AstError.ExpectedExpression);
            }
            try compInfo.tokens.expectToken(.RParen);

            return try createMut(AstNode, allocator, .{
                .Cast = .{
                    .node = inner.?,
                    .toType = toType,
                },
            });
        },
        else => return compInfo.logger.logError(AstError.UnexpectedToken),
    }
}

fn getIdentNode(allocator: Allocator, compInfo: *CompInfo, str: []u8) !*AstNode {
    const newStr = try string.cloneString(allocator, str);
    var node: AstNode = undefined;

    if (compInfo.hasStruct(newStr)) {
        node = .{ .StaticStructInstance = newStr };
    } else if (compInfo.hasError(newStr)) {
        node = .{ .Error = newStr };
    } else {
        node = .{ .Variable = newStr };
    }

    return try createMut(AstNode, allocator, node);
}

fn parseArray(allocator: Allocator, compInfo: *CompInfo) !*AstNode {
    var items = ArrayList(*const AstNode).init(allocator);
    defer items.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RBracket) {
        const item = try parseExpression(allocator, compInfo);
        if (item == null) {
            return compInfo.logger.logError(AstError.ExpectedExpression);
        }

        try items.append(item.?);

        current = try compInfo.tokens.take();
        if (current.type == .RBracket) break;

        compInfo.tokens.returnToken();
        try compInfo.tokens.expectToken(.Comma);
        current = try compInfo.tokens.peak();
    }

    return try createMut(AstNode, allocator, .{
        .Value = .{
            .GeneralArray = try items.toOwnedSlice(),
        },
    });
}

fn parseStructInit(allocator: Allocator, compInfo: *CompInfo, name: []u8) !*AstNode {
    const next = try compInfo.tokens.take();
    var generics: []*const AstTypes = &[_]*const AstTypes{};

    if (next.type == .LAngle) {
        generics = try parseStructInitGenerics(allocator, compInfo);
        _ = try compInfo.tokens.take();
    } else if (next.type != .LBrace) {
        return compInfo.logger.logError(AstError.UnexpectedToken);
    }

    const attributes = try parseStructInitAttributes(allocator, compInfo);

    return try createMut(AstNode, allocator, .{
        .StructInit = .{
            .name = try string.cloneString(allocator, name),
            .attributes = attributes,
            .generics = generics,
        },
    });
}

fn parseStructInitAttributes(allocator: Allocator, compInfo: *CompInfo) ![]AttributeDefinition {
    var attributes = ArrayList(AttributeDefinition).init(allocator);
    defer attributes.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RBrace) {
        const param = try parseStructInitAttribute(allocator, compInfo);
        try attributes.append(param);

        current = try compInfo.tokens.take();
        if (current.type == .RBrace) break;

        compInfo.tokens.returnToken();
        try compInfo.tokens.expectToken(.Comma);
        current = try compInfo.tokens.peak();
    }

    return attributes.toOwnedSlice();
}

fn parseStructInitAttribute(allocator: Allocator, compInfo: *CompInfo) !AttributeDefinition {
    const first = try compInfo.tokens.take();
    if (first.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForStructProperty);
    }

    const next = try compInfo.tokens.take();
    if (next.type == .Comma or next.type == .RBrace) {
        const res = AttributeDefinition{
            .name = try string.cloneString(allocator, first.string.?),
            .value = try create(AstNode, allocator, .{
                .Variable = try string.cloneString(allocator, first.string.?),
            }),
        };

        compInfo.tokens.returnToken();
        return res;
    } else if (next.type != .EqSet) {
        return compInfo.logger.logError(AstError.UnexpectedToken);
    }

    const eqNode = try parseExpression(allocator, compInfo);
    if (eqNode == null) {
        return compInfo.logger.logError(AstError.ExpectedValueForStructProperty);
    }

    return .{
        .name = try string.cloneString(allocator, first.string.?),
        .value = eqNode.?,
    };
}

fn parseStructInitGenerics(allocator: Allocator, compInfo: *CompInfo) ![]*const AstTypes {
    var generics = ArrayList(*const AstTypes).init(allocator);
    defer generics.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RAngle) {
        const genType = try parseType(allocator, compInfo);
        try generics.append(genType);

        current = try compInfo.tokens.peak();
        if (current.type == .RAngle) break;

        try compInfo.tokens.expectToken(.Comma);
    }

    try compInfo.tokens.expectToken(.RAngle);

    return generics.toOwnedSlice();
}

fn parsePropertyAccess(allocator: Allocator, compInfo: *CompInfo) !*AstNode {
    const first = try compInfo.tokens.take();
    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForPropertyAccess;
    }
    const source = try getIdentNode(allocator, compInfo, first.string.?);
    return try parsePropertyAccessUtil(allocator, compInfo, source);
}

fn parsePropertyAccessUtil(allocator: Allocator, compInfo: *CompInfo, node: *AstNode) !*AstNode {
    try compInfo.tokens.expectToken(.Period);

    const ident = try compInfo.tokens.take();
    if (ident.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForPropertyAccess);
    }

    var access = try createMut(AstNode, allocator, .{
        .PropertyAccess = .{
            .value = node,
            .property = try string.cloneString(allocator, ident.string.?),
        },
    });

    const next = try compInfo.tokens.take();
    if (next.type == .LParen) {
        const params = try parseFuncCallParams(allocator, compInfo);
        access = try createMut(AstNode, allocator, .{
            .FuncCall = .{
                .func = access,
                .params = params,
            },
        });
    } else if (next.type == .LBracket) {
        const expr = try parseExpression(allocator, compInfo);
        if (expr == null) {
            return compInfo.logger.logError(AstError.ExpectedExpression);
        }

        access = try createMut(AstNode, allocator, .{
            .IndexValue = .{
                .index = expr.?,
                .value = access,
            },
        });

        try compInfo.tokens.expectToken(.RBracket);
    } else {
        compInfo.tokens.returnToken();
    }

    if ((try compInfo.tokens.peak()).type == .Period) {
        return parsePropertyAccessUtil(allocator, compInfo, access);
    }

    return access;
}

/// name expected to be cloned
fn parseFuncCall(allocator: Allocator, compInfo: *CompInfo, name: []u8) !*AstNode {
    const func = try create(AstNode, allocator, .{
        .FuncReference = try string.cloneString(allocator, name),
    });

    const params = try parseFuncCallParams(allocator, compInfo);

    return try createMut(AstNode, allocator, .{
        .FuncCall = .{
            .func = func,
            .params = params,
        },
    });
}

fn parseFuncDef(allocator: Allocator, compInfo: *CompInfo) !*FuncDecNode {
    var next = try compInfo.tokens.take();
    var nameStr: []u8 = undefined;
    var generics: ?[]GenericType = null;

    if (next.type == .LBracket) {
        generics = try parseGenerics(allocator, compInfo);
        next = try compInfo.tokens.take();
    }

    if (next.type == .Identifier) {
        nameStr = try string.cloneString(allocator, next.string.?);
    } else {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForFunctionName);
    }

    try compInfo.tokens.expectToken(.LParen);

    const params = try parseParams(allocator, compInfo);
    var returnType: *const AstTypes = undefined;

    const retNext = try compInfo.tokens.peak();

    if (retNext.type == .Colon) {
        _ = try compInfo.tokens.take();
        returnType = try parseType(allocator, compInfo);
    } else {
        returnType = try create(AstTypes, allocator, @as(AstTypes, AstTypes.Void));
    }

    try compInfo.tokens.expectToken(.LBrace);

    const index = compInfo.tokens.index;
    const body: *const AstNode = try parseSequence(allocator, compInfo);
    const endIndex = compInfo.tokens.index;
    const bodyTokens = compInfo.tokens.tokens[index..endIndex];

    try compInfo.tokens.expectToken(.RBrace);

    return try createMut(FuncDecNode, allocator, .{
        .name = nameStr,
        .generics = generics,
        .params = params,
        .body = body,
        .bodyTokens = bodyTokens,
        .returnType = returnType,
        .capturedValues = null,
    });
}

fn parseGenerics(allocator: Allocator, compInfo: *CompInfo) ![]GenericType {
    var generics = ArrayList(GenericType).init(allocator);
    defer generics.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RBracket) {
        const generic = try parseGeneric(allocator, compInfo);
        try generics.append(generic);

        current = try compInfo.tokens.peak();
        if (current.type == .RBracket) break;

        try compInfo.tokens.expectToken(.Comma);
    }

    _ = try compInfo.tokens.take();

    return generics.toOwnedSlice();
}

fn parseGeneric(allocator: Allocator, compInfo: *CompInfo) !GenericType {
    const first = try compInfo.tokens.take();
    if (first.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForGenericType);
    }

    var restriction: ?*const AstTypes = null;
    if ((try compInfo.tokens.peak()).type == .Colon) {
        _ = try compInfo.tokens.take();
        restriction = try parseType(allocator, compInfo);
    }

    return .{
        .name = try string.cloneString(allocator, first.string.?),
        .restriction = restriction,
    };
}

fn parseParams(allocator: Allocator, compInfo: *CompInfo) ![]Parameter {
    if ((try compInfo.tokens.peak()).type == .RParen) {
        _ = try compInfo.tokens.take();
        return &[_]Parameter{};
    }

    var params = ArrayList(Parameter).init(allocator);
    defer params.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RParen) {
        const param = try parseParam(allocator, compInfo);
        try params.append(param);

        current = try compInfo.tokens.peak();
        if (current.type == .RParen) break;

        try compInfo.tokens.expectToken(.Comma);
    }

    _ = try compInfo.tokens.take();

    return params.toOwnedSlice();
}

fn parseParam(allocator: Allocator, compInfo: *CompInfo) !Parameter {
    const first = try compInfo.tokens.take();

    if (first.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForParameterName);
    }

    try compInfo.tokens.expectToken(.Colon);
    const paramType = try parseType(allocator, compInfo);

    return .{
        .name = try string.cloneString(allocator, first.string.?),
        .type = paramType,
    };
}

fn parseFuncCallParams(allocator: Allocator, compInfo: *CompInfo) ![]*AstNode {
    if ((try compInfo.tokens.peak()).type == .RParen) {
        _ = try compInfo.tokens.take();
        return &[_]*AstNode{};
    }

    var params = ArrayList(*AstNode).init(allocator);
    defer params.deinit();

    var current = try compInfo.tokens.peak();
    while (current.type != .RParen) {
        const param = try parseExpression(allocator, compInfo);
        if (param == null) {
            return compInfo.logger.logError(AstError.ExpectedExpression);
        }
        try params.append(param.?);

        current = try compInfo.tokens.peak();
        if (current.type == .RParen) break;

        try compInfo.tokens.expectToken(.Comma);
    }

    _ = try compInfo.tokens.take();

    return params.toOwnedSlice();
}

fn getOpPrecedence(op: OpExprTypes) usize {
    for (opPrecedence, 0..) |exprOp, index| {
        if (op == exprOp) return index;
    }

    unreachable;
}

fn rotatePrecedence(rootExprNode: *AstNode) ?*AstNode {
    if (rootExprNode.* != .OpExpr) return null;
    const rootExpr = rootExprNode.OpExpr;
    if (rootExpr.right.* != .OpExpr) return rootExprNode;
    const rightNode = rootExpr.right;
    const rightExpr = rightNode.OpExpr;

    if (getOpPrecedence(rootExpr.type) < getOpPrecedence(rightExpr.type)) {
        const childLeft = rightExpr.left;
        rootExprNode.OpExpr.right = childLeft;
        rightNode.OpExpr.left = rootExprNode;
        return rightNode;
    }

    return rootExprNode;
}

/// note: should only be called from exhaustive switch
/// types other than ones in OpExprTypes are marked unreachable
pub fn tokenTypeToOpType(tokenType: tokenizer.TokenType) OpExprTypes {
    return switch (tokenType) {
        .BitAnd => .BitAnd,
        .BitOr => .BitOr,
        .And => .And,
        .Or => .Or,
        .Add => .Add,
        .Sub => .Sub,
        .Mult => .Mult,
        .Div => .Div,
        .LAngle => .LessThan,
        .RAngle => .GreaterThan,
        .LAngleEq => .LessThanEq,
        .RAngleEq => .GreaterThanEq,
        else => unreachable,
    };
}

pub fn mergeMembers(allocator: Allocator, compInfo: *CompInfo, attrs: []StructAttribute, derive: *const AstTypes) ![]StructAttribute {
    var res = try ArrayList(StructAttribute).initCapacity(allocator, attrs.len);
    const structName = switch (derive.*) {
        .Custom => |custom| custom.name,
        .StaticStructInstance => |inst| inst,
        else => unreachable,
    };
    const deriveDec = compInfo.getStructDec(structName);

    for (attrs) |attr| {
        if (attr.attr != .Member) continue;
        try res.append(attr);
    }

    if (deriveDec) |dec| {
        if (dec.deriveType) |decDerive| {
            const arr = try mergeMembers(allocator, compInfo, dec.attributes, decDerive);
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

fn createBoolNode(allocator: Allocator, value: bool) !*AstNode {
    const node = try createMut(AstNode, allocator, .{
        .Value = .{
            .Bool = value,
        },
    });
    return node;
}

fn createVarDecNode(allocator: Allocator, compInfo: *CompInfo, isConst: bool) !?*AstNode {
    const name = try compInfo.tokens.take();
    if (name.type != .Identifier) {
        return compInfo.logger.logError(AstError.ExpectedIdentifierForVariableName);
    }

    var annotation: ?*const AstTypes = null;

    const next = try compInfo.tokens.take();
    if (next.type == .Colon) {
        annotation = try parseType(allocator, compInfo);
        try compInfo.tokens.expectToken(.EqSet);
    } else if (next.type != .EqSet) {
        return compInfo.logger.logError(AstError.UnexpectedToken);
    }

    const setValue = try parseExpression(allocator, compInfo);
    if (setValue == null) return null;

    return try createMut(AstNode, allocator, .{
        .VarDec = .{
            .name = try string.cloneString(allocator, name.string.?),
            .isConst = isConst,
            .setNode = setValue.?,
            .annotation = annotation,
        },
    });
}

fn parseType(allocator: Allocator, compInfo: *CompInfo) (AstError || Allocator.Error)!*const AstTypes {
    const first = try compInfo.tokens.take();
    var astType: AstTypes = switch (first.type) {
        .Bool => .Bool,
        .StringType => .String,
        .U8 => .{ .Number = .U8 },
        .U16 => .{ .Number = .U16 },
        .U32 => .{ .Number = .U32 },
        .U64 => .{ .Number = .U64 },
        .U128 => .{ .Number = .U128 },
        .I8 => .{ .Number = .I8 },
        .I16 => .{ .Number = .I16 },
        .I32 => .{ .Number = .I32 },
        .I64 => .{ .Number = .I64 },
        .I128 => .{ .Number = .I128 },
        .F32 => .{ .Number = .F32 },
        .F64 => .{ .Number = .F64 },
        .F128 => .{ .Number = .F128 },
        .USize => .{ .Number = .USize },
        .CharType => .Char,
        .QuestionMark => a: {
            const nullable = try parseType(allocator, compInfo);
            break :a .{
                .Nullable = nullable,
            };
        },
        .Identifier => a: {
            const str = try string.cloneString(allocator, first.string.?);
            if (compInfo.hasStruct(str)) {
                const next = try compInfo.tokens.peak();
                var generics: []*const AstTypes = &[_]*const AstTypes{};

                if (next.type == .LAngle) {
                    _ = try compInfo.tokens.take();
                    generics = try parseStructInitGenerics(allocator, compInfo);
                }

                break :a .{
                    .Custom = .{
                        .name = str,
                        .generics = generics,
                    },
                };
            } else if (compInfo.hasError(str)) {
                const next = try compInfo.tokens.peak();
                var generic: ?*const AstTypes = null;

                if (next.type == .LAngle) {
                    _ = try compInfo.tokens.take();
                    generic = try parseType(allocator, compInfo);

                    if (generic) |gen| {
                        if (gen.* == .Error) {
                            return AstError.ErrorPayloadMayNotBeError;
                        }
                    } else {
                        return AstError.ExpectedTypeExpression;
                    }

                    try compInfo.tokens.expectToken(.RAngle);
                }

                break :a .{
                    .Error = .{
                        .name = str,
                        .payload = generic,
                    },
                };
            }

            break :a .{
                .Generic = str,
            };
        },
        else => return compInfo.logger.logError(AstError.UnexpectedToken),
    };

    var next = try compInfo.tokens.peak();
    if (next.type == .LBracket) {
        _ = try compInfo.tokens.take();
        next = try compInfo.tokens.peak();
        printToken(next);

        if (next.type == .RBracket) {
            _ = try compInfo.tokens.take();

            astType = .{
                .DynamicArray = try create(AstTypes, allocator, astType),
            };
        } else {
            const size = try parseExpression(allocator, compInfo);
            if (size == null) {
                return compInfo.logger.logError(AstError.ExpectedSizeForStaticArray);
            }

            try compInfo.tokens.expectToken(.RBracket);

            astType = .{
                .StaticArray = .{
                    .type = try create(AstTypes, allocator, astType),
                    .size = size.?,
                },
            };
        }
    }

    return try create(AstTypes, allocator, astType);
}

pub fn findHoistedNames(allocator: Allocator, tokens: []tokenizer.Token) !HoistedNames {
    var structNames = ArrayList([]u8).init(allocator);
    defer structNames.deinit();
    var errorNames = ArrayList([]u8).init(allocator);
    defer errorNames.deinit();

    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        switch (tokens[i].type) {
            .Struct => {
                if (tokens[i + 1].type == .LBracket) {
                    while (i < tokens.len - 1 and tokens[i + 1].type != .RBracket) : (i += 1) {}
                    i += 1;
                }

                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForStruct;
                }

                const str = try string.cloneString(allocator, tokens[i + 1].string.?);
                try structNames.append(str);
            },
            .Error => {
                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForError;
                }

                const str = try string.cloneString(allocator, tokens[i + 1].string.?);
                try errorNames.append(str);
            },
            else => {},
        }
    }

    return .{
        .structNames = try structNames.toOwnedSlice(),
        .errorNames = try errorNames.toOwnedSlice(),
    };
}

pub fn registerStructsAndErrors(allocator: Allocator, compInfo: *CompInfo) !RegisterStructsAndErrorsResult {
    var structDecs = ArrayList(*StructDecNode).init(allocator);
    defer structDecs.deinit();
    var errorDecs = ArrayList(*const ErrorDecNode).init(allocator);
    defer errorDecs.deinit();

    while (compInfo.tokens.hasNext()) {
        const token = try compInfo.tokens.take();
        if (token.type != .Struct and token.type != .Error) {
            continue;
        }

        compInfo.tokens.returnToken();
        const node = try parseStatement(allocator, compInfo);
        if (node == null) continue;

        switch (node.?.*) {
            .StructDec => |dec| {
                try structDecs.append(dec);
            },
            .ErrorDec => |dec| {
                try errorDecs.append(dec);
            },
            else => unreachable,
        }

        allocator.destroy(node.?);
    }

    return .{
        .structs = try structDecs.toOwnedSlice(),
        .errors = try errorDecs.toOwnedSlice(),
    };
}

pub fn rawNumberToInferredType(num: []u8) !AstNumber {
    if (num[0] == '-') {
        return .{
            .I32 = std.fmt.parseInt(i32, num, 10) catch return scanner.ScanError.InvalidNumber,
        };
    }

    return .{
        .U32 = std.fmt.parseInt(u32, num, 10) catch return scanner.ScanError.InvalidNumber,
    };
}
