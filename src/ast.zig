const std = @import("std");
const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const free = blitz.free;
const string = blitz.string;
const scanner = blitz.scanner;
const clone = blitz.clone;
const blitzCompInfo = blitz.compInfo;
const logger = blitz.logger;
const create = utils.create;
const createMut = utils.createMut;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const TokenError = tokenizer.TokenError;
const Context = blitz.context.Context;

const AstNumberVariantsStrRel = struct {
    str: []const u8,
    val: AstNumberVariants,
};

pub const AstNumberVariants = enum {
    const Self = @This();

    Char,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    F128,

    pub fn getSize(self: Self) u8 {
        return switch (self) {
            .Char => 1,
            .U8 => 1,
            .U16 => 2,
            .U32 => 4,
            .U64 => 8,
            .U128 => 16,
            .I8 => 1,
            .I16 => 2,
            .I32 => 4,
            .I64 => 8,
            .I128 => 16,
            .F32 => 4,
            .F64 => 8,
            .F128 => 16,
        };
    }

    pub fn fromStr(str: []const u8) ?Self {
        const rels = &[_]AstNumberVariantsStrRel{
            .{ .str = "char", .val = .Char },
            .{ .str = "u8", .val = .U8 },
            .{ .str = "u16", .val = .U16 },
            .{ .str = "u32", .val = .U32 },
            .{ .str = "u64", .val = .U64 },
            .{ .str = "u128", .val = .U128 },
            .{ .str = "i8", .val = .I8 },
            .{ .str = "i16", .val = .I16 },
            .{ .str = "i32", .val = .I32 },
            .{ .str = "i64", .val = .I64 },
            .{ .str = "i128", .val = .I128 },
            .{ .str = "f32", .val = .F32 },
            .{ .str = "f64", .val = .F64 },
            .{ .str = "f128", .val = .F128 },
        };

        for (rels) |rel| {
            if (string.compString(rel.str, str)) return rel.val;
        }

        return null;
    }

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .Char => "char",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
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
};

pub const AstNumber = union(AstNumberVariants) {
    const Self = @This();

    Char: u8,
    U8: u8,
    U16: u16,
    U32: u32,
    U64: u64,
    U128: u128,
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
            .Char => "char",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
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

    pub fn toAstNumberVariant(self: Self) AstNumberVariants {
        return switch (self) {
            .Char => .Char,
            .U8 => .U8,
            .U16 => .U16,
            .U32 => .U32,
            .U64 => .U64,
            .U128 => .U128,
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

pub const AstArraySliceType = struct {
    type: scanner.TypeAndAllocInfo,
    size: ?*AstNode,
};

pub const CustomType = struct {
    name: []const u8,
    generics: []AstTypeInfo,
    allowPrivateReads: bool,
};

const ErrorVariantType = struct {
    from: ?[]const u8,
    variant: []const u8,
};

const ErrorAstType = struct {
    name: []const u8,
    payload: ?AstTypeInfo,
};

const Types = enum {
    String,
    Bool,
    Char,
    Void,
    Null,
    Any,
    Number,
    RawNumber,
    ArraySlice,
    Pointer,
    Nullable,
    Custom,
    Generic,
    Function,
    StaticStructInstance,
    Error,
    ErrorVariant,
    VarInfo,
};

pub const AstTypes = union(Types) {
    String,
    Bool,
    Char,
    Void,
    Null,
    Any,
    Number: AstNumberVariants,
    RawNumber: []const u8,
    ArraySlice: AstArraySliceType,
    Pointer: scanner.TypeAndAllocInfo,
    Nullable: AstTypeInfo,
    Custom: CustomType,
    Generic: []const u8,
    Function: *FuncDecNode,
    StaticStructInstance: []const u8,
    Error: ErrorAstType,
    ErrorVariant: ErrorVariantType,
    VarInfo: scanner.TypeAndAllocInfo,
};

pub const AstTypeInfo = struct {
    astType: *AstTypes,
    mutState: scanner.MutState,
};

const StaticTypes = enum {
    String,
    Bool,
    Char,
    Number,
    RawNumber,
    ArraySlice,
    Null,
};

pub const RawNumberNode = struct {
    digits: []const u8,
    numType: AstNumberVariants,
};

pub const AstValues = union(StaticTypes) {
    String: []const u8,
    Bool: bool,
    Char: u8,
    Number: AstNumber,
    RawNumber: RawNumberNode,
    ArraySlice: []*AstNode,
    Null,
};

const VarDecNode = struct {
    name: []const u8,
    mutState: scanner.MutState,
    setNode: *AstNode,
    annotation: ?AstTypeInfo,
};

const CastNode = struct {
    node: *AstNode,
    toType: AstTypeInfo,
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
    Member: AstTypeInfo,
    Function: *FuncDecNode,
};

pub const StructAttribute = struct {
    name: []const u8,
    attr: StructAttributeUnion,
    visibility: MemberVisibility,
    static: bool,
};

pub const StrToTypeInfoRel = struct {
    str: []const u8,
    info: AstTypeInfo,
};

pub const ToScanTypesList = ArrayList([]StrToTypeInfoRel);

pub const StructDecNode = struct {
    const Self = @This();

    name: []const u8,
    generics: []GenericType,
    attributes: []StructAttribute,
    totalMemberList: []StructAttribute,
    deriveType: ?AstTypeInfo,
    toScanTypes: *ToScanTypesList,
};

pub const AttributeDefinition = struct {
    name: []const u8,
    value: *AstNode,
};

const StructInitNode = struct {
    name: []const u8,
    attributes: []AttributeDefinition,
    generics: []AstTypeInfo,
};

pub const GenericType = struct {
    name: []const u8,
    restriction: ?AstTypeInfo,
};

pub const FallbackInfo = struct {
    node: *AstNode, // must always be an if statement node
    hasCondition: bool,
};

const IfStatementNode = struct {
    condition: *AstNode,
    body: *AstNode,
    fallback: ?FallbackInfo,
};

pub const Parameter = struct {
    name: []const u8,
    type: AstTypeInfo,
    mutState: scanner.MutState,
};

const FuncType = enum {
    Builtin,
    StructMethod,
    Normal,
};

pub const FuncDecNode = struct {
    name: []const u8,
    generics: ?[]GenericType,
    params: []Parameter,
    body: *AstNode,
    bodyTokens: []tokenizer.Token,
    returnType: AstTypeInfo,
    capturedValues: ?*blitzCompInfo.CaptureScope,
    capturedTypes: ?*blitzCompInfo.TypeScope,
    capturedFuncs: ?*blitzCompInfo.StringListScope,
    toScanTypes: *ToScanTypesList,
    funcType: FuncType,
    visited: bool,
    globallyDefined: bool,
};

const FuncCallNode = struct {
    func: *AstNode,
    params: []*AstNode,
};

const PropertyAccess = struct {
    value: *AstNode,
    property: []const u8,
};

pub const OpExprTypes = enum(u8) {
    const Self = @This();

    BitAnd = 0,
    BitOr = 1,
    And = 2,
    Or = 3,
    Mult = 4,
    Div = 5,
    Add = 6,
    Sub = 7,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    Equal,
    NotEqual,

    pub fn getOppositeCompOp(self: Self) Self {
        return switch (self) {
            .Equal => .NotEqual,
            .NotEqual => .Equal,
            .LessThan => .GreaterThanEq,
            .GreaterThan => .LessThanEq,
            .LessThanEq => .GreaterThan,
            .GreaterThanEq => .LessThan,
            else => self,
        };
    }
};

const OpExpr = struct {
    type: OpExprTypes,
    left: *AstNode,
    right: *AstNode,
    depth: usize,
};

const IndexValueNode = struct {
    index: *AstNode,
    value: *AstNode,
};

pub const ErrorDecNode = struct {
    name: []const u8,
    variants: [][]const u8,
};

const ValueSetNode = struct {
    value: *AstNode,
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
    variable: []const u8,
};

const ForLoopNode = struct {
    initNode: ?*AstNode,
    condition: *AstNode,
    incNode: *AstNode,
    body: *AstNode,
};

const WhileLoopNode = struct {
    condition: *AstNode,
    body: *AstNode,
};

const PointerNode = struct {
    node: *AstNode,
    mutState: scanner.MutState,
};

const HeapAllocNode = struct {
    node: *AstNode,
};

const ArrayInitNode = struct {
    size: []const u8,
    initType: AstTypeInfo,
    initNode: *AstNode,
    indexIdent: ?[]const u8,
    ptrIdent: ?[]const u8,
};

const AstNodeVariants = enum {
    NoOp,
    StructPlaceholder,
    Break,
    Continue,
    Seq,
    VarDec,
    ValueSet,
    VarEqOp,
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
    InferErrorVariant,
    Group,
    Scope,
    IncOne,
    DecOne,
    ForLoop,
    WhileLoop,
    Pointer,
    Dereference,
    HeapAlloc,
    HeapFree,
    ArrayInit,
};

pub const AstNode = union(AstNodeVariants) {
    NoOp,
    StructPlaceholder,
    Break,
    Continue,
    Seq: []*AstNode,
    VarDec: VarDecNode,
    ValueSet: ValueSetNode,
    VarEqOp: VarEqOpNode,
    Value: AstValues,
    Cast: CastNode,
    Variable: []const u8,
    StructDec: *StructDecNode,
    IfStatement: IfStatementNode,
    FuncDec: []const u8,
    FuncCall: FuncCallNode,
    ReturnNode: *AstNode,
    StructInit: StructInitNode,
    Bang: *AstNode,
    PropertyAccess: PropertyAccess,
    StaticStructInstance: []const u8,
    FuncReference: []const u8,
    OpExpr: OpExpr,
    IndexValue: IndexValueNode,
    ErrorDec: *const ErrorDecNode,
    Error: []const u8,
    InferErrorVariant: []const u8,
    Group: *AstNode,
    Scope: *AstNode,
    IncOne: *AstNode,
    DecOne: *AstNode,
    ForLoop: ForLoopNode,
    WhileLoop: WhileLoopNode,
    Pointer: PointerNode,
    Dereference: *AstNode,
    HeapAlloc: HeapAllocNode,
    HeapFree: *AstNode,
    ArrayInit: ArrayInitNode,
};

pub const AstError = error{
    InvalidExprOperand,
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
    ExpectedSizeForArraySlice,
    ExpectedIdentifierForStructProperty,
    ExpectedValueForStructProperty,
    ExpectedIdentifierPropertyAccessSource,
    UnexpectedGenericOnErrorType,
    ExpectedTypeExpression,
    ErrorPayloadMayNotBeError,
    UnexpectedGeneric,
    UnexpectedMutSpecifierOnGeneric,
    ExpectedU64ForArraySize,
    StructDefinedInLowerScope,
    ErrorDefinedInLowerScope,
    FunctionDefinedInLowerScope,
    UnexpectedDeriveType,
    NegativeNumberWithUnsignedTypeConflict,
    ExpectedIdentifierForArrayInitIndex,
    ExpectedIdentifierForArrayInitPtr,
} || TokenError;

pub const ParseError = AstError || Allocator.Error;

const RegisterStructsAndErrorsResult = struct {
    structs: []*StructDecNode,
    errors: []*const ErrorDecNode,
};

pub const HoistedNames = struct {
    structNames: [][]const u8,
    errorNames: [][]const u8,
};

const OpExprTokenMap = struct {
    type: OpExprTypes,
    token: tokenizer.TokenType,
};

pub const Ast = struct {
    const Self = @This();

    root: *AstNode,
    allocator: Allocator,

    pub fn init(allocator: Allocator, root: *AstNode) Self {
        return Self{
            .root = root,
            .allocator = allocator,
        };
    }
};

pub fn createAst(allocator: Allocator, context: *Context) !Ast {
    const seq = parseSequence(allocator, context, false) catch |e| {
        logger.logParseError(context, e);
        return e;
    };
    return Ast.init(allocator, seq);
}

pub fn parseSequence(
    allocator: Allocator,
    context: *Context,
    fromBlock: bool,
) ParseError!*AstNode {
    var seq: ArrayList(*AstNode) = .empty;
    defer seq.deinit(allocator);

    while (context.tokenUtil.hasNext()) {
        const peakToken = try context.tokenUtil.peakFixed();
        if (peakToken.type == .Semicolon or peakToken.type == .NewLine) {
            _ = try context.tokenUtil.takeFixed();
            continue;
        }

        if (peakToken.type == .RBrace) {
            if (fromBlock) {
                break;
            } else {
                return TokenError.UnexpectedToken;
            }
        }

        const node = try parseStatement(allocator, context) orelse break;

        try seq.append(allocator, node);
    }

    const ownedSlice = try seq.toOwnedSlice(allocator);
    try context.deferCleanup.slices.nodeSlices.append(ownedSlice);

    return try context.pools.nodes.new(.{
        .Seq = ownedSlice,
    });
}

fn parseStatement(
    allocator: Allocator,
    context: *Context,
) (AstError || Allocator.Error)!?*AstNode {
    const first = try context.tokenUtil.take();
    switch (first.type) {
        .Let => {
            const next = try context.tokenUtil.peak();
            var mutState: scanner.MutState = .Const;

            if (next.type == .Mut) {
                mutState = .Mut;
                _ = try context.tokenUtil.take();
            }

            return try createVarDecNode(allocator, context, mutState);
        },
        .If => {
            try context.tokenUtil.expectToken(.LParen);
            const condition = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            try context.tokenUtil.expectToken(.RParen);

            try context.tokenUtil.expectToken(.LBrace);
            const seq = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);

            const fallback = try parseIfChain(allocator, context);

            return try context.pools.nodes.new(.{
                .IfStatement = .{
                    .condition = condition,
                    .body = seq,
                    .fallback = fallback,
                },
            });
        },
        .Fn => {
            const func = try parseFuncDef(allocator, context, false);
            try context.compInfo.addFunction(func.name, func);

            return try context.pools.nodes.new(.{
                .FuncDec = func.name,
            });
        },
        .For => {
            try context.tokenUtil.expectToken(.LParen);

            var initNode: ?*AstNode = null;

            const next = try context.tokenUtil.peak();
            if (next.type != .Semicolon) {
                initNode = try parseStatement(allocator, context);
            }

            try context.tokenUtil.expectToken(.Semicolon);

            const condition = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.Semicolon);

            const incNode = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.RParen);
            try context.tokenUtil.expectToken(.LBrace);
            const body = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);

            return try context.pools.nodes.new(.{
                .ForLoop = .{
                    .initNode = initNode,
                    .condition = condition,
                    .incNode = incNode,
                    .body = body,
                },
            });
        },
        .While => {
            try context.tokenUtil.expectToken(.LParen);

            const condition = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.RParen);
            try context.tokenUtil.expectToken(.LBrace);
            const body = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);

            return try context.pools.nodes.new(.{
                .WhileLoop = .{
                    .condition = condition,
                    .body = body,
                },
            });
        },
        .Identifier => {
            const next = try context.tokenUtil.take();
            switch (next.type) {
                .EqSet => {
                    const setNode = try parseExpression(allocator, context) orelse
                        return AstError.ExpectedExpression;

                    return try context.pools.nodes.new(.{
                        .ValueSet = .{
                            .setNode = setNode,
                            .value = try context.pools.nodes.new(.{
                                .Variable = context.getTokString(first),
                            }),
                        },
                    });
                },
                .AddEq => {
                    const incNode = try parseExpression(allocator, context) orelse
                        return AstError.ExpectedExpression;

                    return try context.pools.nodes.new(.{
                        .VarEqOp = .{
                            .opType = .AddEq,
                            .value = incNode,
                            .variable = context.getTokString(first),
                        },
                    });
                },
                .LParen => {
                    return try parseFuncCall(allocator, context, context.getTokString(first));
                },
                .Period => {
                    context.tokenUtil.returnToken();
                    const identNode = try getIdentNode(
                        context,
                        context.getTokString(first),
                    );
                    return parsePropertyAccess(allocator, context, identNode);
                },
                else => return try context.pools.nodes.new(.{
                    .Variable = context.getTokString(first),
                }),
            }
        },
        .Return => {
            const value = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            try context.tokenUtil.expectToken(.Semicolon);

            return try context.pools.nodes.new(.{
                .ReturnNode = value,
            });
        },
        .Error => {
            if (!context.compInfo.preAst) {
                _ = try context.tokenUtil.take();
                var next = try context.tokenUtil.take();
                if (next.type == .LBrace) {
                    while (next.type != .RBrace) {
                        next = try context.tokenUtil.take();
                    }
                } else if (next.type != .Semicolon) {
                    return TokenError.UnexpectedToken;
                }

                return try context.pools.nodes.new(.NoOp);
            }

            const errNode = try parseError(allocator, context);

            return try context.pools.nodes.new(.{
                .ErrorDec = errNode,
            });
        },
        .Struct => {
            if (!context.compInfo.preAst) {
                var parens: usize = 0;

                var current = try context.tokenUtil.take();
                while (parens > 1 or current.type != .RBrace) {
                    if (current.isOpenToken(false)) {
                        parens += 1;
                    } else if (current.isCloseToken(false)) {
                        parens -= 1;
                    }

                    current = try context.tokenUtil.take();
                }

                return try context.pools.nodes.new(.StructPlaceholder);
            }

            return try parseStruct(allocator, context);
        },
        .LBrace => {
            const seq = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);
            return try context.pools.nodes.new(.{
                .Scope = seq,
            });
        },
        .Asterisk => {
            context.tokenUtil.returnToken();
            const toExpr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            try context.tokenUtil.expectToken(.EqSet);
            const fromExpr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            return try context.pools.nodes.new(.{
                .ValueSet = .{
                    .value = toExpr,
                    .setNode = fromExpr,
                },
            });
        },
        .Break => {
            return try context.pools.nodes.new(.Break);
        },
        .Continue => {
            return try context.pools.nodes.new(.Continue);
        },
        .Delete => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            return try context.pools.nodes.new(.{
                .HeapFree = expr,
            });
        },
        else => {
            return TokenError.UnexpectedToken;
        },
    }
}

fn parseIfChain(allocator: Allocator, context: *Context) !?FallbackInfo {
    if (!context.tokenUtil.hasNext()) return null;

    const next = try context.tokenUtil.peak();
    if (next.type != .Else) return null;
    _ = try context.tokenUtil.take();

    var condition: ?*AstNode = null;

    const nextNext = try context.tokenUtil.peak();
    if (nextNext.type == .If) {
        _ = try context.tokenUtil.take();
        try context.tokenUtil.expectToken(.LParen);

        condition = try parseExpression(allocator, context) orelse
            return AstError.ExpectedExpression;

        try context.tokenUtil.expectToken(.RParen);
    }

    try context.tokenUtil.expectToken(.LBrace);
    const body = try parseSequence(allocator, context, true);
    try context.tokenUtil.expectToken(.RBrace);
    const fallback = try parseIfChain(allocator, context);

    return .{
        .node = try context.pools.nodes.new(.{
            .IfStatement = .{
                .condition = condition orelse try context.pools.nodes.new(.NoOp),
                .body = body,
                .fallback = fallback,
            },
        }),
        .hasCondition = condition != null,
    };
}

fn parseStruct(allocator: Allocator, context: *Context) !?*AstNode {
    try context.compInfo.pushParsedGenericsScope(false);
    defer context.compInfo.popParsedGenericsScope();

    var deriveType: ?AstTypeInfo = null;
    var generics: []GenericType = &[_]GenericType{};

    var first = try context.tokenUtil.take();
    if (first.type == .LBracket) {
        generics = try parseGenerics(allocator, context);
        first = try context.tokenUtil.take();
    } else if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForStructName;
    }

    const structName = context.getTokString(first);
    var next = try context.tokenUtil.take();

    if (next.type == .Colon) {
        next = try context.tokenUtil.peak();

        if (next.type != .Identifier) {
            return AstError.ExpectedIdentifierForDeriveType;
        }

        if (!context.compInfo.hasStruct(context.getTokString(next))) {
            return AstError.ExpectedStructDeriveType;
        }

        deriveType = try parseType(allocator, context);
        if (deriveType) |*derive| {
            if (derive.astType.* != .Custom) {
                return AstError.UnexpectedDeriveType;
            }
            derive.astType.Custom.allowPrivateReads = true;
        }
        try context.tokenUtil.expectToken(.LBrace);
    } else if (next.type != .LBrace) {
        return TokenError.UnexpectedToken;
    }

    const attributes = try parseStructAttributes(allocator, context, structName);

    return try context.pools.nodes.new(.{
        .StructDec = try createMut(StructDecNode, allocator, .{
            .name = structName,
            .generics = generics,
            .attributes = attributes,
            .totalMemberList = &[_]StructAttribute{},
            .deriveType = deriveType,
            .toScanTypes = try utils.createMut(ToScanTypesList, allocator, .empty),
        }),
    });
}

fn parseStructAttributes(
    allocator: Allocator,
    context: *Context,
    structName: []const u8,
) ![]StructAttribute {
    var attributes: ArrayList(StructAttribute) = .empty;
    defer attributes.deinit(allocator);

    var current = try context.tokenUtil.peak();
    while (current.type != .RBrace) {
        const attr = try parseStructAttribute(allocator, context, structName);
        try attributes.append(allocator, attr);

        if (attr.attr == .Member) {
            try context.tokenUtil.expectToken(.Semicolon);
        }

        current = try context.tokenUtil.peak();
        if (current.type == .RBrace) break;
    }

    try context.tokenUtil.expectToken(.RBrace);

    return try attributes.toOwnedSlice(allocator);
}

fn parseStructAttribute(
    allocator: Allocator,
    context: *Context,
    structName: []const u8,
) !StructAttribute {
    const first = try context.tokenUtil.take();
    switch (first.type) {
        .Identifier, .Fn => {
            context.tokenUtil.returnToken();
            return parseStructAttributeUtil(allocator, context, structName, .Private);
        },
        .Prot => return parseStructAttributeUtil(allocator, context, structName, .Protected),
        .Pub => return parseStructAttributeUtil(allocator, context, structName, .Public),
        else => {
            return TokenError.UnexpectedToken;
        },
    }
}

fn parseStructAttributeUtil(
    allocator: Allocator,
    context: *Context,
    structName: []const u8,
    visibility: MemberVisibility,
) !StructAttribute {
    var first = try context.tokenUtil.take();
    var static = false;

    if (first.type == .Static) {
        static = true;
        first = try context.tokenUtil.take();
    }

    switch (first.type) {
        .Identifier => {
            try context.tokenUtil.expectToken(.Colon);
            const attrType = try parseType(allocator, context);

            return .{
                .name = context.getTokString(first),
                .attr = .{
                    .Member = attrType,
                },
                .visibility = visibility,
                .static = static,
            };
        },
        .Fn => {
            const def = try parseFuncDef(allocator, context, true);

            if (!static) {
                const valueCaptures = try utils.initMutPtrT(
                    blitzCompInfo.CaptureScope,
                    allocator,
                );
                def.capturedValues = valueCaptures;
                const selfInfo = utils.astTypesPtrToInfo(try context.pools.types.new(.{
                    .Custom = .{
                        .name = structName,
                        .generics = &.{},
                        .allowPrivateReads = true,
                    },
                }), .Const);
                try valueCaptures.put("self", selfInfo);
            }

            return .{
                .name = def.name,
                .attr = .{
                    .Function = def,
                },
                .visibility = visibility,
                .static = static,
            };
        },
        else => {
            return TokenError.UnexpectedToken;
        },
    }
}

fn parseError(allocator: Allocator, context: *Context) !*const ErrorDecNode {
    const name = try context.tokenUtil.take();
    if (name.type != .Identifier) {
        return AstError.ExpectedIdentifierForErrorName;
    }

    var variants: [][]const u8 = &[_][]const u8{};

    const next = try context.tokenUtil.take();
    if (next.type == .LBrace) {
        variants = try parseVariants(allocator, context);
    } else if (next.type != .Semicolon) {
        return TokenError.UnexpectedToken;
    }

    return try create(ErrorDecNode, allocator, .{
        .name = context.getTokString(name),
        .variants = variants,
    });
}

fn parseVariants(allocator: Allocator, context: *Context) ![][]const u8 {
    var variants: ArrayList([]const u8) = .empty;
    defer variants.deinit(allocator);

    var variant = try context.tokenUtil.take();
    while (variant.type == .Identifier) {
        const comma = try context.tokenUtil.take();
        if (comma.type != .RBrace and comma.type != .Comma) {
            return TokenError.UnexpectedToken;
        }

        const variantClone = context.getTokString(variant);
        try variants.append(allocator, variantClone);

        if (comma.type == .RBrace) break;
        variant = try context.tokenUtil.take();
    }

    return try variants.toOwnedSlice(allocator);
}

fn parseExpression(allocator: Allocator, context: *Context) !?*AstNode {
    var expr = try parseExpressionUtil(allocator, context);
    const next = try context.tokenUtil.peak();
    expr = switch (next.type) {
        .Ampersand,
        .BitOr,
        .And,
        .Or,
        .Add,
        .Sub,
        .Asterisk,
        .Div,
        .Mod,
        .LAngle,
        .RAngle,
        .LAngleEq,
        .RAngleEq,
        .EqComp,
        => a: {
            if (expr == null) {
                return AstError.ExpectedExpression;
            }

            _ = try context.tokenUtil.take();

            const after = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            const depthLeft = getExprDepth(expr.?);
            const depthRight = getExprDepth(after);
            const depth = @max(depthLeft, depthRight) + 1;

            break :a try context.pools.nodes.new(.{
                .OpExpr = .{
                    .type = tokenTypeToOpType(next.type),
                    .left = expr.?,
                    .right = after,
                    .depth = depth,
                },
            });
        },
        .Inc => a: {
            _ = try context.tokenUtil.take();
            break :a try context.pools.nodes.new(.{
                .IncOne = expr.?,
            });
        },
        .Dec => a: {
            _ = try context.tokenUtil.take();
            break :a try context.pools.nodes.new(.{
                .DecOne = expr.?,
            });
        },
        else => expr,
    };

    if (expr) |node| {
        if (node.* == .OpExpr) {
            return rotatePrecedence(node);
        }
    }

    return expr;
}

pub fn getExprDepth(expr: *AstNode) usize {
    if (expr.* == .OpExpr) {
        const left = expr.OpExpr.left;
        const right = expr.OpExpr.right;
        var leftDepth: usize = 0;
        var rightDepth: usize = 0;

        if (left.* == .OpExpr) leftDepth = left.OpExpr.depth;
        if (right.* == .OpExpr) rightDepth = right.OpExpr.depth;

        return @max(leftDepth, rightDepth) + 1;
    }

    return 0;
}

fn parseExpressionUtil(
    allocator: Allocator,
    context: *Context,
) (Allocator.Error || AstError)!?*AstNode {
    const first = try context.tokenUtil.take();
    switch (first.type) {
        .Null => return try context.pools.nodes.new(.{
            .Value = .Null,
        }),
        .New => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            return try context.pools.nodes.new(.{
                .HeapAlloc = .{
                    .node = expr,
                },
            });
        },
        .Number => |numType| {
            return try context.pools.nodes.new(.{
                .Value = .{
                    .RawNumber = .{
                        .digits = context.getTokString(first),
                        .numType = numType,
                    },
                },
            });
        },
        .NegNumber => |numType| {
            switch (numType) {
                .Char, .U8, .U16, .U32, .U64, .U128 => {
                    return AstError.NegativeNumberWithUnsignedTypeConflict;
                },
                else => {},
            }

            return try context.pools.nodes.new(.{
                .Value = .{
                    .RawNumber = .{
                        .digits = context.getTokString(first),
                        .numType = numType,
                    },
                },
            });
        },
        .Period => {
            const next = try context.tokenUtil.take();
            if (next.type != .Identifier) {
                return AstError.ExpectedIdentifierForErrorVariant;
            }

            return try context.pools.nodes.new(.{
                .InferErrorVariant = context.getTokString(next),
            });
        },
        .StringToken => {
            const str = context.getTokString(first);
            const next = try context.tokenUtil.peak();

            const strNode = try context.pools.nodes.new(.{
                .Value = .{
                    .String = str,
                },
            });

            if (next.type == .Period) {
                const propAccess = try parsePropertyAccess(allocator, context, strNode);
                return propAccess;
            }

            return strNode;
        },
        .CharToken => return try context.pools.nodes.new(.{
            .Value = .{
                .Char = context.getTokString(first)[0],
            },
        }),
        .Mut => {
            const next = try context.tokenUtil.peak();
            if (next.type != .Ampersand) return TokenError.UnexpectedToken;

            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            expr.Pointer.mutState = .Mut;

            return expr;
        },
        .Bang => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            return try context.pools.nodes.new(.{
                .Bang = expr,
            });
        },
        .Ampersand => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            return try context.pools.nodes.new(.{
                .Pointer = .{
                    .node = expr,
                    .mutState = .Const,
                },
            });
        },
        .Asterisk => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            return try context.pools.nodes.new(.{
                .Dereference = expr,
            });
        },
        .LParen => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.RParen);

            const groupNode = try context.pools.nodes.new(.{
                .Group = expr,
            });

            const next = try context.tokenUtil.peak();
            if (next.type == .Period) {
                return try parsePropertyAccess(allocator, context, groupNode);
            }

            return groupNode;
        },
        .Identifier => {
            const next = try context.tokenUtil.peak();
            switch (next.type) {
                .LParen => {
                    _ = try context.tokenUtil.take();

                    return try parseFuncCall(allocator, context, context.getTokString(first));
                },
                .Period => {
                    const identNode = try getIdentNode(
                        context,
                        context.getTokString(first),
                    );
                    return try parsePropertyAccess(allocator, context, identNode);
                },
                .LBrace, .LAngle => {
                    if (context.compInfo.hasStruct(context.getTokString(first))) {
                        return try parseStructInit(
                            allocator,
                            context,
                            context.getTokString(first),
                        );
                    }
                },
                .LBracket => {
                    _ = try context.tokenUtil.take();
                    const index = try parseExpression(allocator, context) orelse
                        return AstError.ExpectedExpression;
                    try context.tokenUtil.expectToken(.RBracket);

                    return try context.pools.nodes.new(.{
                        .IndexValue = .{
                            .index = index,
                            .value = try getIdentNode(
                                context,
                                context.getTokString(first),
                            ),
                        },
                    });
                },
                else => {},
            }

            return try getIdentNode(context, context.getTokString(first));
        },
        .LBracket => return parseArray(allocator, context),
        .True => return try createBoolNode(context, true),
        .False => return try createBoolNode(context, false),
        .Cast => {
            try context.tokenUtil.expectToken(.LParen);
            const toType = try parseType(allocator, context);
            try context.tokenUtil.expectToken(.RParen);
            const inner = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            return try context.pools.nodes.new(.{
                .Cast = .{
                    .node = inner,
                    .toType = toType,
                },
            });
        },
        else => return TokenError.UnexpectedToken,
    }
}

fn getIdentNode(context: *Context, str: []const u8) !*AstNode {
    var node: AstNode = undefined;

    if (context.compInfo.hasStruct(str)) {
        node = .{ .StaticStructInstance = str };
    } else if (context.compInfo.hasError(str)) {
        node = .{ .Error = str };
    } else {
        node = .{ .Variable = str };
    }

    return try context.pools.nodes.new(node);
}

fn parseArray(allocator: Allocator, context: *Context) !*AstNode {
    var current = try context.tokenUtil.peak();

    switch (current.type) {
        .RBracket => {
            _ = try context.tokenUtil.take();
            return try context.pools.nodes.new(.{
                .Value = .{
                    .ArraySlice = &[_]*AstNode{},
                },
            });
        },
        .Number => |numType| a: {
            _ = try context.tokenUtil.take();
            const endBracket = try context.tokenUtil.peak();
            if (endBracket.type != .RBracket) {
                context.tokenUtil.returnToken();
                break :a;
            }

            _ = try context.tokenUtil.take();

            const arrType = parseType(allocator, context) catch {
                context.tokenUtil.returnToken();
                context.tokenUtil.returnToken();
                break :a;
            };

            try context.tokenUtil.expectToken(.With);

            var indexIdent: ?[]const u8 = null;
            var ptrIdent: ?[]const u8 = null;
            var hasIdents = false;
            if ((try context.tokenUtil.peak()).type == .LParen) b: {
                hasIdents = true;

                _ = try context.tokenUtil.take();
                const indexToken = try context.tokenUtil.take();
                if (indexToken.type != .Identifier) {
                    return AstError.ExpectedIdentifierForArrayInitIndex;
                }
                indexIdent = context.getTokString(indexToken);

                if ((try context.tokenUtil.peak()).type != .Comma) {
                    break :b;
                }

                _ = try context.tokenUtil.take();

                const ptrToken = try context.tokenUtil.take();
                if (ptrToken.type != .Identifier) {
                    return AstError.ExpectedIdentifierForArrayInitPtr;
                }
                ptrIdent = context.getTokString(ptrToken);
            }

            if (hasIdents) {
                try context.tokenUtil.expectToken(.RParen);
            }

            const initNode = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            if (numType != .U64) {
                return AstError.ExpectedU64ForArraySize;
            }

            return context.pools.nodes.new(.{
                .ArrayInit = .{
                    .size = context.getTokString(current),
                    .initType = arrType,
                    .initNode = initNode,
                    .indexIdent = indexIdent,
                    .ptrIdent = ptrIdent,
                },
            });
        },
        else => {},
    }

    var items: ArrayList(*AstNode) = .empty;
    defer items.deinit(allocator);

    while (current.type != .RBracket) {
        const item = try parseExpression(allocator, context) orelse
            return AstError.ExpectedExpression;

        try items.append(allocator, item);

        current = try context.tokenUtil.take();
        if (current.type == .RBracket) break;

        context.tokenUtil.returnToken();
        try context.tokenUtil.expectToken(.Comma);
        current = try context.tokenUtil.peak();
    }

    const slice = try items.toOwnedSlice(allocator);
    try context.deferCleanup.slices.nodeSlices.append(slice);

    return try context.pools.nodes.new(.{
        .Value = .{
            .ArraySlice = slice,
        },
    });
}

fn parseStructInit(allocator: Allocator, context: *Context, name: []const u8) !*AstNode {
    const next = try context.tokenUtil.take();
    var generics: []AstTypeInfo = &[_]AstTypeInfo{};

    if (next.type == .LAngle) {
        generics = try parseStructInitGenerics(allocator, context);
        _ = try context.tokenUtil.take();
    } else if (next.type != .LBrace) {
        return TokenError.UnexpectedToken;
    }

    const attributes = try parseStructInitAttributes(allocator, context);

    return try context.pools.nodes.new(.{
        .StructInit = .{
            .name = name,
            .attributes = attributes,
            .generics = generics,
        },
    });
}

fn parseStructInitAttributes(allocator: Allocator, context: *Context) ![]AttributeDefinition {
    var attributes: ArrayList(AttributeDefinition) = .empty;
    defer attributes.deinit(allocator);

    var current = try context.tokenUtil.peak();
    while (current.type != .RBrace) {
        const param = try parseStructInitAttribute(allocator, context);
        try attributes.append(allocator, param);

        current = try context.tokenUtil.peak();
        if (current.type == .RBrace) break;

        try context.tokenUtil.expectToken(.Comma);
        current = try context.tokenUtil.peak();
    }

    try context.tokenUtil.expectToken(.RBrace);

    const ownedSlice = try attributes.toOwnedSlice(allocator);
    try context.deferCleanup.slices.attrDefSlices.append(ownedSlice);

    return ownedSlice;
}

fn parseStructInitAttribute(allocator: Allocator, context: *Context) !AttributeDefinition {
    const first = try context.tokenUtil.take();
    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForStructProperty;
    }

    const next = try context.tokenUtil.take();
    if (next.type == .Comma or next.type == .RBrace) {
        const res = AttributeDefinition{
            .name = context.getTokString(first),
            .value = try context.pools.nodes.new(.{
                .Variable = context.getTokString(first),
            }),
        };

        context.tokenUtil.returnToken();
        return res;
    } else if (next.type != .EqSet) {
        return TokenError.UnexpectedToken;
    }

    const eqNode = try parseExpression(allocator, context) orelse
        return AstError.ExpectedValueForStructProperty;

    return .{
        .name = context.getTokString(first),
        .value = eqNode,
    };
}

fn parseStructInitGenerics(allocator: Allocator, context: *Context) ![]AstTypeInfo {
    var generics: ArrayList(AstTypeInfo) = .empty;
    defer generics.deinit(allocator);

    var current = try context.tokenUtil.peak();
    while (current.type != .RAngle) {
        const genType = try parseType(allocator, context);
        try generics.append(allocator, genType);

        current = try context.tokenUtil.peak();
        if (current.type == .RAngle) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    try context.tokenUtil.expectToken(.RAngle);

    const ownedSlice = try generics.toOwnedSlice(allocator);
    try context.deferCleanup.slices.typeInfoSlices.append(ownedSlice);

    return ownedSlice;
}

fn parsePropertyAccess(allocator: Allocator, context: *Context, node: *AstNode) !*AstNode {
    try context.tokenUtil.expectToken(.Period);

    const ident = try context.tokenUtil.take();
    if (ident.type != .Identifier) {
        return AstError.ExpectedIdentifierForPropertyAccess;
    }

    var access = try context.pools.nodes.new(.{
        .PropertyAccess = .{
            .value = node,
            .property = context.getTokString(ident),
        },
    });

    var next = try context.tokenUtil.take();
    while (next.type == .LParen) {
        const params = try parseFuncCallParams(allocator, context);
        access = try context.pools.nodes.new(.{
            .FuncCall = .{
                .func = access,
                .params = params,
            },
        });
        next = try context.tokenUtil.take();
    }

    while (next.type == .LBracket) {
        const expr = try parseExpression(allocator, context) orelse
            return AstError.ExpectedExpression;

        access = try context.pools.nodes.new(.{
            .IndexValue = .{
                .index = expr,
                .value = access,
            },
        });

        try context.tokenUtil.expectToken(.RBracket);
        next = try context.tokenUtil.take();
    }

    if (next.type == .EqSet) {
        const expr = try parseExpression(allocator, context) orelse
            return AstError.ExpectedExpression;

        return try context.pools.nodes.new(.{
            .ValueSet = .{
                .value = access,
                .setNode = expr,
            },
        });
    }

    context.tokenUtil.returnToken();

    const temp = try context.tokenUtil.peak();
    if (temp.type == .Period) {
        return parsePropertyAccess(allocator, context, access);
    }

    return access;
}

/// name expected to be cloned
fn parseFuncCall(allocator: Allocator, context: *Context, name: []const u8) !*AstNode {
    const func = try context.pools.nodes.new(.{
        .FuncReference = name,
    });

    const params = try parseFuncCallParams(allocator, context);

    return try context.pools.nodes.new(.{
        .FuncCall = .{
            .func = func,
            .params = params,
        },
    });
}

fn parseFuncDef(allocator: Allocator, context: *Context, structFn: bool) !*FuncDecNode {
    try context.compInfo.pushParsedGenericsScope(structFn);
    defer context.compInfo.popParsedGenericsScope();

    var next = try context.tokenUtil.take();
    var nameStr: []const u8 = undefined;
    var generics: ?[]GenericType = null;

    if (next.type == .LBracket) {
        generics = try parseGenerics(allocator, context);
        next = try context.tokenUtil.take();
    }

    if (next.type == .Identifier) {
        nameStr = context.getTokString(next);
    } else {
        return AstError.ExpectedIdentifierForFunctionName;
    }

    try context.tokenUtil.expectToken(.LParen);

    const params = try parseParams(allocator, context);
    var returnType: AstTypeInfo = undefined;

    const retNext = try context.tokenUtil.peak();

    if (retNext.type != .LBrace) {
        returnType = try parseType(allocator, context);
    } else {
        returnType = utils.astTypesPtrToInfo(try context.pools.types.new(.Void), .Const);
    }

    try context.tokenUtil.expectToken(.LBrace);

    const index = context.tokenUtil.pos.index;
    const body = try parseSequence(allocator, context, true);
    const endIndex = context.tokenUtil.pos.index;
    const bodyTokens = context.tokenUtil.tokens[index..endIndex];

    try context.tokenUtil.expectToken(.RBrace);

    return try createMut(FuncDecNode, allocator, .{
        .name = nameStr,
        .generics = generics,
        .params = params,
        .body = body,
        .bodyTokens = bodyTokens,
        .returnType = returnType,
        .capturedValues = null,
        .capturedTypes = null,
        .capturedFuncs = null,
        .toScanTypes = try utils.createMut(ToScanTypesList, allocator, .empty),
        .funcType = if (structFn) .StructMethod else .Normal,
        .visited = false,
        .globallyDefined = context.compInfo.getScopeDepth() == 1,
    });
}

fn parseGenerics(allocator: Allocator, context: *Context) ![]GenericType {
    var generics: ArrayList(GenericType) = .empty;
    defer generics.deinit(allocator);

    var current = try context.tokenUtil.peak();
    while (current.type != .RBracket) {
        const generic = try parseGeneric(allocator, context);
        try generics.append(allocator, generic);

        current = try context.tokenUtil.peak();
        if (current.type == .RBracket) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    _ = try context.tokenUtil.take();

    return try generics.toOwnedSlice(allocator);
}

fn parseGeneric(allocator: Allocator, context: *Context) !GenericType {
    const first = try context.tokenUtil.take();
    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForGenericType;
    }
    try context.compInfo.addParsedGeneric(context.getTokString(first));

    var restriction: ?AstTypeInfo = null;
    const current = try context.tokenUtil.peak();
    if (current.type == .Colon) {
        _ = try context.tokenUtil.take();
        restriction = try parseType(allocator, context);
    }

    return .{
        .name = context.getTokString(first),
        .restriction = restriction,
    };
}

fn parseParams(allocator: Allocator, context: *Context) ![]Parameter {
    var current = try context.tokenUtil.peak();

    if (current.type == .RParen) {
        _ = try context.tokenUtil.take();
        return &[_]Parameter{};
    }

    var params: ArrayList(Parameter) = .empty;
    defer params.deinit(allocator);

    while (current.type != .RParen) {
        const param = try parseParam(allocator, context);
        try params.append(allocator, param);

        current = try context.tokenUtil.peak();
        if (current.type == .RParen) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    try context.tokenUtil.expectToken(.RParen);

    return try params.toOwnedSlice(allocator);
}

fn parseParam(allocator: Allocator, context: *Context) !Parameter {
    var first = try context.tokenUtil.take();
    var isConst = true;
    if (first.type == .Mut) {
        first = try context.tokenUtil.take();
        isConst = false;
    }

    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForParameterName;
    }

    try context.tokenUtil.expectToken(.Colon);
    const paramType = try parseType(allocator, context);

    return .{
        .name = context.getTokString(first),
        .type = paramType,
        .mutState = if (isConst) .Const else .Mut,
    };
}

fn parseFuncCallParams(allocator: Allocator, context: *Context) ![]*AstNode {
    if ((try context.tokenUtil.peak()).type == .RParen) {
        _ = try context.tokenUtil.take();
        return &[_]*AstNode{};
    }

    var params: ArrayList(*AstNode) = .empty;
    defer params.deinit(allocator);

    var current = try context.tokenUtil.peak();
    while (current.type != .RParen) {
        const param = try parseExpression(allocator, context) orelse
            return AstError.ExpectedExpression;
        try params.append(allocator, param);

        current = try context.tokenUtil.peak();
        if (current.type == .RParen) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    _ = try context.tokenUtil.take();

    const ownedSlice = try params.toOwnedSlice(allocator);
    try context.deferCleanup.slices.nodeSlices.append(ownedSlice);

    return ownedSlice;
}

fn rotatePrecedence(rootExprNode: *AstNode) ?*AstNode {
    if (rootExprNode.* != .OpExpr) return null;
    const rootExpr = rootExprNode.OpExpr;
    if (rootExpr.right.* != .OpExpr) return rootExprNode;
    const rightNode = rootExpr.right;
    const rightExpr = rightNode.OpExpr;

    if (@intFromEnum(rootExpr.type) < @intFromEnum(rightExpr.type)) {
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
        .Ampersand => .BitAnd,
        .BitOr => .BitOr,
        .And => .And,
        .Or => .Or,
        .Add => .Add,
        .Sub => .Sub,
        .Asterisk => .Mult,
        .Div => .Div,
        .LAngle => .LessThan,
        .RAngle => .GreaterThan,
        .LAngleEq => .LessThanEq,
        .RAngleEq => .GreaterThanEq,
        .EqComp => .Equal,
        else => unreachable,
    };
}

pub fn mergeMembers(
    allocator: Allocator,
    context: *Context,
    attrs: []StructAttribute,
    derive: AstTypeInfo,
) ![]StructAttribute {
    var res = try ArrayList(StructAttribute).initCapacity(allocator, attrs.len);
    const structName = switch (derive.astType.*) {
        .Custom => |custom| custom.name,
        .StaticStructInstance => |inst| inst,
        else => unreachable,
    };
    const deriveDec = context.compInfo.getStructDec(structName);

    for (attrs) |attr| {
        if (attr.attr != .Member) continue;
        try res.append(allocator, attr);
    }

    if (deriveDec) |dec| {
        if (dec.deriveType) |decDerive| {
            const arr = try mergeMembers(allocator, context, dec.attributes, decDerive);
            try res.appendSlice(allocator, arr);
        } else {
            for (dec.attributes) |attr| {
                if (attr.attr != .Member) continue;
                try res.append(allocator, attr);
            }
        }
    }

    return try res.toOwnedSlice(allocator);
}

fn createBoolNode(context: *Context, value: bool) !*AstNode {
    const node = try context.pools.nodes.new(.{
        .Value = .{
            .Bool = value,
        },
    });
    return node;
}

fn createVarDecNode(
    allocator: Allocator,
    context: *Context,
    mutState: scanner.MutState,
) !?*AstNode {
    const name = try context.tokenUtil.take();
    if (name.type != .Identifier) {
        return AstError.ExpectedIdentifierForVariableName;
    }

    var annotation: ?AstTypeInfo = null;

    const next = try context.tokenUtil.take();
    if (next.type == .Colon) {
        annotation = try parseType(allocator, context);
        try context.tokenUtil.expectToken(.EqSet);
    } else if (next.type != .EqSet) {
        return TokenError.UnexpectedToken;
    }

    const setValue = try parseExpression(allocator, context) orelse return null;

    return try context.pools.nodes.new(.{
        .VarDec = .{
            .name = context.getTokString(name),
            .mutState = mutState,
            .setNode = setValue,
            .annotation = annotation,
        },
    });
}

fn parseType(
    allocator: Allocator,
    context: *Context,
) (AstError || Allocator.Error)!AstTypeInfo {
    var first = try context.tokenUtil.take();
    const mutState: scanner.MutState = if (first.type == .Mut) .Mut else .Const;
    if (mutState == .Mut) {
        first = try context.tokenUtil.take();
    }

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
        .CharType => .Char,
        .Asterisk => a: {
            const pointer = try parseType(allocator, context);
            break :a .{
                .Pointer = utils.astTypeInfoToAllocInfo(pointer, .Recycled),
            };
        },
        .QuestionMark => a: {
            const nullable = try parseType(allocator, context);
            break :a .{
                .Nullable = nullable,
            };
        },
        .Identifier => a: {
            const str = context.getTokString(first);
            if (context.compInfo.hasStruct(str)) {
                const next = try context.tokenUtil.peak();
                var generics: []AstTypeInfo = &.{};

                if (next.type == .LAngle) {
                    _ = try context.tokenUtil.take();
                    generics = try parseStructInitGenerics(allocator, context);
                }

                break :a .{
                    .Custom = .{
                        .name = str,
                        .generics = generics,
                        .allowPrivateReads = false,
                    },
                };
            } else if (context.compInfo.hasError(str)) {
                const next = try context.tokenUtil.peak();
                var generic: ?AstTypeInfo = null;

                if (next.type == .LAngle) {
                    _ = try context.tokenUtil.take();
                    generic = try parseType(allocator, context);

                    if (generic) |gen| {
                        if (gen.astType.* == .Error) {
                            return AstError.ErrorPayloadMayNotBeError;
                        }
                    } else {
                        return AstError.ExpectedTypeExpression;
                    }

                    try context.tokenUtil.expectToken(.RAngle);
                }

                break :a .{
                    .Error = .{
                        .name = str,
                        .payload = generic,
                    },
                };
            }

            if (!context.compInfo.hasParsedGeneric(str)) {
                return AstError.UnexpectedGeneric;
            }

            break :a .{
                .Generic = str,
            };
        },
        else => return TokenError.UnexpectedToken,
    };

    var next = try context.tokenUtil.peak();
    if (next.type == .LBracket) {
        _ = try context.tokenUtil.take();
        var size: ?*AstNode = null;
        next = try context.tokenUtil.peak();

        if (next.type != .RBracket) {
            size = try parseExpression(allocator, context) orelse
                return AstError.ExpectedSizeForArraySlice;
        }

        try context.tokenUtil.expectToken(.RBracket);

        const sliceType = utils.astTypesPtrToInfo(
            try context.pools.types.new(astType),
            mutState,
        );
        const newAstType = AstTypes{
            .ArraySlice = .{
                .type = utils.astTypeInfoToAllocInfo(sliceType, .Recycled),
                .size = size,
            },
        };

        astType = newAstType;
    }

    if (astType == .Generic and mutState == .Mut) {
        return AstError.UnexpectedMutSpecifierOnGeneric;
    }

    return utils.astTypesPtrToInfo(
        try context.pools.types.new(astType),
        mutState,
    );
}

pub fn findStructsAndErrors(
    allocator: Allocator,
    tokens: []tokenizer.Token,
    code: []const u8,
) !HoistedNames {
    var structNames: ArrayList([]const u8) = .empty;
    defer structNames.deinit(allocator);
    var errorNames: ArrayList([]const u8) = .empty;
    defer errorNames.deinit(allocator);

    var scopeCount: usize = 0;
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        switch (tokens[i].type) {
            .Struct => {
                if (scopeCount != 0) return AstError.StructDefinedInLowerScope;

                if (tokens[i + 1].type == .LBracket) {
                    while (i < tokens.len - 1 and tokens[i + 1].type != .RBracket) : (i += 1) {}
                    i += 1;
                }

                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForStruct;
                }

                const str = tokens[i + 1].strFromCode(code);
                try structNames.append(allocator, str);
            },
            .Error => {
                if (scopeCount != 0) return AstError.ErrorDefinedInLowerScope;

                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForError;
                }

                const str = tokens[i + 1].strFromCode(code);
                try errorNames.append(allocator, str);
            },
            .LBrace => scopeCount += 1,
            .RBrace => scopeCount -= 1,
            else => {},
        }
    }

    return .{
        .structNames = try structNames.toOwnedSlice(allocator),
        .errorNames = try errorNames.toOwnedSlice(allocator),
    };
}

pub fn registerStructsAndErrors(
    allocator: Allocator,
    context: *Context,
) !RegisterStructsAndErrorsResult {
    var structDecs: ArrayList(*StructDecNode) = .empty;
    defer structDecs.deinit(allocator);
    var errorDecs: ArrayList(*const ErrorDecNode) = .empty;
    defer errorDecs.deinit(allocator);

    while (context.tokenUtil.hasNext()) {
        const token = try context.tokenUtil.take();
        if (token.type != .Struct and token.type != .Error) {
            continue;
        }

        context.tokenUtil.returnToken();
        const node = try parseStatement(allocator, context) orelse continue;

        switch (node.*) {
            .StructDec => |dec| {
                try structDecs.append(allocator, dec);
            },
            .ErrorDec => |dec| {
                try errorDecs.append(allocator, dec);
            },
            else => unreachable,
        }
    }

    return .{
        .structs = try structDecs.toOwnedSlice(allocator),
        .errors = try errorDecs.toOwnedSlice(allocator),
    };
}
