const std = @import("std");
const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const scanner = blitz.scanner;
const compInfo = blitz.compInfo;
const logger = blitz.logger;
const pools = blitz.allocPools;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const TokenError = tokenizer.TokenError;
const Context = blitz.context.Context;
const Writer = std.Io.Writer;

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

    pub fn getSize(self: Self) u8 {
        return switch (self) {
            .Char, .U8, .I8 => 1,
            .U16, .I16 => 2,
            .U32, .I32, .F32 => 4,
            .U64, .I64, .F64 => 8,
            .U128, .I128 => 16,
        };
    }

    pub fn getAlignment(self: Self) u8 {
        return self.getSize();
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
        };

        for (rels) |rel| {
            if (utils.compString(rel.str, str)) return rel.val;
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
        };
    }
};

pub const AstArrayDecType = struct {
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
    Undef,
    Number,
    RawNumber,
    ArrayDec,
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
    const Self = @This();

    String,
    Bool,
    Char,
    Void,
    Null,
    Any,
    Undef,
    Number: AstNumberVariants,
    RawNumber: []const u8,
    ArrayDec: AstArrayDecType,
    Pointer: scanner.TypeAndAllocInfo,
    Nullable: AstTypeInfo,
    Custom: CustomType,
    Generic: []const u8,
    Function: *FuncDecNode,
    StaticStructInstance: []const u8,
    Error: ErrorAstType,
    ErrorVariant: ErrorVariantType,
    VarInfo: scanner.TypeAndAllocInfo,

    pub fn toTypeInfo(self: *Self, mutState: scanner.MutState) AstTypeInfo {
        return .{
            .astType = self,
            .mutState = mutState,
        };
    }

    pub fn toAllocInfo(
        self: *Self,
        mutState: scanner.MutState,
        allocState: scanner.AllocatedState,
    ) scanner.TypeAndAllocInfo {
        return .{
            .info = .{
                .astType = self,
                .mutState = mutState,
            },
            .allocState = allocState,
        };
    }

    pub fn getAlignment(self: Self, context: *Context) !u8 {
        return switch (self) {
            .Null,
            .RawNumber,
            .Undef,
            => unreachable,
            .Number => |num| return num.getAlignment(),

            .Bool, .Char, .ErrorVariant => 1,

            .Void,
            .Any,
            .Generic,
            .Function,
            .Error,
            => 0,

            .ArrayDec, .String, .StaticStructInstance, .Pointer => 8,

            .VarInfo => |inner| return try inner.info.astType.getAlignment(context),

            .Nullable => |inner| try inner.astType.getAlignment(context),
            .Custom => |custom| {
                const dec = context.compInfo.getStructDec(custom.name).?;

                // TODO - possibly setup generics

                var maxAlignment: u8 = 0;
                for (dec.totalMemberList) |member| {
                    if (member.attr != .Member) continue;

                    const itemAlignment = try member.attr.Member.astType.getAlignment(context);
                    maxAlignment = @max(maxAlignment, itemAlignment);
                }

                return maxAlignment;
            },
        };
    }

    pub fn getSize(self: Self, context: *Context) !u64 {
        return switch (self) {
            .Null, .RawNumber, .Undef => unreachable,
            .Void, .Any, .Generic, .Function, .Error => 0,
            .String => 16,
            .Bool, .Char, .ErrorVariant => 1,
            .Number => |num| return num.getSize(),
            .ArrayDec => 16,
            .Pointer, .StaticStructInstance => 8,
            // TODO - maybe optimize for unused states (0 for pointer etc)
            .Nullable => |inner| return try inner.astType.getSize(context) + 1,
            .Custom => |custom| {
                const dec = context.compInfo.getStructDec(custom.name).?;

                // TODO - possibly setup generics

                var size: u64 = 0;
                for (dec.totalMemberList) |member| {
                    if (member.attr != .Member) continue;

                    const itemAlignment = try member.attr.Member.astType.getAlignment(context);
                    var prepadding = if (itemAlignment == 0)
                        0
                    else
                        itemAlignment - (size % itemAlignment);
                    if (prepadding == itemAlignment) prepadding = 0;

                    const memberSize = try member.attr.Member.astType.getSize(context);
                    size += prepadding + memberSize;
                }

                return size;
            },
            .VarInfo => |inner| return try inner.info.astType.getSize(context),
        };
    }

    pub fn getNodeTypeInfo(self: Self, context: *Context) !AstNodeTypeInfo {
        return .{
            .size = try self.getSize(context),
            .alignment = try self.getAlignment(context),
        };
    }
};

pub const AstTypeInfo = struct {
    const Self = @This();

    astType: *AstTypes,
    mutState: scanner.MutState,

    pub fn toAllocInfo(self: Self, state: scanner.AllocatedState) scanner.TypeAndAllocInfo {
        return .{
            .info = self,
            .allocState = state,
        };
    }
};

const StaticTypes = enum {
    String,
    Bool,
    Char,
    Number,
    RawNumber,
    ArrayDec,
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
    ArrayDec: []*AstNode,
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

    pub fn getMemberLocation(
        self: Self,
        context: *Context,
        member: []const u8,
    ) !?u64 {
        var loc: u64 = 0;

        for (self.totalMemberList) |item| {
            if (item.attr != .Member) continue;
            const size = try item.attr.Member.astType.getSize(context);
            const alignment = try item.attr.Member.astType.getAlignment(context);
            const padding = utils.calculatePadding(
                loc,
                alignment,
            );
            loc += padding;

            if (utils.compString(item.name, member)) return loc;
            loc += size;
        }

        return null;
    }
};

pub const AttributeDefinition = struct {
    name: []const u8,
    value: *AstNode,
};

const StructInitNode = struct {
    const Self = @This();

    name: []const u8,
    attributes: []AttributeDefinition,
    generics: []AstTypeInfo,

    pub fn findAttribute(self: Self, attrName: []const u8) ?AttributeDefinition {
        for (self.attributes) |attr| {
            if (utils.compString(attrName, attr.name)) {
                return attr;
            }
        }

        return null;
    }
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

const ParseParamsResult = struct {
    params: []Parameter,
    selfInfo: ?struct { mutState: scanner.MutState },
};

const FuncType = enum {
    Builtin,
    StructMethod,
    Normal,
};

pub const FuncDecNode = struct {
    name: []const u8,
    generics: ?[]GenericType,
    params: ParseParamsResult,
    body: *AstNode,
    bodyTokens: []tokenizer.Token,
    returnType: AstTypeInfo,
    capturedValues: ?*compInfo.CaptureScope,
    capturedTypes: ?*compInfo.TypeScope,
    capturedFuncs: ?*compInfo.StringListScope,
    toScanTypes: *ToScanTypesList,
    funcType: FuncType,
    visited: bool,
    globallyDefined: bool,
};

const FuncCallNode = struct {
    callGenerics: ?[]AstTypeInfo = null,
    params: []*AstNode,
    func: *AstNode,
};

const PropertyAccess = struct {
    value: *AstNode,
    property: []const u8,
};

const ParseContextType = enum {
    Statement,
    Expression,
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
    target: *AstNode,
};

pub const ErrorDecNode = struct {
    name: []const u8,
    variants: [][]const u8,
};

const ValueSetNode = struct {
    value: *AstNode,
    setNode: *AstNode,
};

const VarEqOpNode = struct {
    /// not all op types are necessarily applicable
    opType: OpExprTypes,
    value: *AstNode,
    variable: *AstNode,
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
    UndefValue,
};

pub const AstNodeUnion = union(AstNodeVariants) {
    const Self = @This();

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
    UndefValue,

    pub fn toAstNode(self: Self) AstNode {
        return .{
            .variant = self,
        };
    }
};

const AstNodeTypeInfo = struct {
    size: u64 = 0,
    alignment: u8 = 0,
    accessingFrom: ?[]const u8 = null, // name of struct
    lastVarUse: bool = false,
};

pub const AstNode = struct {
    variant: AstNodeUnion,
    typeInfo: AstNodeTypeInfo = .{},
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
    ExpectedSizeForArrayDec,
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
    SelfStructNameNotFound,
} || TokenError;

pub const ParseError = AstError || Allocator.Error;

pub const RegisterStructsAndErrorsResult = struct {
    structs: []*AstNode,
    errors: []*AstNode,
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

    context: *Context,
    root: *AstNode,

    pub fn init(context: *Context, root: *AstNode) Self {
        return Self{
            .context = context,
            .root = root,
        };
    }

    pub fn deinit(self: *Self) void {
        pools.recursiveReleaseNodeAll(self.context, self.root);
    }
};

pub fn createAst(allocator: Allocator, context: *Context, writer: *Writer) !Ast {
    const seq = parseSequence(allocator, context, false) catch |e| {
        logger.logParseError(context, e, writer);
        return e;
    };
    return Ast.init(context, seq);
}

pub fn parseSequence(
    allocator: Allocator,
    context: *Context,
    fromBlock: bool,
) ParseError!*AstNode {
    var seq: ArrayList(*AstNode) = .empty;

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
    try context.deferCleanup.nodeSlices.append(allocator, ownedSlice);

    const seqVariant = AstNodeUnion{
        .Seq = ownedSlice,
    };
    return try context.pools.newNode(context, seqVariant.toAstNode());
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

            const ifVariant: AstNodeUnion = .{
                .IfStatement = .{
                    .condition = condition,
                    .body = seq,
                    .fallback = fallback,
                },
            };
            return try context.pools.newNode(context, ifVariant.toAstNode());
        },
        .Fn => {
            const func = try parseFuncDef(allocator, context, null);
            try context.compInfo.addFunction(func.name, func);

            const funcDecVariant: AstNodeUnion = .{
                .FuncDec = func.name,
            };
            return try context.pools.newNode(context, funcDecVariant.toAstNode());
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

            const forLoopVariant: AstNodeUnion = .{
                .ForLoop = .{
                    .initNode = initNode,
                    .condition = condition,
                    .incNode = incNode,
                    .body = body,
                },
            };
            return try context.pools.newNode(context, forLoopVariant.toAstNode());
        },
        .While => {
            try context.tokenUtil.expectToken(.LParen);

            const condition = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.RParen);
            try context.tokenUtil.expectToken(.LBrace);
            const body = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);

            const whileLoopVariant: AstNodeUnion = .{
                .WhileLoop = .{
                    .condition = condition,
                    .body = body,
                },
            };
            return try context.pools.newNode(context, whileLoopVariant.toAstNode());
        },
        .Identifier => {
            const next = try context.tokenUtil.take();
            switch (next.type) {
                .EqSet => {
                    const setNode = try parseExpression(allocator, context) orelse
                        return AstError.ExpectedExpression;

                    const variableVariant: AstNodeUnion = .{
                        .Variable = context.getTokString(first),
                    };
                    const valueSetVariant: AstNodeUnion = .{
                        .ValueSet = .{
                            .setNode = setNode,
                            .value = try context.pools.newNode(context, variableVariant.toAstNode()),
                        },
                    };
                    return try context.pools.newNode(context, valueSetVariant.toAstNode());
                },
                .AddEq,
                .SubEq,
                .MultEq,
                .DivEq,
                .AndEq,
                .OrEq,
                .BitAndEq,
                .BitOrEq,
                => {
                    const incNode = try parseExpression(allocator, context) orelse
                        return AstError.ExpectedExpression;

                    const variableVariant: AstNodeUnion = .{
                        .Variable = context.getTokString(first),
                    };

                    const varEqOpVariant: AstNodeUnion = .{
                        .VarEqOp = .{
                            .opType = tokenTypeToOpType(next.type),
                            .value = incNode,
                            .variable = try context.pools.newNode(context, variableVariant.toAstNode()),
                        },
                    };
                    return try context.pools.newNode(context, varEqOpVariant.toAstNode());
                },
                .Period => {
                    context.tokenUtil.returnToken();

                    const identStr = context.getTokString(first);
                    const nodeVariant = if (context.compInfo.hasStruct(identStr)) AstNodeUnion{
                        .StaticStructInstance = identStr,
                    } else AstNodeUnion{
                        .Variable = identStr,
                    };
                    const node = try context.pools.newNode(context, nodeVariant.toAstNode());

                    return parsePropertyAccess(allocator, context, node, .Statement);
                },
                .LBracket => {
                    context.tokenUtil.returnToken();
                    const variableVariant: AstNodeUnion = .{
                        .Variable = context.getTokString(first),
                    };
                    const variableNode = try context.pools.newNode(context, variableVariant.toAstNode());
                    return parsePropertyAccess(allocator, context, variableNode, .Statement);
                },
                .LParen => {
                    return try parseFuncCall(allocator, context, context.getTokString(first));
                },
                .LAngle => {
                    const generics = try parseInitGenerics(allocator, context);
                    try context.tokenUtil.expectToken(.LParen);
                    var funcCall = try parseFuncCall(
                        allocator,
                        context,
                        context.getTokString(first),
                    );
                    funcCall.variant.FuncCall.callGenerics = generics;
                    return funcCall;
                },
                else => {
                    const variableVariant: AstNodeUnion = .{
                        .Variable = context.getTokString(first),
                    };
                    return try context.pools.newNode(context, variableVariant.toAstNode());
                },
            }
        },
        .Return => {
            const value = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            try context.tokenUtil.expectToken(.Semicolon);

            const returnVariant: AstNodeUnion = .{
                .ReturnNode = value,
            };
            return try context.pools.newNode(context, returnVariant.toAstNode());
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

                return context.staticPtrs.nodes.noOp;
            }

            const errNode = try parseError(allocator, context);

            const errDecVariant: AstNodeUnion = .{
                .ErrorDec = errNode,
            };
            return try context.pools.newNode(context, errDecVariant.toAstNode());
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

                return context.staticPtrs.nodes.structPlaceholder;
            }

            return try parseStruct(allocator, context);
        },
        .LBrace => {
            const seq = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);
            const scopeVariant: AstNodeUnion = .{
                .Scope = seq,
            };
            return try context.pools.newNode(context, scopeVariant.toAstNode());
        },
        .Break => {
            return context.staticPtrs.nodes.breakNode;
        },
        .Continue => {
            return context.staticPtrs.nodes.continueNode;
        },
        .Delete => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const heapVariant: AstNodeUnion = .{
                .HeapFree = expr,
            };
            return try context.pools.newNode(context, heapVariant.toAstNode());
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

    const ifVariant: AstNodeUnion = .{
        .IfStatement = .{
            .condition = condition orelse context.staticPtrs.nodes.noOp,
            .body = body,
            .fallback = fallback,
        },
    };
    const node = try context.pools.newNode(context, ifVariant.toAstNode());

    return .{
        .node = node,
        .hasCondition = condition != null,
    };
}

fn parseStruct(allocator: Allocator, context: *Context) !?*AstNode {
    try context.compInfo.pushParsedGenericsScope(allocator, false);
    defer context.compInfo.popParsedGenericsScope(context);

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

    const structDecVariant: AstNodeUnion = .{
        .StructDec = try utils.createMut(StructDecNode, allocator, .{
            .name = structName,
            .generics = generics,
            .attributes = attributes,
            .totalMemberList = &[_]StructAttribute{},
            .deriveType = deriveType,
            .toScanTypes = try utils.createMut(ToScanTypesList, allocator, .empty),
        }),
    };
    return try context.pools.newNode(context, structDecVariant.toAstNode());
}

fn parseStructAttributes(
    allocator: Allocator,
    context: *Context,
    structName: []const u8,
) ![]StructAttribute {
    var attributes: ArrayList(StructAttribute) = .empty;

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
            const def = try parseFuncDef(allocator, context, structName);

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

    return try utils.create(ErrorDecNode, allocator, .{
        .name = context.getTokString(name),
        .variants = variants,
    });
}

fn parseVariants(allocator: Allocator, context: *Context) ![][]const u8 {
    var variants: ArrayList([]const u8) = .empty;

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

            const opExprVariant: AstNodeUnion = .{
                .OpExpr = .{
                    .type = tokenTypeToOpType(next.type),
                    .left = expr.?,
                    .right = after,
                    .depth = depth,
                },
            };
            break :a try context.pools.newNode(context, opExprVariant.toAstNode());
        },
        .Inc => a: {
            _ = try context.tokenUtil.take();
            const incVariant: AstNodeUnion = .{
                .IncOne = expr.?,
            };
            break :a try context.pools.newNode(context, incVariant.toAstNode());
        },
        .Dec => a: {
            _ = try context.tokenUtil.take();
            const decVariant: AstNodeUnion = .{
                .DecOne = expr.?,
            };
            break :a try context.pools.newNode(context, decVariant.toAstNode());
        },
        else => expr,
    };

    if (expr) |node| {
        if (node.variant == .OpExpr) {
            return rotatePrecedence(node);
        }
    }

    return expr;
}

pub fn getExprDepth(expr: *AstNode) usize {
    if (expr.variant == .OpExpr) {
        const left = expr.variant.OpExpr.left;
        const right = expr.variant.OpExpr.right;
        var leftDepth: usize = 0;
        var rightDepth: usize = 0;

        if (left.variant == .OpExpr) leftDepth = left.variant.OpExpr.depth;
        if (right.variant == .OpExpr) rightDepth = right.variant.OpExpr.depth;

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
        .Undef => {
            const undefVariant: AstNodeUnion = .{ .UndefValue = {} };
            return try context.pools.newNode(context, undefVariant.toAstNode());
        },
        .Null => {
            const valueVariant: AstNodeUnion = .{
                .Value = .Null,
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
        },
        .New => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const heapVariant: AstNodeUnion = .{
                .HeapAlloc = .{
                    .node = expr,
                },
            };
            return try context.pools.newNode(context, heapVariant.toAstNode());
        },
        .Number => |numType| {
            const valueVariant: AstNodeUnion = .{
                .Value = .{
                    .RawNumber = .{
                        .digits = context.getTokString(first),
                        .numType = numType,
                    },
                },
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
        },
        .NegNumber => |numType| {
            switch (numType) {
                .Char, .U8, .U16, .U32, .U64, .U128 => {
                    return AstError.NegativeNumberWithUnsignedTypeConflict;
                },
                else => {},
            }

            const valueVariant: AstNodeUnion = .{
                .Value = .{
                    .RawNumber = .{
                        .digits = context.getTokString(first),
                        .numType = numType,
                    },
                },
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
        },
        .Period => {
            const next = try context.tokenUtil.take();
            if (next.type != .Identifier) {
                return AstError.ExpectedIdentifierForErrorVariant;
            }

            const errVariant: AstNodeUnion = .{
                .InferErrorVariant = context.getTokString(next),
            };
            return try context.pools.newNode(context, errVariant.toAstNode());
        },
        .StringToken => {
            const str = context.getTokString(first);
            const next = try context.tokenUtil.peak();

            const strVariant: AstNodeUnion = .{
                .Value = .{
                    .String = str,
                },
            };
            const strNode = try context.pools.newNode(context, strVariant.toAstNode());

            if (next.type == .Period) {
                const propAccess = try parsePropertyAccess(
                    allocator,
                    context,
                    strNode,
                    .Expression,
                );
                return propAccess;
            }

            return strNode;
        },
        .CharToken => {
            const valueVariant: AstNodeUnion = .{
                .Value = .{
                    .Char = context.getTokString(first)[0],
                },
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
        },
        .Mut => {
            const next = try context.tokenUtil.peak();
            if (next.type != .Ampersand) return TokenError.UnexpectedToken;

            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            expr.variant.Pointer.mutState = .Mut;

            return expr;
        },
        .Bang => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const bangVariant: AstNodeUnion = .{
                .Bang = expr,
            };
            return try context.pools.newNode(context, bangVariant.toAstNode());
        },
        .Ampersand => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const ptrVariant: AstNodeUnion = .{
                .Pointer = .{
                    .node = expr,
                    .mutState = .Const,
                },
            };
            return try context.pools.newNode(context, ptrVariant.toAstNode());
        },
        .LParen => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            try context.tokenUtil.expectToken(.RParen);

            const groupVariant: AstNodeUnion = .{
                .Group = expr,
            };
            const groupNode = try context.pools.newNode(context, groupVariant.toAstNode());

            return try parsePropertyAccessIfPossible(allocator, context, groupNode, .Expression);
        },
        .Identifier => {
            const next = try context.tokenUtil.peak();
            switch (next.type) {
                .LParen => {
                    _ = try context.tokenUtil.take();

                    return try parseFuncCall(allocator, context, context.getTokString(first));
                },
                .LAngle => a: {
                    const tokPos = context.tokenUtil.pos;
                    _ = try context.tokenUtil.take();
                    const tokString = context.getTokString(first);
                    const generics = parseInitGenerics(allocator, context) catch {
                        context.tokenUtil.pos = tokPos;
                        break :a;
                    };
                    const openToken = try context.tokenUtil.take();
                    if (openToken.type == .LParen) {
                        var funcCall = try parseFuncCall(allocator, context, tokString);
                        funcCall.variant.FuncCall.callGenerics = generics;
                        return funcCall;
                    } else if (openToken.type == .LBrace) {
                        return try parseStructInit(allocator, context, tokString, generics);
                    }
                },
                .LBrace => {
                    _ = try context.tokenUtil.take();
                    const structName = context.getTokString(first);
                    if (context.compInfo.hasStruct(structName)) {
                        return try parseStructInit(
                            allocator,
                            context,
                            context.getTokString(first),
                            &[_]AstTypeInfo{},
                        );
                    }
                },
                .Period, .LBracket => {
                    const identNode = try getIdentNode(
                        context,
                        context.getTokString(first),
                    );
                    return try parsePropertyAccess(allocator, context, identNode, .Expression);
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

            const castVariant: AstNodeUnion = .{
                .Cast = .{
                    .node = inner,
                    .toType = toType,
                },
            };
            return try context.pools.newNode(context, castVariant.toAstNode());
        },
        else => return TokenError.UnexpectedToken,
    }
}

fn getIdentNode(context: *Context, str: []const u8) !*AstNode {
    var node: AstNodeUnion = undefined;

    if (context.compInfo.hasStruct(str)) {
        node = .{ .StaticStructInstance = str };
    } else if (context.compInfo.hasError(str)) {
        node = .{ .Error = str };
    } else {
        node = .{ .Variable = str };
    }

    return try context.pools.newNode(context, node.toAstNode());
}

fn parseArray(allocator: Allocator, context: *Context) !*AstNode {
    var current = try context.tokenUtil.peak();

    switch (current.type) {
        .RBracket => {
            _ = try context.tokenUtil.take();
            const valueVariant: AstNodeUnion = .{
                .Value = .{
                    .ArrayDec = &[_]*AstNode{},
                },
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
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

            const arrayInitVariant: AstNodeUnion = .{
                .ArrayInit = .{
                    .size = context.getTokString(current),
                    .initType = arrType,
                    .initNode = initNode,
                    .indexIdent = indexIdent,
                    .ptrIdent = ptrIdent,
                },
            };
            return context.pools.newNode(context, arrayInitVariant.toAstNode());
        },
        else => {},
    }

    var items: ArrayList(*AstNode) = .empty;

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
    try context.deferCleanup.nodeSlices.append(allocator, slice);

    const valueVariant: AstNodeUnion = .{
        .Value = .{
            .ArrayDec = slice,
        },
    };
    return try context.pools.newNode(context, valueVariant.toAstNode());
}

fn parseStructInit(
    allocator: Allocator,
    context: *Context,
    name: []const u8,
    generics: []AstTypeInfo,
) !*AstNode {
    const attributes = try parseStructInitAttributes(allocator, context);

    const structInitVariant: AstNodeUnion = .{
        .StructInit = .{
            .name = name,
            .attributes = attributes,
            .generics = generics,
        },
    };
    return try context.pools.newNode(context, structInitVariant.toAstNode());
}

fn parseStructInitAttributes(allocator: Allocator, context: *Context) ![]AttributeDefinition {
    var attributes: ArrayList(AttributeDefinition) = .empty;

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
    try context.deferCleanup.attrDefSlices.append(allocator, ownedSlice);

    return ownedSlice;
}

fn parseStructInitAttribute(allocator: Allocator, context: *Context) !AttributeDefinition {
    const first = try context.tokenUtil.take();
    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForStructProperty;
    }

    const next = try context.tokenUtil.take();
    if (next.type == .Comma or next.type == .RBrace) {
        const variableVariant: AstNodeUnion = .{
            .Variable = context.getTokString(first),
        };
        const res = AttributeDefinition{
            .name = context.getTokString(first),
            .value = try context.pools.newNode(context, variableVariant.toAstNode()),
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

fn parseInitGenerics(allocator: Allocator, context: *Context) ![]AstTypeInfo {
    var generics: ArrayList(AstTypeInfo) = .empty;

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
    try context.deferCleanup.typeInfoSlices.append(allocator, ownedSlice);

    return ownedSlice;
}

fn parsePropertyAccessIfPossible(
    allocator: Allocator,
    context: *Context,
    node: *AstNode,
    parseContext: ParseContextType,
) !*AstNode {
    const next = try context.tokenUtil.peak();

    if (parseContext == .Expression) switch (next.type) {
        .EqSet,
        .AddEq,
        .SubEq,
        .MultEq,
        .DivEq,
        .AndEq,
        .OrEq,
        .BitAndEq,
        .BitOrEq,
        => {
            _ = try context.tokenUtil.take();
            return AstError.UnexpectedToken;
        },
        else => {},
    };

    if (switch (next.type) {
        .Period,
        .LBracket,
        .LParen,

        .EqSet,
        .AddEq,
        .SubEq,
        .MultEq,
        .DivEq,
        .AndEq,
        .OrEq,
        .BitAndEq,
        .BitOrEq,
        => false,
        else => true,
    }) return node;

    return try parsePropertyAccess(allocator, context, node, parseContext);
}

fn parsePropertyAccess(
    allocator: Allocator,
    context: *Context,
    node: *AstNode,
    parseContext: ParseContextType,
) ParseError!*AstNode {
    const next = try context.tokenUtil.take();

    if (parseContext == .Expression) switch (next.type) {
        .EqSet,
        .AddEq,
        .SubEq,
        .MultEq,
        .DivEq,
        .AndEq,
        .OrEq,
        .BitAndEq,
        .BitOrEq,
        => return AstError.UnexpectedToken,
        else => {},
    };

    switch (next.type) {
        .Period => {
            const prop = try context.tokenUtil.take();

            if (prop.type == .Asterisk) {
                const derefVariant = AstNodeUnion{
                    .Dereference = node,
                };
                const derefNode = try context.pools.newNode(context, derefVariant.toAstNode());
                return try parsePropertyAccessIfPossible(
                    allocator,
                    context,
                    derefNode,
                    parseContext,
                );
            }

            if (prop.type != .Identifier) {
                return AstError.ExpectedIdentifierForPropertyAccess;
            }

            const propertyAccessVariant = AstNodeUnion{
                .PropertyAccess = .{
                    .value = node,
                    .property = context.getTokString(prop),
                },
            };
            const access = try context.pools.newNode(context, propertyAccessVariant.toAstNode());
            return try parsePropertyAccessIfPossible(allocator, context, access, parseContext);
        },
        .LBracket => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            const indexValueVariant = AstNodeUnion{
                .IndexValue = .{
                    .index = expr,
                    .target = node,
                },
            };
            const access = try context.pools.newNode(context, indexValueVariant.toAstNode());

            try context.tokenUtil.expectToken(.RBracket);
            return try parsePropertyAccessIfPossible(allocator, context, access, parseContext);
        },
        .EqSet => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            const valueSetVariant: AstNodeUnion = .{
                .ValueSet = .{
                    .value = node,
                    .setNode = expr,
                },
            };
            return try context.pools.newNode(context, valueSetVariant.toAstNode());
        },
        .AddEq,
        .SubEq,
        .MultEq,
        .DivEq,
        .AndEq,
        .OrEq,
        .BitAndEq,
        .BitOrEq,
        => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            const valueSetVariant: AstNodeUnion = .{
                .VarEqOp = .{
                    .opType = tokenTypeToOpType(next.type),
                    .variable = node,
                    .value = expr,
                },
            };
            return try context.pools.newNode(context, valueSetVariant.toAstNode());
        },
        .LParen => {
            const params = try parseFuncCallParams(allocator, context);
            const funcCallVariant: AstNodeUnion = .{
                .FuncCall = .{
                    .func = node,
                    .params = params,
                },
            };
            const access = try context.pools.newNode(context, funcCallVariant.toAstNode());
            return try parsePropertyAccessIfPossible(allocator, context, access, parseContext);
        },
        else => return AstError.UnexpectedToken,
    }
}

fn parseFuncCall(allocator: Allocator, context: *Context, name: []const u8) !*AstNode {
    const funcRefVariant: AstNodeUnion = .{
        .FuncReference = name,
    };
    const func = try context.pools.newNode(context, funcRefVariant.toAstNode());

    const params = try parseFuncCallParams(allocator, context);

    const funcCallVariant: AstNodeUnion = .{
        .FuncCall = .{
            .func = func,
            .params = params,
        },
    };
    return try context.pools.newNode(context, funcCallVariant.toAstNode());
}

fn parseFuncDef(
    allocator: Allocator,
    context: *Context,
    selfStructNameOrNull: ?[]const u8,
) !*FuncDecNode {
    try context.compInfo.pushParsedGenericsScope(allocator, true);
    defer context.compInfo.popParsedGenericsScope(context);

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

    const params = try parseParams(allocator, context, selfStructNameOrNull);
    var returnType: AstTypeInfo = undefined;

    const retNext = try context.tokenUtil.peak();

    if (retNext.type != .LBrace) {
        returnType = try parseType(allocator, context);
    } else {
        returnType = context.staticPtrs.types.voidType;
    }

    try context.tokenUtil.expectToken(.LBrace);

    const index = context.tokenUtil.pos;
    const body = try parseSequence(allocator, context, true);
    const endIndex = context.tokenUtil.pos;
    const bodyTokens = context.tokenUtil.tokens[index..endIndex];

    try context.tokenUtil.expectToken(.RBrace);

    return try utils.createMut(FuncDecNode, allocator, .{
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
        .funcType = if (selfStructNameOrNull == null) .Normal else .StructMethod,
        .visited = false,
        .globallyDefined = context.compInfo.getScopeDepth() == 1,
    });
}

fn parseGenerics(allocator: Allocator, context: *Context) ![]GenericType {
    var generics: ArrayList(GenericType) = .empty;

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
    const str = context.getTokString(first);
    try context.compInfo.addParsedGeneric(allocator, str);

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

fn parseParams(
    allocator: Allocator,
    context: *Context,
    selfStructNameOrNull: ?[]const u8,
) !ParseParamsResult {
    var res = ParseParamsResult{
        .params = &[_]Parameter{},
        .selfInfo = null,
    };
    var current = try context.tokenUtil.peak();

    if (current.type == .RParen) {
        _ = try context.tokenUtil.take();
        return res;
    }

    var params: ArrayList(Parameter) = .empty;

    while (current.type != .RParen) {
        const param = try parseParam(allocator, context, selfStructNameOrNull);
        try params.append(allocator, param);

        if (utils.compString(param.name, "self")) {
            res.selfInfo = .{ .mutState = param.mutState };
        }

        current = try context.tokenUtil.peak();
        if (current.type == .RParen) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    try context.tokenUtil.expectToken(.RParen);

    res.params = try params.toOwnedSlice(allocator);
    return res;
}

fn parseParam(
    allocator: Allocator,
    context: *Context,
    selfStructNameOrNull: ?[]const u8,
) !Parameter {
    var first = try context.tokenUtil.take();
    var isConst = true;
    if (first.type == .Mut) {
        first = try context.tokenUtil.take();
        isConst = false;
    }

    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForParameterName;
    }

    const tokString = context.getTokString(first);
    if (utils.compString(tokString, "self")) {
        const selfStructName = selfStructNameOrNull orelse return AstError.SelfStructNameNotFound;

        const typeNode = try context.pools.newType(context, .{
            .Custom = .{
                .name = selfStructName,
                .generics = &[_]AstTypeInfo{},
                .allowPrivateReads = true,
            },
        });

        return .{
            .name = tokString,
            .type = typeNode.toTypeInfo(if (isConst) .Const else .Mut),
            .mutState = if (isConst) .Const else .Mut,
        };
    }

    try context.tokenUtil.expectToken(.Colon);
    const paramType = try parseType(allocator, context);

    return .{
        .name = tokString,
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
    try context.deferCleanup.nodeSlices.append(allocator, ownedSlice);

    return ownedSlice;
}

fn rotatePrecedence(rootExprNode: *AstNode) ?*AstNode {
    if (rootExprNode.variant != .OpExpr) return null;
    const rootExpr = rootExprNode.variant.OpExpr;
    if (rootExpr.right.variant != .OpExpr) return rootExprNode;
    const rightNode = rootExpr.right;
    const rightExpr = rightNode.variant.OpExpr;

    if (@intFromEnum(rootExpr.type) < @intFromEnum(rightExpr.type)) {
        const childLeft = rightExpr.left;
        rootExprNode.variant.OpExpr.right = childLeft;
        rightNode.variant.OpExpr.left = rootExprNode;
        return rightNode;
    }

    return rootExprNode;
}

/// note: should only be called from exhaustive switch
/// types other than ones in OpExprTypes are marked unreachable
pub fn tokenTypeToOpType(tokenType: tokenizer.TokenType) OpExprTypes {
    return switch (tokenType) {
        .Ampersand => .BitAnd,
        .BitOr, .BitOrEq => .BitOr,
        .And, .AndEq => .And,
        .Or, .OrEq => .Or,
        .Sub, .SubEq => .Sub,
        .Add, .AddEq => .Add,
        .Asterisk, .MultEq => .Mult,
        .Div, .DivEq => .Div,
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
    const valueVariant: AstNodeUnion = .{
        .Value = .{
            .Bool = value,
        },
    };
    const node = try context.pools.newNode(context, valueVariant.toAstNode());
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

    const varDecVariant: AstNodeUnion = .{
        .VarDec = .{
            .name = context.getTokString(name),
            .mutState = mutState,
            .setNode = setValue,
            .annotation = annotation,
        },
    };
    return try context.pools.newNode(context, varDecVariant.toAstNode());
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
        .CharType => .Char,
        .Asterisk => a: {
            const pointer = try parseType(allocator, context);
            break :a .{
                .Pointer = pointer.toAllocInfo(.Recycled),
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
                    generics = try parseInitGenerics(allocator, context);
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
                return AstError.ExpectedSizeForArrayDec;
        }

        try context.tokenUtil.expectToken(.RBracket);

        const sliceType = (try context.pools.newType(context, astType)).toTypeInfo(mutState);
        const newAstType = AstTypes{
            .ArrayDec = .{
                .type = sliceType.toAllocInfo(.Recycled),
                .size = size,
            },
        };

        astType = newAstType;
    }

    if (astType == .Generic and mutState == .Mut) {
        return AstError.UnexpectedMutSpecifierOnGeneric;
    }

    const resType = try context.pools.newType(context, astType);
    return resType.toTypeInfo(mutState);
}

pub fn findStructsAndErrors(
    allocator: Allocator,
    tokens: []tokenizer.Token,
    code: []const u8,
) !HoistedNames {
    var structNames: ArrayList([]const u8) = .empty;
    var errorNames: ArrayList([]const u8) = .empty;

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
    var structDecs: ArrayList(*AstNode) = .empty;
    var errorDecs: ArrayList(*AstNode) = .empty;

    while (context.tokenUtil.hasNext()) {
        const token = try context.tokenUtil.take();
        if (token.type != .Struct and token.type != .Error) {
            continue;
        }

        context.tokenUtil.returnToken();
        const node = try parseStatement(allocator, context) orelse continue;

        switch (node.variant) {
            .StructDec => {
                try structDecs.append(allocator, node);
            },
            .ErrorDec => {
                try errorDecs.append(allocator, node);
            },
            else => unreachable,
        }
    }

    return .{
        .structs = try structDecs.toOwnedSlice(allocator),
        .errors = try errorDecs.toOwnedSlice(allocator),
    };
}
