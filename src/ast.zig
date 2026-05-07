const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;

const blitz = @import("blitz.zig");
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const scanner = blitz.scanner;
const compInfo = blitz.compInfo;
const logger = blitz.logger;
const pools = blitz.allocPools;
const TokenError = tokenizer.TokenError;
const Context = blitz.context.Context;
const constants = blitz.constants;
const vmInfo = blitz.vmInfo;
const identStore = blitz.identStore;

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
            if (std.mem.eql(u8, rel.str, str)) return rel.val;
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

const NodeIndexOrU64Variants = enum {
    Node,
    U64,
};

pub const NodeIndexOrU64 = union(NodeIndexOrU64Variants) {
    Node: *AstNode,
    U64: u64,
};

pub const AstArrayDecType = struct {
    type: scanner.TypeAndAllocInfo,
    size: ?NodeIndexOrU64,
};

pub const CustomType = struct {
    nameIdentId: identStore.IdentId,
    generics: []AstTypeInfo,
    allowPrivateReads: bool,
};

const EnumVariantType = struct {
    fromIdentId: ?identStore.IdentId,
    variantIdentId: identStore.IdentId,
};

const ErrorVariantType = struct {
    fromIdentId: identStore.IdentId,
    variantIdentId: identStore.IdentId,
};

const ErrorAstType = struct {
    nameIdentId: identStore.IdentId,
    payload: ?AstTypeInfo,
};

const StructMethodInfo = struct {
    customSrc: CustomType,
    func: *FuncDecNode,
};

const Types = enum {
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
    StructMethod,
    StaticStructInstance,
    Error,
    EnumVariant,
    ErrorVariant,
    Enum,
    VarInfo,
};

pub const AstTypes = union(Types) {
    const Self = @This();

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
    Generic: identStore.IdentId,
    Function: *FuncDecNode,
    StructMethod: StructMethodInfo,
    StaticStructInstance: identStore.IdentId,
    Error: ErrorAstType,
    EnumVariant: EnumVariantType,
    ErrorVariant: ErrorVariantType,
    Enum: identStore.IdentId,
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

    pub fn getAlignment(self: Self, allocator: Allocator, context: *Context) !u8 {
        return switch (self) {
            .Null,
            .RawNumber,
            .Undef,
            => unreachable,

            .Number => |num| return num.getAlignment(),

            .Bool, .Char, .EnumVariant, .ErrorVariant => 1,

            .Void,
            .Any,
            .Function,
            .StructMethod,
            .Error,
            .Enum,
            => 0,

            .Generic => |name| {
                const genType = try context.compInfo.getGeneric(allocator, context, name) orelse
                    return 0;

                // generic loop detection, if generic loops become indirect, this
                // should be more involved
                if (genType.info.astType.* == .Generic) {
                    const nextType = try context.compInfo.getGeneric(
                        allocator,
                        context,
                        genType.info.astType.Generic,
                    ) orelse return 0;
                    if (genType.info.astType == nextType.info.astType) return 0;
                }

                return try genType.info.astType.getAlignment(allocator, context);
            },

            .ArrayDec, .StaticStructInstance, .Pointer => 8,

            .VarInfo => |inner| try inner.info.astType.getAlignment(allocator, context),

            .Nullable => |inner| try inner.astType.getAlignment(allocator, context),
            .Custom => |custom| {
                const dec = context.compInfo.getStructDec(custom.nameIdentId).?;

                var maxAlignment: u8 = 0;
                for (dec.totalMemberList) |member| {
                    if (member.attr != .Member) continue;

                    const itemAlignment = try member.attr.Member.astType.getAlignment(
                        allocator,
                        context,
                    );
                    maxAlignment = @max(maxAlignment, itemAlignment);
                }

                return maxAlignment;
            },
        };
    }

    pub fn getSize(self: Self, allocator: Allocator, context: *Context) !u64 {
        return switch (self) {
            .Null, .RawNumber, .Undef => unreachable,
            .Void, .Any, .Function, .StructMethod, .Error, .Enum => 0,
            .Bool, .Char, .EnumVariant, .ErrorVariant => 1,
            .Number => |num| num.getSize(),
            .Pointer => |ptr| {
                if (ptr.info.astType.* == .ArrayDec and ptr.info.astType.ArrayDec.size != null) {
                    return 16;
                }
                return 8;
            },
            .StaticStructInstance => 8,
            .Generic => |name| {
                const genType = try context.compInfo.getGeneric(allocator, context, name) orelse
                    return 0;

                // generic loop detection, if generic loops become indirect, this
                // should be more involved
                if (genType.info.astType.* == .Generic) {
                    const nextType = try context.compInfo.getGeneric(
                        allocator,
                        context,
                        genType.info.astType.Generic,
                    ) orelse return 0;
                    if (genType.info.astType == nextType.info.astType) return 0;
                }

                return try genType.info.astType.getSize(allocator, context);
            },
            // TODO - maybe optimize for unused states (0 for pointer etc)
            .Nullable => |inner| try inner.astType.getSize(allocator, context) + 1,
            .ArrayDec => |dec| {
                if (dec.size) |size| {
                    const arrSize = scanner.indexNumberFromNode(size) catch {
                        return AstTypeError.ExpectedU64OrU32ForArrayDecSize;
                    };

                    const itemSize = try dec.type.info.astType.getSize(allocator, context);
                    return arrSize * itemSize;
                }

                return 16;
            },
            .Custom => |custom| {
                const dec = context.compInfo.getStructDec(custom.nameIdentId).?;

                var size: u64 = 0;
                for (dec.totalMemberList) |member| {
                    if (member.attr != .Member) continue;

                    const itemAlignment = try member.attr.Member.astType.getAlignment(
                        allocator,
                        context,
                    );
                    var prePadding = if (itemAlignment == 0)
                        0
                    else
                        itemAlignment - (size % itemAlignment);
                    if (prePadding == itemAlignment) prePadding = 0;

                    const memberSize = try member.attr.Member.astType.getSize(allocator, context);
                    size += prePadding + memberSize;
                }

                return size;
            },
            .VarInfo => |inner| try inner.info.astType.getSize(allocator, context),
        };
    }

    pub fn getNodeTypeInfo(self: Self, allocator: Allocator, context: *Context) !AstNodeTypeInfo {
        return .{
            .size = try self.getSize(allocator, context),
            .alignment = try self.getAlignment(allocator, context),
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
    nameIdentId: identStore.IdentId,
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
    nameIdentId: identStore.IdentId,
    attr: StructAttributeUnion,
    visibility: MemberVisibility,
    static: bool,
};

pub const StrToTypeInfoRel = struct {
    identId: identStore.IdentId,
    info: AstTypeInfo,
};

pub const ToScanTypesList = ArrayList([]StrToTypeInfoRel);

pub const StructDecNode = struct {
    const Self = @This();

    nameIdentId: identStore.IdentId,
    generics: []GenericType,
    attributes: []StructAttribute,
    totalMemberList: []StructAttribute = &[_]StructAttribute{},
    totalMethodList: []StructAttribute = &[_]StructAttribute{},
    toScanTypes: *ToScanTypesList,
    endPos: usize,

    pub fn getMemberLocation(
        self: Self,
        allocator: Allocator,
        context: *Context,
        memberIdentId: identStore.IdentId,
    ) !?u64 {
        var loc: u64 = 0;

        for (self.totalMemberList) |item| {
            const size = try item.attr.Member.astType.getSize(allocator, context);
            const alignment = try item.attr.Member.astType.getAlignment(allocator, context);
            const padding = utils.calculatePadding(
                loc,
                alignment,
            );
            loc += padding;

            if (item.nameIdentId == memberIdentId) return loc;
            loc += size;
        }

        return null;
    }

    pub fn isPropFunction(self: Self, propertyIdentId: identStore.IdentId) bool {
        for (self.totalMethodList) |member| {
            if (member.nameIdentId == propertyIdentId) return true;
        }

        return false;
    }
};

pub const AttributeDefinition = struct {
    nameIdentId: identStore.IdentId,
    value: *AstNode,
};

const StructInitNode = struct {
    const Self = @This();

    nameIdentId: identStore.IdentId,
    attributes: []AttributeDefinition,
    generics: []AstTypeInfo,

    pub fn findAttribute(self: Self, attrIdentId: identStore.IdentId) ?AttributeDefinition {
        for (self.attributes) |attr| {
            if (attrIdentId == attr.nameIdentId) return attr;
        }

        return null;
    }
};

pub const GenericType = struct {
    nameIdentId: identStore.IdentId,
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
    nameIdentId: identStore.IdentId,
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

const GenericFuncInstance = struct {
    labelId: ?u32 = null,
    funcRootNode: *AstNode,
};

const FuncGenericStateVariants = enum {
    Generic,
    Normal,
};

const FuncGenericState = union(FuncGenericStateVariants) {
    Generic: struct {
        generics: []GenericType,
        genericInstances: *ArrayList(GenericFuncInstance),
    },
    Normal: struct {
        labelId: ?u32 = null,
    },
};

pub const FuncDecNode = struct {
    const Self = @This();

    labelsGenerated: bool = false,
    capturedTypes: ?*compInfo.TypeScope = null,
    capturedFuncs: ?*compInfo.IdentIdListScope = null,
    capturedVariables: ?*compInfo.CaptureScope = null,
    methodOn: ?identStore.IdentId = null,
    visited: bool = false,
    nameIdentId: identStore.IdentId,
    params: ParseParamsResult,
    body: *AstNode,
    bodyTokens: []tokenizer.Token,
    returnType: AstTypeInfo,
    definedCaptures: []FuncCaptures,
    toScanTypes: *ToScanTypesList,
    funcType: FuncType,
    globallyDefined: bool,
    genericState: FuncGenericState,

    pub fn onGenericStruct(self: Self, context: *Context) bool {
        const methodOn = self.methodOn orelse return false;
        const structDec = context.compInfo.getStructDec(methodOn) orelse return false;
        return structDec.generics.len > 0;
    }
};

const FuncParseStructInfo = struct {
    nameIdentId: identStore.IdentId,
    isGeneric: bool,
    isStatic: bool,
};

const FuncCallNode = struct {
    callGenerics: ?[]AstTypeInfo = null,
    params: []*AstNode,
    func: *AstNode,
};

const FuncCaptures = struct {
    identId: identStore.IdentId,
    isPtr: bool,
    mutState: scanner.MutState,
};

const PropertyAccess = struct {
    value: *AstNode,
    property: identStore.IdentId,
};

const ParseContextType = enum {
    Statement,
    Expression,
};

pub const OpExprTypes = enum(u8) {
    const Self = @This();

    BitAnd = 0,
    BitOr = 1,
    LessThan = 2,
    GreaterThan = 3,
    LessThanEq = 4,
    GreaterThanEq = 5,
    Equal = 6,
    NotEqual = 7,
    And = 8,
    Or = 9,
    Mult = 10,
    Div = 11,
    Add = 12,
    Sub = 13,

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

pub const ErrorOrEnumDecNode = struct {
    nameIdentId: identStore.IdentId,
    variants: []identStore.IdentId,
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
    indexIdentId: ?identStore.IdentId,
    ptrIdentId: ?identStore.IdentId,
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
    InferEnumVariant,
    EnumDec,
    Enum,
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
    Variable: identStore.IdentId,
    StructDec: *StructDecNode,
    IfStatement: IfStatementNode,
    FuncDec: identStore.IdentId,
    FuncCall: FuncCallNode,
    ReturnNode: *AstNode,
    StructInit: StructInitNode,
    Bang: *AstNode,
    PropertyAccess: PropertyAccess,
    StaticStructInstance: identStore.IdentId,
    FuncReference: identStore.IdentId,
    OpExpr: OpExpr,
    IndexValue: IndexValueNode,
    ErrorDec: *const ErrorOrEnumDecNode,
    Error: identStore.IdentId,
    InferEnumVariant: identStore.IdentId,
    EnumDec: *const ErrorOrEnumDecNode,
    Enum: identStore.IdentId,
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

const AstTypeInfoDataVariant = enum {
    PropertyAccess,
    VarOrVarDec,
    ArrDecPtr,
    Others,
};

const AstTypeInfoData = union(AstTypeInfoDataVariant) {
    PropertyAccess: identStore.IdentId,
    VarOrVarDec: struct {
        lastVarUse: bool = false,
    },
    ArrDecPtr: struct {
        makesSliceWithLen: ?u64 = null,
    },
    Others: struct {
        resolvesToFunc: ?*FuncDecNode = null,
        funcGenInstanceIndex: ?u32 = null,
    },
};

pub const AstTypeInfoNodeType = enum {
    Slice,
    Struct,
    Other,
};

pub const AstNodeTypeInfo = struct {
    size: u64 = 0,
    alignment: u8 = 0,
    nodeType: AstTypeInfoNodeType = .Other,
    data: AstTypeInfoData = .{ .Others = .{} },
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
    NegativeNumberWithUnsignedTypeConflict,
    ExpectedIdentifierForArrayInitIndex,
    ExpectedIdentifierForArrayInitPtr,
    SelfStructNameNotFound,
    UnexpectedSelfParamOnStaticFunction,
    ExpectedIdentifierForEnumName,
    ExpectedIdentifierForEnumVariant,
    EnumDefinedInLowerScope,
    ExpectedNameForEnum,
    StructMethodsCannotDefineCaptureGroups,
    EmptyFunctionCaptures,
    ExpectedUniqueStructDecAttribute,
} || TokenError;

pub const ParseError = AstError || Allocator.Error;

pub const AstTypeError = error{
    ExpectedU64OrU32ForArrayDecSize,
};

pub const HoistedNodes = struct {
    structs: []*AstNode,
    errors: []*AstNode,
    enums: []*AstNode,
};

pub const HoistedNames = struct {
    structIdentIds: []identStore.IdentId,
    errorIdentIds: []identStore.IdentId,
    enumIdentIds: []identStore.IdentId,
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
    const seq = parseModule(allocator, context) catch |e| {
        logger.logParseError(context, e, writer);
        return e;
    };
    return Ast.init(context, seq);
}

pub fn parseModule(allocator: Allocator, context: *Context) !*AstNode {
    var seq: ArrayList(*AstNode) = .empty;

    while (context.tokenUtil.hasNext()) {
        var peakToken = try context.tokenUtil.peakFixed();
        while (peakToken.type == .Semicolon or peakToken.type == .NewLine) {
            _ = try context.tokenUtil.takeFixed();
            peakToken = try context.tokenUtil.peakFixed();
            continue;
        }

        const node = switch (peakToken.type) {
            // TODO - make export keyword to export fn
            .Fn => a: {
                _ = try context.tokenUtil.take();
                break :a try handleParseFunction(allocator, context);
            },
            .Struct => {
                _ = try context.tokenUtil.take();
                const name = try context.tokenUtil.take();
                if (name.type != .Identifier) return AstError.UnexpectedToken;
                const dec = context.compInfo.getStructDec(name.identId).?;
                context.tokenUtil.pos = dec.endPos;
                continue;
            },
            .Enum => try handleParseEnum(allocator, context),
            else => {
                _ = try context.tokenUtil.take();
                return AstError.UnexpectedToken;
            },
        };

        try seq.append(allocator, node);
    }

    const ownedSlice = try seq.toOwnedSlice(allocator);
    try context.deferCleanup.nodeSlices.append(allocator, ownedSlice);

    const seqVariant = AstNodeUnion{
        .Seq = ownedSlice,
    };
    return try context.pools.newNode(context, seqVariant.toAstNode());
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

            const ifVariant = AstNodeUnion{
                .IfStatement = .{
                    .condition = condition,
                    .body = seq,
                    .fallback = fallback,
                },
            };
            return try context.pools.newNode(context, ifVariant.toAstNode());
        },
        .Fn => return try handleParseFunction(allocator, context),
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

            const forLoopVariant = AstNodeUnion{
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

            const whileLoopVariant = AstNodeUnion{
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

                    const variableVariant = AstNodeUnion{
                        .Variable = first.identId,
                    };
                    const valueSetVariant = AstNodeUnion{
                        .ValueSet = .{
                            .setNode = setNode,
                            .value = try context.pools.newNode(
                                context,
                                variableVariant.toAstNode(),
                            ),
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

                    const variableVariant = AstNodeUnion{
                        .Variable = first.identId,
                    };

                    const varEqOpVariant = AstNodeUnion{
                        .VarEqOp = .{
                            .opType = tokenTypeToOpType(next.type),
                            .value = incNode,
                            .variable = try context.pools.newNode(
                                context,
                                variableVariant.toAstNode(),
                            ),
                        },
                    };
                    return try context.pools.newNode(context, varEqOpVariant.toAstNode());
                },
                .Period => {
                    context.tokenUtil.returnToken();

                    const nodeVariant = if (context.compInfo.hasStruct(first.identId)) AstNodeUnion{
                        .StaticStructInstance = first.identId,
                    } else AstNodeUnion{
                        .Variable = first.identId,
                    };
                    const node = try context.pools.newNode(context, nodeVariant.toAstNode());

                    return parsePropertyAccess(allocator, context, node, .Statement);
                },
                .LBracket => {
                    context.tokenUtil.returnToken();
                    const variableVariant = AstNodeUnion{
                        .Variable = first.identId,
                    };
                    const variableNode = try context.pools.newNode(
                        context,
                        variableVariant.toAstNode(),
                    );
                    return parsePropertyAccess(allocator, context, variableNode, .Statement);
                },
                .LParen => {
                    return try parseFuncCall(allocator, context, first.identId);
                },
                .LAngle => {
                    const generics = try parseInitGenerics(allocator, context);
                    try context.tokenUtil.expectToken(.LParen);
                    var funcCall = try parseFuncCall(allocator, context, first.identId);
                    funcCall.variant.FuncCall.callGenerics = generics;
                    return funcCall;
                },
                else => {
                    const variableVariant = AstNodeUnion{
                        .Variable = first.identId,
                    };
                    return try context.pools.newNode(context, variableVariant.toAstNode());
                },
            }
        },
        .Return => {
            const value = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            try context.tokenUtil.expectToken(.Semicolon);

            const returnVariant = AstNodeUnion{
                .ReturnNode = value,
            };
            return try context.pools.newNode(context, returnVariant.toAstNode());
        },
        .Error => return AstError.ErrorDefinedInLowerScope,
        .Struct => return AstError.StructDefinedInLowerScope,
        .Enum => return AstError.EnumDefinedInLowerScope,
        .LBrace => {
            const seq = try parseSequence(allocator, context, true);
            try context.tokenUtil.expectToken(.RBrace);
            const scopeVariant = AstNodeUnion{
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
            const heapVariant = AstNodeUnion{
                .HeapFree = expr,
            };
            return try context.pools.newNode(context, heapVariant.toAstNode());
        },
        else => {
            return TokenError.UnexpectedToken;
        },
    }
}

fn handleParseEnum(allocator: Allocator, context: *Context) !*AstNode {
    const enumDec = try parseEnumDec(allocator, context);
    const enumDecVariant = AstNodeUnion{
        .EnumDec = enumDec,
    };
    return context.pools.newNode(context, enumDecVariant.toAstNode());
}

fn handleParseError(allocator: Allocator, context: *Context) !*AstNode {
    const errNode = try parseError(allocator, context);
    const errDecVariant = AstNodeUnion{
        .ErrorDec = errNode,
    };
    return try context.pools.newNode(context, errDecVariant.toAstNode());
}

fn handleParseFunction(allocator: Allocator, context: *Context) !*AstNode {
    const name = try context.tokenUtil.peak();
    if (name.type != .Identifier) return AstError.UnexpectedToken;

    const func = if (context.compInfo.getFunctionAsGlobal(name.identId)) |func| a: {
        const tokens = func.bodyTokens;
        const last = tokens[tokens.len - 1];
        context.tokenUtil.pos = last.end;
        break :a func;
    } else a: {
        const func = try parseFuncDef(allocator, context, null);
        try context.compInfo.addFunction(func.nameIdentId, func);
        break :a func;
    };

    const funcDecVariant = AstNodeUnion{
        .FuncDec = func.nameIdentId,
    };
    return try context.pools.newNode(context, funcDecVariant.toAstNode());
}

fn parseEnumDec(allocator: Allocator, context: *Context) !*const ErrorOrEnumDecNode {
    const name = try context.tokenUtil.take();
    if (name.type != .Identifier) {
        return AstError.ExpectedIdentifierForEnumName;
    }

    try context.tokenUtil.expectToken(.LBrace);
    const variants = try parseVariants(allocator, context);

    return try utils.create(ErrorOrEnumDecNode, allocator, .{
        .nameIdentId = name.identId,
        .variants = variants,
    });
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

    const ifVariant = AstNodeUnion{
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

fn parseStructDec(allocator: Allocator, context: *Context) !*AstNode {
    try context.compInfo.pushParsedGenericsScope(allocator, false);
    defer context.compInfo.popParsedGenericsScope(context);

    var generics: []GenericType = &[_]GenericType{};

    var current = try context.tokenUtil.take();
    if (current.type != .Identifier) {
        return AstError.ExpectedIdentifierForStructName;
    }
    const identToken = current;
    current = try context.tokenUtil.take();

    if (current.type == .LBracket) {
        generics = try parseGenerics(allocator, context);
        current = try context.tokenUtil.take();
    }

    if (current.type != .LBrace) {
        return TokenError.UnexpectedToken;
    }

    const attributes = try parseStructAttributes(
        allocator,
        context,
        identToken.identId,
        generics.len > 0,
    );
    context.compInfo.attributeSet.clearRetainingCapacity();

    const structDecVariant = AstNodeUnion{
        .StructDec = try utils.createMut(StructDecNode, allocator, .{
            .nameIdentId = identToken.identId,
            .generics = generics,
            .attributes = attributes,
            .toScanTypes = try utils.createMut(ToScanTypesList, allocator, .empty),
            .endPos = context.tokenUtil.pos,
        }),
    };
    return try context.pools.newNode(context, structDecVariant.toAstNode());
}

fn parseStructAttributes(
    allocator: Allocator,
    context: *Context,
    structIdentId: identStore.IdentId,
    isGeneric: bool,
) ![]StructAttribute {
    var attributes: ArrayList(StructAttribute) = .empty;

    var current = try context.tokenUtil.peak();
    while (current.type != .RBrace) {
        const prePos = context.tokenUtil.pos;
        const attr = try parseStructAttribute(allocator, context, structIdentId, isGeneric);
        try attributes.append(allocator, attr);

        if (context.compInfo.attributeSet.contains(attr.nameIdentId)) {
            context.tokenUtil.pos = prePos;
            const next = try context.tokenUtil.take();
            if (next.type == .Pub or next.type == .Prot) {
                _ = try context.tokenUtil.take();
            }
            return AstError.ExpectedUniqueStructDecAttribute;
        }

        try context.compInfo.attributeSet.put(attr.nameIdentId, {});

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
    structIdentId: identStore.IdentId,
    isGeneric: bool,
) !StructAttribute {
    const first = try context.tokenUtil.take();
    switch (first.type) {
        .Identifier, .Fn => {
            context.tokenUtil.returnToken();
            return parseStructAttributeUtil(allocator, context, structIdentId, .Private, isGeneric);
        },
        .Prot => return parseStructAttributeUtil(
            allocator,
            context,
            structIdentId,
            .Protected,
            isGeneric,
        ),
        .Pub => return parseStructAttributeUtil(
            allocator,
            context,
            structIdentId,
            .Public,
            isGeneric,
        ),
        else => {
            return TokenError.UnexpectedToken;
        },
    }
}

fn parseStructAttributeUtil(
    allocator: Allocator,
    context: *Context,
    structIdentId: identStore.IdentId,
    visibility: MemberVisibility,
    isGeneric: bool,
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
                .nameIdentId = first.identId,
                .attr = .{
                    .Member = attrType,
                },
                .visibility = visibility,
                .static = static,
            };
        },
        .Fn => {
            const def = try parseFuncDef(
                allocator,
                context,
                .{
                    .nameIdentId = structIdentId,
                    .isGeneric = isGeneric,
                    .isStatic = static,
                },
            );

            return .{
                .nameIdentId = def.nameIdentId,
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

fn parseError(allocator: Allocator, context: *Context) !*const ErrorOrEnumDecNode {
    const name = try context.tokenUtil.take();
    if (name.type != .Identifier) {
        return AstError.ExpectedIdentifierForErrorName;
    }

    var variants: []identStore.IdentId = &[_]identStore.IdentId{};

    const next = try context.tokenUtil.take();
    if (next.type == .LBrace) {
        variants = try parseVariants(allocator, context);
    } else if (next.type != .Semicolon) {
        return TokenError.UnexpectedToken;
    }

    return try utils.create(ErrorOrEnumDecNode, allocator, .{
        .nameIdentId = name.identId,
        .variants = variants,
    });
}

fn parseVariants(allocator: Allocator, context: *Context) ![]identStore.IdentId {
    var variants: ArrayList(identStore.IdentId) = .empty;

    var variant = try context.tokenUtil.take();
    while (variant.type == .Identifier) {
        const comma = try context.tokenUtil.take();
        if (comma.type != .RBrace and comma.type != .Comma) {
            return TokenError.UnexpectedToken;
        }

        try variants.append(allocator, variant.identId);

        if (comma.type == .RBrace) break;
        variant = try context.tokenUtil.take();
    }

    return variants.items;
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

            const opExprVariant = AstNodeUnion{
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
            const incVariant = AstNodeUnion{
                .IncOne = expr.?,
            };
            break :a try context.pools.newNode(context, incVariant.toAstNode());
        },
        .Dec => a: {
            _ = try context.tokenUtil.take();
            const decVariant = AstNodeUnion{
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
            const undefVariant = AstNodeUnion{ .UndefValue = {} };
            return try context.pools.newNode(context, undefVariant.toAstNode());
        },
        .Null => {
            const valueVariant = AstNodeUnion{
                .Value = .Null,
            };
            return try context.pools.newNode(context, valueVariant.toAstNode());
        },
        .New => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const heapVariant = AstNodeUnion{
                .HeapAlloc = .{
                    .node = expr,
                },
            };
            return try context.pools.newNode(context, heapVariant.toAstNode());
        },
        .Number => |numType| {
            const valueVariant = AstNodeUnion{
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

            const valueVariant = AstNodeUnion{
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

            const errOrEnumVariant = AstNodeUnion{
                .InferEnumVariant = next.identId,
            };
            return try context.pools.newNode(context, errOrEnumVariant.toAstNode());
        },
        .StringToken => {
            const str = context.getTokString(first);
            const next = try context.tokenUtil.peak();

            const strVariant = AstNodeUnion{
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
            const valueVariant = AstNodeUnion{
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
            const bangVariant = AstNodeUnion{
                .Bang = expr,
            };
            return try context.pools.newNode(context, bangVariant.toAstNode());
        },
        .Ampersand => {
            const expr = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;
            const ptrVariant = AstNodeUnion{
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

            const groupVariant = AstNodeUnion{
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

                    return try parseFuncCall(allocator, context, first.identId);
                },
                .LAngle => a: {
                    const tokPos = context.tokenUtil.pos;
                    _ = try context.tokenUtil.take();
                    const generics = parseInitGenerics(allocator, context) catch {
                        context.tokenUtil.pos = tokPos;
                        break :a;
                    };
                    const openToken = try context.tokenUtil.take();
                    if (openToken.type == .LParen) {
                        var funcCall = try parseFuncCall(allocator, context, first.identId);
                        funcCall.variant.FuncCall.callGenerics = generics;
                        return funcCall;
                    } else if (openToken.type == .LBrace) {
                        return try parseStructInit(allocator, context, first.identId, generics);
                    }
                },
                .LBrace => {
                    _ = try context.tokenUtil.take();
                    if (context.compInfo.hasStruct(first.identId)) {
                        return try parseStructInit(
                            allocator,
                            context,
                            first.identId,
                            &[_]AstTypeInfo{},
                        );
                    }
                },
                .Period, .LBracket => {
                    const identNode = try getIdentNode(context, first.identId);
                    return try parsePropertyAccess(allocator, context, identNode, .Expression);
                },
                else => {},
            }

            return try getIdentNode(context, first.identId);
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

            const castVariant = AstNodeUnion{
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

fn getIdentNode(context: *Context, identId: identStore.IdentId) !*AstNode {
    var node: AstNodeUnion = undefined;

    if (context.compInfo.hasStruct(identId)) {
        node = .{ .StaticStructInstance = identId };
    } else if (context.compInfo.hasEnum(identId)) {
        node = .{ .Enum = identId };
    } else if (context.compInfo.hasError(identId)) {
        node = .{ .Error = identId };
    } else {
        node = .{ .Variable = identId };
    }

    return try context.pools.newNode(context, node.toAstNode());
}

fn parseArray(allocator: Allocator, context: *Context) !*AstNode {
    var current = try context.tokenUtil.peak();

    switch (current.type) {
        .RBracket => {
            _ = try context.tokenUtil.take();
            const valueVariant = AstNodeUnion{
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

            var indexIdentId: ?identStore.IdentId = null;
            var ptrIdentId: ?identStore.IdentId = null;
            var hasIdents = false;
            if ((try context.tokenUtil.peak()).type == .LParen) b: {
                hasIdents = true;

                _ = try context.tokenUtil.take();
                const indexToken = try context.tokenUtil.take();
                if (indexToken.type != .Identifier) {
                    return AstError.ExpectedIdentifierForArrayInitIndex;
                }
                indexIdentId = indexToken.identId;

                if ((try context.tokenUtil.peak()).type != .Comma) {
                    break :b;
                }

                _ = try context.tokenUtil.take();

                const ptrToken = try context.tokenUtil.take();
                if (ptrToken.type != .Identifier) {
                    return AstError.ExpectedIdentifierForArrayInitPtr;
                }
                ptrIdentId = ptrToken.identId;
            }

            if (hasIdents) {
                try context.tokenUtil.expectToken(.RParen);
            }

            const initNode = try parseExpression(allocator, context) orelse
                return AstError.ExpectedExpression;

            if (numType != .U64) {
                return AstError.ExpectedU64ForArraySize;
            }

            const arrayInitVariant = AstNodeUnion{
                .ArrayInit = .{
                    .size = context.getTokString(current),
                    .initType = arrType,
                    .initNode = initNode,
                    .indexIdentId = indexIdentId,
                    .ptrIdentId = ptrIdentId,
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

        current = try context.tokenUtil.peak();
        if (current.type == .RBracket) break;

        try context.tokenUtil.expectToken(.Comma);
        current = try context.tokenUtil.peak();
    }

    try context.tokenUtil.expectToken(.RBracket);

    const slice = try items.toOwnedSlice(allocator);
    try context.deferCleanup.nodeSlices.append(allocator, slice);

    const valueVariant = AstNodeUnion{
        .Value = .{
            .ArrayDec = slice,
        },
    };
    return try context.pools.newNode(context, valueVariant.toAstNode());
}

fn parseStructInit(
    allocator: Allocator,
    context: *Context,
    nameIdentId: identStore.IdentId,
    generics: []AstTypeInfo,
) !*AstNode {
    const attributes = try parseStructInitAttributes(allocator, context);

    const structInitVariant = AstNodeUnion{
        .StructInit = .{
            .nameIdentId = nameIdentId,
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
        const variableVariant = AstNodeUnion{
            .Variable = first.identId,
        };
        const res = AttributeDefinition{
            .nameIdentId = first.identId,
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
        .nameIdentId = first.identId,
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
                    .property = prop.identId,
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

            const valueSetVariant = AstNodeUnion{
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

            const valueSetVariant = AstNodeUnion{
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
            const funcCallVariant = AstNodeUnion{
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

fn parseFuncCall(
    allocator: Allocator,
    context: *Context,
    nameIdentId: identStore.IdentId,
) !*AstNode {
    const funcRefVariant = AstNodeUnion{
        .FuncReference = nameIdentId,
    };
    const func = try context.pools.newNode(context, funcRefVariant.toAstNode());

    const params = try parseFuncCallParams(allocator, context);

    const funcCallVariant = AstNodeUnion{
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
    structInfoOrNull: ?FuncParseStructInfo,
) !*FuncDecNode {
    try context.compInfo.pushParsedGenericsScope(allocator, true);
    defer context.compInfo.popParsedGenericsScope(context);

    var next = try context.tokenUtil.take();
    var nameIdentId: identStore.IdentId = undefined;
    var genericsOrNull: ?[]GenericType = null;
    var captures: []FuncCaptures = &[_]FuncCaptures{};

    if (next.type == .Identifier) {
        nameIdentId = next.identId;
        next = try context.tokenUtil.peak();
    } else {
        return AstError.ExpectedIdentifierForFunctionName;
    }

    if (next.type == .LBracket) {
        _ = try context.tokenUtil.take();
        genericsOrNull = try parseGenerics(allocator, context);
    }

    try context.tokenUtil.expectToken(.LParen);

    const params = try parseParams(allocator, context, structInfoOrNull);
    var returnType: AstTypeInfo = undefined;

    var retNext = try context.tokenUtil.peak();

    if (retNext.type == .LBracket) {
        if (structInfoOrNull != null) {
            return AstError.StructMethodsCannotDefineCaptureGroups;
        }

        _ = try context.tokenUtil.take();
        captures = try parseFuncCaptures(allocator, context);

        retNext = try context.tokenUtil.peak();
    }

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

    const fromGenericStruct = if (structInfoOrNull) |info| info.isGeneric else false;
    const genericState: FuncGenericState = if (fromGenericStruct or genericsOrNull != null) a: {
        const generics: []GenericType = if (genericsOrNull) |generics|
            generics
        else
            &[_]GenericType{};
        break :a .{
            .Generic = .{
                .generics = generics,
                .genericInstances = try utils.createMut(
                    ArrayList(GenericFuncInstance),
                    allocator,
                    .empty,
                ),
            },
        };
    } else .{
        .Normal = .{},
    };

    return try utils.createMut(FuncDecNode, allocator, .{
        .nameIdentId = nameIdentId,
        .params = params,
        .body = body,
        .bodyTokens = bodyTokens,
        .returnType = returnType,
        .definedCaptures = captures,
        .toScanTypes = try utils.createMut(ToScanTypesList, allocator, .empty),
        .funcType = if (structInfoOrNull == null) .Normal else .StructMethod,
        .globallyDefined = context.compInfo.getScopeDepth() == 1,
        .methodOn = if (structInfoOrNull) |info| info.nameIdentId else null,
        .genericState = genericState,
    });
}

fn parseFuncCaptures(allocator: Allocator, context: *Context) ![]FuncCaptures {
    var res: ArrayList(FuncCaptures) = .empty;

    var current = try context.tokenUtil.peak();
    if (current.type == .RBracket) {
        return AstError.EmptyFunctionCaptures;
    }

    while (current.type != .RBracket) {
        var mutState: scanner.MutState = .Const;
        if (current.type == .Mut) {
            mutState = .Mut;
            _ = try context.tokenUtil.take();
            current = try context.tokenUtil.peak();
        }

        const isPtr = switch (current.type) {
            .Ampersand => a: {
                _ = try context.tokenUtil.take();
                current = try context.tokenUtil.peak();
                break :a true;
            },
            .Identifier => false,
            else => {
                _ = try context.tokenUtil.take();
                return AstError.UnexpectedToken;
            },
        };
        if (current.type != .Identifier) {
            return AstError.UnexpectedToken;
        }

        try res.append(allocator, .{
            .identId = current.identId,
            .isPtr = isPtr,
            .mutState = mutState,
        });

        _ = try context.tokenUtil.take();
        current = try context.tokenUtil.peak();
        if (current.type == .RBracket) break;

        try context.tokenUtil.expectToken(.Comma);
    }

    _ = try context.tokenUtil.take();

    return try res.toOwnedSlice(allocator);
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

    try context.tokenUtil.expectToken(.RBracket);

    return try generics.toOwnedSlice(allocator);
}

fn parseGeneric(allocator: Allocator, context: *Context) !GenericType {
    const first = try context.tokenUtil.take();
    if (first.type != .Identifier) {
        return AstError.ExpectedIdentifierForGenericType;
    }
    try context.compInfo.addParsedGeneric(allocator, first.identId);

    var restriction: ?AstTypeInfo = null;
    const current = try context.tokenUtil.peak();
    if (current.type == .Colon) {
        _ = try context.tokenUtil.take();
        restriction = try parseType(allocator, context);
    }

    return .{
        .nameIdentId = first.identId,
        .restriction = restriction,
    };
}

fn parseParams(
    allocator: Allocator,
    context: *Context,
    structInfoOrNull: ?FuncParseStructInfo,
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
        const param = try parseParam(allocator, context, structInfoOrNull);
        try params.append(allocator, param);

        if (param.nameIdentId == identStore.KNOWN_IDENT_IDS.self) {
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
    structInfoOrNull: ?FuncParseStructInfo,
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

    if (first.identId == identStore.KNOWN_IDENT_IDS.self) {
        if (structInfoOrNull) |structInfo| {
            if (structInfo.isStatic) {
                return AstError.UnexpectedSelfParamOnStaticFunction;
            }

            const typeNode = try context.pools.newType(context, .{
                .Custom = .{
                    .nameIdentId = structInfo.nameIdentId,
                    .generics = &[_]AstTypeInfo{},
                    .allowPrivateReads = true,
                },
            });

            return .{
                .nameIdentId = first.identId,
                .type = typeNode.toTypeInfo(if (isConst) .Const else .Mut),
                .mutState = if (isConst) .Const else .Mut,
            };
        } else {
            return AstError.SelfStructNameNotFound;
        }
    }

    try context.tokenUtil.expectToken(.Colon);
    const paramType = try parseType(allocator, context);

    return .{
        .nameIdentId = first.identId,
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

    if (@intFromEnum(rootExpr.type) < @intFromEnum(rightExpr.type) or
        (rootExpr.type == .Sub and rightExpr.type == .Sub))
    {
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

fn createBoolNode(context: *Context, value: bool) !*AstNode {
    const valueVariant = AstNodeUnion{
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

    const varDecVariant = AstNodeUnion{
        .VarDec = .{
            .nameIdentId = name.identId,
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
            if (context.compInfo.hasStruct(first.identId)) {
                const next = try context.tokenUtil.peak();
                var generics: []AstTypeInfo = &.{};

                if (next.type == .LAngle) {
                    _ = try context.tokenUtil.take();
                    generics = try parseInitGenerics(allocator, context);
                }

                break :a .{
                    .Custom = .{
                        .nameIdentId = first.identId,
                        .generics = generics,
                        .allowPrivateReads = false,
                    },
                };
            } else if (context.compInfo.hasEnum(first.identId)) {
                break :a .{
                    .Enum = first.identId,
                };
            } else if (context.compInfo.hasError(first.identId)) {
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
                        .nameIdentId = first.identId,
                        .payload = generic,
                    },
                };
            }

            if (!context.compInfo.hasParsedGeneric(first.identId)) {
                return AstError.UnexpectedGeneric;
            }

            break :a .{
                .Generic = first.identId,
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
                .size = if (size) |sizeNode| .{ .Node = sizeNode } else null,
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

pub fn findHoistedInfo(allocator: Allocator, tokens: []tokenizer.Token) !HoistedNames {
    var structNames: ArrayList(identStore.IdentId) = .empty;
    var errorNames: ArrayList(identStore.IdentId) = .empty;
    var enumNames: ArrayList(identStore.IdentId) = .empty;

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

                try structNames.append(allocator, tokens[i + 1].identId);
            },
            .Error => {
                if (scopeCount != 0) return AstError.ErrorDefinedInLowerScope;

                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForError;
                }

                try errorNames.append(allocator, tokens[i + 1].identId);
            },
            .Enum => {
                if (scopeCount != 0) return AstError.EnumDefinedInLowerScope;

                if (tokens[i + 1].type != .Identifier) {
                    return AstError.ExpectedNameForEnum;
                }

                try enumNames.append(allocator, tokens[i + 1].identId);
            },
            .LBrace => scopeCount += 1,
            .RBrace => scopeCount -= 1,
            else => {},
        }
    }

    return .{
        .structIdentIds = structNames.items,
        .errorIdentIds = errorNames.items,
        .enumIdentIds = enumNames.items,
    };
}

pub fn hoistRelevantNodes(
    allocator: Allocator,
    context: *Context,
) !HoistedNodes {
    var structDecs: ArrayList(*AstNode) = .empty;
    var errorDecs: ArrayList(*AstNode) = .empty;
    var enumDecs: ArrayList(*AstNode) = .empty;

    while (context.tokenUtil.hasNext()) {
        const token = try context.tokenUtil.peak();

        switch (token.type) {
            .Struct => {
                _ = try context.tokenUtil.take();
                const structNode = try parseStructDec(allocator, context);
                try structDecs.append(allocator, structNode);
            },
            .Error => {
                const errorNode = try handleParseError(allocator, context);
                try errorDecs.append(allocator, errorNode);
            },
            .Enum => {
                const enumNode = try handleParseEnum(allocator, context);
                try enumDecs.append(allocator, enumNode);
            },
            else => {
                _ = try context.tokenUtil.take();
            },
        }
    }

    return .{
        .structs = structDecs.items,
        .errors = errorDecs.items,
        .enums = enumDecs.items,
    };
}
