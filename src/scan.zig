const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const builtins = blitz.builtins;
const clone = blitz.clone;
const blitzCompInfo = blitz.compInfo;
const vmInfo = blitz.vmInfo;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;

pub const ScanInfo = struct {
    allowErrorWithoutVariants: bool = false,
    allowStaticStructInstance: bool = false,
};

pub const RetNodeInfo = struct {
    node: blitzAst.AstNode,
    conditional: bool,
};

pub const MutState = enum {
    const Self = @This();

    Mut,
    Const,

    pub fn orMut(self: Self, other: MutState) Self {
        if (self == .Mut) return self;
        return other;
    }

    pub fn orConst(self: Self, other: MutState) Self {
        if (self == .Const) return self;
        return other;
    }
};

pub const AllocatedState = enum {
    Recycled,
    Allocated,
};

pub const TypeAndAllocInfo = struct {
    info: blitzAst.AstTypeInfo,
    allocState: AllocatedState,
};

const MutMatchBehavior = enum {
    Assign, // allows mut to const
    Strict, // must match exactly
};

pub const ScanNodeError = Allocator.Error || ScanError || clone.CloneError;

pub const ScanError = error{
    // misc
    ScanStartedInLowerScope,
    InvalidCast,
    ExpectedBooleanBang,
    ExpectedBooleanIfCondition,
    UnsupportedFeature,
    ExpectedU64ForIndex,
    StaticStructInstanceCannotBeUsedAsVariable,
    InvalidNumber,
    IfStatementMayOnlyHaveOneElse,
    ElseBranchOutOfOrder,
    NestedVarInfoDetected,
    RawNumberTooBigForType,
    CannotSetGenericToVarInfo,

    // pointers
    PointerTypeMismatch,
    CannotDereferenceNonPointerValue,
    CannotTakePointerOfRawValue,
    CannotFreeNonPointerType,

    // arrays
    ArraySliceTypeMismatch,
    ExpectedArrayForIndexTarget,
    ExpectedU64OrU32ForArraySliceSize,
    ExpectedEqualArraySliceSizes,
    SizedSliceSetToUnknownSizedSlice,
    ArrayInitTypeInitializerMismatch,

    // loops
    ExpectedBooleanLoopCondition,
    LoopControlFlowUsedOutsideOfLoop,

    // variables
    VariableAnnotationMismatch,
    VariableAlreadyExists,
    VoidVariableDec,
    VariableTypeAndValueTypeMismatch,
    AssigningToConstVariable,
    PointerTypeConstMismatch,
    StrictMutTypeMismatch,
    InvalidSetValueTarget,
    UndefVariableRequiresAnnotation,

    // functions
    ExpectedFunctionReturn,
    FunctionCallParamTypeMismatch,
    FunctionCallParamCountMismatch,
    FunctionReturnTypeMismatch,
    IdentifierNotAFunction,
    CannotCallNonFunctionNode,
    VariableIsUndefined,
    FunctionNotInScope,
    FunctionReturnsHaveDifferentTypes,
    FunctionReturnIsNotExhaustive,
    FunctionMissingReturn,
    UnexpectedReturnStatement,
    ExpectedMutableParameter,
    CallGenericsAndFuncDecGenericCountMismatch,
    UnexpectedCallGenerics,

    // structs
    StructInitGenericCountMismatch,
    StructInitAttributeCountMismatch,
    StructInitMemberTypeMismatch,
    StructInitAttributeNotFound,
    InvalidProperty,
    StaticAccessFromStructInstance,
    NonStaticAccessFromStaticStructReference,
    SelfUsedOutsideStruct,
    UndefinedStruct,
    RestrictedPropertyAccess,
    InvalidPropertySource,
    NonPublicStructFieldAccessFromOutsideDefinition,
    GenericStructMethodRedefiningStructGeneric,

    // operations
    MathOpOnNonNumberType,
    MathOpTypeMismatch,
    ExpectedBoolInBoolOp,
    InvalidBitOperation,
    BitMaskWithMismatchingSize,
    NumberTypeMismatch,
    ComparisonOnNonNumberType,
    CannotIncDecNonNumberType,

    // generics
    EmptyGenericType,
    CustomGenericMismatch,
    ConflictingGenericParameters,
    GenericRestrictionConflict,
    UnexpectedRecursiveGeneric,

    // errors
    ExpectedUseOfErrorVariants,
    ErrorDoesNotHaveVariants,
    ErrorVariantDoesNotExist,
};

const StructInitMemberInfo = struct {
    initInfo: TypeAndAllocInfo,
    defInfo: blitzAst.AstTypeInfo,
};

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, context: *Context) !void {
    while (context.compInfo.variableScopes.scopes.items.len > 1) {
        return ScanError.ScanStartedInLowerScope;
    }

    try context.compInfo.pushScope(false);
    defer context.compInfo.popScope();

    try scanNodeForFunctions(allocator, context, ast.root);
    const res = try scanNode(allocator, context, ast.root, true);
    defer releaseIfAllocated(context, res);

    try scanFunctionCalls(allocator, context);
}

pub fn scanNode(
    allocator: Allocator,
    context: *Context,
    node: *blitzAst.AstNode,
    withGenDef: bool,
) ScanNodeError!TypeAndAllocInfo {
    switch (node.variant) {
        .NoOp, .ErrorDec => {
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .UndefValue => return context.staticPtrs.types.undefType.toAllocInfo(.Recycled),
        .StaticStructInstance => |inst| {
            if (!context.scanInfo.allowStaticStructInstance) {
                return ScanError.StaticStructInstanceCannotBeUsedAsVariable;
            }

            const structInstanceType = try context.pools.types.new(.{
                .StaticStructInstance = inst,
            });
            return structInstanceType.toAllocInfo(.Const, .Allocated);
        },
        .Cast => |cast| {
            const clonedCastResult = try clone.replaceGenericsOnTypeInfo(
                allocator,
                context,
                cast.toType.toAllocInfo(.Recycled),
                withGenDef,
            );

            if (cast.node.variant == .Value and cast.node.variant.Value == .RawNumber) {
                return clonedCastResult;
            }

            const origNodeType = try scanNode(allocator, context, cast.node, withGenDef);
            defer releaseIfAllocated(context, origNodeType);
            const nodeType = try escapeVarInfo(origNodeType);

            if (isAnyType(nodeType.info.astType) or
                isPrimitive(nodeType.info.astType) and
                    isPrimitive(cast.toType.astType))
            {
                return clonedCastResult;
            }

            if (nodeType.info.astType.* == .ArraySlice and cast.toType.astType.* == .ArraySlice) {
                return clonedCastResult;
            }

            return ScanError.InvalidCast;
        },
        .Value => |val| {
            const valueRes: blitzAst.AstTypes = switch (val) {
                .Null => .Null,
                .String => .String,
                .Bool => a: {
                    node.typeInfo.size = try (blitzAst.AstTypes{ .Bool = {} }).getSize(context);
                    break :a .Bool;
                },
                .Char => a: {
                    node.typeInfo.size = try (blitzAst.AstTypes{ .Char = {} }).getSize(context);
                    break :a .Char;
                },
                .Number => |num| a: {
                    const numVariant = num.toAstNumberVariant();
                    node.typeInfo.size = numVariant.getSize();
                    break :a .{ .Number = numVariant };
                },
                .RawNumber => |num| a: {
                    if (!verifyRawNumberMagnitude(num)) {
                        return ScanError.RawNumberTooBigForType;
                    }
                    node.typeInfo.size = num.numType.getSize();
                    break :a .{ .Number = num.numType };
                },
                .ArraySlice => |arr| {
                    const inferredType = try inferArraySliceType(
                        allocator,
                        context,
                        arr,
                        withGenDef,
                    );
                    node.typeInfo = try inferredType.info.astType.getNodeTypeInfo(context);
                    const valueVariant: blitzAst.AstNodeUnion = .{
                        .Value = .{
                            .Number = .{ .U64 = arr.len },
                        },
                    };
                    const arraySliceType = try context.pools.types.new(.{
                        .ArraySlice = .{
                            .type = inferredType,
                            .size = try context.pools.nodes.new(valueVariant.toAstNode()),
                        },
                    });

                    const sliceSize = vmInfo.POINTER_SIZE * 2;
                    const inferredTypeSize = try inferredType.info.astType.getSize(context);
                    node.typeInfo.size = inferredTypeSize * arr.len + sliceSize;

                    return arraySliceType.toAllocInfo(inferredType.info.mutState, .Allocated);
                },
            };

            const valueType = try context.pools.types.new(valueRes);
            node.typeInfo = try valueType.getNodeTypeInfo(context);
            return valueType.toAllocInfo(.Mut, .Allocated);
        },
        .IndexValue => |indexInfo| {
            const origIndexType = try scanNode(allocator, context, indexInfo.index, withGenDef);
            defer releaseIfAllocated(context, origIndexType);
            const indexType = try escapeVarInfo(origIndexType);
            const origTargetType = try scanNode(allocator, context, indexInfo.target, withGenDef);
            defer releaseIfAllocated(context, origTargetType);
            const targetType = try escapeVarInfo(origTargetType);

            if (indexType.info.astType.* == .Number and indexType.info.astType.Number != .U64) {
                return ScanError.ExpectedU64ForIndex;
            }

            if (targetType.info.astType.* == .ArraySlice) {
                const arr = targetType.info.astType.*.ArraySlice;
                const resType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    arr.type,
                    withGenDef,
                );
                node.typeInfo.size = try resType.info.astType.getSize(context);
                return resType;
            }

            return ScanError.ExpectedArrayForIndexTarget;
        },
        .OpExpr => |op| {
            const origLeft = try scanNode(allocator, context, op.left, withGenDef);
            const left = try escapeVarInfoAndRelease(context, origLeft);
            const origRight = try scanNode(allocator, context, op.right, withGenDef);
            const right = try escapeVarInfoAndRelease(context, origRight);

            switch (op.type) {
                .BitAnd, .BitOr => {
                    if (left.info.astType.* != .Number or right.info.astType.* != .Number) {
                        return ScanError.InvalidBitOperation;
                    }

                    const leftSize = left.info.astType.Number.getSize();
                    const rightSize = right.info.astType.Number.getSize();
                    if (leftSize != rightSize) {
                        return ScanError.BitMaskWithMismatchingSize;
                    }

                    releaseIfAllocated(context, right);
                    const resType = try clone.replaceGenericsOnTypeInfoAndRelease(
                        allocator,
                        context,
                        left,
                        withGenDef,
                    );
                    node.typeInfo.size = try resType.info.astType.getSize(context);
                    return resType;
                },
                .And, .Or => {
                    if (left.info.astType.* != .Bool or right.info.astType.* != .Bool) {
                        return ScanError.ExpectedBoolInBoolOp;
                    }

                    node.typeInfo.size = try (blitzAst.AstTypes{ .Bool = {} }).getSize(context);

                    releaseIfAllocated(context, left);
                    releaseIfAllocated(context, right);

                    return context.staticPtrs.types.boolType.toAllocInfo(.Recycled);
                },
                .Add, .Sub, .Mult, .Div => {
                    if (isAnyType(left.info.astType)) {
                        if (right.info.astType.* != .Number and !isAnyType(right.info.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(
                            allocator,
                            context,
                            left.info,
                            right.info,
                            withGenDef,
                        );
                        if (matches) {
                            releaseIfAllocated(context, left);
                            var res = try clone.replaceGenericsOnTypeInfoAndRelease(
                                allocator,
                                context,
                                right,
                                withGenDef,
                            );
                            res.info.mutState = .Mut;
                            node.typeInfo.size = try res.info.astType.getSize(context);
                            return res;
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (isAnyType(right.info.astType)) {
                        if (left.info.astType.* != .Number and !isAnyType(left.info.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(
                            allocator,
                            context,
                            left.info,
                            right.info,
                            withGenDef,
                        );
                        if (matches) {
                            releaseIfAllocated(context, right);
                            var resType = try clone.replaceGenericsOnTypeInfoAndRelease(
                                allocator,
                                context,
                                left,
                                withGenDef,
                            );
                            node.typeInfo.size = try resType.info.astType.getSize(context);
                            return resType;
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (left.info.astType.* != .Number or right.info.astType.* != .Number) {
                        return ScanError.MathOpOnNonNumberType;
                    }

                    const leftEnum = @intFromEnum(left.info.astType.Number);
                    const rightEnum = @intFromEnum(right.info.astType.Number);
                    if (leftEnum != rightEnum) {
                        return ScanError.NumberTypeMismatch;
                    }

                    node.typeInfo.size = left.info.astType.Number.getSize();

                    if (op.type == .Div) {
                        const isFloat = switch (left.info.astType.Number) {
                            .F32, .F64, .F128 => true,
                            else => false,
                        };

                        if (isFloat) {
                            releaseIfAllocated(context, right);
                            return left;
                        }

                        return context.staticPtrs.types.f32Type.toAllocInfo(.Recycled);
                    }

                    releaseIfAllocated(context, right);
                    return left;
                },
                .LessThan,
                .GreaterThan,
                .LessThanEq,
                .GreaterThanEq,
                .Equal,
                .NotEqual,
                => {
                    if (!isAnyType(left.info.astType) and !isAnyType(right.info.astType)) {
                        if (left.info.astType.* != .Number or right.info.astType.* != .Number) {
                            return ScanError.ComparisonOnNonNumberType;
                        }

                        const leftEnum = @intFromEnum(left.info.astType.Number);
                        const rightEnum = @intFromEnum(right.info.astType.Number);
                        if (leftEnum != rightEnum) {
                            return ScanError.NumberTypeMismatch;
                        }
                    }

                    node.typeInfo.size = try (blitzAst.AstTypes{ .Bool = {} }).getSize(context);

                    releaseIfAllocated(context, left);
                    releaseIfAllocated(context, right);
                    return context.staticPtrs.types.boolType.toAllocInfo(.Recycled);
                },
            }
        },
        .IncOne, .DecOne => |val| {
            const valType = try scanNode(allocator, context, val, withGenDef);
            if (valType.info.astType.* != .Number) return ScanError.CannotIncDecNonNumberType;
            if (valType.info.astType.* != .VarInfo) return ScanError.InvalidSetValueTarget;
            if (valType.info.mutState == .Const) return ScanError.AssigningToConstVariable;
            return valType;
        },
        .Group,
        => |val| {
            const resType = try scanNode(allocator, context, val, withGenDef);
            node.typeInfo.size = try resType.info.astType.getSize(context);
            return resType;
        },
        .ReturnNode => |ret| {
            if (!context.compInfo.returnInfo.info.inFunction) {
                return ScanError.UnexpectedReturnStatement;
            }

            const origValType = try scanNode(allocator, context, ret, withGenDef);
            const valType = try escapeVarInfoAndRelease(context, origValType);
            const retTypeInfo = try clone.cloneAstTypeInfo(
                allocator,
                context,
                valType.info,
                withGenDef,
            );
            const retType = retTypeInfo.toAllocInfo(.Allocated);

            releaseIfAllocated(context, valType);

            if (context.compInfo.returnInfo.info.retType) |prevRetType| {
                const matches = try matchTypes(
                    allocator,
                    context,
                    prevRetType.info,
                    valType.info,
                    withGenDef,
                );
                if (!matches) {
                    return ScanError.FunctionReturnsHaveDifferentTypes;
                }

                releaseIfAllocated(context, retType);
            } else {
                context.compInfo.returnInfo.info.retType = retType;
            }

            context.compInfo.returnInfo.info.exhaustive = true;
            context.compInfo.returnInfo.info.lockExhaustive = true;

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncReference => |ref| {
            const dec = try context.compInfo.getFunction(ref);

            if (dec) |funcRef| {
                const funcType = try context.pools.types.new(.{
                    .Function = funcRef,
                });
                return funcType.toAllocInfo(.Const, .Allocated);
            }

            return ScanError.IdentifierNotAFunction;
        },
        .PropertyAccess => |access| {
            // TODO - node size for this
            if (access.value.variant == .Error) {
                context.scanInfo.allowErrorWithoutVariants = true;
            }

            if (access.value.variant == .StaticStructInstance) {
                context.scanInfo.allowStaticStructInstance = true;
            }

            const origValueInfo = try scanNode(allocator, context, access.value, withGenDef);
            defer releaseIfAllocated(context, origValueInfo);
            const valueInfo = try escapeVarInfo(origValueInfo);
            context.scanInfo.allowStaticStructInstance = false;
            context.scanInfo.allowErrorWithoutVariants = false;

            const valid: bool = switch (valueInfo.info.astType.*) {
                .Generic => {
                    var anyType = context.staticPtrs.types.anyType;
                    anyType.mutState = valueInfo.info.mutState;
                    return anyType.toAllocInfo(.Recycled);
                },
                .ArraySlice => {
                    const propType = try builtins.getArraySlicePropType(
                        context,
                        access.property,
                    );
                    return propType.toAllocInfo(.Recycled);
                },
                .String => {
                    const propType = try builtins.getStringPropType(
                        context,
                        access.property,
                    );
                    return propType.toAllocInfo(.Recycled);
                },
                .Custom => |custom| a: {
                    const def = context.compInfo.getStructDec(custom.name) orelse break :a false;

                    try context.compInfo.pushGenScope(false);
                    defer context.compInfo.popGenScope();

                    const defGenerics = def.generics[0..custom.generics.len];
                    for (custom.generics, defGenerics) |customGen, genDef| {
                        const clonedGenType = try clone.replaceGenericsOnTypeInfo(
                            allocator,
                            context,
                            customGen.toAllocInfo(.Recycled),
                            withGenDef,
                        );
                        try context.compInfo.setGeneric(genDef.name, clonedGenType);
                    }

                    const propType = try validateCustomProps(
                        allocator,
                        context,
                        custom,
                        access.property,
                        withGenDef,
                    );

                    if (propType) |t| {
                        var copy = t;
                        copy.info.mutState = valueInfo.info.mutState;
                        const varInfo = try context.pools.types.new(.{
                            .VarInfo = copy,
                        });
                        return varInfo.toAllocInfo(
                            origValueInfo.info.mutState.orConst(valueInfo.info.mutState),
                            .Allocated,
                        );
                    }

                    break :a false;
                },
                .StaticStructInstance => |name| a: {
                    try context.compInfo.pushGenScope(false);
                    defer context.compInfo.popGenScope();
                    var propType = try validateStaticStructProps(
                        allocator,
                        context,
                        name,
                        access.property,
                    );

                    if (propType) |t| {
                        if (!utils.compString(name, "self")) {
                            const dec = context.compInfo.getStructDec(name).?;

                            if (t.astType.* == .Function) {
                                for (dec.generics) |gen| {
                                    if (gen.restriction) |restriction| {
                                        const typeClone = try clone.replaceGenericsOnTypeInfo(
                                            allocator,
                                            context,
                                            restriction.toAllocInfo(.Recycled),
                                            false,
                                        );
                                        try context.compInfo.setGeneric(gen.name, typeClone);
                                    }
                                }
                            }
                        }

                        propType.?.mutState = valueInfo.info.mutState;
                        return t.toAllocInfo(.Allocated);
                    }

                    break :a false;
                },
                .Error => |err| {
                    const errDec = context.compInfo.getErrorDec(err.name).?;
                    if (errDec.variants.len > 0) {
                        if (!utils.inStringArr(errDec.variants, access.property)) {
                            return ScanError.ErrorVariantDoesNotExist;
                        }
                    } else {
                        return ScanError.ErrorDoesNotHaveVariants;
                    }

                    const errType = try context.pools.types.new(.{
                        .ErrorVariant = .{
                            .from = err.name,
                            .variant = access.property,
                        },
                    });
                    return errType.toAllocInfo(valueInfo.info.mutState, .Allocated);
                },
                else => false,
            };

            if (!valid) {
                return ScanError.InvalidProperty;
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .Seq => |seq| {
            for (seq) |seqNode| {
                const res = try scanNode(allocator, context, seqNode, withGenDef);
                releaseIfAllocated(context, res);
            }
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .VarDec => |dec| {
            if (context.compInfo.getVariableTypeFixed(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            const origSetType = try scanNode(allocator, context, dec.setNode, withGenDef);
            var setType = try escapeVarInfoAndRelease(context, origSetType);

            if (dec.setNode.variant == .UndefValue) {
                if (dec.annotation == null) {
                    return ScanError.UndefVariableRequiresAnnotation;
                }
            } else if (setType.info.astType.* == .Void) {
                return ScanError.VoidVariableDec;
            }

            if (dec.annotation) |annotation| {
                const matches = try matchTypes(
                    allocator,
                    context,
                    annotation,
                    setType.info,
                    false,
                );
                if (!matches) {
                    return ScanError.VariableAnnotationMismatch;
                }

                releaseIfAllocated(context, setType);
                setType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    annotation.toAllocInfo(.Recycled),
                    withGenDef,
                );
            }

            try context.compInfo.setVariableType(dec.name, setType, dec.mutState);
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .ValueSet => |set| {
            const origValType = try scanNode(allocator, context, set.value, withGenDef);
            defer releaseIfAllocated(context, origValType);
            if (set.value.variant != .Dereference and origValType.info.astType.* != .VarInfo) {
                return ScanError.InvalidSetValueTarget;
            }
            if (origValType.info.mutState == .Const) return ScanError.AssigningToConstVariable;
            const valType = try escapeVarInfo(origValType);

            const setType = try scanNode(allocator, context, set.setNode, withGenDef);
            defer releaseIfAllocated(context, setType);

            const matches = try matchTypes(
                allocator,
                context,
                valType.info,
                setType.info,
                withGenDef,
            );
            if (!matches) {
                return ScanError.VariableTypeAndValueTypeMismatch;
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .VarEqOp => |op| {
            switch (op.opType) {
                .AddEq, .SubEq, .MultEq, .DivEq => {
                    const varType = try context.compInfo.getVariableType(op.variable, withGenDef);
                    const variable = if (varType) |val|
                        val
                    else {
                        return ScanError.VariableIsUndefined;
                    };

                    if (variable.info.mutState == .Const) {
                        return ScanError.AssigningToConstVariable;
                    }

                    const left = variable;
                    const right = try scanNode(allocator, context, op.value, withGenDef);
                    defer releaseIfAllocated(context, right);

                    if (isAnyType(left.info.astType)) {
                        if (right.info.astType.* != .Number and !isAnyType(right.info.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(
                            allocator,
                            context,
                            left.info,
                            right.info,
                            withGenDef,
                        );
                        if (!matches) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (isAnyType(right.info.astType)) {
                        if (left.info.astType.* != .Number and !isAnyType(right.info.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(
                            allocator,
                            context,
                            left.info,
                            right.info,
                            withGenDef,
                        );
                        if (!matches) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    const leftEnum = @intFromEnum(left.info.astType.Number);
                    const rightEnum = @intFromEnum(right.info.astType.Number);
                    if (leftEnum != rightEnum) {
                        return ScanError.NumberTypeMismatch;
                    }
                },
                else => {},
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .Variable => |name| {
            const varInfo = try context.compInfo.getVariableType(name, withGenDef);

            if (varInfo) |info| {
                const res = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    info,
                    withGenDef,
                );
                node.typeInfo.size = try info.info.astType.getSize(context);
                return res;
            }

            std.debug.print("undefined :: {s}\n", .{name});
            return ScanError.VariableIsUndefined;
        },
        .StructPlaceholder => return context.staticPtrs.types.voidType.toAllocInfo(.Recycled),
        .StructDec => |dec| {
            try scanAttributes(context, dec);
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .IfStatement => |statement| {
            try context.compInfo.pushScope(true);
            defer context.compInfo.popScope();
            try scanNodeForFunctions(allocator, context, statement.body);
            const prev = try context.compInfo.returnInfo.newInfo(false);

            const origConditionType = try scanNode(
                allocator,
                context,
                statement.condition,
                withGenDef,
            );
            defer releaseIfAllocated(context, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.info.astType.* != .Bool and statement.condition.variant != .NoOp) {
                return ScanError.ExpectedBooleanIfCondition;
            }

            const bodyRes = try scanNode(allocator, context, statement.body, withGenDef);
            defer releaseIfAllocated(context, bodyRes);

            try context.compInfo.returnInfo.collapse(context, prev, withGenDef);

            if (statement.fallback) |fallback| {
                if (!context.compInfo.returnInfo.hasType()) {
                    context.compInfo.returnInfo.setExhaustive(false);
                }

                try scanIfFallback(allocator, context, fallback, withGenDef);
            } else {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .ForLoop => |loop| {
            try context.compInfo.pushScopeWithType(true, .Loop);
            defer context.compInfo.popScope();
            const prev = try context.compInfo.returnInfo.newInfo(false);

            if (loop.initNode) |init| {
                const res = try scanNode(allocator, context, init, withGenDef);
                defer releaseIfAllocated(context, res);
            }

            const origConditionType = try scanNode(allocator, context, loop.condition, withGenDef);
            defer releaseIfAllocated(context, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.info.astType.* != .Bool) {
                return ScanError.ExpectedBooleanLoopCondition;
            }

            const incNodeType = try scanNode(allocator, context, loop.incNode, withGenDef);
            defer releaseIfAllocated(context, incNodeType);
            const bodyRes = try scanNode(allocator, context, loop.body, withGenDef);
            defer releaseIfAllocated(context, bodyRes);

            try context.compInfo.returnInfo.collapse(context, prev, withGenDef);
            if (context.compInfo.returnInfo.hasType()) {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .WhileLoop => |loop| {
            try context.compInfo.pushScopeWithType(true, .Loop);
            defer context.compInfo.popScope();
            const prev = try context.compInfo.returnInfo.newInfo(false);

            const origConditionType = try scanNode(allocator, context, loop.condition, withGenDef);
            defer releaseIfAllocated(context, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.info.astType.* != .Bool) {
                return ScanError.ExpectedBooleanLoopCondition;
            }

            const bodyRes = try scanNode(allocator, context, loop.body, withGenDef);
            defer releaseIfAllocated(context, bodyRes);

            try context.compInfo.returnInfo.collapse(context, prev, withGenDef);
            if (context.compInfo.returnInfo.hasType()) {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncDec => |name| {
            try context.compInfo.pushScopeWithType(true, .Function);
            defer context.compInfo.popScope();
            try context.compInfo.pushGenScope(true);
            defer context.compInfo.popGenScope();

            const prev = context.compInfo.returnInfo.setInFunction(true);
            defer context.compInfo.returnInfo.revertInFunction(prev);
            const lastRetInfo = try context.compInfo.returnInfo.newInfo(true);
            defer context.compInfo.returnInfo.swapFree(context, lastRetInfo);

            try context.compInfo.addCaptureScope();
            defer context.compInfo.popCaptureScope();
            try context.compInfo.addGenericCaptureScope();
            defer context.compInfo.popGenericCaptureScope();

            const func = context.compInfo.getFunctionAsGlobal(name).?;

            if (func.visited) {
                return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
            }

            if (func.generics == null) {
                func.visited = true;
            }

            try scanFuncBodyAndReturn(allocator, context, func, false);

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncCall => |call| {
            const prev = context.compInfo.returnInfo.setInFunction(true);
            defer context.compInfo.returnInfo.revertInFunction(prev);

            const lastRetInfo = try context.compInfo.returnInfo.newInfo(true);
            defer context.compInfo.returnInfo.swapFree(context, lastRetInfo);

            const tempDec = try scanNode(allocator, context, call.func, withGenDef);
            defer releaseIfAllocated(context, tempDec);
            const dec = try escapeVarInfo(tempDec);
            if (dec.info.astType.* != .Function) return ScanError.CannotCallNonFunctionNode;
            const func = dec.info.astType.Function;

            try context.compInfo.pushGenScope(true);
            defer context.compInfo.popGenScope();

            if (call.callGenerics) |callGenerics| {
                if (func.generics) |defGenerics| {
                    if (callGenerics.len != defGenerics.len) {
                        return ScanError.CallGenericsAndFuncDecGenericCountMismatch;
                    }
                } else {
                    return ScanError.UnexpectedCallGenerics;
                }
            }

            if (func.params.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            const paramTypes = try allocator.alloc(TypeAndAllocInfo, call.params.len);
            defer {
                for (paramTypes) |paramType| {
                    releaseIfAllocated(context, paramType);
                }
                allocator.free(paramTypes);
            }

            for (call.params, 0..) |param, index| {
                paramTypes[index] = try scanNode(allocator, context, param, withGenDef);
            }

            {
                try context.compInfo.pushScopeWithType(true, .Function);
                defer context.compInfo.popScope();

                if (call.callGenerics) |callGenerics| {
                    for (callGenerics, func.generics.?) |callGenericType, decGeneric| {
                        if (decGeneric.restriction) |restriction| {
                            const matches = try matchTypes(
                                allocator,
                                context,
                                callGenericType,
                                restriction,
                                false,
                            );
                            if (!matches) {
                                return ScanError.GenericRestrictionConflict;
                            }
                        }

                        const typeClone = try clone.cloneAstTypeInfo(
                            allocator,
                            context,
                            callGenericType,
                            withGenDef,
                        );
                        try context.compInfo.setGeneric(
                            decGeneric.name,
                            typeClone.toAllocInfo(.Allocated),
                        );
                    }
                } else {
                    _ = try setGenTypesFromParams(
                        allocator,
                        context,
                        func,
                        paramTypes,
                        withGenDef,
                    );
                }

                for (func.params) |param| {
                    const typeClone = try clone.replaceGenericsOnTypeInfo(
                        allocator,
                        context,
                        param.type.toAllocInfo(.Recycled),
                        withGenDef,
                    );

                    try context.compInfo.setVariableType(
                        param.name,
                        typeClone,
                        param.mutState,
                    );
                }

                const allowSelf = func.funcType == .StructMethod;
                const usesUndefinedVars = checkUndefVars(context, func.body, allowSelf);
                if (usesUndefinedVars) {
                    return ScanError.VariableIsUndefined;
                }
            }

            if (func.generics != null) {
                const genScope = context.compInfo.genericScopes.getCurrentScope().?;
                const scannedBefore = try fnHasScannedWithSameGenTypes(
                    allocator,
                    context,
                    func,
                    genScope,
                    withGenDef,
                );
                if (!scannedBefore) {
                    const scopeRels = try genScopeToRels(
                        allocator,
                        context,
                        genScope,
                        withGenDef,
                    );
                    try func.toScanTypes.append(allocator, scopeRels);
                    try context.compInfo.addFuncToScan(func, scopeRels, withGenDef);
                }
            }

            const resType = try clone.replaceGenericsOnTypeInfo(
                allocator,
                context,
                func.returnType.toAllocInfo(.Recycled),
                withGenDef,
            );
            node.typeInfo.size = try resType.info.astType.getSize(context);
            return resType;
        },
        .StructInit => |init| {
            // TODO - node size for this
            try context.compInfo.pushGenScope(true);
            defer context.compInfo.popGenScope();

            const structDec = context.compInfo.getStructDec(init.name).?;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.StructInitGenericCountMismatch;
            }

            try setInitGenerics(
                allocator,
                context,
                init.generics,
                structDec.generics,
                withGenDef,
            );

            if (init.attributes.len != structDec.totalMemberList.len) {
                return ScanError.StructInitAttributeCountMismatch;
            }

            const genScope = context.compInfo.genericScopes.getCurrentScope();
            if (structDec.generics.len > 0) {
                if (genScope) |scope| {
                    for (structDec.attributes) |attr| {
                        if (attr.attr != .Function) continue;
                        const func = attr.attr.Function;
                        if (func.generics == null) {
                            const scannedBefore = try fnHasScannedWithSameGenTypes(
                                allocator,
                                context,
                                func,
                                scope,
                                withGenDef,
                            );
                            if (scannedBefore) continue;
                        }

                        const scopeRels = try genScopeToRels(
                            allocator,
                            context,
                            scope,
                            withGenDef,
                        );
                        defer allocator.free(scopeRels);

                        for (scopeRels) |rel| {
                            var typeCaptures = func.capturedTypes orelse a: {
                                const captures = try utils.initMutPtrT(
                                    blitzCompInfo.CaptureScope,
                                    allocator,
                                );
                                func.capturedTypes = captures;
                                break :a captures;
                            };

                            if (!typeCaptures.contains(rel.str)) {
                                try typeCaptures.put(rel.str, rel.info.toAllocInfo(.Allocated));
                            }
                        }
                    }
                }
            }

            // TODO - store number of attributes before getting here (probably exists already)
            var initAttrRel: ArrayList(StructInitMemberInfo) = .empty;
            defer {
                for (initAttrRel.items) |item| {
                    releaseIfAllocated(context, item.initInfo);
                }
                initAttrRel.deinit(allocator);
            }

            for (structDec.attributes) |attr| {
                if (attr.attr != .Member or attr.static) continue;

                var found = false;
                for (init.attributes) |initAttr| {
                    if (utils.compString(initAttr.name, attr.name)) {
                        found = true;
                        const attrType = try scanNode(
                            allocator,
                            context,
                            initAttr.value,
                            withGenDef,
                        );
                        try initAttrRel.append(allocator, .{
                            .initInfo = attrType,
                            .defInfo = attr.attr.Member,
                        });
                        break;
                    }
                }

                if (!found) {
                    return ScanError.StructInitAttributeNotFound;
                }
            }

            try context.compInfo.pushGenScope(true);
            defer context.compInfo.popGenScope();

            if (structDec.deriveType) |derive| {
                try setInitDeriveGenerics(allocator, context, derive);
            }

            for (initAttrRel.items) |attrRel| {
                const matches = try matchTypes(
                    allocator,
                    context,
                    attrRel.defInfo,
                    attrRel.initInfo.info,
                    withGenDef,
                );
                if (!matches) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            const generics = try allocator.alloc(blitzAst.AstTypeInfo, init.generics.len);
            try context.deferCleanup.slices.typeInfoSlices.append(generics);
            for (init.generics, 0..) |gen, index| {
                generics[index] = try clone.cloneAstTypeInfo(
                    allocator,
                    context,
                    gen,
                    withGenDef,
                );
            }

            const customType = try context.pools.types.new(.{
                .Custom = .{
                    .generics = generics,
                    .name = init.name,
                    .allowPrivateReads = false,
                },
            });
            return customType.toAllocInfo(.Mut, .Allocated);
        },
        .Bang => |bang| {
            const origBangType = try scanNode(allocator, context, bang, withGenDef);
            defer releaseIfAllocated(context, origBangType);
            const bangType = try escapeVarInfo(origBangType);
            if (bangType.info.astType.* != .Bool) return ScanError.ExpectedBooleanBang;

            node.typeInfo.size = try (blitzAst.AstTypes{ .Bool = {} }).getSize(context);
            return context.staticPtrs.types.boolType.toAllocInfo(.Recycled);
        },
        .Error => |err| {
            const dec = context.compInfo.getErrorDec(err).?;
            if (dec.variants.len > 0 and !context.scanInfo.allowErrorWithoutVariants) {
                return ScanError.ExpectedUseOfErrorVariants;
            }

            const errorType = try context.pools.types.new(.{
                .Error = .{
                    .name = err,
                    .payload = null,
                },
            });
            return errorType.toAllocInfo(.Const, .Allocated);
        },
        .Scope => |scope| {
            try context.compInfo.pushScope(true);
            defer context.compInfo.popScope();
            const prev = try context.compInfo.returnInfo.newInfo(false);

            try scanNodeForFunctions(allocator, context, scope);
            try context.compInfo.returnInfo.collapse(context, prev, withGenDef);

            return try scanNode(allocator, context, scope, withGenDef);
        },
        .Pointer => |ptr| {
            const ptrType = try scanNode(allocator, context, ptr.node, withGenDef);

            if (ptr.mutState == .Mut and
                ptrType.info.astType.* == .VarInfo and
                ptrType.info.mutState == .Const)
            {
                return ScanError.PointerTypeConstMismatch;
            }

            switch (ptrType.info.astType.*) {
                .Bool, .Char, .Void, .Null, .Number, .RawNumber => {
                    return ScanError.CannotTakePointerOfRawValue;
                },
                else => {},
            }

            const ptrTypeInfo = try escapeVarInfoAndRelease(context, ptrType);

            node.typeInfo.size = vmInfo.POINTER_SIZE;

            const res = try context.pools.types.new(.{
                .Pointer = ptrTypeInfo,
            });
            return res.toAllocInfo(ptr.mutState, .Allocated);
        },
        .Dereference => |target| {
            const ptrTypeResult = try scanNode(allocator, context, target, withGenDef);
            const ptrType = try escapeVarInfoAndRelease(context, ptrTypeResult);
            if (ptrType.info.astType.* != .Pointer) {
                return ScanError.CannotDereferenceNonPointerValue;
            }

            node.typeInfo.size = try ptrType.info.astType.Pointer.info.astType.getSize(context);

            if (ptrType.allocState == .Allocated) {
                const res = ptrType.info.astType.Pointer;
                context.pools.types.release(ptrType.info.astType);
                return res;
            }

            const resType = ptrType.info.astType.Pointer;
            return resType;
        },
        .HeapAlloc => |*alloc| {
            const exprTypeResult = try scanNode(allocator, context, alloc.*.node, withGenDef);
            const exprType = try escapeVarInfoAndRelease(context, exprTypeResult);
            const typeClone = try clone.replaceGenericsOnTypeInfoAndRelease(
                allocator,
                context,
                exprType,
                withGenDef,
            );

            const ptrType = try context.pools.types.new(.{
                .Pointer = typeClone,
            });
            return ptrType.toAllocInfo(.Mut, .Allocated);
        },
        .HeapFree => |toFree| {
            const exprTypeResult = try scanNode(allocator, context, toFree, withGenDef);
            defer releaseIfAllocated(context, exprTypeResult);
            const exprType = try escapeVarInfo(exprTypeResult);
            if (exprType.info.astType.* != .Pointer and exprType.info.astType.* != .ArraySlice) {
                return ScanError.CannotFreeNonPointerType;
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .ArrayInit => |init| {
            try context.compInfo.pushScope(true);
            defer context.compInfo.popScope();

            if (init.indexIdent) |ident| {
                var info = context.staticPtrs.types.u64Type;
                info.mutState = .Const;
                try context.compInfo.setVariableType(
                    ident,
                    info.toAllocInfo(.Recycled),
                    .Const,
                );
            }

            if (init.ptrIdent) |ident| {
                const initType = init.initType.toAllocInfo(.Recycled);
                const typePtr = try context.pools.types.new(.{ .Pointer = initType });
                const info = typePtr.toAllocInfo(.Const, .Allocated);
                try context.compInfo.setVariableType(
                    ident,
                    info,
                    .Const,
                );
            }

            const initNodeTypeResult = try scanNode(allocator, context, init.initNode, withGenDef);
            defer releaseIfAllocated(context, initNodeTypeResult);
            const initNodeType = try escapeVarInfo(initNodeTypeResult);

            const matches = try matchTypes(
                allocator,
                context,
                init.initType,
                initNodeType.info,
                withGenDef,
            );
            if (!matches) {
                return ScanError.ArrayInitTypeInitializerMismatch;
            }

            const initTypeClone = try clone.replaceGenericsOnTypeInfo(
                allocator,
                context,
                init.initType.toAllocInfo(.Recycled),
                withGenDef,
            );

            const valueVariant: blitzAst.AstNodeUnion = .{
                .Value = .{
                    .RawNumber = .{
                        .digits = init.size,
                        .numType = .U64,
                    },
                },
            };
            const arraySliceType = try context.pools.types.new(.{
                .ArraySlice = .{
                    .type = initTypeClone,
                    .size = try context.pools.nodes.new(valueVariant.toAstNode()),
                },
            });

            const arrSize = std.fmt.parseInt(u64, init.size, 10) catch
                return ScanError.InvalidNumber;
            const sliceSize = vmInfo.POINTER_SIZE * 2;
            node.typeInfo.size = try initTypeClone.info.astType.getSize(context) * arrSize + sliceSize;

            return arraySliceType.toAllocInfo(.Mut, .Allocated);
        },
        .InferErrorVariant => |variant| {
            const errorVariant = try context.pools.types.new(.{
                .ErrorVariant = .{
                    .from = null,
                    .variant = variant,
                },
            });
            return errorVariant.toAllocInfo(.Const, .Allocated);
        },
        .Break, .Continue => {
            if (!context.compInfo.inLoopScope()) {
                return ScanError.LoopControlFlowUsedOutsideOfLoop;
            }
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
    }
}

fn genInGenInfoRels(rels: []blitzAst.StrToTypeInfoRel, name: []const u8) bool {
    for (rels) |rel| {
        if (utils.compString(name, rel.gen)) {
            return true;
        }
    }

    return false;
}

fn escapeVarInfo(node: TypeAndAllocInfo) !TypeAndAllocInfo {
    var res = node;

    if (res.info.astType.* == .VarInfo) {
        res = res.info.astType.VarInfo;
    }

    if (res.info.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }

    return res;
}

// for var infos that were cloned. only to be used after scan node
fn escapeVarInfoAndRelease(context: *Context, result: TypeAndAllocInfo) !TypeAndAllocInfo {
    const allocated = result.allocState;
    var res = result;

    if (res.info.astType.* == .VarInfo) {
        const temp = res.info.astType.VarInfo;

        if (allocated == .Allocated) {
            context.pools.types.release(res.info.astType);
        }

        res = temp;
    }

    if (res.info.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }

    return res;
}

/// true if uses undefined variables
fn checkUndefVars(context: *Context, node: *const blitzAst.AstNode, allowSelf: bool) bool {
    var undef = false;

    return switch (node.variant) {
        .Variable => |name| {
            if (allowSelf and utils.compString("self", name)) return false;
            return !context.compInfo.isVariableInScope(name);
        },
        .Cast => |cast| checkUndefVars(context, cast.node, allowSelf),
        .IncOne,
        .DecOne,
        .Group,
        .Scope,
        .Bang,
        .ReturnNode,
        => |inner| checkUndefVars(context, inner, allowSelf),
        .ForLoop => |loop| {
            undef = undef or checkUndefVars(context, loop.condition, allowSelf);
            undef = undef or checkUndefVars(context, loop.body, allowSelf);
            undef = undef or checkUndefVars(context, loop.incNode, allowSelf);
            if (loop.initNode) |init| {
                undef = undef or checkUndefVars(context, init, allowSelf);
            }
            return undef;
        },
        .WhileLoop => |loop| {
            undef = undef or checkUndefVars(context, loop.condition, allowSelf);
            undef = undef or checkUndefVars(context, loop.body, allowSelf);
            return undef;
        },
        .FuncCall => |func| checkUndefVars(context, func.func, allowSelf),
        .IfStatement => |statement| {
            undef = undef or checkUndefVars(context, statement.condition, allowSelf);
            undef = undef or checkUndefVars(context, statement.body, allowSelf);

            if (statement.fallback) |fallback| {
                undef = undef or checkUndefVars(context, fallback.node, allowSelf);
            }

            return undef;
        },
        .IndexValue => |index| {
            undef = undef or checkUndefVars(context, index.target, allowSelf);
            undef = undef or checkUndefVars(context, index.index, allowSelf);
            return undef;
        },
        .OpExpr => |expr| {
            undef = undef or checkUndefVars(context, expr.left, allowSelf);
            undef = undef or checkUndefVars(context, expr.right, allowSelf);
            return undef;
        },
        .PropertyAccess => |access| {
            return checkUndefVars(context, access.value, allowSelf);
        },
        .Seq => |seq| {
            for (seq) |innerNode| {
                return checkUndefVars(context, innerNode, allowSelf);
            }

            return undef;
        },
        .StructInit => |init| {
            for (init.attributes) |attr| {
                return checkUndefVars(context, attr.value, allowSelf);
            }

            return undef;
        },
        .ValueSet => |set| {
            undef = undef or checkUndefVars(context, set.value, allowSelf);
            undef = undef or checkUndefVars(context, set.setNode, allowSelf);
            return undef;
        },
        .VarDec => |dec| {
            return checkUndefVars(context, dec.setNode, allowSelf);
        },
        .VarEqOp => |op| {
            return checkUndefVars(context, op.value, allowSelf);
        },
        else => return false,
    };
}

fn scanFunctionCalls(allocator: Allocator, context: *Context) !void {
    _ = context.compInfo.returnInfo.setInFunction(true);
    defer context.compInfo.returnInfo.revertInFunction(false);

    const functions = context.compInfo.functionsToScan;
    while (functions.items.len > 0) {
        const toScanItem = functions.pop().?;
        const func = toScanItem.func;

        try context.compInfo.pushGenScope(false);
        defer context.compInfo.popGenScope();
        try context.compInfo.pushScopeWithType(false, .Function);
        defer context.compInfo.popScope();

        const lastRetInfo = try context.compInfo.returnInfo.newInfo(true);
        defer context.compInfo.returnInfo.swapFree(context, lastRetInfo);

        if (func.capturedTypes) |captured| {
            var captureIt = captured.iterator();
            while (captureIt.next()) |item| {
                const clonedType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    item.value_ptr.*,
                    toScanItem.withGenDef,
                );
                try context.compInfo.setGeneric(item.key_ptr.*, clonedType);
            }
        }

        if (func.capturedFuncs) |captured| {
            for (captured.items) |item| {
                try context.compInfo.addScopedFunction(item);
            }
        }

        if (func.capturedValues) |captured| {
            var captureIt = captured.iterator();
            while (captureIt.next()) |item| {
                const value = item.value_ptr.*;
                const paramType = try escapeVarInfo(value);

                std.debug.print("setting :: {s}\n", .{item.key_ptr.*});
                const clonedType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    paramType,
                    false,
                );
                try context.compInfo.setVariableType(
                    item.key_ptr.*,
                    clonedType,
                    value.info.mutState,
                );
            }
        }

        for (toScanItem.genTypes) |rel| {
            const typeClone = try clone.cloneAstTypeInfo(
                allocator,
                context,
                rel.info,
                false,
            );
            try context.compInfo.setGeneric(rel.str, typeClone.toAllocInfo(.Allocated));
        }

        try scanFuncBodyAndReturn(allocator, context, func, toScanItem.withGenDef);
    }
}

fn setGenTypesFromParams(
    allocator: Allocator,
    context: *Context,
    func: *blitzAst.FuncDecNode,
    paramTypes: []TypeAndAllocInfo,
    withGenDef: bool,
) !bool {
    var includesGenerics = false;

    for (func.params, paramTypes) |decParam, origCallParamInfo| {
        const callParamType = try escapeVarInfo(origCallParamInfo);
        var isGeneric = false;

        switch (decParam.type.astType.*) {
            .Generic => |generic| {
                const typePtr = try clone.cloneAstTypeInfo(
                    allocator,
                    context,
                    callParamType.info,
                    withGenDef,
                );

                const currentGenScope = context.compInfo.genericScopes.getCurrentScope();
                if (currentGenScope) |scope| {
                    if (scope.get(generic)) |genType| {
                        const matches = try matchTypes(
                            allocator,
                            context,
                            callParamType.info,
                            genType.info,
                            false,
                        );
                        if (!matches) {
                            return ScanError.GenericRestrictionConflict;
                        }
                    }
                }

                try context.compInfo.setGeneric(generic, typePtr.toAllocInfo(.Allocated));
                isGeneric = true;
            },
            .Custom => |custom| {
                isGeneric = try matchParamGenericTypes(
                    allocator,
                    context,
                    custom,
                    callParamType.info.astType,
                );
            },
            else => {},
        }

        const matches = try matchTypes(
            allocator,
            context,
            decParam.type,
            callParamType.info,
            false,
        );
        if (!matches) {
            return ScanError.FunctionCallParamTypeMismatch;
        }

        if (callParamType.info.astType.* == .Pointer and
            callParamType.info.mutState == .Const and
            decParam.type.mutState == .Mut and
            !isAnyType(decParam.type.astType))
        {
            return ScanError.ExpectedMutableParameter;
        }

        includesGenerics = includesGenerics or isGeneric;
    }

    return includesGenerics;
}

fn genScopeToRels(
    allocator: Allocator,
    context: *Context,
    genScope: *blitzCompInfo.TypeScope,
    withGenDef: bool,
) ![]blitzAst.StrToTypeInfoRel {
    const slice = try allocator.alloc(blitzAst.StrToTypeInfoRel, genScope.count());
    var i: usize = 0;
    var scopeIt = genScope.iterator();
    while (scopeIt.next()) |entry| {
        const infoClone = try clone.cloneAstTypeInfo(
            allocator,
            context,
            entry.value_ptr.info,
            withGenDef,
        );
        slice[i] = .{
            .str = entry.key_ptr.*,
            .info = infoClone,
        };

        i += 1;
    }

    return slice;
}

fn fnHasScannedWithSameGenTypes(
    allocator: Allocator,
    context: *Context,
    func: *blitzAst.FuncDecNode,
    genScope: *blitzCompInfo.TypeScope,
    withGenDef: bool,
) !bool {
    outer: for (func.toScanTypes.items) |scannedScope| {
        for (scannedScope) |rel| {
            const genType = genScope.get(rel.str);
            if (genType == null) continue :outer;
            const matches = try matchTypesUtil(
                allocator,
                context,
                genType.?.info,
                rel.info,
                withGenDef,
                .Strict,
            );
            if (!matches) {
                continue :outer;
            }
        }

        return true;
    }

    return false;
}

fn isAnyType(astType: *const blitzAst.AstTypes) bool {
    return astType.* == .Generic or astType.* == .Any;
}

fn applyVariableCaptures(
    allocator: Allocator,
    context: *Context,
    func: *blitzAst.FuncDecNode,
    scope: *blitzCompInfo.CaptureScope,
) !void {
    if (func.capturedValues) |captured| {
        free.freeVariableCaptures(allocator, context, captured, .Allocated);
        allocator.destroy(captured);
    }

    func.capturedValues = scope;
}

fn applyGenericCaptures(
    allocator: Allocator,
    context: *Context,
    func: *blitzAst.FuncDecNode,
    scope: *blitzCompInfo.TypeScope,
) !void {
    if (func.capturedTypes) |captured| {
        free.freeGenericCaptures(allocator, context, captured, .Allocated);
        allocator.destroy(captured);
    }

    func.capturedTypes = scope;
}

fn applyFunctionCaptures(
    allocator: Allocator,
    func: *blitzAst.FuncDecNode,
    scope: *blitzCompInfo.StringListScope,
) !void {
    if (func.capturedFuncs) |captured| {
        captured.deinit(allocator);
        allocator.destroy(captured);
    }

    func.capturedFuncs = scope;
}

fn scanIfFallback(
    allocator: Allocator,
    context: *Context,
    fallback: blitzAst.FallbackInfo,
    withGenDef: bool,
) !void {
    if (!fallback.hasCondition and fallback.node.variant.IfStatement.fallback != null) {
        const nextFallback = fallback.node.variant.IfStatement.fallback.?;
        if (!nextFallback.hasCondition) {
            return ScanError.IfStatementMayOnlyHaveOneElse;
        } else {
            return ScanError.ElseBranchOutOfOrder;
        }
    }

    const fallbackRes = try scanNode(allocator, context, fallback.node, withGenDef);
    releaseIfAllocated(context, fallbackRes);
}

fn setInitGenerics(
    allocator: Allocator,
    context: *Context,
    genTypes: []blitzAst.AstTypeInfo,
    decGens: []blitzAst.GenericType,
    withGenDef: bool,
) !void {
    for (genTypes, decGens) |t, decGen| {
        if (decGen.restriction) |restriction| {
            const matches = try matchTypes(allocator, context, restriction, t, withGenDef);
            if (!matches) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        const typeClone = try clone.replaceGenericsOnTypeInfo(
            allocator,
            context,
            t.toAllocInfo(.Recycled),
            withGenDef,
        );
        try context.compInfo.setGeneric(decGen.name, typeClone);
    }
}

fn setInitDeriveGenerics(
    allocator: Allocator,
    context: *Context,
    deriveType: blitzAst.AstTypeInfo,
) !void {
    const generics = deriveType.astType.Custom.generics;
    const deriveName = switch (deriveType.astType.*) {
        .Custom => |custom| custom.name,
        .StaticStructInstance => |inst| inst,
        else => unreachable,
    };
    const deriveDec = context.compInfo.getStructDec(deriveName);
    const decGens = deriveDec.?.generics;

    for (generics, decGens) |gen, decGen| {
        const clonedType = try clone.replaceGenericsOnTypeInfo(
            allocator,
            context,
            gen.toAllocInfo(.Recycled),
            true,
        );

        if (decGen.restriction) |restriction| {
            const matches = try matchTypes(allocator, context, clonedType.info, restriction, true);
            if (!matches) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        try context.compInfo.setGeneric(decGen.name, clonedType);
    }
}

fn matchParamGenericTypes(
    allocator: Allocator,
    context: *Context,
    custom: blitzAst.CustomType,
    paramType: *const blitzAst.AstTypes,
) !bool {
    switch (paramType.*) {
        .Custom => |paramCustom| {
            if (!utils.compString(custom.name, paramCustom.name)) {
                return ScanError.FunctionCallParamTypeMismatch;
            }

            var hasGeneric = false;

            for (custom.generics, 0..) |gen, index| {
                const paramGen = paramCustom.generics[index];

                switch (gen.astType.*) {
                    .Generic => |generic| {
                        const typeClone = try clone.replaceGenericsOnTypeInfo(
                            allocator,
                            context,
                            paramGen.toAllocInfo(.Recycled),
                            false,
                        );

                        const genType = try context.compInfo.getGeneric(generic);
                        if (genType) |t| {
                            const matches = try matchTypes(
                                allocator,
                                context,
                                t.info,
                                typeClone.info,
                                true,
                            );
                            if (!matches) {
                                return ScanError.ConflictingGenericParameters;
                            }
                        }

                        try context.compInfo.setGeneric(generic, typeClone);
                        hasGeneric = true;
                    },
                    .Custom => |newCustom| {
                        hasGeneric = try matchParamGenericTypes(
                            allocator,
                            context,
                            newCustom,
                            paramGen.astType,
                        );
                    },
                    else => {},
                }
            }

            return hasGeneric;
        },
        else => return ScanError.FunctionCallParamTypeMismatch,
    }
}

fn scanFuncBodyAndReturn(
    allocator: Allocator,
    context: *Context,
    func: *blitzAst.FuncDecNode,
    withGenDef: bool,
) !void {
    try context.compInfo.pushScopeWithType(true, .Function);
    defer context.compInfo.popScope();

    for (func.params) |param| {
        const typeClone = try clone.cloneAstTypeInfo(
            allocator,
            context,
            param.type,
            withGenDef,
        );

        try context.compInfo.setVariableType(
            param.name,
            typeClone.toAllocInfo(.Recycled),
            param.mutState,
        );
    }

    defer {
        for (func.params) |param| {
            const varType = context.compInfo.getVariableTypeFixed(param.name);
            if (varType) |t| {
                const innerType = t.info.astType.VarInfo.info.astType;
                free.recursiveReleaseType(allocator, context, innerType);
                t.info.astType.VarInfo = context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
            }
        }
    }

    try scanNodeForFunctions(allocator, context, func.body);
    const bodyRes = try scanNode(allocator, context, func.body, withGenDef);
    defer releaseIfAllocated(context, bodyRes);

    const scope = context.compInfo.consumeVariableCaptures();
    if (scope) |s| {
        try applyVariableCaptures(allocator, context, func, s);
    }

    const genScope = context.compInfo.consumeGenericCaptures();
    if (genScope) |s| {
        try applyGenericCaptures(allocator, context, func, s);
    }

    const funcScope = context.compInfo.consumeFunctionCaptures();
    if (funcScope) |s| {
        try applyFunctionCaptures(allocator, func, s);
    }

    if (func.returnType.astType.* != .Void) {
        if (!context.compInfo.returnInfo.info.exhaustive) {
            return ScanError.FunctionReturnIsNotExhaustive;
        }

        if (context.compInfo.returnInfo.info.retType) |retType| {
            const matches = try matchTypes(
                allocator,
                context,
                func.returnType,
                retType.info,
                withGenDef,
            );
            if (!matches) {
                return ScanError.FunctionReturnTypeMismatch;
            }
        } else {
            return ScanError.FunctionMissingReturn;
        }
    } else if (context.compInfo.returnInfo.info.retType != null) {
        return ScanError.FunctionReturnTypeMismatch;
    }
}

fn validateSelfProps(
    allocator: Allocator,
    context: *Context,
    name: []const u8,
    prop: []const u8,
    inOwnedMethod: bool,
) !?blitzAst.AstTypeInfo {
    const structDec = context.compInfo.getStructDec(name);

    if (structDec) |dec| {
        for (dec.attributes) |attr| {
            const nameMatches = utils.compString(attr.name, prop);
            if (nameMatches) {
                if (attr.visibility == .Public or attr.visibility == .Protected or inOwnedMethod) {
                    return try clone.cloneStructAttributeUnion(
                        allocator,
                        context,
                        attr.attr,
                        false,
                    );
                }
            }
        }

        if (dec.deriveType) |derive| {
            const deriveName = switch (derive.astType.*) {
                .StaticStructInstance => |structName| structName,
                .Custom => |custom| custom.name,
                else => unreachable,
            };

            return validateSelfProps(allocator, context, deriveName, prop, false);
        }

        return null;
    }

    return ScanError.UndefinedStruct;
}

fn validateStaticStructProps(
    allocator: Allocator,
    context: *Context,
    name: []const u8,
    prop: []const u8,
) !?blitzAst.AstTypeInfo {
    const dec = context.compInfo.getStructDec(name).?;

    for (dec.attributes) |attr| {
        if (!utils.compString(attr.name, prop)) continue;
        if (!attr.static) return ScanError.NonStaticAccessFromStaticStructReference;
        if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

        return try clone.cloneStructAttributeUnionType(allocator, context, attr.attr, false);
    }

    return ScanError.InvalidProperty;
}

fn validateCustomProps(
    allocator: Allocator,
    context: *Context,
    custom: blitzAst.CustomType,
    prop: []const u8,
    withGenDef: bool,
) !?TypeAndAllocInfo {
    const dec = context.compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (attr.static) continue;

            if (utils.compString(attr.name, prop)) {
                if (!custom.allowPrivateReads and attr.visibility != .Public) {
                    return ScanError.NonPublicStructFieldAccessFromOutsideDefinition;
                }

                const res = try clone.cloneStructAttributeUnionType(
                    allocator,
                    context,
                    attr.attr,
                    withGenDef,
                );
                return res.toAllocInfo(.Allocated);
            }
        }

        if (structDec.deriveType) |deriveType| {
            switch (deriveType.astType.*) {
                .Custom => |c| return try validateCustomProps(
                    allocator,
                    context,
                    c,
                    prop,
                    withGenDef,
                ),
                else => unreachable,
            }
        }

        return null;
    }

    return ScanError.InvalidPropertySource;
}

fn scanAttributes(context: *Context, dec: *blitzAst.StructDecNode) !void {
    for (dec.attributes) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                try context.compInfo.addFuncToScan(func, &[_]blitzAst.StrToTypeInfoRel{}, false);
            },
        }
    }
}

fn isInt(astType: *const blitzAst.AstTypes) bool {
    return switch (astType.*) {
        .Number => |num| switch (num) {
            .U8,
            .U16,
            .U32,
            .U64,
            .U128,
            .I8,
            .I16,
            .I32,
            .I64,
            .I128,
            => true,
            else => false,
        },
        else => false,
    };
}

pub fn matchTypes(
    allocator: Allocator,
    context: *Context,
    toType: blitzAst.AstTypeInfo,
    fromType: blitzAst.AstTypeInfo,
    withGenDef: bool,
) ScanNodeError!bool {
    return try matchTypesUtil(allocator, context, toType, fromType, withGenDef, .Assign);
}

/// match types as if fromType is being set to toType to match mutability
pub fn matchTypesUtil(
    allocator: Allocator,
    context: *Context,
    toType: blitzAst.AstTypeInfo,
    fromType: blitzAst.AstTypeInfo,
    withGenDef: bool,
    mutMatchBehavior: MutMatchBehavior,
) !bool {
    const type1 = toType.astType.*;
    const type2 = fromType.astType.*;

    if (type1 == .Any or type2 == .Any) return true;
    if (type1 == .Undef or type2 == .Undef) return true;

    if (type1 == .Generic and type2 == .Generic) {
        if (withGenDef) {
            var genType1 = try context.compInfo.getGeneric(type1.Generic);
            if (genType1) |*gType| {
                if (gType.info.astType.* == .Generic and
                    utils.compString(type1.Generic, gType.info.astType.Generic))
                {
                    return ScanError.UnexpectedRecursiveGeneric;
                }
            } else if (withGenDef) {
                return ScanError.EmptyGenericType;
            }

            var genType2 = try context.compInfo.getGeneric(type2.Generic);
            if (genType2) |*gType| {
                if (gType.info.astType.* == .Generic and
                    utils.compString(type2.Generic, gType.info.astType.Generic))
                {
                    return ScanError.UnexpectedRecursiveGeneric;
                }
            } else if (withGenDef) {
                return ScanError.EmptyGenericType;
            }

            return matchTypesUtil(
                allocator,
                context,
                genType1.?.info,
                genType2.?.info,
                withGenDef,
                mutMatchBehavior,
            );
        }

        return true;
    }

    if (type1 == .Generic) {
        if (!withGenDef) return true;

        var genType = try context.compInfo.getGeneric(type1.Generic);
        if (genType) |*gType| {
            if (gType.info.astType.* == .Generic and
                utils.compString(gType.info.astType.Generic, type1.Generic))
            {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypesUtil(
                allocator,
                context,
                gType.info,
                fromType,
                withGenDef,
                mutMatchBehavior,
            );
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        }

        return true;
    }

    if (type2 == .Generic) {
        if (!withGenDef) return true;

        var genType = try context.compInfo.getGeneric(type2.Generic);
        if (genType) |*gType| {
            if (gType.info.astType.* == .Generic and
                utils.compString(gType.info.astType.Generic, type2.Generic))
            {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypesUtil(
                allocator,
                context,
                toType,
                gType.info,
                withGenDef,
                mutMatchBehavior,
            );
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        }

        return true;
    }

    if (type1 == .VarInfo) {
        return matchTypesUtil(
            allocator,
            context,
            type1.VarInfo.info,
            fromType,
            withGenDef,
            mutMatchBehavior,
        );
    }

    if (type2 == .VarInfo) {
        return matchTypesUtil(
            allocator,
            context,
            toType,
            type2.VarInfo.info,
            withGenDef,
            mutMatchBehavior,
        );
    }

    return switch (type1) {
        .String => type2 == .String,
        .Bool => type2 == .Bool,
        .Char => type2 == .Char,
        .Void => type2 == .Void,
        .Null => type2 == .Nullable or type2 == .Null,
        .Nullable => |inner| type2 == .Null or try matchTypesUtil(
            allocator,
            context,
            inner,
            fromType,
            withGenDef,
            mutMatchBehavior,
        ),
        .RawNumber => return type2 == .Number or type2 == .RawNumber,
        .Number => |num| {
            if (type2 == .Number) {
                return @intFromEnum(num) == @intFromEnum(type2.Number);
            }

            return type2 == .RawNumber;
        },
        .ArraySlice => |arr| {
            const array: blitzAst.AstArraySliceType = switch (type2) {
                .ArraySlice => |arraySlice| arraySlice,
                else => return false,
            };

            if (arr.size != null and array.size != null) {
                const sizeType1 = try scanNode(allocator, context, arr.size.?, withGenDef);
                defer releaseIfAllocated(context, sizeType1);
                const sizeType2 = try scanNode(allocator, context, array.size.?, withGenDef);
                defer releaseIfAllocated(context, sizeType2);

                if (!isInt(sizeType1.info.astType) or !isInt(sizeType2.info.astType)) {
                    return false;
                }
            }

            if (arr.size != null and array.size == null) {
                return ScanError.SizedSliceSetToUnknownSizedSlice;
            }

            const matches = try matchTypesUtil(
                allocator,
                context,
                arr.type.info,
                array.type.info,
                withGenDef,
                mutMatchBehavior,
            );
            return try matchMutState(toType, fromType, matches, mutMatchBehavior);
        },
        .Custom => |custom| {
            if (type2 == .StaticStructInstance and
                utils.compString(custom.name, type2.StaticStructInstance))
            {
                return try matchMutState(toType, fromType, true, mutMatchBehavior);
            }

            if (type2 != .Custom) return false;
            if (!utils.compString(type1.Custom.name, type2.Custom.name)) return false;
            if (custom.generics.len != type2.Custom.generics.len) return false;

            for (custom.generics, type2.Custom.generics) |gen1, gen2| {
                const genMatch = try matchTypesUtil(
                    allocator,
                    context,
                    gen1,
                    gen2,
                    withGenDef,
                    mutMatchBehavior,
                );
                if (!genMatch) return ScanError.CustomGenericMismatch;
            }

            return try matchMutState(toType, fromType, true, mutMatchBehavior);
        },
        .Error => |err| switch (type2) {
            .Error => |err2| a: {
                break :a utils.compString(err.name, err2.name);
            },
            .ErrorVariant => |err2| {
                if (err2.from) |from| {
                    return utils.compString(err.name, from);
                }

                const errDec = context.compInfo.getErrorDec(err.name);
                if (errDec) |dec| {
                    return utils.inStringArr(dec.variants, err2.variant);
                }

                return false;
            },
            else => {
                if (err.payload) |payload| {
                    const matches = try matchTypesUtil(
                        allocator,
                        context,
                        payload,
                        fromType,
                        withGenDef,
                        mutMatchBehavior,
                    );
                    return try matchMutState(toType, fromType, matches, mutMatchBehavior);
                }

                return false;
            },
        },
        .ErrorVariant => |err| switch (type2) {
            .Error => |err2| {
                if (err.from) |from| {
                    return utils.compString(err2.name, from);
                }

                const errDec = context.compInfo.getErrorDec(err2.name);
                if (errDec) |dec| {
                    return utils.inStringArr(dec.variants, err.variant);
                }

                return false;
            },
            .ErrorVariant => |err2| {
                const variantsMatch = utils.compString(err.variant, err2.variant);

                if (err.from != null and err2.from != null) {
                    return utils.compString(err.from.?, err2.from.?) and variantsMatch;
                }

                return variantsMatch;
            },
            else => false,
        },
        .StaticStructInstance => |inst| {
            if (type2 == .Custom and utils.compString(inst, type2.Custom.name)) {
                return try matchMutState(toType, fromType, true, mutMatchBehavior);
            }

            return false;
        },
        .Pointer => |ptr| {
            if (type2 != .Pointer) return ScanError.PointerTypeMismatch;
            const res = try matchTypesUtil(
                allocator,
                context,
                ptr.info,
                type2.Pointer.info,
                withGenDef,
                mutMatchBehavior,
            );
            return try matchMutState(toType, fromType, res, mutMatchBehavior);
        },
        else => false,
    };
}

fn matchMutState(
    toType: blitzAst.AstTypeInfo,
    fromType: blitzAst.AstTypeInfo,
    typesMatched: bool,
    mutMatchBehavior: MutMatchBehavior,
) !bool {
    if (!typesMatched) {
        return false;
    }

    switch (mutMatchBehavior) {
        .Assign => {
            if (toType.astType.* == .Pointer and
                toType.mutState == .Mut and
                fromType.mutState == .Const)
            {
                return ScanError.PointerTypeConstMismatch;
            }
        },
        .Strict => {
            if (toType.mutState != fromType.mutState) {
                return ScanError.StrictMutTypeMismatch;
            }
        },
    }

    return true;
}

pub fn isPrimitive(astType: *const blitzAst.AstTypes) bool {
    return switch (astType.*) {
        .String, .Bool, .Char, .Number, .Null, .RawNumber => true,
        .Nullable => |inner| isPrimitive(inner.astType),
        else => false,
    };
}

fn getPropertyType(
    allocator: Allocator,
    context: *Context,
    source: blitzAst.AstTypes,
    prop: []const u8,
) !blitzAst.AstTypes {
    return switch (source) {
        .StaticStructInstance => |inst| try getStructPropType(context, false, inst, prop),
        .ArraySlice => try builtins.getArraySlicePropTypes(prop),
        .String => try builtins.getStringPropTypes(prop),
        .Custom => |custom| getCustomPropType(allocator, context, custom, prop),
        else => ScanError.UnsupportedFeature,
    };
}

fn getCustomPropType(
    allocator: Allocator,
    context: *Context,
    custom: blitzAst.CustomType,
    prop: []const u8,
) !blitzAst.AstTypes {
    const dec = context.compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (!utils.compString(attr.name, prop)) continue;
            if (attr.static) return ScanError.StaticAccessFromStructInstance;

            if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

            return try clone.cloneStructAttributeUnion(allocator, context, attr.attr, false);
        }

        if (structDec.deriveType) |deriveType| {
            if (deriveType.* == .Custom) {
                return try getCustomPropType(allocator, context, deriveType.Custom, prop);
            } else return ScanError.UnexpectedDeriveType;
        }

        return ScanError.InvalidProperty;
    }

    return ScanError.InvalidPropertySource;
}

fn getStructPropType(
    context: *Context,
    allowNonStatic: bool,
    inst: []const u8,
    prop: []const u8,
) !blitzAst.AstTypes {
    const dec = context.compInfo.getStructDec(inst) orelse return ScanError.InvalidPropertySource;

    for (dec.?.attributes) |attr| {
        if (!attr.static and allowNonStatic) continue;
        if (!utils.compString(attr.name, prop)) continue;

        switch (attr.visibility) {
            .Public => {},
            else => return ScanError.UnsupportedFeature,
        }

        switch (attr.attr) {
            .Member => |member| return member.*,
            .Function => |func| return .{ .Function = func },
        }
    }

    return ScanError.InvalidProperty;
}

fn inferArraySliceType(
    allocator: Allocator,
    context: *Context,
    arr: []*blitzAst.AstNode,
    withGenDef: bool,
) !TypeAndAllocInfo {
    if (arr.len == 0) {
        var voidType = context.staticPtrs.types.voidType;
        voidType.mutState = .Mut;
        return voidType.toAllocInfo(.Recycled);
    }

    var firstType = try scanNode(allocator, context, arr[0], withGenDef);
    var mutState = firstType.info.mutState;

    for (arr[1..]) |item| {
        const exprType = try scanNode(allocator, context, item, withGenDef);
        defer releaseIfAllocated(context, exprType);

        mutState = mutState.orConst(exprType.info.mutState);

        const matches = try matchTypes(allocator, context, exprType.info, firstType.info, false);
        if (!matches) {
            return ScanError.ArraySliceTypeMismatch;
        }
    }

    firstType.info.mutState = mutState;
    return firstType;
}

fn scanNodeForFunctions(
    allocator: Allocator,
    context: *Context,
    node: *const blitzAst.AstNode,
) !void {
    switch (node.variant) {
        .FuncDec => |dec| {
            try context.compInfo.addScopedFunction(dec);
        },
        .Seq => |seq| {
            for (seq) |seqNode| {
                try scanNodeForFunctions(allocator, context, seqNode);
            }
        },
        else => {},
    }
}

fn verifyRawNumberMagnitude(node: blitzAst.RawNumberNode) bool {
    switch (node.numType) {
        .U8 => _ = std.fmt.parseInt(u8, node.digits, 10) catch return false,
        .U16 => _ = std.fmt.parseInt(u16, node.digits, 10) catch return false,
        .U32 => _ = std.fmt.parseInt(u32, node.digits, 10) catch return false,
        .U64 => _ = std.fmt.parseInt(u64, node.digits, 10) catch return false,
        .U128 => _ = std.fmt.parseInt(u128, node.digits, 10) catch return false,
        .I8 => _ = std.fmt.parseInt(i8, node.digits, 10) catch return false,
        .I16 => _ = std.fmt.parseInt(i16, node.digits, 10) catch return false,
        .I32 => _ = std.fmt.parseInt(i32, node.digits, 10) catch return false,
        .I64 => _ = std.fmt.parseInt(i64, node.digits, 10) catch return false,
        .I128 => _ = std.fmt.parseInt(i128, node.digits, 10) catch return false,
        else => return true,
    }

    return true;
}

pub fn releaseIfAllocated(context: *Context, result: TypeAndAllocInfo) void {
    if (result.allocState == .Allocated) {
        context.pools.types.releaseRecurse(result.info.astType);
    }
}
