const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const builtins = blitz.builtins;
const clone = blitz.clone;
const compInfo = blitz.compInfo;
const vmInfo = blitz.vmInfo;
const allocPools = blitz.allocPools;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;

pub const ScanInfo = struct {
    allowErrorWithoutVariants: bool = false,
    allowStaticStructInstance: bool = false,
};

pub const RetNodeInfo = struct {
    node: ast.AstNode,
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
    info: ast.AstTypeInfo,
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
    InvalidEqOperationType,

    // pointers
    PointerTypeMismatch,
    CannotDereferenceNonPointerValue,
    CannotTakePointerOfRawValue,
    CannotFreeNonPointerType,

    // arrays
    ArrayTypeMismatch,
    ExpectedArrayForIndexTarget,
    ExpectedU64OrU32ForArrayDecSize,
    ExpectedEqualArrayDecSizes,
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
    ValueSetTargetNotAVariable,

    // functions
    ExpectedFunctionReturn,
    FunctionCallParamTypeMismatch,
    FunctionCallParamCountMismatch,
    FunctionReturnTypeMismatch,
    IdentifierNotAFunction,
    CannotCallNonFunctionNode,
    VariableIsUndefined,
    FunctionNotInScope,
    FunctionReturnIsNotExhaustive,
    FunctionMissingReturn,
    UnexpectedReturnStatement,
    ExpectedMutableParameter,
    CallGenericsAndFuncDecGenericCountMismatch,
    UnexpectedCallGenerics,
    UnexpectedSelfParameter,
    ExpectedSelfParameterToBeFirst,
    ExpectedSelfParameter,
    CaptureVariableIsNotInScope,
    CaptureVariableConstMismatch,

    // structs
    GenericCountMismatch,
    StructInitAttributeCountMismatch,
    StructInitMemberTypeMismatch,
    StructInitAttributeNotFound,
    InvalidProperty,
    StaticAccessFromStructInstance,
    NonStaticAccessFromStaticStructReference,
    SelfUsedOutsideStruct,
    StructDoesNotExist,
    RestrictedPropertyAccess,
    InvalidPropertySource,
    NonPublicStructFieldAccessFromOutsideDefinition,
    GenericStructMethodRedefiningStructGeneric,
    ExpectedMutableStructInstance,

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

    // enums
    EnumVariantDoesNotExist,
};

const StructInitMemberInfo = struct {
    initInfo: TypeAndAllocInfo,
    defInfo: ast.AstTypeInfo,
};

pub fn typeScan(allocator: Allocator, tree: ast.Ast, context: *Context) !void {
    while (context.compInfo.variableScopes.scopes.items.len > 1) {
        return ScanError.ScanStartedInLowerScope;
    }

    try context.compInfo.pushScope(allocator, false);
    defer context.compInfo.popScope(context);

    try scanNodeForFunctions(allocator, context, tree.root);
    const res = try scanNode(allocator, context, tree.root, true);
    defer releaseIfAllocated(context, res);

    try scanFunctionCalls(allocator, context);
}

pub fn scanNode(
    allocator: Allocator,
    context: *Context,
    node: *ast.AstNode,
    withGenDef: bool,
) ScanNodeError!TypeAndAllocInfo {
    switch (node.variant) {
        .NoOp, .ErrorDec, .EnumDec => {
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .UndefValue => return context.staticPtrs.types.undefType.toAllocInfo(.Recycled),
        .StaticStructInstance => |inst| {
            if (!context.scanInfo.allowStaticStructInstance) {
                return ScanError.StaticStructInstanceCannotBeUsedAsVariable;
            }

            const structInstanceType = try context.pools.newType(context, .{
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

            if (nodeType.info.astType.* == .ArrayDec and cast.toType.astType.* == .ArrayDec) {
                return clonedCastResult;
            }

            return ScanError.InvalidCast;
        },
        .Value => |val| {
            const valueRes: ast.AstTypes = switch (val) {
                .Null => .Null,
                .String => .String,
                .Bool => .Bool,
                .Char => .Char,
                .Number => |num| a: {
                    const numVariant = num.toAstNumberVariant();
                    break :a .{ .Number = numVariant };
                },
                .RawNumber => |num| a: {
                    if (!verifyRawNumberMagnitude(num)) {
                        return ScanError.RawNumberTooBigForType;
                    }
                    break :a .{ .Number = num.numType };
                },
                .ArrayDec => |arr| {
                    const inferredType = try inferArrayDecType(
                        allocator,
                        context,
                        arr,
                        withGenDef,
                    );
                    const valueVariant: ast.AstNodeUnion = .{
                        .Value = .{
                            .Number = .{ .U64 = arr.len },
                        },
                    };
                    const arrayDecType = try context.pools.newType(context, .{
                        .ArrayDec = .{
                            .type = inferredType,
                            .size = try context.pools.newNode(context, valueVariant.toAstNode()),
                        },
                    });

                    const inferredTypeSize = try inferredType.info.astType.getSize(context);
                    node.typeInfo.alignment = try inferredType.info.astType.getAlignment(context);
                    const itemPadding = utils.calculatePadding(
                        inferredTypeSize,
                        node.typeInfo.alignment,
                    );
                    node.typeInfo.size = (inferredTypeSize + itemPadding) * arr.len;

                    return arrayDecType.toAllocInfo(inferredType.info.mutState, .Allocated);
                },
            };

            const valueType = try context.pools.newType(context, valueRes);
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

            if (targetType.info.astType.* == .ArrayDec) {
                const arr = targetType.info.astType.*.ArrayDec;
                const resType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    arr.type,
                    withGenDef,
                );
                node.typeInfo.size = try resType.info.astType.getSize(context);
                node.typeInfo.alignment = try resType.info.astType.getAlignment(context);
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
                    node.typeInfo.alignment = try resType.info.astType.getAlignment(context);
                    return resType;
                },
                .And, .Or => {
                    if (left.info.astType.* != .Bool or right.info.astType.* != .Bool) {
                        return ScanError.ExpectedBoolInBoolOp;
                    }

                    node.typeInfo.size = 1;
                    node.typeInfo.alignment = 1;

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
                            node.typeInfo.alignment = try res.info.astType.getAlignment(context);
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
                            const alignment = try resType.info.astType.getAlignment(context);
                            node.typeInfo.alignment = alignment;
                            return resType;
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (left.info.astType.* == .Number and right.info.astType.* == .Number) {
                        const leftEnum = @intFromEnum(left.info.astType.Number);
                        const rightEnum = @intFromEnum(right.info.astType.Number);
                        if (leftEnum != rightEnum) {
                            return ScanError.NumberTypeMismatch;
                        }

                        node.typeInfo.size = left.info.astType.Number.getSize();
                        node.typeInfo.alignment = left.info.astType.Number.getAlignment();
                    } else if (left.info.astType.* == .Char and right.info.astType.* == .Char) {
                        node.typeInfo.size = 1;
                        node.typeInfo.alignment = 1;
                        releaseIfAllocated(context, right);
                        releaseIfAllocated(context, left);

                        return context.staticPtrs.types.charType.toAllocInfo(.Recycled);
                    }

                    if (op.type == .Div) {
                        const isFloat = switch (left.info.astType.Number) {
                            .F32, .F64 => true,
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

                    node.typeInfo.size = 1;
                    node.typeInfo.alignment = 1;

                    releaseIfAllocated(context, left);
                    releaseIfAllocated(context, right);
                    return context.staticPtrs.types.boolType.toAllocInfo(.Recycled);
                },
            }
        },
        .IncOne, .DecOne => |val| {
            const valType = try scanNode(allocator, context, val, withGenDef);

            if (valType.info.astType.* != .VarInfo) return ScanError.InvalidSetValueTarget;
            if (valType.info.astType.VarInfo.info.astType.* != .Number) {
                return ScanError.CannotIncDecNonNumberType;
            }
            if (valType.info.mutState == .Const) return ScanError.AssigningToConstVariable;
            return valType;
        },
        .Group,
        => |val| {
            const resType = try scanNode(allocator, context, val, withGenDef);
            node.typeInfo.size = try resType.info.astType.getSize(context);
            node.typeInfo.alignment = try resType.info.astType.getAlignment(context);
            return resType;
        },
        .ReturnNode => |ret| {
            if (context.compInfo.currentFuncReturn == null) {
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

            const matches = try matchTypes(
                allocator,
                context,
                context.compInfo.currentFuncReturn.?,
                retTypeInfo,
                withGenDef,
            );
            if (!matches) {
                return ScanError.FunctionReturnTypeMismatch;
            }

            releaseIfAllocated(context, retType);

            context.compInfo.returnInfo.info.hasType = true;
            context.compInfo.returnInfo.info.exhaustive = true;
            context.compInfo.returnInfo.info.lockExhaustive = true;

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncReference => |ref| {
            const dec = try context.compInfo.getFunction(allocator, ref);

            if (dec) |funcRef| {
                const funcType = try context.pools.newType(context, .{
                    .Function = funcRef,
                });
                return funcType.toAllocInfo(.Const, .Allocated);
            }

            return ScanError.IdentifierNotAFunction;
        },
        .PropertyAccess => |access| {
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
                .ArrayDec => {
                    const propType = try builtins.getArrayDecPropType(
                        context,
                        access.property,
                    );
                    node.typeInfo.size = try propType.astType.getSize(context);
                    node.typeInfo.alignment = try propType.astType.getAlignment(context);
                    return propType.toAllocInfo(.Recycled);
                },
                .String => {
                    const propType = try builtins.getStringPropType(
                        context,
                        access.property,
                    );
                    node.typeInfo.size = try propType.astType.getSize(context);
                    node.typeInfo.alignment = try propType.astType.getAlignment(context);
                    return propType.toAllocInfo(.Recycled);
                },
                .Custom => |custom| a: {
                    const def = context.compInfo.getStructDec(custom.name) orelse break :a false;
                    node.typeInfo.accessingFrom = custom.name;

                    try context.compInfo.pushGenScope(allocator, false);
                    defer context.compInfo.popGenScope(context);

                    if (def.generics.len < custom.generics.len) {
                        return ScanError.GenericCountMismatch;
                    }

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
                        if (t.info.astType.* == .Function) {
                            const func = t.info.astType.Function;
                            const strictMutState = valueInfo.info.mutState.orConst(
                                origValueInfo.info.mutState,
                            );
                            if (func.params.selfInfo) |info| {
                                if (info.mutState == .Mut and strictMutState == .Const) {
                                    return ScanError.ExpectedMutableStructInstance;
                                }
                            }
                        }

                        node.typeInfo.size = try t.info.astType.getSize(context);
                        node.typeInfo.alignment = try t.info.astType.getAlignment(context);

                        var copy = t;
                        copy.info.mutState = valueInfo.info.mutState;
                        const varInfo = try context.pools.newType(context, .{
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
                    try context.compInfo.pushGenScope(allocator, false);
                    defer context.compInfo.popGenScope(context);
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

                    const errOrEnumType = try context.pools.newType(context, .{
                        .ErrorOrEnumVariant = .{
                            .from = err.name,
                            .variant = access.property,
                        },
                    });
                    return errOrEnumType.toAllocInfo(valueInfo.info.mutState, .Allocated);
                },
                .Enum => |enumName| {
                    const enumDec = context.compInfo.getEnumDec(enumName).?;
                    if (!utils.inStringArr(enumDec.variants, access.property)) {
                        return ScanError.EnumVariantDoesNotExist;
                    }

                    const errOrEnumType = try context.pools.newType(context, .{
                        .ErrorOrEnumVariant = .{
                            .from = enumName,
                            .variant = access.property,
                        },
                    });
                    return errOrEnumType.toAllocInfo(.Const, .Allocated);
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

            try context.compInfo.setVariableType(context, dec.name, setType, node, dec.mutState);
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
                .Add,
                .Sub,
                .Mult,
                .Div,
                .And,
                .Or,
                .BitAnd,
                .BitOr,
                => {
                    const varType = try scanNode(allocator, context, op.variable, withGenDef);
                    defer releaseIfAllocated(context, varType);

                    if (varType.info.astType.* != .VarInfo) {
                        return ScanError.ValueSetTargetNotAVariable;
                    }

                    if (varType.info.mutState == .Const) {
                        return ScanError.AssigningToConstVariable;
                    }

                    const left = try escapeVarInfo(varType);
                    const rightOrig = try scanNode(allocator, context, op.value, withGenDef);
                    const right = try escapeVarInfo(rightOrig);
                    defer releaseIfAllocated(context, rightOrig);

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
                else => return ScanError.InvalidEqOperationType,
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .Variable => |name| {
            const varInfo = try context.compInfo.getVariableType(
                allocator,
                context,
                name,
                withGenDef,
            );

            if (varInfo) |info| {
                const res = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    info,
                    withGenDef,
                );
                node.typeInfo.size = try info.info.astType.getSize(context);
                node.typeInfo.alignment = try info.info.astType.getAlignment(context);

                try context.compInfo.setVariableLastUsedNode(name, node);

                return res;
            }

            return ScanError.VariableIsUndefined;
        },
        .StructPlaceholder => return context.staticPtrs.types.voidType.toAllocInfo(.Recycled),
        .StructDec => |dec| {
            try scanAttributes(allocator, context, dec);
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .IfStatement => |statement| {
            try context.compInfo.pushScope(allocator, true);
            defer context.compInfo.popScope(context);
            try scanNodeForFunctions(allocator, context, statement.body);
            const prev = try context.compInfo.returnInfo.newInfo(allocator);

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

            try context.compInfo.returnInfo.collapse(prev);

            if (statement.fallback) |fallback| {
                if (!context.compInfo.returnInfo.info.hasType) {
                    context.compInfo.returnInfo.setExhaustive(false);
                }

                try scanIfFallback(allocator, context, fallback, withGenDef);
            } else if (statement.condition.variant != .NoOp) {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .ForLoop => |loop| {
            try context.compInfo.pushScopeWithType(allocator, true, .Loop);
            defer context.compInfo.popScope(context);
            const prev = try context.compInfo.returnInfo.newInfo(allocator);

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

            try context.compInfo.returnInfo.collapse(prev);
            if (context.compInfo.returnInfo.info.hasType) {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .WhileLoop => |loop| {
            try context.compInfo.pushScopeWithType(allocator, true, .Loop);
            defer context.compInfo.popScope(context);
            const prev = try context.compInfo.returnInfo.newInfo(allocator);

            const origConditionType = try scanNode(allocator, context, loop.condition, withGenDef);
            defer releaseIfAllocated(context, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.info.astType.* != .Bool) {
                return ScanError.ExpectedBooleanLoopCondition;
            }

            const bodyRes = try scanNode(allocator, context, loop.body, withGenDef);
            defer releaseIfAllocated(context, bodyRes);

            try context.compInfo.returnInfo.collapse(prev);
            if (context.compInfo.returnInfo.info.hasType) {
                context.compInfo.returnInfo.setExhaustive(false);
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncDec => |name| {
            const func = context.compInfo.getFunctionAsGlobal(name).?;

            if (func.visited) {
                return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
            }

            const capturedVariables = if (func.capturedVariables) |vars| vars else a: {
                const captureScope = try utils.createMut(
                    compInfo.CaptureScope,
                    allocator,
                    compInfo.CaptureScope.init(allocator),
                );
                func.capturedVariables = captureScope;
                break :a captureScope;
            };

            for (func.definedCaptures) |capture| {
                const origVarType = try context.compInfo.getVariableType(
                    allocator,
                    context,
                    capture.ident,
                    withGenDef,
                ) orelse return ScanError.VariableIsUndefined;
                var varType = try escapeVarInfo(origVarType);
                const incomingMutState = origVarType.info.mutState.orConst(varType.info.mutState);
                if (capture.isPtr and incomingMutState == .Const and capture.mutState == .Mut) {
                    return ScanError.CaptureVariableConstMismatch;
                }
                varType.info.mutState = capture.mutState;

                if (capture.isPtr and capture.mutState == .Mut and
                    varType.info.mutState == .Const)
                {
                    return ScanError.PointerTypeConstMismatch;
                }

                const captureType = if (capture.isPtr) a: {
                    const ptrType = try context.pools.newType(context, .{
                        .Pointer = varType,
                    });
                    break :a ptrType.toAllocInfo(
                        capture.mutState,
                        .Allocated,
                    );
                } else varType;

                try capturedVariables.put(capture.ident, captureType);
            }

            try context.compInfo.pushScopeWithTypeAndVarLeak(allocator, true, false, .Function);
            defer context.compInfo.popScope(context);
            try context.compInfo.pushGenScope(allocator, true);
            defer context.compInfo.popGenScope(context);

            const lastRetInfo = try context.compInfo.returnInfo.newInfo(allocator);
            defer context.compInfo.returnInfo.info = lastRetInfo;

            try context.compInfo.addCaptureScope(allocator);
            defer context.compInfo.popCaptureScope(context);
            try context.compInfo.addGenericCaptureScope(allocator);
            defer context.compInfo.popGenericCaptureScope(context);

            if (func.params.selfInfo != null) {
                return ScanError.UnexpectedSelfParameter;
            }

            if (func.generics == null) {
                func.visited = true;
            }

            try scanFuncBodyAndReturn(allocator, context, func, false);

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .FuncCall => |call| {
            const lastRetInfo = try context.compInfo.returnInfo.newInfo(allocator);
            defer context.compInfo.returnInfo.info = lastRetInfo;

            const tempDec = try scanNode(allocator, context, call.func, withGenDef);
            defer releaseIfAllocated(context, tempDec);
            const dec = try escapeVarInfo(tempDec);
            if (dec.info.astType.* != .Function) return ScanError.CannotCallNonFunctionNode;
            const func = dec.info.astType.Function;

            try context.compInfo.pushGenScope(allocator, true);
            defer context.compInfo.popGenScope(context);

            if (call.callGenerics) |callGenerics| {
                if (func.generics) |defGenerics| {
                    if (callGenerics.len != defGenerics.len) {
                        return ScanError.CallGenericsAndFuncDecGenericCountMismatch;
                    }
                } else {
                    return ScanError.UnexpectedCallGenerics;
                }
            }

            for (func.definedCaptures) |value| {
                if (!context.compInfo.isVariableInScope(value.ident)) {
                    return ScanError.CaptureVariableIsNotInScope;
                }
            }

            if (func.params.selfInfo != null and
                !utils.compString("self", func.params.params[0].name))
            {
                return ScanError.ExpectedSelfParameterToBeFirst;
            }

            const lenOffset: u32 = if (func.params.selfInfo != null) 1 else 0;
            const decParams = func.params.params[lenOffset..];
            if (decParams.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            {
                try context.compInfo.pushScopeWithType(allocator, true, .Function);
                defer context.compInfo.popScope(context);

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
                        decParams,
                        call.params,
                        withGenDef,
                    );
                }

                if (func.params.selfInfo) |info| {
                    try context.compInfo.setVariableType(
                        context,
                        "self",
                        func.params.params[0].type.toAllocInfo(.Recycled),
                        null,
                        info.mutState,
                    );
                }

                for (decParams) |param| {
                    const typeClone = try clone.replaceGenericsOnTypeInfo(
                        allocator,
                        context,
                        param.type.toAllocInfo(.Recycled),
                        withGenDef,
                    );

                    try context.compInfo.setVariableType(
                        context,
                        param.name,
                        typeClone,
                        null,
                        param.mutState,
                    );
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
                    try context.compInfo.addFuncToScan(allocator, func, scopeRels, withGenDef);
                }
            }

            const resType = try clone.replaceGenericsOnTypeInfo(
                allocator,
                context,
                func.returnType.toAllocInfo(.Recycled),
                withGenDef,
            );
            node.typeInfo.size = try resType.info.astType.getSize(context);
            node.typeInfo.alignment = try resType.info.astType.getAlignment(context);
            return resType;
        },
        .StructInit => |init| {
            try context.compInfo.pushGenScope(allocator, true);
            defer context.compInfo.popGenScope(context);

            const structDec = context.compInfo.getStructDec(init.name) orelse
                return ScanError.StructDoesNotExist;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.GenericCountMismatch;
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
            if (structDec.generics.len > 0) a: {
                const scope = genScope orelse break :a;

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

                    var typeCaptures = func.capturedTypes orelse b: {
                        const captures = try utils.initMutPtrT(
                            compInfo.CaptureScope,
                            allocator,
                        );
                        func.capturedTypes = captures;
                        break :b captures;
                    };

                    for (scopeRels) |rel| {
                        if (!typeCaptures.contains(rel.str)) {
                            try typeCaptures.put(rel.str, rel.info.toAllocInfo(.Allocated));
                        } else {
                            allocPools.recursiveReleaseType(context, rel.info.astType);
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
            }

            for (structDec.totalMemberList) |attr| {
                if (attr.attr != .Member or attr.static) continue;
                const initAttr = init.findAttribute(attr.name) orelse
                    return ScanError.StructInitAttributeNotFound;

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
            }

            try context.compInfo.pushGenScope(allocator, true);
            defer context.compInfo.popGenScope(context);

            var initAlignment: u8 = 0;
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

                const attrType = attrRel.initInfo.info.astType;
                const attrAlignment = try attrType.getAlignment(context);

                const padding = utils.calculatePadding(node.typeInfo.size, attrAlignment);
                node.typeInfo.size += try attrType.getSize(context) + padding;
                initAlignment = @max(initAlignment, attrAlignment);
            }
            node.typeInfo.alignment = initAlignment;

            const generics = try allocator.alloc(ast.AstTypeInfo, init.generics.len);
            try context.deferCleanup.typeInfoSlices.append(allocator, generics);
            for (init.generics, 0..) |gen, index| {
                generics[index] = try clone.cloneAstTypeInfo(
                    allocator,
                    context,
                    gen,
                    withGenDef,
                );
            }

            const customType = try context.pools.newType(context, .{
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

            node.typeInfo.size = 1;
            node.typeInfo.alignment = 1;
            return context.staticPtrs.types.boolType.toAllocInfo(.Recycled);
        },
        .Error => |err| {
            const dec = context.compInfo.getErrorDec(err).?;
            if (dec.variants.len > 0 and !context.scanInfo.allowErrorWithoutVariants) {
                return ScanError.ExpectedUseOfErrorVariants;
            }

            const errorType = try context.pools.newType(context, .{
                .Error = .{
                    .name = err,
                    .payload = null,
                },
            });
            return errorType.toAllocInfo(.Const, .Allocated);
        },
        .Enum => |enumName| {
            const enumType = try context.pools.newType(context, .{
                .Enum = enumName,
            });
            return enumType.toAllocInfo(.Const, .Allocated);
        },
        .Scope => |scope| {
            try context.compInfo.pushScope(allocator, true);
            defer context.compInfo.popScope(context);
            const prev = try context.compInfo.returnInfo.newInfo(allocator);

            try scanNodeForFunctions(allocator, context, scope);
            try context.compInfo.returnInfo.collapse(prev);

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
            node.typeInfo.alignment = vmInfo.POINTER_SIZE;

            const res = try context.pools.newType(context, .{
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

            const pointer = ptrType.info.astType.Pointer;
            node.typeInfo.size = try pointer.info.astType.getSize(context);
            node.typeInfo.alignment = try pointer.info.astType.getAlignment(context);

            if (ptrType.allocState == .Allocated) {
                const res = ptrType.info.astType.Pointer;
                context.releasePoolType(ptrType.info.astType);
                return res;
            }

            return ptrType.info.astType.Pointer;
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

            const ptrType = try context.pools.newType(context, .{
                .Pointer = typeClone,
            });
            return ptrType.toAllocInfo(.Mut, .Allocated);
        },
        .HeapFree => |toFree| {
            const exprTypeResult = try scanNode(allocator, context, toFree, withGenDef);
            defer releaseIfAllocated(context, exprTypeResult);
            const exprType = try escapeVarInfo(exprTypeResult);
            if (exprType.info.astType.* != .Pointer and exprType.info.astType.* != .ArrayDec) {
                return ScanError.CannotFreeNonPointerType;
            }

            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
        .ArrayInit => |init| {
            try context.compInfo.pushScope(allocator, true);
            defer context.compInfo.popScope(context);

            if (init.indexIdent) |ident| {
                var info = context.staticPtrs.types.u64Type;
                info.mutState = .Const;
                try context.compInfo.setVariableType(
                    context,
                    ident,
                    info.toAllocInfo(.Recycled),
                    null,
                    .Const,
                );
            }

            if (init.ptrIdent) |ident| {
                const initType = init.initType.toAllocInfo(.Recycled);
                const typePtr = try context.pools.newType(context, .{ .Pointer = initType });
                const info = typePtr.toAllocInfo(.Const, .Allocated);
                try context.compInfo.setVariableType(
                    context,
                    ident,
                    info,
                    null,
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

            const valueVariant: ast.AstNodeUnion = .{
                .Value = .{
                    .RawNumber = .{
                        .digits = init.size,
                        .numType = .U64,
                    },
                },
            };
            const arrayDecType = try context.pools.newType(context, .{
                .ArrayDec = .{
                    .type = initTypeClone,
                    .size = try context.pools.newNode(context, valueVariant.toAstNode()),
                },
            });

            const arrSize = std.fmt.parseInt(u64, init.size, 10) catch
                return ScanError.InvalidNumber;
            const initTypeSize = try initTypeClone.info.astType.getSize(context);
            node.typeInfo.size = initTypeSize * arrSize;
            node.typeInfo.alignment = try initTypeClone.info.astType.getAlignment(context);

            return arrayDecType.toAllocInfo(.Mut, .Allocated);
        },
        .InferErrorOrEnumVariant => |variant| {
            const errorOrEnumVariant = try context.pools.newType(context, .{
                .ErrorOrEnumVariant = .{
                    .from = null,
                    .variant = variant,
                },
            });
            return errorOrEnumVariant.toAllocInfo(.Const, .Allocated);
        },
        .Break, .Continue => {
            if (!context.compInfo.inLoopScope()) {
                return ScanError.LoopControlFlowUsedOutsideOfLoop;
            }
            return context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
        },
    }
}

fn genInGenInfoRels(rels: []ast.StrToTypeInfoRel, name: []const u8) bool {
    for (rels) |rel| {
        if (utils.compString(name, rel.gen)) {
            return true;
        }
    }

    return false;
}

fn escapeVarInfo(
    node: TypeAndAllocInfo,
) !TypeAndAllocInfo {
    const allocated = node.allocState;
    var res = node;

    if (res.info.astType.* == .VarInfo) {
        res = res.info.astType.VarInfo;
    }

    if (res.info.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }

    if (allocated == .Recycled and res.allocState == .Allocated) {
        res.allocState = .Recycled;
    }

    return res;
}

// for var infos that were cloned. only to be used after scan node
fn escapeVarInfoAndRelease(
    context: *Context,
    result: TypeAndAllocInfo,
) !TypeAndAllocInfo {
    const allocated = result.allocState;
    var res = result;

    if (res.info.astType.* == .VarInfo) {
        const temp = res.info.astType.VarInfo;

        if (allocated == .Allocated) {
            context.releasePoolType(res.info.astType);
        }

        res = temp;
    }

    if (res.info.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }

    if (allocated == .Recycled and res.allocState == .Allocated) {
        res.allocState = .Recycled;
    }

    return res;
}

/// true if uses undefined variables
fn checkUndefVars(context: *Context, node: *const ast.AstNode, allowSelf: bool) bool {
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
    const functions = context.compInfo.functionsToScan;
    while (functions.items.len > 0) {
        const toScanItem = functions.pop().?;
        const func = toScanItem.func;

        try context.compInfo.pushGenScope(allocator, false);
        defer context.compInfo.popGenScope(context);
        try context.compInfo.pushScopeWithType(allocator, false, .Function);
        defer context.compInfo.popScope(context);

        const lastRetInfo = try context.compInfo.returnInfo.newInfo(allocator);
        defer context.compInfo.returnInfo.info = lastRetInfo;

        if (func.capturedTypes) |captured| {
            var captureIt = captured.iterator();
            while (captureIt.next()) |item| {
                var clonedType = try clone.replaceGenericsOnTypeInfo(
                    allocator,
                    context,
                    item.value_ptr.*,
                    toScanItem.withGenDef,
                );
                clonedType.allocState = .Recycled;
                try context.compInfo.setGeneric(item.key_ptr.*, clonedType);
            }
        }

        if (func.capturedFuncs) |captured| {
            for (captured.items) |item| {
                try context.compInfo.addScopedFunction(allocator, item);
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
    decParams: []ast.Parameter,
    callParams: []*ast.AstNode,
    withGenDef: bool,
) !bool {
    var includesGenerics = false;

    for (decParams, 0..) |decParam, paramIndex| {
        const origCallParam = try scanNode(allocator, context, callParams[paramIndex], withGenDef);
        const callParamType = try escapeVarInfoAndRelease(context, origCallParam);
        defer releaseIfAllocated(context, callParamType);
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
    genScope: *compInfo.TypeScope,
    withGenDef: bool,
) ![]ast.StrToTypeInfoRel {
    const slice = try allocator.alloc(ast.StrToTypeInfoRel, genScope.count());
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
    func: *ast.FuncDecNode,
    genScope: *compInfo.TypeScope,
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

fn isAnyType(astType: *const ast.AstTypes) bool {
    return astType.* == .Generic or astType.* == .Any;
}

fn applyGenericCaptures(
    context: *Context,
    func: *ast.FuncDecNode,
    scope: *compInfo.TypeScope,
) !void {
    if (func.capturedTypes) |captured| {
        allocPools.releaseGenericCaptures(context, captured, .Allocated);
    }

    func.capturedTypes = scope;
}

fn applyFunctionCaptures(
    func: *ast.FuncDecNode,
    scope: *compInfo.StringListScope,
) !void {
    func.capturedFuncs = scope;
}

fn scanIfFallback(
    allocator: Allocator,
    context: *Context,
    fallback: ast.FallbackInfo,
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
    genTypes: []ast.AstTypeInfo,
    decGens: []ast.GenericType,
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

fn matchParamGenericTypes(
    allocator: Allocator,
    context: *Context,
    custom: ast.CustomType,
    paramType: *const ast.AstTypes,
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

                        const genType = try context.compInfo.getGeneric(
                            allocator,
                            context,
                            generic,
                        );
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
    func: *ast.FuncDecNode,
    withGenDef: bool,
) !void {
    try context.compInfo.pushScopeWithType(allocator, true, .Function);
    defer context.compInfo.popScope(context);

    const prevRetType = context.compInfo.currentFuncReturn;
    context.compInfo.currentFuncReturn = func.returnType;
    defer context.compInfo.currentFuncReturn = prevRetType;

    for (func.params.params) |param| {
        const typeClone = try clone.cloneAstTypeInfo(
            allocator,
            context,
            param.type,
            withGenDef,
        );

        try context.compInfo.setVariableType(
            context,
            param.name,
            typeClone.toAllocInfo(.Recycled),
            null,
            param.mutState,
        );
    }

    defer {
        for (func.params.params) |param| {
            const varType = context.compInfo.getVariableTypeFixed(param.name);
            if (varType) |t| {
                const innerType = t.info.astType.VarInfo.info.astType;
                allocPools.recursiveReleaseType(context, innerType);
                t.info.astType.VarInfo = context.staticPtrs.types.voidType.toAllocInfo(.Recycled);
            }
        }
    }

    for (func.definedCaptures) |capture| {
        const item = func.capturedVariables.?.get(capture.ident).?;
        const captureType = try escapeVarInfo(item);

        try context.compInfo.setVariableType(
            context,
            capture.ident,
            captureType.info.toAllocInfo(.Recycled),
            null,
            item.info.mutState,
        );
    }

    defer {
        for (func.definedCaptures) |capture| {
            const captureType = context.compInfo.getVariableTypeFixed(capture.ident);
            if (captureType) |capType| {
                const innerType = capType.info.astType.VarInfo.info.astType;
                allocPools.recursiveReleaseType(context, innerType);
                capType.info.astType.VarInfo = context.staticPtrs.types.voidType.toAllocInfo(
                    .Recycled,
                );
            }
        }
    }

    try scanNodeForFunctions(allocator, context, func.body);
    const bodyRes = try scanNode(allocator, context, func.body, withGenDef);
    defer releaseIfAllocated(context, bodyRes);

    const genScope = context.compInfo.consumeGenericCaptures();
    if (genScope) |s| {
        try applyGenericCaptures(context, func, s);
    }

    const funcScope = context.compInfo.consumeFunctionCaptures();
    if (funcScope) |s| {
        try applyFunctionCaptures(func, s);
    }

    if (func.returnType.astType.* != .Void) {
        if (!context.compInfo.returnInfo.info.exhaustive) {
            return ScanError.FunctionReturnIsNotExhaustive;
        }

        if (!context.compInfo.returnInfo.info.hasType) {
            return ScanError.FunctionMissingReturn;
        }
    }
}

fn validateSelfProps(
    allocator: Allocator,
    context: *Context,
    name: []const u8,
    prop: []const u8,
    inOwnedMethod: bool,
) !?ast.AstTypeInfo {
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

        return null;
    }

    return ScanError.StructDoesNotExist;
}

fn validateStaticStructProps(
    allocator: Allocator,
    context: *Context,
    name: []const u8,
    prop: []const u8,
) !?ast.AstTypeInfo {
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
    custom: ast.CustomType,
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

        return null;
    }

    return ScanError.InvalidPropertySource;
}

fn scanAttributes(allocator: Allocator, context: *Context, dec: *ast.StructDecNode) !void {
    for (dec.attributes) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                if (!attr.static and func.params.selfInfo == null) {
                    return ScanError.ExpectedSelfParameter;
                }

                try context.compInfo.addFuncToScan(
                    allocator,
                    func,
                    &[_]ast.StrToTypeInfoRel{},
                    false,
                );
            },
        }
    }
}

fn isInt(astType: *const ast.AstTypes) bool {
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
    toType: ast.AstTypeInfo,
    fromType: ast.AstTypeInfo,
    withGenDef: bool,
) ScanNodeError!bool {
    return try matchTypesUtil(allocator, context, toType, fromType, withGenDef, .Assign);
}

/// match types as if fromType is being set to toType to match mutability
pub fn matchTypesUtil(
    allocator: Allocator,
    context: *Context,
    toType: ast.AstTypeInfo,
    fromType: ast.AstTypeInfo,
    withGenDef: bool,
    mutMatchBehavior: MutMatchBehavior,
) !bool {
    const type1 = toType.astType.*;
    const type2 = fromType.astType.*;

    if (type1 == .Any or type2 == .Any) return true;
    if (type1 == .Undef or type2 == .Undef) return true;

    if (type1 == .Generic and type2 == .Generic) {
        if (withGenDef) {
            var genType1 = try context.compInfo.getGeneric(allocator, context, type1.Generic);
            if (genType1) |*gType| {
                if (gType.info.astType.* == .Generic and
                    utils.compString(type1.Generic, gType.info.astType.Generic))
                {
                    return ScanError.UnexpectedRecursiveGeneric;
                }
            } else if (withGenDef) {
                return ScanError.EmptyGenericType;
            }

            var genType2 = try context.compInfo.getGeneric(allocator, context, type2.Generic);
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

        var genType = try context.compInfo.getGeneric(allocator, context, type1.Generic);
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

        var genType = try context.compInfo.getGeneric(allocator, context, type2.Generic);
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
        .Nullable => |inner| {
            if (type2 == .Nullable) {
                return try matchTypesUtil(
                    allocator,
                    context,
                    inner,
                    type2.Nullable,
                    withGenDef,
                    mutMatchBehavior,
                );
            }

            return type2 == .Null or try matchTypesUtil(
                allocator,
                context,
                inner,
                fromType,
                withGenDef,
                mutMatchBehavior,
            );
        },
        .RawNumber => return type2 == .Number or type2 == .RawNumber,
        .Number => |num| {
            if (type2 == .Number) {
                return @intFromEnum(num) == @intFromEnum(type2.Number);
            }

            return type2 == .RawNumber;
        },
        .ArrayDec => |arr| {
            const array: ast.AstArrayDecType = switch (type2) {
                .ArrayDec => |arrayDec| arrayDec,
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
            .Error => |err2| return utils.compString(err.name, err2.name),
            .ErrorOrEnumVariant => |err2| {
                const matchesErr = a: {
                    if (err2.from) |from| {
                        break :a utils.compString(err.name, from);
                    }

                    const errDec = context.compInfo.getErrorDec(err.name).?;
                    break :a utils.inStringArr(errDec.variants, err2.variant);
                };

                if (matchesErr) return true;

                if (err.payload) |payload| {
                    return matchTypesUtil(
                        allocator,
                        context,
                        payload,
                        fromType,
                        withGenDef,
                        mutMatchBehavior,
                    );
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
        .ErrorOrEnumVariant => |err| switch (type2) {
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
            .Enum => |enum2Name| {
                if (err.from) |from| {
                    return utils.compString(enum2Name, from);
                }

                const errDec = context.compInfo.getEnumDec(enum2Name);
                if (errDec) |dec| {
                    return utils.inStringArr(dec.variants, err.variant);
                }

                return false;
            },
            .ErrorOrEnumVariant => |errOrEnum2| {
                const variantsMatch = utils.compString(err.variant, errOrEnum2.variant);

                if (err.from != null and errOrEnum2.from != null) {
                    return utils.compString(err.from.?, errOrEnum2.from.?) and variantsMatch;
                }

                return variantsMatch;
            },
            else => false,
        },
        .Enum => |enumName| switch (type2) {
            .Enum => |enum2Name| return utils.compString(enumName, enum2Name),
            .ErrorOrEnumVariant => |errOrEnum2| {
                if (errOrEnum2.from) |fromEnumName| {
                    return utils.compString(enumName, fromEnumName);
                }

                const enumDec = context.compInfo.getEnumDec(enumName).?;
                return utils.inStringArr(enumDec.variants, errOrEnum2.variant);
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
    toType: ast.AstTypeInfo,
    fromType: ast.AstTypeInfo,
    typesMatched: bool,
    mutMatchBehavior: MutMatchBehavior,
) !bool {
    if (!typesMatched) {
        return false;
    }

    switch (mutMatchBehavior) {
        .Assign => {
            const matchRequired = toType.astType.* == .Pointer;
            if (matchRequired and toType.mutState == .Mut and fromType.mutState == .Const) {
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

pub fn isPrimitive(astType: *const ast.AstTypes) bool {
    return switch (astType.*) {
        .String, .Bool, .Char, .Number, .Null, .RawNumber => true,
        .Nullable => |inner| isPrimitive(inner.astType),
        else => false,
    };
}

fn getPropertyType(
    allocator: Allocator,
    context: *Context,
    source: ast.AstTypes,
    prop: []const u8,
) !ast.AstTypes {
    return switch (source) {
        .StaticStructInstance => |inst| try getStructPropType(context, false, inst, prop),
        .ArrayDec => try builtins.getArrayDecPropTypes(prop),
        .String => try builtins.getStringPropTypes(prop),
        .Custom => |custom| getCustomPropType(allocator, context, custom, prop),
        else => ScanError.UnsupportedFeature,
    };
}

fn getCustomPropType(
    allocator: Allocator,
    context: *Context,
    custom: ast.CustomType,
    prop: []const u8,
) !ast.AstTypes {
    const dec = context.compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (!utils.compString(attr.name, prop)) continue;
            if (attr.static) return ScanError.StaticAccessFromStructInstance;

            if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

            return try clone.cloneStructAttributeUnion(allocator, context, attr.attr, false);
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
) !ast.AstTypes {
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

fn inferArrayDecType(
    allocator: Allocator,
    context: *Context,
    arr: []*ast.AstNode,
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
            return ScanError.ArrayTypeMismatch;
        }
    }

    firstType.info.mutState = mutState;
    return firstType;
}

fn scanNodeForFunctions(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
) !void {
    switch (node.variant) {
        .FuncDec => |dec| {
            try context.compInfo.addScopedFunction(allocator, dec);
        },
        .Seq => |seq| {
            for (seq) |seqNode| {
                try scanNodeForFunctions(allocator, context, seqNode);
            }
        },
        else => {},
    }
}

fn verifyRawNumberMagnitude(node: ast.RawNumberNode) bool {
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
        allocPools.recursiveReleaseType(context, result.info.astType);
    }
}
