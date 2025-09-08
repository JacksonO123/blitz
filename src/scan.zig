const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const builtins = @import("builtins.zig");
const clone = blitz.clone;
const number = blitz.number;
const string = blitz.string;
const blitzCompInfo = blitz.compInfo;
const Allocator = std.mem.Allocator;
const CompInfo = blitzCompInfo.CompInfo;
const ArrayList = std.ArrayList;
const create = utils.create;
const createMut = utils.createMut;

pub const RetNodeInfo = struct {
    node: blitzAst.AstNode,
    conditional: bool,
};

const MutMatchBehavior = enum {
    Assign, // allows mut to const
    Strict, // must match exactly
};

pub const ScanNodeError = Allocator.Error || ScanError || clone.CloneError;

pub const ScanError = error{
    // misc
    InvalidCast,
    ExpectedBooleanBang,
    ExpectedBooleanIfCondition,
    UnsupportedFeature,
    ExpectedUSizeForIndex,
    StaticStructInstanceCannotBeUsedAsVariable,
    InvalidNumber,
    IfStatementMayOnlyHaveOneElse,
    ElseBranchOutOfOrder,
    NestedVarInfoDetected,

    // pointers
    PointerTypeMismatch,
    CannotDereferenceNonPointerValue,
    CannotTakePointerOfRawValue,

    // arrays
    ArraySliceTypeMismatch,
    ExpectedArrayForIndexTarget,
    ExpectedUSizeOrU32ForArraySliceSize,
    ExpectedEqualArraySliceSizes,
    SizedSliceSetToUnknownSizedSlice,
    ArrayInitTypeInitializerMismatch,

    // loops
    ExpectedBooleanLoopCondition,

    // variables
    VariableAnnotationMismatch,
    VariableAlreadyExists,
    VoidVariableDec,
    VariableTypeAndValueTypeMismatch,
    AssigningToConstVariable,
    PointerTypeConstMismatch,
    StrictMutTypeMismatch,
    CannotSetToNonVarTypeValue,

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

    // structs
    StructInitGenericCountMismatch,
    StructInitAttributeCountMismatch,
    StructInitMemberTypeMismatch,
    StructInitAttributeNotFound,
    InvalidProperty,
    StaticAccessFromStructInstance,
    NonStaticAccessFromStaticStructReference,
    UnexpectedDeriveType,
    SelfUsedOutsideStruct,
    UndefinedStruct,
    RestrictedPropertyAccess,
    InvalidPropertySource,
    NonPublicStructFieldAccessFromOutsideDefinition,

    // operations
    MathOpOnNonNumberType,
    MathOpTypeMismatch,
    ExpectedBoolInBoolOp,
    InvalidBitOperation,
    BitMaskWithMismatchingSize,
    NumberTypeMismatch,
    ComparisonOnNonNumberType,

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

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, compInfo: *CompInfo) !void {
    while (compInfo.variableScopes.scopes.items.len > 1) compInfo.popScope();

    try scanNodeForFunctions(allocator, compInfo, ast.root);
    const nodeType = try scanNode(allocator, compInfo, ast.root, true);
    free.freeAstTypeInfo(allocator, nodeType);

    try scanFunctionCalls(allocator, compInfo);
}

pub fn scanNodes(
    allocator: Allocator,
    compInfo: *CompInfo,
    nodes: []*blitzAst.AstNode,
    withGenDef: bool,
) (ScanError || Allocator.Error || clone.CloneError)!void {
    for (nodes) |node| {
        const nodeType = try scanNode(allocator, compInfo, node, withGenDef);
        free.freeAstTypeInfo(allocator, nodeType);
    }
}

pub fn scanNode(
    allocator: Allocator,
    compInfo: *CompInfo,
    node: *blitzAst.AstNode,
    withGenDef: bool,
) ScanNodeError!blitzAst.AstTypeInfo {
    switch (node.*) {
        .NoOp, .ErrorDec => return try utils.astTypesToInfo(allocator, .Void, true),

        .StaticStructInstance => |inst| {
            if (!compInfo.scanner.allowStaticStructInstance) {
                return ScanError.StaticStructInstanceCannotBeUsedAsVariable;
            }

            return try utils.astTypesToInfo(allocator, .{
                .StaticStructInstance = try string.cloneString(allocator, inst),
            }, true);
        },
        .Cast => |cast| {
            const clonedCastType = try clone.cloneAstTypeInfo(allocator, compInfo, cast.toType, withGenDef);

            if (cast.node.* == .Value and cast.node.Value == .RawNumber) {
                return clonedCastType;
            }

            const origNodeType = try scanNode(allocator, compInfo, cast.node, withGenDef);
            defer free.freeAstTypeInfo(allocator, origNodeType);
            const nodeType = try escapeVarInfo(origNodeType);

            if (isAnyType(nodeType.astType) or isPrimitive(nodeType.astType) and isPrimitive(cast.toType.astType)) {
                return clonedCastType;
            }

            if (nodeType.astType.* == .ArraySlice and cast.toType.astType.* == .ArraySlice) {
                return clonedCastType;
            }

            return ScanError.InvalidCast;
        },
        .Value => |val| {
            const valueRes: blitzAst.AstTypes = switch (val) {
                .Null => .Null,
                .String => .String,
                .Bool => .Bool,
                .Char => .Char,
                .Number => |num| .{ .Number = num.toAstNumberVariant() },
                .RawNumber => |num| .{ .Number = num.numType },
                .ArraySlice => |arr| {
                    const inferredType = try inferArraySliceType(allocator, compInfo, arr, withGenDef);
                    const arraySliceType = try createMut(blitzAst.AstTypes, allocator, .{
                        .ArraySlice = .{
                            .type = inferredType,
                            .size = try createMut(blitzAst.AstNode, allocator, .{
                                .Value = .{
                                    .Number = .{ .USize = arr.len },
                                },
                            }),
                        },
                    });

                    return utils.astTypesPtrToInfo(arraySliceType, inferredType.isConst);
                },
            };

            return try utils.astTypesToInfo(allocator, valueRes, false);
        },
        .Type => |t| return try utils.astTypesToInfo(allocator, t, false),
        .IndexValue => |indexInfo| {
            const origIndexType = try scanNode(allocator, compInfo, indexInfo.index, withGenDef);
            defer free.freeAstTypeInfo(allocator, origIndexType);
            const indexType = try escapeVarInfo(origIndexType);
            const origValueType = try scanNode(allocator, compInfo, indexInfo.value, withGenDef);
            defer free.freeAstTypeInfo(allocator, origValueType);
            const valueType = try escapeVarInfo(origValueType);

            if (indexType.astType.* == .Number and indexType.astType.Number != .USize) {
                return ScanError.ExpectedUSizeForIndex;
            }

            if (valueType.astType.* == .ArraySlice) {
                const arr = valueType.astType.*.ArraySlice;
                return try clone.cloneAstTypeInfo(allocator, compInfo, arr.type, false);
            }

            return ScanError.ExpectedArrayForIndexTarget;
        },
        .OpExpr => |op| {
            const origLeft = try scanNode(allocator, compInfo, op.left, withGenDef);
            defer free.freeAstTypeInfo(allocator, origLeft);
            const left = try escapeVarInfo(origLeft);
            const origRight = try scanNode(allocator, compInfo, op.right, withGenDef);
            defer free.freeAstTypeInfo(allocator, origRight);
            const right = try escapeVarInfo(origRight);

            switch (op.type) {
                .BitAnd, .BitOr => {
                    if (left.astType.* != .Number or right.astType.* != .Number) return ScanError.InvalidBitOperation;

                    if (left.astType.Number.getSize() != right.astType.Number.getSize()) return ScanError.BitMaskWithMismatchingSize;
                    const typeClone = try clone.cloneAstTypesPtrMut(allocator, compInfo, left.astType, withGenDef);
                    return utils.astTypesPtrToInfo(typeClone, false);
                },
                .And, .Or => {
                    if (left.astType.* != .Bool or right.astType.* != .Bool) return ScanError.ExpectedBoolInBoolOp;
                    return try utils.astTypesToInfo(allocator, .Bool, false);
                },
                .Add, .Sub, .Mult, .Div => {
                    if (isAnyType(left.astType)) {
                        if (right.astType.* != .Number and !isAnyType(right.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (try matchTypes(allocator, compInfo, left, right, withGenDef)) {
                            var res = try clone.cloneAstTypeInfo(allocator, compInfo, right, withGenDef);
                            res.isConst = false;
                            return res;
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (isAnyType(right.astType)) {
                        if (left.astType.* != .Number and !isAnyType(left.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (try matchTypes(allocator, compInfo, left, right, withGenDef)) {
                            const typeClone = try clone.cloneAstTypesPtrMut(allocator, compInfo, left.astType, withGenDef);
                            return utils.astTypesPtrToInfo(typeClone, false);
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (op.type == .Div) {
                        if (switch (left.astType.Number) {
                            .F32, .F64, .F128 => true,
                            else => false,
                        }) {
                            const typeClone = try clone.cloneAstTypesPtrMut(allocator, compInfo, left.astType, withGenDef);
                            return utils.astTypesPtrToInfo(typeClone, false);
                        }

                        return try utils.astTypesToInfo(allocator, .{
                            .Number = .F32,
                        }, false);
                    }

                    const typeClone = try clone.cloneAstTypesPtrMut(allocator, compInfo, left.astType, withGenDef);
                    return utils.astTypesPtrToInfo(typeClone, false);
                },
                .LessThan,
                .GreaterThan,
                .LessThanEq,
                .GreaterThanEq,
                .Equal,
                .NotEqual,
                => {
                    if (!isAnyType(left.astType) and !isAnyType(right.astType)) {
                        if (left.astType.* != .Number or right.astType.* != .Number) {
                            return ScanError.ComparisonOnNonNumberType;
                        }
                        if (@intFromEnum(left.astType.Number) != @intFromEnum(right.astType.Number)) {
                            return ScanError.NumberTypeMismatch;
                        }
                    }

                    return try utils.astTypesToInfo(allocator, .Bool, false);
                },
            }
        },
        .IncOne,
        .DecOne,
        .Group,
        => |val| {
            const valType = try scanNode(allocator, compInfo, val, withGenDef);
            return utils.astTypesPtrToInfo(valType.astType, false);
        },
        .ReturnNode => |ret| {
            if (!compInfo.returnInfo.info.inFunction) {
                return ScanError.UnexpectedReturnStatement;
            }

            const valType = try scanNode(allocator, compInfo, ret, withGenDef);

            if (compInfo.returnInfo.info.retType) |retType| {
                const matches = try matchTypes(allocator, compInfo, retType, valType, withGenDef);
                if (!matches) {
                    return ScanError.FunctionReturnsHaveDifferentTypes;
                }
                free.freeAstTypeInfo(allocator, valType);
            } else {
                compInfo.returnInfo.info.retType = valType;
            }

            compInfo.returnInfo.info.exhaustive = true;
            compInfo.returnInfo.info.lockExhaustive = true;

            var res = try clone.cloneAstTypeInfo(allocator, compInfo, valType, withGenDef);
            res.isConst = true;
            return res;
        },
        .FuncReference => |ref| {
            const dec = try compInfo.getFunction(ref);

            if (dec) |funcRef| {
                return try utils.astTypesToInfo(allocator, .{
                    .Function = funcRef,
                }, true);
            }

            return ScanError.IdentifierNotAFunction;
        },
        .PropertyAccess => |access| {
            if (access.value.* == .Error) {
                compInfo.scanner.allowErrorWithoutVariants = true;
            }

            if (access.value.* == .StaticStructInstance) {
                compInfo.scanner.allowStaticStructInstance = true;
            }

            const origValueInfo = try scanNode(allocator, compInfo, access.value, withGenDef);
            defer free.freeAstTypeInfo(allocator, origValueInfo);
            const valueInfo = try escapeVarInfo(origValueInfo);
            compInfo.scanner.allowStaticStructInstance = false;
            compInfo.scanner.allowErrorWithoutVariants = false;

            const valid: bool = switch (valueInfo.astType.*) {
                .Generic => return try utils.astTypesToInfo(allocator, .Any, valueInfo.isConst),
                .ArraySlice => return try builtins.getArraySlicePropType(allocator, access.property),
                .String => return try builtins.getStringPropType(allocator, access.property),
                .Custom => |custom| a: {
                    const def = compInfo.getStructDec(custom.name) orelse break :a false;
                    compInfo.setPreviousAccessedStruct(def.name);

                    var genNameArr = try ArrayList([]u8).initCapacity(allocator, custom.generics.len);
                    var genTypeArr = try ArrayList(blitzAst.AstTypeInfo).initCapacity(allocator, custom.generics.len);
                    defer genNameArr.deinit();
                    defer genTypeArr.deinit();

                    for (custom.generics, 0..) |gen, index| {
                        const genDef = def.generics[index];
                        const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, gen, withGenDef);
                        try genNameArr.append(genDef.name);
                        try genTypeArr.append(typeClone);
                    }

                    try compInfo.pushGenScope(false);
                    defer compInfo.popGenScope();

                    for (genNameArr.items, genTypeArr.items) |name, genType| {
                        try compInfo.setGeneric(name, genType);
                    }

                    var propType = try validateCustomProps(allocator, compInfo, custom, access.property, withGenDef);

                    if (propType) |*t| {
                        std.debug.print("HERE\n", .{});
                        blitz.debug.printTypeInfo(compInfo, t.*);
                        std.debug.print("\n", .{});
                        t.*.isConst = valueInfo.isConst;
                        return typeInfoToVarInfo(allocator, t.*, origValueInfo.isConst or valueInfo.isConst);
                    }

                    break :a false;
                },
                .StaticStructInstance => |name| a: {
                    try compInfo.pushGenScope(false);
                    defer compInfo.popGenScope();
                    var propType = try validateStaticStructProps(allocator, compInfo, name, access.property);

                    if (propType) |t| {
                        if (!string.compString(name, "self")) {
                            const dec = compInfo.getStructDec(name).?;

                            if (t.astType.* == .Function) {
                                for (dec.generics) |gen| {
                                    if (gen.restriction) |restriction| {
                                        const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, restriction, false);
                                        try compInfo.setGeneric(gen.name, typeClone);
                                    }
                                }
                            }
                        }

                        propType.?.isConst = valueInfo.isConst;
                        return t;
                    }

                    break :a false;
                },
                .Error => |err| {
                    const errDec = compInfo.getErrorDec(err.name).?;
                    if (errDec.variants) |variants| {
                        if (!string.inStringArr(variants, access.property)) {
                            return ScanError.ErrorVariantDoesNotExist;
                        }
                    } else {
                        return ScanError.ErrorDoesNotHaveVariants;
                    }

                    return try utils.astTypesToInfo(allocator, .{
                        .ErrorVariant = .{
                            .from = try string.cloneString(allocator, err.name),
                            .variant = try string.cloneString(allocator, access.property),
                        },
                    }, valueInfo.isConst);
                },
                else => false,
            };

            if (!valid) return ScanError.InvalidProperty;

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .Seq => |seq| {
            try scanNodes(allocator, compInfo, seq.nodes, withGenDef);
            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .VarDec => |*dec| {
            if (compInfo.getVariableTypeFixed(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            const origSetType = try scanNode(allocator, compInfo, dec.setNode, withGenDef);
            var setType = try escapeVarInfoAndFree(allocator, origSetType);

            if (setType.astType.* == .Void) {
                return ScanError.VoidVariableDec;
            }

            if (dec.annotation) |annotation| {
                const matches = try matchTypes(allocator, compInfo, annotation, setType, false);
                if (!matches) {
                    return ScanError.VariableAnnotationMismatch;
                }

                free.freeAstTypeInfo(allocator, origSetType);
                setType = try clone.cloneAstTypeInfo(allocator, compInfo, annotation, withGenDef);
            }

            dec.setType = setType;

            try compInfo.setVariableType(dec.name, setType, dec.isConst);
            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .ValueSet => |set| {
            const origValType = try scanNode(allocator, compInfo, set.value, withGenDef);
            defer free.freeAstTypeInfo(allocator, origValType);
            if (origValType.isConst) return ScanError.AssigningToConstVariable;
            if (origValType.astType.* != .VarInfo) return ScanError.CannotSetToNonVarTypeValue;
            const valType = try escapeVarInfo(origValType);

            const setType = try scanNode(allocator, compInfo, set.setNode, withGenDef);
            defer free.freeAstTypeInfo(allocator, setType);

            const matches = try matchTypes(allocator, compInfo, valType, setType, withGenDef);
            if (!matches) {
                return ScanError.VariableTypeAndValueTypeMismatch;
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .VarEqOp => |op| {
            switch (op.opType) {
                .AddEq, .SubEq, .MultEq, .DivEq => {
                    const variable = if (try compInfo.getVariableType(op.variable, withGenDef)) |val|
                        val
                    else {
                        return ScanError.VariableIsUndefined;
                    };

                    if (variable.isConst) {
                        return ScanError.AssigningToConstVariable;
                    }

                    const left = variable;
                    const right = try scanNode(allocator, compInfo, op.value, withGenDef);

                    if (isAnyType(left.astType)) {
                        if (right.astType.* != .Number and !isAnyType(right.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(allocator, compInfo, left, right, withGenDef);
                        if (!matches) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (isAnyType(right.astType)) {
                        if (left.astType.* != .Number and !isAnyType(right.astType)) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        const matches = try matchTypes(allocator, compInfo, left, right, withGenDef);
                        if (!matches) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (@intFromEnum(left.astType.Number) != @intFromEnum(right.astType.Number)) {
                        return ScanError.NumberTypeMismatch;
                    }
                },
                else => {},
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .Variable => |name| {
            if (compInfo.isInStructMethod() and string.compString(name, "self")) {
                return try utils.astTypesToInfo(allocator, .{
                    .StaticStructInstance = try string.cloneString(allocator, name),
                }, true);
            }

            const varInfo = try compInfo.getVariableType(name, withGenDef);

            if (varInfo) |info| {
                return try clone.cloneAstTypeInfo(allocator, compInfo, info, withGenDef);
            }

            return ScanError.VariableIsUndefined;
        },
        .StructPlaceholder => return try utils.astTypesToInfo(allocator, .Void, false),
        .StructDec => |dec| {
            try compInfo.addCurrentStruct(dec.name);
            defer _ = compInfo.popCurrentStruct();

            compInfo.enteringStruct();
            try scanAttributes(compInfo, dec.attributes);
            compInfo.exitingStruct();

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .IfStatement => |statement| {
            try compInfo.pushScope(true);
            defer compInfo.popScope();
            try scanNodeForFunctions(allocator, compInfo, statement.body);
            const prev = try compInfo.returnInfo.newInfo(false);

            const origConditionType = try scanNode(allocator, compInfo, statement.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.astType.* != .Bool) return ScanError.ExpectedBooleanIfCondition;

            const body = try scanNode(allocator, compInfo, statement.body, withGenDef);
            defer free.freeAstTypeInfo(allocator, body);

            try compInfo.returnInfo.collapse(compInfo, prev, withGenDef);

            if (statement.fallback) |fallback| {
                if (!compInfo.returnInfo.hasType()) {
                    compInfo.returnInfo.setExhaustive(false);
                }

                try scanIfFallback(allocator, compInfo, fallback, withGenDef);
            } else {
                compInfo.returnInfo.setExhaustive(false);
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .ForLoop => |loop| {
            try compInfo.pushScope(true);
            defer compInfo.popScope();
            const prev = try compInfo.returnInfo.newInfo(false);

            if (loop.initNode) |init| {
                const initType = try scanNode(allocator, compInfo, init, withGenDef);
                free.freeAstTypeInfo(allocator, initType);
            }

            const origConditionType = try scanNode(allocator, compInfo, loop.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.astType.* != .Bool) {
                return ScanError.ExpectedBooleanLoopCondition;
            }

            const incType = try scanNode(allocator, compInfo, loop.incNode, withGenDef);
            free.freeAstTypeInfo(allocator, incType);

            const bodyType = try scanNode(allocator, compInfo, loop.body, withGenDef);
            free.freeAstTypeInfo(allocator, bodyType);

            try compInfo.returnInfo.collapse(compInfo, prev, withGenDef);
            if (compInfo.returnInfo.hasType()) {
                compInfo.returnInfo.setExhaustive(false);
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .WhileLoop => |loop| {
            const prev = try compInfo.returnInfo.newInfo(false);

            const origConditionType = try scanNode(allocator, compInfo, loop.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, origConditionType);
            const conditionType = try escapeVarInfo(origConditionType);
            if (conditionType.astType.* != .Bool) {
                return ScanError.ExpectedBooleanLoopCondition;
            }

            const bodyType = try scanNode(allocator, compInfo, loop.body, withGenDef);
            free.freeAstTypeInfo(allocator, bodyType);

            try compInfo.returnInfo.collapse(compInfo, prev, withGenDef);
            if (compInfo.returnInfo.hasType()) {
                compInfo.returnInfo.setExhaustive(false);
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .FuncDec => |name| {
            try compInfo.pushScope(true);
            defer compInfo.popScope();
            try compInfo.pushGenScope(true);
            defer compInfo.popGenScope();
            const prev = compInfo.returnInfo.setInFunction(true);
            defer compInfo.returnInfo.revertInFunction(prev);
            const lastRetInfo = try compInfo.returnInfo.newInfo(true);
            defer compInfo.returnInfo.swapFree(lastRetInfo);

            try compInfo.addCaptureScope();
            defer compInfo.popCaptureScope();
            try compInfo.addGenericCaptureScope();
            defer compInfo.popGenericCaptureScope();

            const func = compInfo.getFunctionAsGlobal(name).?;

            if (func.visited) {
                return try utils.astTypesToInfo(allocator, .Void, true);
            }

            if (func.generics == null) {
                func.visited = true;
            }

            try scanFuncBodyAndReturn(allocator, compInfo, func, false);

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .FuncCall => |call| {
            const prev = compInfo.returnInfo.setInFunction(true);
            defer compInfo.returnInfo.revertInFunction(prev);

            const lastRetInfo = try compInfo.returnInfo.newInfo(true);
            defer compInfo.returnInfo.swapFree(lastRetInfo);

            const tempDec = try scanNode(allocator, compInfo, call.func, withGenDef);
            const dec = try escapeVarInfoAndFree(allocator, tempDec);
            // only destroy pointer because function declaration instance must be preserved
            defer allocator.destroy(dec.astType);
            if (dec.astType.* != .Function) return ScanError.CannotCallNonFunctionNode;
            const func = dec.astType.Function;

            try compInfo.pushGenScope(true);
            defer compInfo.popGenScope();

            const prevAccessed = compInfo.getPreviousAccessedStruct();
            if (prevAccessed) |accessed| {
                try compInfo.addCurrentStruct(accessed);
                compInfo.enteringStruct();
            }

            defer {
                if (prevAccessed != null) {
                    compInfo.setPreviousAccessedStruct(null);
                    compInfo.exitingStruct();
                    _ = compInfo.popCurrentStruct();
                }
            }

            const paramTypes = try allocator.alloc(blitzAst.AstTypeInfo, call.params.len);
            defer allocator.free(paramTypes);
            for (call.params, 0..) |param, index| {
                paramTypes[index] = try scanNode(allocator, compInfo, param, withGenDef);
            }

            if (func.params.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            {
                try compInfo.pushScope(true);
                defer compInfo.popScope();

                _ = try setGenTypesFromParams(allocator, compInfo, func, paramTypes, withGenDef);

                for (func.params) |param| {
                    const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, param.type, withGenDef);
                    try compInfo.setVariableType(param.name, typeClone, param.isConst);
                }

                const usesUndefinedVars = checkUndefVars(compInfo, func.body);
                if (usesUndefinedVars) {
                    return ScanError.VariableIsUndefined;
                }
            }

            if (func.generics != null) {
                const genScope = compInfo.genericScopes.getCurrentScope().?;
                const scannedBefore = try fnHasScannedWithSameGenTypes(allocator, compInfo, func, genScope, withGenDef);
                if (!scannedBefore) {
                    const scopeRels = try genScopeToRels(allocator, compInfo, genScope, withGenDef);
                    try func.toScanTypes.append(scopeRels);
                    try compInfo.addFuncToScan(func, scopeRels, withGenDef);
                }
            }

            return try clone.cloneAstTypeInfo(allocator, compInfo, func.returnType, withGenDef);
        },
        .StructInit => |init| {
            try compInfo.pushGenScope(true);
            defer compInfo.popGenScope();

            const structDec = compInfo.getStructDec(init.name).?;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.StructInitGenericCountMismatch;
            }

            try setInitGenerics(allocator, compInfo, init.generics, structDec.generics, withGenDef);

            if (structDec.deriveType) |derive| {
                try setInitDeriveGenerics(allocator, compInfo, derive);
            }

            if (init.attributes.len != structDec.totalMemberList.len) {
                return ScanError.StructInitAttributeCountMismatch;
            }

            for (structDec.totalMemberList) |attr| {
                if (attr.static) continue;
                if (attr.attr != .Member) continue;

                var attrNode: ?*blitzAst.AstNode = null;
                for (init.attributes) |initAttr| {
                    if (string.compString(initAttr.name, attr.name)) {
                        attrNode = initAttr.value;
                        break;
                    }
                }

                if (attrNode == null) {
                    return ScanError.StructInitAttributeNotFound;
                }

                const attrType = try scanNode(allocator, compInfo, attrNode.?, withGenDef);
                defer free.freeAstTypeInfo(allocator, attrType);

                const matches = try matchTypes(allocator, compInfo, attr.attr.Member, attrType, withGenDef);
                if (!matches) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            const generics = try allocator.alloc(blitzAst.AstTypeInfo, init.generics.len);
            for (init.generics, 0..) |gen, index| {
                generics[index] = try clone.cloneAstTypeInfo(allocator, compInfo, gen, withGenDef);
            }

            return try utils.astTypesToInfo(allocator, .{
                .Custom = .{
                    .generics = generics,
                    .name = try string.cloneString(allocator, init.name),
                },
            }, false);
        },
        .Bang => |bang| {
            const origBangType = try scanNode(allocator, compInfo, bang, withGenDef);
            defer free.freeAstTypeInfo(allocator, origBangType);
            const bangType = try escapeVarInfo(origBangType);
            if (bangType.astType.* != .Bool) return ScanError.ExpectedBooleanBang;

            return try utils.astTypesToInfo(allocator, .Bool, false);
        },
        .Error => |err| {
            const dec = compInfo.getErrorDec(err).?;
            if (dec.variants != null and !compInfo.scanner.allowErrorWithoutVariants) {
                return ScanError.ExpectedUseOfErrorVariants;
            }

            return try utils.astTypesToInfo(allocator, .{
                .Error = .{
                    .name = try string.cloneString(allocator, err),
                    .payload = null,
                },
            }, true);
        },
        .Scope => |scope| {
            try compInfo.pushScope(true);
            defer compInfo.popScope();
            const prev = try compInfo.returnInfo.newInfo(false);

            try scanNodeForFunctions(allocator, compInfo, scope);
            try compInfo.returnInfo.collapse(compInfo, prev, withGenDef);

            return scanNode(allocator, compInfo, scope, withGenDef);
        },
        .Pointer => |ptr| {
            var ptrType = try scanNode(allocator, compInfo, ptr.node, withGenDef);

            if (!ptr.isConst and ptrType.astType.* == .VarInfo and ptrType.isConst) {
                return ScanError.PointerTypeConstMismatch;
            }

            switch (ptrType.astType.*) {
                .Bool, .Char, .Void, .Null, .Number, .RawNumber => {
                    return ScanError.CannotTakePointerOfRawValue;
                },
                else => {},
            }

            ptrType = try escapeVarInfoAndFree(allocator, ptrType);

            return try utils.astTypesToInfo(allocator, .{
                .Pointer = ptrType,
            }, ptr.isConst);
        },
        .Dereference => |deref| {
            var ptrType = try scanNode(allocator, compInfo, deref, withGenDef);
            ptrType = try escapeVarInfoAndFree(allocator, ptrType);
            if (ptrType.astType.* != .Pointer) return ScanError.CannotDereferenceNonPointerValue;

            const res = ptrType.astType.Pointer;
            ptrType.astType.* = .Void;
            free.freeAstTypeInfo(allocator, ptrType);

            return res;
        },
        .HeapAlloc => |*alloc| {
            var exprType = try scanNode(allocator, compInfo, alloc.*.node, withGenDef);
            exprType = try escapeVarInfoAndFree(allocator, exprType);
            const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, exprType, withGenDef);
            alloc.allocType = exprType;

            const ptrType = try createMut(blitzAst.AstTypes, allocator, .{
                .Pointer = typeClone,
            });

            return utils.astTypesPtrToInfo(ptrType, false);
        },
        .ArrayInit => |init| {
            var initNodeType = try scanNode(allocator, compInfo, init.initNode, withGenDef);
            initNodeType = try escapeVarInfoAndFree(allocator, initNodeType);
            defer free.freeAstTypeInfo(allocator, initNodeType);

            const matches = try matchTypes(allocator, compInfo, init.initType, initNodeType, withGenDef);
            if (!matches) {
                return ScanError.ArrayInitTypeInitializerMismatch;
            }

            const initTypeClone = try clone.cloneAstTypeInfo(allocator, compInfo, init.initType, withGenDef);

            return utils.astTypesToInfo(allocator, .{
                .ArraySlice = .{
                    .type = initTypeClone,
                    .size = try createMut(blitzAst.AstNode, allocator, .{
                        .Value = .{
                            .RawNumber = .{
                                .digits = try string.cloneString(allocator, init.size),
                                .numType = .USize,
                            },
                        },
                    }),
                },
            }, false);
        },
    }
}

fn typeInfoToVarInfo(allocator: Allocator, info: blitzAst.AstTypeInfo, isConst: bool) !blitzAst.AstTypeInfo {
    return .{
        .astType = try createMut(blitzAst.AstTypes, allocator, .{
            .VarInfo = info,
        }),
        .isConst = isConst,
    };
}

fn escapeVarInfo(node: blitzAst.AstTypeInfo) !blitzAst.AstTypeInfo {
    var res = node;
    if (res.astType.* == .VarInfo) {
        res = res.astType.VarInfo;
    }
    if (res.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }
    return res;
}

fn escapeVarInfoAndFree(allocator: Allocator, node: blitzAst.AstTypeInfo) !blitzAst.AstTypeInfo {
    var res = node;
    if (res.astType.* == .VarInfo) {
        const temp = res.astType.VarInfo;
        res.astType.* = .Void;
        free.freeAstTypeInfo(allocator, res);
        res = temp;
    }
    if (res.astType.* == .VarInfo) {
        return ScanError.NestedVarInfoDetected;
    }
    return res;
}

/// true if uses undefined variables
fn checkUndefVars(compInfo: *CompInfo, node: *const blitzAst.AstNode) bool {
    var undef = false;

    return switch (node.*) {
        .Variable => |name| {
            if (string.compString(name, "self")) {
                return !compInfo.isInStructMethod();
            }

            const inScope = compInfo.isVariableInScope(name);
            return !inScope;
        },
        .Cast => |cast| checkUndefVars(compInfo, cast.node),
        .IncOne,
        .DecOne,
        .Group,
        .Scope,
        .Bang,
        .ReturnNode,
        => |inner| checkUndefVars(compInfo, inner),
        .ForLoop => |loop| {
            undef = undef or checkUndefVars(compInfo, loop.condition);
            undef = undef or checkUndefVars(compInfo, loop.body);
            undef = undef or checkUndefVars(compInfo, loop.incNode);
            if (loop.initNode) |init| {
                undef = undef or checkUndefVars(compInfo, init);
            }
            return undef;
        },
        .WhileLoop => |loop| {
            undef = undef or checkUndefVars(compInfo, loop.condition);
            undef = undef or checkUndefVars(compInfo, loop.body);
            return undef;
        },
        .FuncCall => |func| checkUndefVars(compInfo, func.func),
        .IfStatement => |statement| {
            undef = undef or checkUndefVars(compInfo, statement.condition);
            undef = undef or checkUndefVars(compInfo, statement.body);

            if (statement.fallback) |fallback| {
                undef = undef or checkUndefVarsIfFallback(compInfo, fallback);
            }

            return undef;
        },
        .IndexValue => |index| {
            undef = undef or checkUndefVars(compInfo, index.value);
            undef = undef or checkUndefVars(compInfo, index.index);
            return undef;
        },
        .OpExpr => |expr| {
            undef = undef or checkUndefVars(compInfo, expr.left);
            undef = undef or checkUndefVars(compInfo, expr.right);
            return undef;
        },
        .PropertyAccess => |access| {
            undef = undef or checkUndefVars(compInfo, access.value);
            return undef;
        },
        .Seq => |seq| {
            for (seq.nodes) |innerNode| {
                undef = undef or checkUndefVars(compInfo, innerNode);
            }

            return undef;
        },
        .StructInit => |init| {
            for (init.attributes) |attr| {
                undef = undef or checkUndefVars(compInfo, attr.value);
            }

            return undef;
        },
        .ValueSet => |set| {
            undef = undef or checkUndefVars(compInfo, set.value);
            undef = undef or checkUndefVars(compInfo, set.setNode);
            return undef;
        },
        .VarDec => |dec| {
            return checkUndefVars(compInfo, dec.setNode);
        },
        .VarEqOp => |op| {
            return checkUndefVars(compInfo, op.value);
        },
        else => return false,
    };
}

fn checkUndefVarsIfFallback(compInfo: *CompInfo, fallback: *const blitzAst.IfFallback) bool {
    var undef = false;
    if (fallback.condition) |condition| {
        undef = undef or checkUndefVars(compInfo, condition);
    }
    undef = undef or checkUndefVars(compInfo, fallback.body);

    if (fallback.fallback) |innerFallback| {
        undef = undef or checkUndefVarsIfFallback(compInfo, innerFallback);
    }

    return undef;
}

fn scanFunctionCalls(allocator: Allocator, compInfo: *CompInfo) !void {
    try compInfo.pushScope(false);
    defer compInfo.popScope();
    _ = compInfo.returnInfo.setInFunction(true);
    defer compInfo.returnInfo.revertInFunction(false);

    const functions = compInfo.functionsToScan;
    while (functions.items.len > 0) {
        const toScanItem = functions.pop().?;
        const func = toScanItem.func;

        try compInfo.pushGenScope(false);
        defer compInfo.popGenScope();
        try compInfo.pushScope(false);
        defer compInfo.popScope();

        const lastRetInfo = try compInfo.returnInfo.newInfo(true);
        defer compInfo.returnInfo.swapFree(lastRetInfo);

        if (func.capturedTypes) |captured| {
            var captureIt = captured.iterator();
            while (captureIt.next()) |item| {
                const clonedType = try clone.cloneAstTypeInfo(allocator, compInfo, item.value_ptr.*, toScanItem.withGenDef);
                try compInfo.setGeneric(item.key_ptr.*, clonedType);
            }
        }

        if (func.capturedFuncs) |captured| {
            for (captured.items) |item| {
                try compInfo.addScopedFunction(try string.cloneString(allocator, item));
            }
        }

        if (func.capturedValues) |captured| {
            var captureIt = captured.iterator();
            while (captureIt.next()) |item| {
                const value = item.value_ptr.*;
                const paramType = try escapeVarInfo(value);

                const clonedType = try clone.cloneAstTypeInfo(allocator, compInfo, paramType, false);
                try compInfo.setVariableType(
                    item.key_ptr.*,
                    clonedType,
                    value.isConst,
                );
            }
        }

        for (toScanItem.genTypes) |rel| {
            const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, rel.info, false);
            try compInfo.setGeneric(rel.gen, typeClone);
        }

        try scanFuncBodyAndReturn(allocator, compInfo, func, toScanItem.withGenDef);
    }
}

fn setGenTypesFromParams(
    allocator: Allocator,
    compInfo: *CompInfo,
    func: *blitzAst.FuncDecNode,
    paramTypes: []blitzAst.AstTypeInfo,
    withGenDef: bool,
) !bool {
    var includesGenerics = false;

    for (func.params, paramTypes) |decParam, origCallParamType| {
        const callParamType = try escapeVarInfo(origCallParamType);
        var isGeneric = false;

        switch (decParam.type.astType.*) {
            .Generic => |generic| {
                const typePtr = try clone.cloneAstTypeInfo(allocator, compInfo, callParamType, withGenDef);

                const currentGenScope = compInfo.genericScopes.getCurrentScope();
                if (currentGenScope) |scope| {
                    if (scope.get(generic)) |genType| {
                        const matches = try matchTypes(allocator, compInfo, callParamType, genType, false);
                        if (!matches) {
                            return ScanError.GenericRestrictionConflict;
                        }
                    }
                }

                try compInfo.setGeneric(generic, typePtr);
                isGeneric = true;
            },
            .Custom => |custom| {
                isGeneric = try matchParamGenericTypes(allocator, compInfo, custom, callParamType.astType);
            },
            else => {},
        }

        if (!try matchTypes(allocator, compInfo, decParam.type, callParamType, false)) {
            return ScanError.FunctionCallParamTypeMismatch;
        }

        if (callParamType.astType.* == .Pointer and
            callParamType.isConst and
            !decParam.type.isConst and
            !isAnyType(decParam.type.astType))
        {
            return ScanError.ExpectedMutableParameter;
        }

        free.freeAstTypeInfo(allocator, origCallParamType);

        includesGenerics = includesGenerics or isGeneric;
    }

    return includesGenerics;
}

fn estimateStackSize(astType: *const blitzAst.AstTypes) u32 {
    switch (astType.*) {
        .Number => |num| {
            return switch (num) {
                .U8, .I8 => 1,
                .U16, .I16 => 2,
                .U32, .I32, .F32 => 4,
                .U64, .I64, .F64 => 8,
                .U128, .I128, .F128 => 16,
                .USize, .ISize => 8,
            };
        },
        else => {},
    }

    return 0;
}

fn genScopeToRels(
    allocator: Allocator,
    compInfo: *CompInfo,
    genScope: *blitzCompInfo.TypeScope,
    withGenDef: bool,
) ![]blitzAst.GenToTypeInfoRel {
    const slice = try allocator.alloc(blitzAst.GenToTypeInfoRel, genScope.count());
    var i: usize = 0;
    var scopeIt = genScope.iterator();
    while (scopeIt.next()) |entry| {
        slice[i] = .{
            .gen = entry.key_ptr.*,
            .info = try clone.cloneAstTypeInfo(allocator, compInfo, entry.value_ptr.*, withGenDef),
        };

        i += 1;
    }

    return slice;
}

fn fnHasScannedWithSameGenTypes(
    allocator: Allocator,
    compInfo: *CompInfo,
    func: *blitzAst.FuncDecNode,
    genScope: *blitzCompInfo.TypeScope,
    withGenDef: bool,
) !bool {
    outer: for (func.toScanTypes.items) |scannedScope| {
        for (scannedScope) |rel| {
            const genType = genScope.get(rel.gen).?;
            const matches = try matchTypesUtil(allocator, compInfo, genType, rel.info, withGenDef, .Strict);
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

fn applyVariableCaptures(allocator: Allocator, func: *blitzAst.FuncDecNode, scope: *blitzCompInfo.CaptureScope) !void {
    if (func.capturedValues) |captured| {
        free.freeVariableCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    func.capturedValues = scope;
}

fn applyGenericCaptures(allocator: Allocator, func: *blitzAst.FuncDecNode, scope: *blitzCompInfo.TypeScope) !void {
    if (func.capturedTypes) |captured| {
        free.freeGenericCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    func.capturedTypes = scope;
}

fn applyFunctionCaptures(allocator: Allocator, func: *blitzAst.FuncDecNode, scope: *blitzCompInfo.StringListScope) !void {
    if (func.capturedFuncs) |captured| {
        captured.deinit();
        allocator.destroy(captured);
    }

    func.capturedFuncs = scope;
}

fn scanIfFallback(
    allocator: Allocator,
    compInfo: *CompInfo,
    fallback: *const blitzAst.IfFallback,
    withGenDef: bool,
) !void {
    const prev = try compInfo.returnInfo.newInfo(false);

    if (fallback.condition == null and fallback.fallback != null) {
        const nextFallback = fallback.fallback.?;
        if (nextFallback.condition == null) {
            return ScanError.IfStatementMayOnlyHaveOneElse;
        } else {
            return ScanError.ElseBranchOutOfOrder;
        }
    }

    if (fallback.condition) |condition| {
        const nodeType = try scanNode(allocator, compInfo, condition, withGenDef);
        free.freeAstTypeInfo(allocator, nodeType);
    }

    const bodyType = try scanNode(allocator, compInfo, fallback.body, withGenDef);
    free.freeAstTypeInfo(allocator, bodyType);

    if (compInfo.returnInfo.info.retType == null) {
        compInfo.returnInfo.setExhaustive(false);
    }

    try compInfo.returnInfo.collapse(compInfo, prev, withGenDef);

    if (fallback.fallback) |innerFallback| {
        try scanIfFallback(allocator, compInfo, innerFallback, withGenDef);
    }
}

fn setInitGenerics(
    allocator: Allocator,
    compInfo: *CompInfo,
    genTypes: []blitzAst.AstTypeInfo,
    decGens: []blitzAst.GenericType,
    withGenDef: bool,
) !void {
    for (genTypes, decGens) |t, decGen| {
        if (decGen.restriction) |restriction| {
            if (!(try matchTypes(allocator, compInfo, restriction, t, withGenDef))) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, t, withGenDef);
        try compInfo.setGeneric(decGen.name, typeClone);
    }
}

fn setInitDeriveGenerics(allocator: Allocator, compInfo: *CompInfo, deriveType: blitzAst.AstTypeInfo) !void {
    const generics = deriveType.astType.Custom.generics;
    const deriveName = switch (deriveType.astType.*) {
        .Custom => |custom| custom.name,
        .StaticStructInstance => |inst| inst,
        else => unreachable,
    };
    const deriveDec = compInfo.getStructDec(deriveName);
    const decGens = deriveDec.?.generics;

    for (generics, decGens) |gen, decGen| {
        const clonedType = try clone.cloneAstTypeInfo(allocator, compInfo, gen, true);

        if (decGen.restriction) |restriction| {
            if (!(try matchTypes(allocator, compInfo, clonedType, restriction, true))) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        try compInfo.setGeneric(decGen.name, clonedType);
    }
}

fn matchParamGenericTypes(
    allocator: Allocator,
    compInfo: *CompInfo,
    custom: blitzAst.CustomType,
    paramType: *const blitzAst.AstTypes,
) !bool {
    switch (paramType.*) {
        .Custom => |paramCustom| {
            if (!string.compString(custom.name, paramCustom.name)) return ScanError.FunctionCallParamTypeMismatch;

            var hasGeneric = false;

            for (custom.generics, 0..) |gen, index| {
                const paramGen = paramCustom.generics[index];

                switch (gen.astType.*) {
                    .Generic => |generic| {
                        const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, paramGen, false);

                        const genType = try compInfo.getGeneric(generic);
                        if (genType) |t| {
                            const matches = try matchTypes(allocator, compInfo, t, typeClone, true);
                            if (!matches) {
                                return ScanError.ConflictingGenericParameters;
                            }
                        }

                        try compInfo.setGeneric(generic, typeClone);
                        hasGeneric = true;
                    },
                    .Custom => |newCustom| {
                        hasGeneric = try matchParamGenericTypes(allocator, compInfo, newCustom, paramGen.astType);
                    },
                    else => {},
                }
            }

            return hasGeneric;
        },
        else => return ScanError.FunctionCallParamTypeMismatch,
    }
}

fn scanFuncBodyAndReturn(allocator: Allocator, compInfo: *CompInfo, func: *blitzAst.FuncDecNode, withGenDef: bool) !void {
    try compInfo.pushScope(true);
    defer compInfo.popScope();

    for (func.params) |param| {
        const typeClone = try clone.cloneAstTypeInfo(allocator, compInfo, param.type, withGenDef);
        try compInfo.setVariableType(param.name, typeClone, param.isConst);
    }

    try scanNodeForFunctions(allocator, compInfo, func.body);
    const bodyType = try scanNode(allocator, compInfo, func.body, withGenDef);
    free.freeAstTypeInfo(allocator, bodyType);

    const scope = compInfo.consumeVariableCaptures();
    if (scope) |s| {
        try applyVariableCaptures(allocator, func, s);
    }

    const genScope = compInfo.consumeGenericCaptures();
    if (genScope) |s| {
        try applyGenericCaptures(allocator, func, s);
    }

    const funcScope = compInfo.consumeFunctionCaptures();
    if (funcScope) |s| {
        try applyFunctionCaptures(allocator, func, s);
    }

    if (func.returnType.astType.* != .Void) {
        if (!compInfo.returnInfo.info.exhaustive) {
            return ScanError.FunctionReturnIsNotExhaustive;
        }

        if (compInfo.returnInfo.info.retType) |retType| {
            if (!try matchTypes(allocator, compInfo, func.returnType, retType, withGenDef)) {
                return ScanError.FunctionReturnTypeMismatch;
            }
        } else {
            return ScanError.FunctionMissingReturn;
        }
    } else if (compInfo.returnInfo.info.retType != null) {
        return ScanError.FunctionReturnTypeMismatch;
    }
}

fn validateSelfProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8, inOwnedMethod: bool) !?blitzAst.AstTypeInfo {
    const structDec = compInfo.getStructDec(name);

    if (structDec) |dec| {
        for (dec.attributes) |attr| {
            const nameMatches = string.compString(attr.name, prop);
            if (nameMatches) {
                if (attr.visibility == .Public or attr.visibility == .Protected or inOwnedMethod) {
                    return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
                }
            }
        }

        if (dec.deriveType) |derive| {
            const deriveName = switch (derive.astType.*) {
                .StaticStructInstance => |structName| structName,
                .Custom => |custom| custom.name,
                else => unreachable,
            };

            return validateSelfProps(allocator, compInfo, deriveName, prop, false);
        }

        return null;
    }

    return ScanError.UndefinedStruct;
}

fn validateStaticStructProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8) !?blitzAst.AstTypeInfo {
    if (string.compString(name, "self")) {
        const currentStruct = compInfo.getCurrentStruct();

        if (currentStruct) |current| {
            return validateSelfProps(allocator, compInfo, current, prop, true);
        } else {
            return ScanError.SelfUsedOutsideStruct;
        }
    }

    const dec = compInfo.getStructDec(name).?;

    for (dec.attributes) |attr| {
        if (!string.compString(attr.name, prop)) continue;
        if (!attr.static) return ScanError.NonStaticAccessFromStaticStructReference;
        if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

        return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
    }

    return ScanError.InvalidProperty;
}

fn validateCustomProps(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8, withGenDef: bool) !?blitzAst.AstTypeInfo {
    const dec = compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (attr.static) continue;

            if (string.compString(attr.name, prop)) {
                if (attr.visibility != .Public) {
                    return ScanError.NonPublicStructFieldAccessFromOutsideDefinition;
                }

                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, withGenDef);
            }
        }

        if (structDec.deriveType) |deriveType| {
            switch (deriveType.astType.*) {
                .Custom => |c| return try validateCustomProps(allocator, compInfo, c, prop, withGenDef),
                else => return ScanError.UnexpectedDeriveType,
            }
        }

        return null;
    }

    return ScanError.InvalidPropertySource;
}

fn scanAttributes(compInfo: *CompInfo, attrs: []blitzAst.StructAttribute) !void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                try compInfo.addFuncToScan(func, &[_]blitzAst.GenToTypeInfoRel{}, false);
            },
        }
    }
}

fn isNumber(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
        .Number => true,
        else => false,
    };
}

fn isUnsignedInt(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
        .Number => |num| switch (num) {
            .U8, .U16, .U32, .U64, .U128, .USize => true,
            else => false,
        },
        else => false,
    };
}

fn isInt(astType: *const blitzAst.AstTypes) bool {
    return switch (astType.*) {
        .Number => |num| switch (num) {
            .U8, .U16, .U32, .U64, .U128, .USize => true,
            .I8, .I16, .I32, .I64, .I128 => true,
            else => false,
        },
        else => false,
    };
}

fn isFloat(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
        .Number => |num| switch (num) {
            .F32, .F64, .F128 => true,
            else => false,
        },
        else => false,
    };
}

pub fn matchTypes(
    allocator: Allocator,
    compInfo: *CompInfo,
    toType: blitzAst.AstTypeInfo,
    fromType: blitzAst.AstTypeInfo,
    withGenDef: bool,
) (ScanError || Allocator.Error || clone.CloneError)!bool {
    return matchTypesUtil(allocator, compInfo, toType, fromType, withGenDef, .Assign);
}

/// match types as if fromType is being set to toType to match mutability
pub fn matchTypesUtil(
    allocator: Allocator,
    compInfo: *CompInfo,
    toType: blitzAst.AstTypeInfo,
    fromType: blitzAst.AstTypeInfo,
    withGenDef: bool,
    mutMatchBehavior: MutMatchBehavior,
) !bool {
    const type1 = toType.astType.*;
    const type2 = fromType.astType.*;

    if (type1 == .Any or type2 == .Any) return true;

    if (type1 == .Generic and type2 == .Generic) {
        if (withGenDef) {
            var genType1 = try compInfo.getGeneric(type1.Generic);
            if (genType1) |*gType| {
                if (gType.*.astType.* == .Generic and string.compString(type1.Generic, gType.*.astType.Generic)) {
                    return ScanError.UnexpectedRecursiveGeneric;
                }
            } else if (withGenDef) {
                return ScanError.EmptyGenericType;
            }

            var genType2 = try compInfo.getGeneric(type2.Generic);
            if (genType2) |*gType| {
                if (gType.*.astType.* == .Generic and string.compString(type2.Generic, gType.*.astType.Generic)) {
                    return ScanError.UnexpectedRecursiveGeneric;
                }
            } else if (withGenDef) {
                return ScanError.EmptyGenericType;
            }

            return matchTypesUtil(allocator, compInfo, genType1.?, genType2.?, withGenDef, mutMatchBehavior);
        }

        return true;
    }

    if (type1 == .Generic) {
        if (!withGenDef) return true;

        var genType = try compInfo.getGeneric(type1.Generic);
        if (genType) |*gType| {
            if (gType.*.astType.* == .Generic and string.compString(gType.*.astType.Generic, type1.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypesUtil(allocator, compInfo, gType.*, fromType, withGenDef, mutMatchBehavior);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        }

        return true;
    }

    if (type2 == .Generic) {
        if (!withGenDef) return true;

        var genType = try compInfo.getGeneric(type2.Generic);
        if (genType) |*gType| {
            if (gType.*.astType.* == .Generic and string.compString(gType.*.astType.Generic, type2.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypesUtil(allocator, compInfo, toType, gType.*, withGenDef, mutMatchBehavior);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        }

        return true;
    }

    if (type1 == .VarInfo) {
        return matchTypesUtil(allocator, compInfo, type1.VarInfo, fromType, withGenDef, mutMatchBehavior);
    }

    if (type2 == .VarInfo) {
        return matchTypesUtil(allocator, compInfo, toType, type2.VarInfo, withGenDef, mutMatchBehavior);
    }

    return switch (type1) {
        .String => type2 == .String,
        .Bool => type2 == .Bool,
        .Char => type2 == .Char,
        .Void => type2 == .Void,
        .Null => type2 == .Nullable or type2 == .Null,
        .Nullable => |inner| type2 == .Null or try matchTypesUtil(
            allocator,
            compInfo,
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
                const sizeType1 = try scanNode(allocator, compInfo, arr.size.?, withGenDef);
                defer free.freeAstTypeInfo(allocator, sizeType1);
                const sizeType2 = try scanNode(allocator, compInfo, array.size.?, withGenDef);
                defer free.freeAstTypeInfo(allocator, sizeType2);

                if (!isInt(sizeType1.astType) or !isInt(sizeType2.astType)) {
                    return false;
                }
            }

            if (arr.size != null and array.size == null) {
                return ScanError.SizedSliceSetToUnknownSizedSlice;
            }

            const matches = try matchTypesUtil(
                allocator,
                compInfo,
                arr.type,
                array.type,
                withGenDef,
                mutMatchBehavior,
            );
            return try matchMutState(toType, fromType, matches, mutMatchBehavior);
        },
        .Custom => |custom| {
            if (type2 == .StaticStructInstance and string.compString(custom.name, type2.StaticStructInstance)) {
                return try matchMutState(toType, fromType, true, mutMatchBehavior);
            }

            if (type2 != .Custom) return false;
            if (!string.compString(type1.Custom.name, type2.Custom.name)) return false;
            if (custom.generics.len != type2.Custom.generics.len) return false;

            for (custom.generics, type2.Custom.generics) |gen1, gen2| {
                const genMatch = try matchTypesUtil(allocator, compInfo, gen1, gen2, withGenDef, mutMatchBehavior);
                if (!genMatch) return ScanError.CustomGenericMismatch;
            }

            return try matchMutState(toType, fromType, true, mutMatchBehavior);
        },
        .Error => |err| switch (type2) {
            .Error => |err2| string.compString(err.name, err2.name),
            .ErrorVariant => |err2| string.compString(err.name, err2.from),
            else => {
                if (err.payload) |payload| {
                    const matches = try matchTypesUtil(allocator, compInfo, payload, fromType, withGenDef, mutMatchBehavior);
                    return try matchMutState(toType, fromType, matches, mutMatchBehavior);
                }

                return false;
            },
        },
        .ErrorVariant => |err| switch (type2) {
            .Error => |err2| string.compString(err.from, err2.name),
            .ErrorVariant => |err2| string.compString(err.from, err2.from) and string.compString(err.variant, err2.variant),
            else => false,
        },
        .StaticStructInstance => |inst| {
            if (type2 == .Custom and string.compString(inst, type2.Custom.name)) {
                return try matchMutState(toType, fromType, true, mutMatchBehavior);
            }

            return false;
        },
        .Pointer => |ptr| {
            if (type2 != .Pointer) return ScanError.PointerTypeMismatch;
            const res = try matchTypesUtil(allocator, compInfo, ptr, type2.Pointer, withGenDef, mutMatchBehavior);
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
            if (toType.astType.* == .Pointer and !toType.isConst and fromType.isConst) {
                return ScanError.PointerTypeConstMismatch;
            }
        },
        .Strict => {
            if (toType.isConst != fromType.isConst) {
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

fn getPropertyType(allocator: Allocator, compInfo: *CompInfo, source: blitzAst.AstTypes, prop: []u8) !blitzAst.AstTypes {
    return switch (source) {
        .StaticStructInstance => |inst| try getStructPropType(compInfo, false, inst, prop),
        .ArraySlice => try builtins.getArraySlicePropTypes(prop),
        .String => try builtins.getStringPropTypes(prop),
        .Custom => |custom| getCustomPropType(allocator, compInfo, custom, prop),
        else => ScanError.UnsupportedFeature,
    };
}

fn getCustomPropType(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8) !blitzAst.AstTypes {
    const dec = compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (!string.compString(attr.name, prop)) continue;
            if (attr.static) return ScanError.StaticAccessFromStructInstance;

            if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

            return try clone.cloneStructAttributeUnion(allocator, compInfo, attr.attr, false);
        }

        if (structDec.deriveType) |deriveType| {
            if (deriveType.* == .Custom) {
                return try getCustomPropType(allocator, compInfo, deriveType.Custom, prop);
            } else return ScanError.UnexpectedDeriveType;
        }

        return ScanError.InvalidProperty;
    }

    return ScanError.InvalidPropertySource;
}

fn getStructPropType(compInfo: *CompInfo, allowNonStatic: bool, inst: []u8, prop: []u8) !blitzAst.AstTypes {
    const dec = compInfo.getStructDec(inst) orelse return ScanError.InvalidPropertySource;

    for (dec.?.attributes) |attr| {
        if (!attr.static and allowNonStatic) continue;
        if (!string.compString(attr.name, prop)) continue;

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

fn inferArraySliceType(allocator: Allocator, compInfo: *CompInfo, arr: []*blitzAst.AstNode, withGenDef: bool) !blitzAst.AstTypeInfo {
    if (arr.len == 0) return try utils.astTypesToInfo(allocator, .Void, false);

    const firstType = try scanNode(allocator, compInfo, arr[0], withGenDef);
    var isConst = firstType.isConst;

    for (arr[1..]) |item| {
        const exprType = try scanNode(allocator, compInfo, item, withGenDef);
        defer free.freeAstTypeInfo(allocator, exprType);

        isConst = isConst or exprType.isConst;

        if (!try matchTypes(allocator, compInfo, exprType, firstType, false)) {
            return ScanError.ArraySliceTypeMismatch;
        }
    }

    return utils.astTypesPtrToInfo(firstType.astType, isConst);
}

fn scanNodeForFunctions(allocator: Allocator, compInfo: *CompInfo, node: *const blitzAst.AstNode) !void {
    switch (node.*) {
        .FuncDec => |dec| {
            try compInfo.addScopedFunction(try string.cloneString(allocator, dec));
        },
        .Seq => |seq| {
            for (seq.nodes) |seqNode| {
                try scanNodeForFunctions(allocator, compInfo, seqNode);
            }
        },
        else => {},
    }
}

// const MaxTypeRel = struct {
//     str: []const u8,
//     type: blitzAst.AstNumberVariants,
// };

// fn getBestNumberType(num: []u8) !blitzAst.AstNumberVariants {
//     const u32Max = getStrTypeRel(.U32, "4294967295");
//     const u64Max = getStrTypeRel(.U64, "18446744073709551615");
//     const u128Max = getStrTypeRel(.U128, "340282366920938463463374607431768211455");
//     const uNumMaxes = &[_]MaxTypeRel{ u32Max, u64Max, u128Max };

//     for (uNumMaxes) |max| {
//         if (num.len < max.str.len) {
//             return max.type;
//         }

//         if (num.len > max.str.len) {
//             continue;
//         }

//         var i: u32 = 0;
//         while (i < max.str.len) : (i += 1) {
//             const index = max.str.len - i - 1;
//             if (max.str[index] < num[index]) continue;
//         }

//         return max.type;
//     }

//     return CodeGenError.RawNumberIsTooBig;
// }

// fn getStrTypeRel(numType: blitzAst.AstNumberVariants, str: []const u8) MaxTypeRel {
//     return .{
//         .str = str,
//         .type = numType,
//     };
// }
