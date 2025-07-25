const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const builtins = @import("builtins.zig");
const clone = blitz.clone;
const number = blitz.number;
const string = blitz.string;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;
const ArrayList = std.ArrayList;
const create = utils.create;

// debug
const debug = @import("debug.zig");
const printNode = debug.printNode;
const printType = debug.printType;
const printGenerics = debug.printGenerics;

pub const RetNodeInfo = struct {
    node: blitzAst.AstNode,
    conditional: bool,
};

pub const ScanError = error{
    // misc
    InvalidCast,
    ExpectedBooleanBang,
    ExpectedBooleanIfCondition,
    UnsupportedFeature,
    ExpectedUSizeForIndex,
    StaticStructInstanceCannotBeUsedAsValue,
    InvalidNumber,
    IfStatementMayOnlyHaveOneElse,
    ElseBranchOutOfOrder,

    // arrays
    StaticArrayTypeMismatch,
    ExpectedArrayForIndexTarget,
    ExpectedUSizeOrU32ForStaticArraySize,
    ExpectedEqualStaticArraySizes,

    // loops
    ExpectedBooleanLoopCondition,

    // variables
    VariableAnnotationMismatch,
    VariableAlreadyExists,
    VoidVariableDec,
    VariableTypeAndValueTypeMismatch,
    AssigningToConstVariable,

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
    StructDefinedInLowerScope,

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

    // possibly remove
    InvalidPropertySource,
};

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, compInfo: *CompInfo) !void {
    while (compInfo.variableScopes.scopes.items.len > 1) compInfo.popScope();

    try scanNodeForFunctions(allocator, compInfo, ast.root);
    const nodeType = try scanNode(allocator, compInfo, ast.root, true);
    free.freeAstTypeInfo(allocator, nodeType);
}

pub fn scanNodes(
    allocator: Allocator,
    compInfo: *CompInfo,
    nodes: []*const blitzAst.AstNode,
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
    node: *const blitzAst.AstNode,
    withGenDef: bool,
) (Allocator.Error || ScanError || clone.CloneError)!blitzAst.AstTypeInfo {
    switch (node.*) {
        .NoOp, .ErrorDec => return try utils.astTypesToInfo(allocator, .Void, true),

        .StaticStructInstance => |inst| {
            if (!compInfo.scanner.allowStaticStructInstance) {
                return ScanError.StaticStructInstanceCannotBeUsedAsValue;
            }

            return try utils.astTypesToInfo(allocator, .{
                .StaticStructInstance = try string.cloneString(allocator, inst),
            }, true);
        },
        .Cast => |cast| {
            const clonedCastType = try clone.cloneAstTypesPtr(allocator, compInfo, cast.toType, withGenDef);
            if (cast.node.* == .Value and cast.node.*.Value == .RawNumber) {
                // TODO add some restrictions for sign
                return utils.astTypesPtrToInfo(clonedCastType, false);
            }

            const nodeType = try scanNode(allocator, compInfo, cast.node, withGenDef);
            defer free.freeAstTypeInfo(allocator, nodeType);

            if (isPrimary(nodeType.astType) and isPrimary(cast.toType)) {
                return utils.astTypesPtrToInfo(clonedCastType, false);
            }

            return ScanError.InvalidCast;
        },
        .Value => |val| {
            const valueRes: blitzAst.AstTypes = switch (val) {
                .Null => .Null,
                .String => .String,
                .Bool => .Bool,
                .Char => .Char,
                .Number => |num| .{ .Number = num.toType() },
                .RawNumber => |num| {
                    // TODO - do better number inference
                    if (num[0] == '-') return try utils.astTypesToInfo(allocator, .{ .Number = .I32 }, false);

                    var hasPeriod = false;
                    for (num) |char| {
                        if (char == '.') {
                            hasPeriod = true;
                            break;
                        }
                    }

                    if (hasPeriod) {
                        return try utils.astTypesToInfo(allocator, .{ .Number = .F32 }, false);
                    }

                    return try utils.astTypesToInfo(allocator, .{ .Number = .U32 }, false);
                },
                .GeneralArray => |arr| {
                    const inferredType = try inferStaticArrType(allocator, compInfo, arr, withGenDef);
                    const generalArrType = try create(blitzAst.AstTypes, allocator, .{
                        .GeneralArray = .{
                            .type = inferredType.astType,
                            .size = try create(blitzAst.AstNode, allocator, .{
                                .Value = .{
                                    .Number = .{ .USize = arr.len },
                                },
                            }),
                        },
                    });

                    return utils.astTypesPtrToInfo(generalArrType, inferredType.isConst);
                },
            };

            return try utils.astTypesToInfo(allocator, valueRes, false);
        },
        .Type => |t| return try utils.astTypesToInfo(allocator, t, false),
        .IndexValue => |indexInfo| {
            const indexType = try scanNode(allocator, compInfo, indexInfo.index, withGenDef);
            defer free.freeAstTypeInfo(allocator, indexType);
            const valueType = try scanNode(allocator, compInfo, indexInfo.value, withGenDef);
            defer free.freeAstTypeInfo(allocator, valueType);

            if (indexType.astType.* == .Number and indexType.astType.Number != .USize) {
                return ScanError.ExpectedUSizeForIndex;
            }

            return switch (valueType.astType.*) {
                .StaticArray => |arr| utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, arr.type, false), valueType.isConst),
                .DynamicArray => |arr| utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, arr, false), valueType.isConst),
                else => return ScanError.ExpectedArrayForIndexTarget,
            };
        },
        .OpExpr => |op| {
            const left = try scanNode(allocator, compInfo, op.left, withGenDef);
            defer free.freeAstTypeInfo(allocator, left);
            const right = try scanNode(allocator, compInfo, op.right, withGenDef);
            defer free.freeAstTypeInfo(allocator, right);

            switch (op.type) {
                .BitAnd, .BitOr => {
                    if (left.astType.* != .Number or right.astType.* != .Number) return ScanError.InvalidBitOperation;
                    if (!compareNumberBitSize(left.astType.Number, right.astType.Number)) return ScanError.BitMaskWithMismatchingSize;
                    return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, left.astType, withGenDef), false);
                },
                .And, .Or => {
                    if (left.astType.* != .Bool or right.astType.* != .Bool) return ScanError.ExpectedBoolInBoolOp;
                    return try utils.astTypesToInfo(allocator, .Bool, false);
                },
                .Add, .Sub, .Mult, .Div => {
                    if (left.astType.* == .Generic or left.astType.* == .Any) {
                        if (right.astType.* != .Number and right.astType.* != .Generic and right.astType.* != .Any) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (try matchTypes(allocator, compInfo, left.astType, right.astType, withGenDef)) {
                            return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, right.astType, withGenDef), false);
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (right.astType.* == .Generic or right.astType.* == .Any) {
                        if (left.astType.* != .Number and left.astType.* != .Generic and left.astType.* != .Any) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (try matchTypes(allocator, compInfo, left.astType, right.astType, withGenDef)) {
                            return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, left.astType, withGenDef), false);
                        } else {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (@intFromEnum(left.astType.Number) != @intFromEnum(right.astType.Number)) {
                        return ScanError.NumberTypeMismatch;
                    }

                    if (op.type == .Div) {
                        if (switch (left.astType.Number) {
                            .F32, .F64, .F128 => true,
                            else => false,
                        }) {
                            return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, left.astType, withGenDef), false);
                        }

                        return try utils.astTypesToInfo(allocator, .{
                            .Number = .F32,
                        }, false);
                    }

                    return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, left.astType, withGenDef), false);
                },
                .LessThan,
                .GreaterThan,
                .LessThanEq,
                .GreaterThanEq,
                => {
                    if (left.astType.* != .Number or right.astType.* != .Number) {
                        return ScanError.ComparisonOnNonNumberType;
                    }
                    if (@intFromEnum(left.astType.Number) != @intFromEnum(right.astType.Number)) {
                        return ScanError.NumberTypeMismatch;
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
                if (!(try matchTypes(allocator, compInfo, retType, valType.astType, withGenDef))) {
                    return ScanError.FunctionReturnsHaveDifferentTypes;
                }
                free.freeAstTypeInfo(allocator, valType);
            } else {
                compInfo.returnInfo.info.retType = valType.astType;
            }

            compInfo.returnInfo.info.exhaustive = true;
            compInfo.returnInfo.info.lockExhaustive = true;

            return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, valType.astType, withGenDef), true);
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

            const valueInfo = try scanNode(allocator, compInfo, access.value, withGenDef);
            compInfo.scanner.allowStaticStructInstance = false;
            compInfo.scanner.allowErrorWithoutVariants = false;
            defer free.freeAstTypeInfo(allocator, valueInfo);

            const valid: bool = switch (valueInfo.astType.*) {
                .Generic => return try utils.astTypesToInfo(allocator, .Any, valueInfo.isConst),
                .DynamicArray => |itemType| {
                    const signature = try builtins.getDynamicArrayPropType(allocator, compInfo, access.property, itemType, withGenDef);
                    return utils.astTypesPtrToInfo(signature, true);
                },
                .StaticArray => builtins.validateStaticArrayProps(access.property),
                .String => builtins.validateStringProps(access.property),
                .Custom => |custom| a: {
                    const def = compInfo.getStructDec(custom.name);
                    if (def == null) break :a false;

                    compInfo.setPreviousAccessedStruct(def.?.name);

                    var genNameArr = try ArrayList([]u8).initCapacity(allocator, custom.generics.len);
                    var genTypeArr = try ArrayList(*const blitzAst.AstTypes).initCapacity(allocator, custom.generics.len);
                    defer genNameArr.deinit();
                    defer genTypeArr.deinit();

                    for (custom.generics, 0..) |gen, index| {
                        const genDef = def.?.generics[index];
                        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, gen, withGenDef);
                        try genNameArr.append(genDef.name);
                        try genTypeArr.append(typeClone);
                    }

                    try compInfo.pushGenScope(false);
                    defer compInfo.popGenScope();

                    for (genNameArr.items, genTypeArr.items) |name, genType| {
                        try compInfo.setGeneric(name, genType);
                    }

                    const propType = try validateCustomProps(allocator, compInfo, custom, access.property, withGenDef);

                    if (propType) |t| {
                        return utils.astTypesPtrToInfo(t, valueInfo.isConst);
                    }

                    break :a false;
                },
                .StaticStructInstance => |name| a: {
                    try compInfo.pushGenScope(false);
                    defer compInfo.popGenScope();
                    const propType = try validateStaticStructProps(allocator, compInfo, name, access.property);

                    if (propType) |t| {
                        if (!string.compString(name, "self")) {
                            const dec = compInfo.getStructDec(name).?;

                            if (t.* == .Function) {
                                for (dec.generics) |gen| {
                                    if (gen.restriction) |restriction| {
                                        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, restriction, false);
                                        try compInfo.setGeneric(gen.name, typeClone);
                                    }
                                }
                            }
                        }

                        return utils.astTypesPtrToInfo(t, valueInfo.isConst);
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

            // TODO - update with builtin prop types (array.length, array.push, etc)
            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .Seq => |seq| {
            try scanNodes(allocator, compInfo, seq.nodes, withGenDef);
            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .VarDec => |dec| {
            if (compInfo.getVariableTypeFixed(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            const setType = try scanNode(allocator, compInfo, dec.setNode, withGenDef);

            if (setType.astType.* == .Void) {
                return ScanError.VoidVariableDec;
            }

            if (dec.annotation) |annotation| {
                if (try matchTypes(allocator, compInfo, annotation, setType.astType, false)) {
                    free.freeAstTypeInfo(allocator, setType);
                    const annotationClone = try clone.cloneAstTypesPtr(allocator, compInfo, annotation, withGenDef);
                    try compInfo.setVariableType(dec.name, annotationClone, dec.isConst);
                } else {
                    return ScanError.VariableAnnotationMismatch;
                }
            } else {
                try compInfo.setVariableType(dec.name, setType.astType, dec.isConst);
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .ValueSet => |set| {
            const valType = try scanNode(allocator, compInfo, set.value, withGenDef);
            defer free.freeAstTypeInfo(allocator, valType);
            if (valType.isConst) return ScanError.AssigningToConstVariable;

            const setNode = try scanNode(allocator, compInfo, set.setNode, withGenDef);
            defer free.freeAstTypeInfo(allocator, setNode);

            if (!(try matchTypes(allocator, compInfo, valType.astType, setNode.astType, withGenDef))) {
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

                    const left = variable.varType;
                    const right = try scanNode(allocator, compInfo, op.value, withGenDef);

                    if (left.* == .Generic or left.* == .Any) {
                        if (right.astType.* != .Number and right.astType.* != .Generic and right.astType.* != .Any) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (!(try matchTypes(allocator, compInfo, left, right.astType, withGenDef))) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    } else if (right.astType.* == .Generic or right.astType.* == .Any) {
                        if (left.* != .Number and left.* != .Generic and left.* != .Any) {
                            return ScanError.MathOpOnNonNumberType;
                        }

                        if (!(try matchTypes(allocator, compInfo, left, right.astType, withGenDef))) {
                            return ScanError.MathOpTypeMismatch;
                        }
                    }

                    if (@intFromEnum(left.Number) != @intFromEnum(right.astType.Number)) {
                        return ScanError.NumberTypeMismatch;
                    }
                },
                else => {},
            }

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .Variable => |v| {
            if (compInfo.isInStructMethod() and string.compString(v, "self")) {
                return try utils.astTypesToInfo(allocator, .{
                    .StaticStructInstance = try string.cloneString(allocator, v),
                }, true);
            }

            const varType = try compInfo.getVariableType(v, withGenDef);

            if (varType) |t| {
                const varTypeClone = try clone.cloneAstTypesPtr(allocator, compInfo, t.varType, withGenDef);
                return utils.astTypesPtrToInfo(varTypeClone, t.isConst);
            }

            return ScanError.VariableIsUndefined;
        },
        .StructPlaceholder => {
            const scopeDepth = compInfo.getScopeDepth();
            if (scopeDepth != 1) {
                return ScanError.StructDefinedInLowerScope;
            }

            return try utils.astTypesToInfo(allocator, .Void, false);
        },
        .StructDec => |dec| {
            try compInfo.addCurrentStruct(dec.name);
            defer _ = compInfo.popCurrentStruct();

            compInfo.enteringStruct();
            try scanAttributes(allocator, compInfo, dec.attributes, false);
            compInfo.exitingStruct();

            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .IfStatement => |statement| {
            try compInfo.pushScope(true);
            defer compInfo.popScope();
            try scanNodeForFunctions(allocator, compInfo, statement.body);
            const prev = try compInfo.returnInfo.newInfo(false);

            const conditionType = try scanNode(allocator, compInfo, statement.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, conditionType);
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

            const conditionType = try scanNode(allocator, compInfo, loop.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, conditionType);
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

            const conditionType = try scanNode(allocator, compInfo, loop.condition, withGenDef);
            defer free.freeAstTypeInfo(allocator, conditionType);
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

            const scanRes = try scanFuncBodyAndReturn(allocator, compInfo, func, false);
            free.freeType(allocator, scanRes);

            // TODO - replace with function type for anonymous functions sorta thing
            return try utils.astTypesToInfo(allocator, .Void, true);
        },
        .FuncCall => |call| {
            const prev = compInfo.returnInfo.setInFunction(true);
            defer compInfo.returnInfo.revertInFunction(prev);

            try compInfo.pushGenScope(true);
            defer compInfo.popGenScope();
            const lastRetInfo = try compInfo.returnInfo.newInfo(true);
            defer compInfo.returnInfo.swapFree(lastRetInfo);

            const dec = try scanNode(allocator, compInfo, call.func, withGenDef);
            defer allocator.destroy(dec.astType);
            if (dec.astType.* != .Function) return ScanError.CannotCallNonFunctionNode;
            const func = dec.astType.Function;

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

            for (func.params, paramTypes) |param, paramType| {
                var isGeneric = false;

                switch (param.type.*) {
                    .Generic => |generic| {
                        const typePtr = try clone.cloneAstTypesPtr(allocator, compInfo, paramType.astType, withGenDef);

                        if (try compInfo.getGeneric(generic)) |gen| {
                            if (!(try matchTypes(allocator, compInfo, paramType.astType, gen, false))) {
                                return ScanError.GenericRestrictionConflict;
                            }
                        }

                        try compInfo.setGeneric(generic, typePtr);
                        isGeneric = true;
                    },
                    .Custom => |custom| {
                        try matchParamGenericTypes(allocator, compInfo, custom, paramType.astType);
                    },
                    else => {},
                }

                if (!try matchTypes(allocator, compInfo, param.type, paramType.astType, false)) {
                    return ScanError.FunctionCallParamTypeMismatch;
                }

                if (!isPrimary(paramType.astType) and paramType.isConst and !param.isConst) {
                    return ScanError.ExpectedMutableParameter;
                }

                free.freeAstTypeInfo(allocator, paramType);
            }

            try compInfo.pushScope(false);
            defer compInfo.popScope();

            if (func.builtin) {
                return utils.astTypesPtrToInfo(try clone.cloneAstTypesPtr(allocator, compInfo, func.returnType, withGenDef), false);
            }

            if (func.capturedValues) |captured| {
                var captureIt = captured.iterator();
                while (captureIt.next()) |item| {
                    const clonedType = try clone.cloneAstTypesPtr(allocator, compInfo, item.value_ptr.*.varType, withGenDef);
                    try compInfo.setVariableType(item.key_ptr.*, clonedType, true);
                }
            }

            if (func.capturedTypes) |captured| {
                var captureIt = captured.iterator();
                while (captureIt.next()) |item| {
                    const clonedType = try clone.cloneAstTypesPtr(allocator, compInfo, item.value_ptr.*, withGenDef);
                    try compInfo.setGeneric(item.key_ptr.*, clonedType);
                }
            }

            const bodyReturnScanRes = try scanFuncBodyAndReturn(allocator, compInfo, func, withGenDef);
            return utils.astTypesPtrToInfo(bodyReturnScanRes, false);
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
                if (attr.attr == .Function) continue;

                var attrNode: ?*const blitzAst.AstNode = null;
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

                if (!try matchTypes(allocator, compInfo, attr.attr.Member, attrType.astType, withGenDef)) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            return try utils.astTypesToInfo(allocator, .{
                .Custom = .{
                    .generics = try clone.cloneTypesArr(allocator, compInfo, init.generics, false),
                    .name = try string.cloneString(allocator, init.name),
                },
            }, false);
        },
        .Bang => |bang| {
            const bangType = try scanNode(allocator, compInfo, bang, withGenDef);
            defer free.freeAstTypeInfo(allocator, bangType);
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
    }
}

fn applyVariableCaptures(allocator: Allocator, func: *blitzAst.FuncDecNode, scope: *utils.CaptureScope) !void {
    if (func.capturedValues) |captured| {
        utils.freeVariableCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    func.capturedValues = scope;
}

fn applyGenericCaptures(allocator: Allocator, func: *blitzAst.FuncDecNode, scope: *utils.TypeScope) !void {
    if (func.capturedTypes) |captured| {
        utils.freeGenericCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    func.capturedTypes = scope;
}

fn scanIfFallback(allocator: Allocator, compInfo: *CompInfo, fallback: *const blitzAst.IfFallback, withGenDef: bool) !void {
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

fn compareNumberBitSize(num1: blitzAst.AstNumberVariants, num2: blitzAst.AstNumberVariants) bool {
    return switch (num1) {
        .USize => num2 == .USize,
        .U8 => num2 == .I8,
        .U16 => num2 == .I16,
        .U32 => num2 == .F32 or num2 == .I32,
        .U64 => num2 == .F64 or num2 == .I64,
        .U128 => num2 == .F128 or num2 == .I128,
        .I8 => num2 == .U8,
        .I16 => num2 == .U16,
        .I32 => num2 == .U32 or num2 == .F32,
        .I64 => num2 == .U64 or num2 == .F64,
        .I128 => num2 == .U128 or num2 == .F128,
        .F32 => num2 == .I32 or num2 == .U32,
        .F64 => num2 == .I64 or num2 == .U64,
        .F128 => num2 == .I128 or num2 == .U128,
    };
}

fn setInitGenerics(
    allocator: Allocator,
    compInfo: *CompInfo,
    genTypes: []*const blitzAst.AstTypes,
    decGens: []blitzAst.GenericType,
    withGenDef: bool,
) !void {
    for (genTypes, decGens) |t, decGen| {
        if (decGen.restriction) |restriction| {
            if (!(try matchTypes(allocator, compInfo, t, restriction, withGenDef))) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, t, withGenDef);
        try compInfo.setGeneric(decGen.name, typeClone);
    }
}

fn setInitDeriveGenerics(allocator: Allocator, compInfo: *CompInfo, deriveType: *const blitzAst.AstTypes) !void {
    const generics = deriveType.Custom.generics;
    const deriveName = switch (deriveType.*) {
        .Custom => |custom| custom.name,
        .StaticStructInstance => |inst| inst,
        else => unreachable,
    };
    const deriveDec = compInfo.getStructDec(deriveName);
    const decGens = deriveDec.?.generics;

    for (generics, decGens) |gen, decGen| {
        const clonedType = try clone.cloneAstTypesPtr(allocator, compInfo, gen, true);

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
) !void {
    switch (paramType.*) {
        .Custom => |paramCustom| {
            if (!string.compString(custom.name, paramCustom.name)) return ScanError.FunctionCallParamTypeMismatch;

            for (custom.generics, 0..) |gen, index| {
                const paramGen = paramCustom.generics[index];

                switch (gen.*) {
                    .Generic => |generic| {
                        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, paramGen, false);

                        const genType = try compInfo.getGeneric(generic);
                        if (genType) |t| {
                            if (!(try matchTypes(allocator, compInfo, t, typeClone, true))) {
                                return ScanError.ConflictingGenericParameters;
                            }
                        }

                        try compInfo.setGeneric(generic, typeClone);
                    },
                    .Custom => |newCustom| {
                        try matchParamGenericTypes(allocator, compInfo, newCustom, paramGen);
                    },
                    else => {},
                }
            }
        },
        else => return ScanError.FunctionCallParamTypeMismatch,
    }
}

fn scanFuncBodyAndReturn(allocator: Allocator, compInfo: *CompInfo, func: *blitzAst.FuncDecNode, withGenDef: bool) !*const blitzAst.AstTypes {
    try compInfo.pushScope(true);
    defer compInfo.popScope();

    for (func.params) |param| {
        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, param.type, false);
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

    if (func.returnType.* != .Void) {
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

    return try clone.cloneAstTypesPtr(allocator, compInfo, func.returnType, withGenDef);
}

fn validateSelfProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8) !?*const blitzAst.AstTypes {
    const structDec = compInfo.getStructDec(name);

    if (structDec) |dec| {
        for (dec.attributes) |attr| {
            if ((attr.visibility == .Public or attr.visibility == .Protected) and string.compString(attr.name, prop)) {
                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
            }
        }

        if (dec.deriveType) |derive| {
            const deriveName = switch (derive.*) {
                .StaticStructInstance => |structName| structName,
                .Custom => |custom| custom.name,
                else => unreachable,
            };

            return validateSelfProps(allocator, compInfo, deriveName, prop);
        }

        return null;
    }

    return ScanError.UndefinedStruct;
}

fn validateStaticStructProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8) !?*const blitzAst.AstTypes {
    if (string.compString(name, "self")) {
        const currentStruct = compInfo.getCurrentStruct();

        if (currentStruct) |current| {
            return validateSelfProps(allocator, compInfo, current, prop);
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

fn validateCustomProps(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8, withGenDef: bool) !?*const blitzAst.AstTypes {
    const dec = compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (attr.static) continue;

            if (string.compString(attr.name, prop)) {
                if (attr.visibility != .Public) return null;

                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, withGenDef);
            }
        }

        if (structDec.deriveType) |deriveType| {
            switch (deriveType.*) {
                .Custom => |c| return try validateCustomProps(allocator, compInfo, c, prop, withGenDef),
                else => return ScanError.UnexpectedDeriveType,
            }
        }

        return null;
    }

    return ScanError.InvalidPropertySource;
}

fn scanAttributes(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.StructAttribute, withGenDef: bool) !void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                const prev = compInfo.returnInfo.setInFunction(true);
                defer compInfo.returnInfo.revertInFunction(prev);
                try compInfo.pushScope(false);
                defer compInfo.popScope();
                const lastRetInfo = try compInfo.returnInfo.newInfo(true);
                defer compInfo.returnInfo.swapFree(lastRetInfo);

                try scanNodeForFunctions(allocator, compInfo, func.body);

                for (func.params) |param| {
                    const clonedPtr = try clone.cloneAstTypesPtr(allocator, compInfo, param.type, false);
                    try compInfo.setVariableType(param.name, clonedPtr, false);
                }

                const bodyType = try scanNode(allocator, compInfo, func.body, withGenDef);
                free.freeAstTypeInfo(allocator, bodyType);
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
    type1Param: *const blitzAst.AstTypes,
    type2Param: *const blitzAst.AstTypes,
    withGenDef: bool,
) !bool {
    const type1 = type1Param.*;
    const type2 = type2Param.*;

    if (type1 == .Any or type2 == .Any) return true;

    if (type1 == .Generic and type2 == .Generic) {
        if (withGenDef) {
            const genType1 = (try compInfo.getGeneric(type1.Generic)).?;
            if (genType1.* == .Generic and string.compString(type1.Generic, genType1.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            const genType2 = (try compInfo.getGeneric(type2.Generic)).?;
            if (genType2.* == .Generic and string.compString(type2.Generic, genType2.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypes(allocator, compInfo, genType1, genType2, withGenDef);
        }

        return true;
    }

    if (type1 == .Generic) {
        if (!withGenDef) return true;

        const genType = try compInfo.getGeneric(type1.Generic);
        if (genType) |gType| {
            if (gType.* == .Generic and string.compString(gType.Generic, type1.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypes(allocator, compInfo, gType, type2Param, withGenDef);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        } else return true;
    }

    if (type2 == .Generic) {
        if (!withGenDef) return true;

        const genType = try compInfo.getGeneric(type2.Generic);
        if (genType) |gType| {
            if (gType.* == .Generic and string.compString(gType.Generic, type2.Generic)) {
                return ScanError.UnexpectedRecursiveGeneric;
            }

            return matchTypes(allocator, compInfo, type1Param, gType, withGenDef);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        } else return true;
    }

    return switch (type1) {
        .String => type2 == .String,
        .Bool => type2 == .Bool,
        .Char => type2 == .Char,
        .Void => type2 == .Void,
        .Null => type2 == .Nullable or type2 == .Null,
        .Nullable => |inner| type2 == .Null or try matchTypes(allocator, compInfo, inner, type2Param, withGenDef),
        .Number => |num| type2 == .Number and @intFromEnum(num) == @intFromEnum(type2.Number),
        .DynamicArray => |arr| {
            if (type2 == .GeneralArray) return true;
            return type2 == .DynamicArray and try matchTypes(allocator, compInfo, arr, type2.DynamicArray, withGenDef);
        },
        .StaticArray => |arr| {
            const array: blitzAst.AstStaticArrayType = switch (type2) {
                .StaticArray => |staticArr| staticArr,
                .GeneralArray => |generalArr| generalArr,
                else => return false,
            };

            const sizeType1 = try scanNode(allocator, compInfo, arr.size, withGenDef);
            defer free.freeAstTypeInfo(allocator, sizeType1);
            const sizeType2 = try scanNode(allocator, compInfo, array.size, withGenDef);
            defer free.freeAstTypeInfo(allocator, sizeType2);

            if (!isInt(sizeType1.astType) or !isInt(sizeType2.astType)) {
                return false;
            }

            if (type2 == .GeneralArray) {
                if (arr.size.* != .Value and arr.size.Value != .Number and arr.size.Value != .RawNumber) {
                    return ScanError.ExpectedUSizeOrU32ForStaticArraySize;
                }

                var arrSize: blitzAst.AstNumber = undefined;
                if (arr.size.Value == .Number) {
                    arrSize = arr.size.Value.Number;
                } else {
                    arrSize = try blitzAst.rawNumberToInferredType(arr.size.Value.RawNumber);
                }

                if (arrSize != .USize and arrSize != .U32) {
                    return ScanError.ExpectedUSizeOrU32ForStaticArraySize;
                }

                const num1 = if (arrSize == .U32) @as(usize, arrSize.U32) else arrSize.USize;
                const num2 = type2.GeneralArray.size.Value.Number.USize;

                if (num1 != num2) {
                    return ScanError.ExpectedEqualStaticArraySizes;
                }
            }

            return try matchTypes(allocator, compInfo, arr.type, array.type, withGenDef);
        },
        .Custom => |custom| {
            if (type2 == .StaticStructInstance and string.compString(custom.name, type2.StaticStructInstance)) {
                return true;
            }

            if (type2 != .Custom) return false;
            if (!string.compString(type1.Custom.name, type2.Custom.name)) return false;
            if (custom.generics.len != type2.Custom.generics.len) return false;

            for (custom.generics, 0..) |gen, index| {
                const genMatch = try matchTypes(allocator, compInfo, gen, type2.Custom.generics[index], withGenDef);
                if (!genMatch) return ScanError.CustomGenericMismatch;
            }

            return true;
        },
        .Error => |err| switch (type2) {
            .Error => |err2| string.compString(err.name, err2.name),
            .ErrorVariant => |err2| string.compString(err.name, err2.from),
            else => {
                if (err.payload) |payload| {
                    return matchTypes(allocator, compInfo, payload, type2Param, withGenDef);
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
                return true;
            }

            return false;
        },
        else => false,
    };
}

fn isPrimary(astType: *const blitzAst.AstTypes) bool {
    return switch (astType.*) {
        .String, .Bool, .Char, .Number, .Null, .RawNumber => true,
        .Nullable => |inner| isPrimary(inner),
        else => false,
    };
}

fn getPropertyType(allocator: Allocator, compInfo: *CompInfo, source: blitzAst.AstTypes, prop: []u8) !blitzAst.AstTypes {
    return switch (source) {
        .StaticStructInstance => |inst| try getStructPropType(compInfo, false, inst, prop),
        .StaticArray => try builtins.getStaticArrayPropTypes(prop),
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
    const dec = compInfo.getStructDec(inst);
    if (dec == null) return ScanError.InvalidPropertySource;

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

fn inferStaticArrType(allocator: Allocator, compInfo: *CompInfo, arr: []*const blitzAst.AstNode, withGenDef: bool) !blitzAst.AstTypeInfo {
    if (arr.len == 0) return try utils.astTypesToInfo(allocator, .Void, true);

    const firstType = try scanNode(allocator, compInfo, arr[0], withGenDef);
    var isConst = firstType.isConst;

    for (arr[1..]) |item| {
        const exprType = try scanNode(allocator, compInfo, item, withGenDef);
        defer free.freeAstTypeInfo(allocator, exprType);

        isConst = isConst or exprType.isConst;

        if (!try matchTypes(allocator, compInfo, exprType.astType, firstType.astType, false)) {
            return ScanError.StaticArrayTypeMismatch;
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
