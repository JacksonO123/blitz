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

pub const ScanError = error{
    // misc
    StaticArrayTypeMismatch,
    InvalidCast,
    ExpectedBooleanBang,
    ExpectedBooleanIfCondition,
    UnsupportedFeature,
    ExpectedUSizeForIndex,
    ExpectedArrayForIndexTarget,

    // variables
    VariableAnnotationMismatch,
    VariableAlreadyExists,
    VoidVariableDec,
    VariableTypeAndValueTypeMismatch,

    // functions
    ExpectedFunctionReturn,
    FunctionCallParamTypeMismatch,
    FunctionCallParamCountMismatch,
    FunctionReturnTypeMismatch,
    IdentifierNotAFunction,
    CannotCallNonFunctionNode,
    VariableIsUndefined,

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

    // operations
    MathOpOnNonNumberType,
    OpExprTypeMismatch,
    ExpectedBoolInBoolOp,
    InvalidBitOperation,
    BitMaskWithMismatchingSize,
    NumberTypeMismatch,

    // generics
    EmptyGenericType,
    CustomGenericMismatch,
    ConflictingGenericParameters,
    GenericRestrictionConflict,

    // errors
    ExpectedUseOfErrorVariants,

    // possibly remove
    InvalidPropertySource,
};

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, compInfo: *CompInfo) !void {
    while (compInfo.variableScopes.items.len > 1) compInfo.popScope();

    const nodeType = try scanNode(allocator, compInfo, ast.root, true);
    free.freeStackType(allocator, &nodeType);
}

pub fn scanNodes(
    allocator: Allocator,
    compInfo: *CompInfo,
    nodes: []*const blitzAst.AstNode,
    withGenDef: bool,
) (ScanError || Allocator.Error || clone.CloneError)!void {
    for (nodes) |node| {
        const nodeType = try scanNode(allocator, compInfo, node, withGenDef);
        free.freeStackType(allocator, &nodeType);
    }
}

pub fn scanNode(
    allocator: Allocator,
    compInfo: *CompInfo,
    node: *const blitzAst.AstNode,
    withGenDef: bool,
) (Allocator.Error || ScanError || clone.CloneError)!blitzAst.AstTypes {
    switch (node.*) {
        .NoOp, .ErrorDec => return .Void,

        .StaticStructInstance => |inst| {
            return .{ .StaticStructInstance = inst };
        },
        .Cast => |cast| {
            const nodeType = try scanNode(allocator, compInfo, cast.node, withGenDef);
            defer free.freeStackType(allocator, &nodeType);

            if (isPrimary(nodeType) and isPrimary(cast.toType.*)) {
                return try clone.cloneAstTypes(allocator, compInfo, cast.toType.*, false);
            }

            return ScanError.InvalidCast;
        },
        .Value => |val| {
            const valueType: blitzAst.AstTypes = switch (val) {
                .String => .String,
                .Bool => .Bool,
                .Char => .Char,
                .Number => |num| .{ .Number = num.type },
                .StaticArray => |arr| a: {
                    const buf = try std.fmt.allocPrint(allocator, "{}", .{arr.len});

                    break :a .{
                        .StaticArray = .{
                            .type = try create(blitzAst.AstTypes, allocator, try inferStaticArrType(allocator, compInfo, arr, withGenDef)),
                            .size = try create(blitzAst.AstNode, allocator, .{
                                .Value = .{
                                    .Number = .{
                                        .type = .USize,
                                        .value = buf,
                                    },
                                },
                            }),
                        },
                    };
                },
            };

            return valueType;
        },
        .Type => |t| return t,
        .IndexValue => |index| {
            const indexType = try scanNode(allocator, compInfo, index.index, withGenDef);
            defer free.freeStackType(allocator, &indexType);
            const valueType = try scanNode(allocator, compInfo, index.value, withGenDef);
            defer free.freeStackType(allocator, &valueType);

            if (indexType == .Number and indexType.Number != .USize) {
                return ScanError.ExpectedUSizeForIndex;
            }

            return switch (valueType) {
                .StaticArray => |arr| try clone.cloneAstTypes(allocator, compInfo, arr.type.*, false),
                .DynamicArray => |arr| try clone.cloneAstTypes(allocator, compInfo, arr.*, false),
                else => return ScanError.ExpectedArrayForIndexTarget,
            };
        },
        .OpExpr => |op| {
            const left = try scanNode(allocator, compInfo, op.left, withGenDef);
            defer free.freeStackType(allocator, &left);
            const right = try scanNode(allocator, compInfo, op.right, withGenDef);
            defer free.freeStackType(allocator, &right);

            if (std.meta.activeTag(left) != std.meta.activeTag(right)) return ScanError.OpExprTypeMismatch;

            switch (op.type) {
                .BitAnd, .BitOr => {
                    if (left != .Number) return ScanError.InvalidBitOperation;
                    if (!compareNumberBitSize(left.Number, right.Number)) return ScanError.BitMaskWithMismatchingSize;
                    return try clone.cloneAstTypes(allocator, compInfo, left, false);
                },
                .And, .Or => {
                    if (left != .Bool) return ScanError.ExpectedBoolInBoolOp;
                    return .Bool;
                },
                .Add, .Sub, .Mult, .Div => {
                    if (left != .Number) return ScanError.MathOpOnNonNumberType;
                    if (@intFromEnum(left.Number) != @intFromEnum(right.Number)) return ScanError.NumberTypeMismatch;

                    if (op.type == .Div) {
                        if (switch (left.Number) {
                            .F8, .F16, .F32, .F64, .F128 => true,
                            else => false,
                        }) {
                            return try clone.cloneAstTypes(allocator, compInfo, left, false);
                        }

                        return .{
                            .Number = .F32,
                        };
                    }

                    return try clone.cloneAstTypes(allocator, compInfo, left, false);
                },
            }
        },
        .FuncReference => |ref| {
            const dec = compInfo.getFunction(ref);

            if (dec) |funcRef| {
                return .{
                    .Function = funcRef,
                };
            }

            return ScanError.IdentifierNotAFunction;
        },
        .PropertyAccess => |access| {
            const res = try scanNode(allocator, compInfo, access.value, withGenDef);
            defer free.freeStackType(allocator, &res);

            const valid = switch (res) {
                .DynamicArray => builtins.validateDynamicArrayProps(access.property),
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

                    try compInfo.pushGenScope();
                    defer compInfo.popGenScope();

                    for (genNameArr.items, genTypeArr.items) |name, genType| {
                        try compInfo.setGeneric(name, genType);
                    }

                    const propType = try validateCustomProps(allocator, compInfo, custom, access.property, withGenDef);

                    if (propType) |t| {
                        return t;
                    }

                    break :a false;
                },
                .StaticStructInstance => |name| a: {
                    try compInfo.pushGenScope();
                    defer compInfo.popGenScope();
                    const propType = try validateStaticStructProps(allocator, compInfo, name, access.property);

                    if (propType) |t| {
                        if (!string.compString(name, "self")) {
                            const dec = compInfo.getStructDec(name).?;

                            if (t == .Function) {
                                for (dec.generics) |gen| {
                                    if (gen.restriction) |restriction| {
                                        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, restriction, false);
                                        try compInfo.setGeneric(gen.name, typeClone);
                                    }
                                }
                            }
                        }

                        return t;
                    }

                    break :a false;
                },
                .Error => |err| a: {
                    const errDec = compInfo.getErrorDec(err).?;
                    if (!blitzAst.variantExists(errDec.variants, access.property)) break :a false;

                    return .{
                        .ErrorVariant = .{
                            .from = try string.cloneString(allocator, err),
                            .variant = try string.cloneString(allocator, access.property),
                        },
                    };
                },
                else => false,
            };

            if (!valid) return ScanError.InvalidProperty;

            // TODO - update with builtin prop types (array.length, array.push, etc)
            return .Void;
        },
        .ReturnNode => |ret| {
            return try scanNode(allocator, compInfo, ret, withGenDef);
        },
        .Seq => |seq| {
            try scanNodes(allocator, compInfo, seq.nodes, withGenDef);
            return .Void;
        },
        .VarDec => |dec| {
            if (compInfo.getVariableType(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            const setType = try scanNode(allocator, compInfo, dec.setNode, withGenDef);

            if (setType == .Void) {
                return ScanError.VoidVariableDec;
            }

            const setPtr = try create(blitzAst.AstTypes, allocator, setType);

            if (dec.annotation) |annotation| {
                if (try matchTypes(allocator, compInfo, annotation.*, setType, false)) {
                    try compInfo.setVariableType(dec.name, setPtr);
                } else {
                    return ScanError.VariableAnnotationMismatch;
                }
            } else {
                try compInfo.setVariableType(dec.name, setPtr);
            }

            return .Void;
        },
        .VarSet => |set| {
            const varType = compInfo.getVariableType(set.variable);
            if (varType == null) return ScanError.VariableIsUndefined;

            const setNode = try scanNode(allocator, compInfo, set.setNode, withGenDef);
            defer free.freeStackType(allocator, &setNode);

            if (!(try matchTypes(allocator, compInfo, varType.?, setNode, withGenDef))) {
                return ScanError.VariableTypeAndValueTypeMismatch;
            }

            return .Void;
        },
        .Variable => |v| {
            if (compInfo.isInStructMethod() and string.compString(v, "self")) {
                return .{
                    .StaticStructInstance = v,
                };
            }

            const varType = compInfo.getVariableType(v);

            if (varType) |t| {
                return try clone.cloneAstTypes(allocator, compInfo, t, false);
            }

            return ScanError.VariableIsUndefined;
        },
        .StructDec => |dec| {
            try compInfo.pushRegGenScope();
            try compInfo.addCurrentStruct(dec.name);
            defer _ = compInfo.popCurrentStruct();
            defer compInfo.popRegGenScope();

            for (dec.generics) |generic| {
                try compInfo.addAvailableGeneric(generic.name);
            }

            compInfo.enteringStruct();
            try scanAttributes(allocator, compInfo, dec.attributes);
            compInfo.exitingStruct();

            for (dec.generics) |generic| {
                compInfo.removeAvailableGeneric(generic.name);
            }

            return .Void;
        },
        .IfStatement => |statement| {
            try compInfo.pushScope();
            const conditionType = try scanNode(allocator, compInfo, statement.condition, withGenDef);
            defer free.freeStackType(allocator, &conditionType);
            if (conditionType != .Bool) return ScanError.ExpectedBooleanIfCondition;
            compInfo.popScope();

            return .Void;
        },
        .FuncDec => |name| {
            try compInfo.pushScope();
            defer compInfo.popScope();

            const func = compInfo.getFunction(name).?;
            const isGeneric = func.generics != null;

            const scanRes = try scanFuncBodyAndReturn(allocator, compInfo, func, !isGeneric);
            free.freeStackType(allocator, &scanRes);

            // TODO - replace with function type for anonymous functions sorta thing
            return .Void;
        },
        .FuncCall => |call| {
            try compInfo.pushScope();
            defer compInfo.popScope();
            try compInfo.pushGenScope();
            defer compInfo.popGenScope();

            const dec = try scanNode(allocator, compInfo, call.func, withGenDef);
            if (dec != .Function) return ScanError.CannotCallNonFunctionNode;
            const func = dec.Function;

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

            if (func.params.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            for (func.params, 0..) |param, index| {
                const paramType = try scanNode(allocator, compInfo, call.params[index], withGenDef);
                var isGeneric = false;

                switch (param.type.*) {
                    .Generic => |generic| {
                        const typePtr = try create(blitzAst.AstTypes, allocator, paramType);

                        if (compInfo.getGeneric(generic)) |gen| {
                            if (!(try matchTypes(allocator, compInfo, paramType, gen.*, false))) {
                                return ScanError.GenericRestrictionConflict;
                            }
                        }

                        try compInfo.setGeneric(generic, typePtr);
                        isGeneric = true;
                    },
                    .Custom => |custom| {
                        try matchParamGenericTypes(allocator, compInfo, custom, &paramType);
                    },
                    else => {},
                }

                if (!try matchTypes(allocator, compInfo, param.type.*, paramType, false)) {
                    return ScanError.FunctionCallParamTypeMismatch;
                }

                if (!isGeneric) {
                    free.freeStackType(allocator, &paramType);
                }
            }

            return try scanFuncBodyAndReturn(allocator, compInfo, func, true);
        },
        .StructInit => |init| {
            try compInfo.pushGenScope();
            defer compInfo.popGenScope();

            const structDec = compInfo.getStructDec(init.name).?;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.StructInitGenericCountMismatch;
            }

            try setInitGenerics(allocator, compInfo, init.generics, structDec.generics);

            if (structDec.deriveType) |derived| {
                try setInitDerivedGenerics(allocator, compInfo, derived);
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
                    }
                }

                if (attrNode == null) {
                    return ScanError.StructInitAttributeNotFound;
                }

                const attrType = try scanNode(allocator, compInfo, attrNode.?, withGenDef);
                defer free.freeStackType(allocator, &attrType);

                if (!try matchTypes(allocator, compInfo, attr.attr.Member.*, attrType, true)) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            return .{
                .Custom = .{
                    .generics = try clone.cloneTypesArr(allocator, compInfo, init.generics, false),
                    .name = try string.cloneString(allocator, init.name),
                },
            };
        },
        .Bang => |bang| {
            const bangType = try scanNode(allocator, compInfo, bang, withGenDef);
            defer free.freeStackType(allocator, &bangType);
            if (bangType != .Bool) return ScanError.ExpectedBooleanBang;

            return .Bool;
        },
        .Error => |err| {
            const dec = compInfo.getErrorDec(err).?;
            if (dec.variants.len > 0) return ScanError.ExpectedUseOfErrorVariants;

            return .{
                .Error = try string.cloneString(allocator, err),
            };
        },
    }
}

fn compareNumberBitSize(num1: blitzAst.AstNumberVariants, num2: blitzAst.AstNumberVariants) bool {
    return switch (num1) {
        .USize => num2 == .USize,
        .U8 => num2 == .F8 or num2 == .I8,
        .U16 => num2 == .F16 or num2 == .I16,
        .U32 => num2 == .F32 or num2 == .I32,
        .U64 => num2 == .F64 or num2 == .I64,
        .U128 => num2 == .F128 or num2 == .I128,
        .I8 => num2 == .U8 or num2 == .F8,
        .I16 => num2 == .U16 or num2 == .F16,
        .I32 => num2 == .U32 or num2 == .F32,
        .I64 => num2 == .U64 or num2 == .F64,
        .I128 => num2 == .U128 or num2 == .F128,
        .F8 => num2 == .I8 or num2 == .U8,
        .F16 => num2 == .I16 or num2 == .U16,
        .F32 => num2 == .I32 or num2 == .U32,
        .F64 => num2 == .I64 or num2 == .U64,
        .F128 => num2 == .I128 or num2 == .U128,
    };
}

fn setInitGenerics(allocator: Allocator, compInfo: *CompInfo, genTypes: []*const blitzAst.AstTypes, decGens: []blitzAst.GenericType) !void {
    for (genTypes, 0..) |t, index| {
        const decGen = decGens[index];

        if (decGens[index].restriction) |restriction| {
            if (!(try matchTypes(allocator, compInfo, t.*, restriction.*, false))) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, t, false);
        try compInfo.setGeneric(decGen.name, typeClone);
    }
}

fn setInitDerivedGenerics(allocator: Allocator, compInfo: *CompInfo, deriveType: *const blitzAst.AstTypes) !void {
    const generics = deriveType.Custom.generics;
    const derivedDec = compInfo.getStructDec(deriveType.Custom.name);
    const decGens = derivedDec.?.generics;

    for (generics, decGens) |gen, decGen| {
        const clonedType = try clone.cloneAstTypesPtr(allocator, compInfo, gen, true);

        if (decGen.restriction) |restriction| {
            if (!(try matchTypes(allocator, compInfo, clonedType.*, restriction.*, true))) {
                return ScanError.GenericRestrictionConflict;
            }
        }

        try compInfo.setGeneric(decGen.name, clonedType);
    }
}

fn matchParamGenericTypes(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, paramType: *const blitzAst.AstTypes) !void {
    switch (paramType.*) {
        .Custom => |paramCustom| {
            if (!string.compString(custom.name, paramCustom.name)) return ScanError.FunctionCallParamTypeMismatch;

            for (custom.generics, 0..) |gen, index| {
                const paramGen = paramCustom.generics[index];

                switch (gen.*) {
                    .Generic => |generic| {
                        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, paramGen, false);

                        const genType = compInfo.getGeneric(generic);
                        if (genType) |t| {
                            if (!(try matchTypes(allocator, compInfo, t.*, typeClone.*, true))) {
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

fn scanFuncBodyAndReturn(allocator: Allocator, compInfo: *CompInfo, func: *const blitzAst.FuncDecNode, withGenDef: bool) !blitzAst.AstTypes {
    for (func.params) |param| {
        const typeClone = try clone.cloneAstTypesPtr(allocator, compInfo, param.type, false);
        try compInfo.setVariableType(param.name, typeClone);
    }

    {
        try compInfo.pushScope();
        defer compInfo.popScope();
        const bodyType = try scanNode(allocator, compInfo, func.body, withGenDef);
        free.freeStackType(allocator, &bodyType);
    }

    switch (func.body.*) {
        .Seq => |seq| {
            if (seq.nodes.len == 0) {
                if (func.returnType.* == .Void) {
                    return .Void;
                } else {
                    return ScanError.ExpectedFunctionReturn;
                }
            }

            const last = seq.nodes[seq.nodes.len - 1];
            const lastType = try scanNode(allocator, compInfo, last, withGenDef);
            defer free.freeStackType(allocator, &lastType);

            if (last.* == .ReturnNode or func.returnType.* != .Void) {
                if (!try matchTypes(allocator, compInfo, func.returnType.*, lastType, withGenDef)) {
                    return ScanError.FunctionReturnTypeMismatch;
                }
            }
        },
        .ReturnNode => |ret| {
            const retType = try scanNode(allocator, compInfo, ret, withGenDef);
            defer free.freeStackType(allocator, &retType);

            if (!try matchTypes(allocator, compInfo, func.returnType.*, retType, withGenDef)) {
                return ScanError.FunctionReturnTypeMismatch;
            }
        },
        else => if (func.returnType.* != .Void) return ScanError.FunctionReturnTypeMismatch,
    }

    return clone.cloneAstTypes(allocator, compInfo, func.returnType.*, withGenDef);
}

fn validateSelfProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8) !?blitzAst.AstTypes {
    const structDec = compInfo.getStructDec(name);

    if (structDec) |dec| {
        for (dec.attributes) |attr| {
            if ((attr.visibility == .Public or attr.visibility == .Protected) and string.compString(attr.name, prop)) {
                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
            }
        }

        if (dec.deriveType) |derived| {
            const derivedName = switch (derived.*) {
                .StaticStructInstance => |structName| structName,
                .Custom => |custom| custom.name,
                else => unreachable,
            };

            return validateSelfProps(allocator, compInfo, derivedName, prop);
        }

        return null;
    }

    return ScanError.UndefinedStruct;
}

fn validateStaticStructProps(allocator: Allocator, compInfo: *CompInfo, name: []u8, prop: []u8) !?blitzAst.AstTypes {
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

fn validateCustomProps(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8, withGenDef: bool) !?blitzAst.AstTypes {
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

fn scanAttributes(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.StructAttribute) !void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                try compInfo.pushScope();
                defer compInfo.popScope();

                for (func.params) |param| {
                    const clonedPtr = try clone.cloneAstTypesPtr(allocator, compInfo, param.type, false);
                    try compInfo.setVariableType(param.name, clonedPtr);
                }

                const bodyType = try scanNode(allocator, compInfo, func.body, true);
                free.freeStackType(allocator, &bodyType);
            },
        }
    }
}

// fn matchNumber(num1: blitzAst.AstNumberVariants, num2: blitzAst.AstNumberVariants) bool {
//     return switch (num1) {
//         .U8 => num2 == .U8,
//         .U16 => num2 == .U16,
//         .U32 => num2 == .U32,
//         .U64 => num2 == .U64,
//         .U128 => num2 == .U128,
//         .I8 => num2 == .I8,
//         .I16 => num2 == .I16,
//         .I32 => num2 == .I32,
//         .I64 => num2 == .I64,
//         .I128 => num2 == .I128,
//         .F8 => num2 == .F8,
//         .F16 => num2 == .F16,
//         .F32 => num2 == .F32,
//         .F64 => num2 == .F64,
//         .F128 => num2 == .F128,
//         .USize => num2 == .USize,
//     };
// }

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

fn isInt(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
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
            .F8, .F16, .F32, .F64, .F128 => true,
            else => false,
        },
        else => false,
    };
}

fn matchTypes(allocator: Allocator, compInfo: *CompInfo, type1: blitzAst.AstTypes, type2: blitzAst.AstTypes, withGenDef: bool) !bool {
    if (type1 == .Generic and type2 == .Generic) {
        if (withGenDef) {
            return string.compString(type1.Generic, type2.Generic);
        }

        return true;
    }

    if (type1 == .Generic) {
        const genType = compInfo.getGeneric(type1.Generic);
        if (genType) |gType| {
            return matchTypes(allocator, compInfo, gType.*, type2, withGenDef);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        } else return true;
    }

    if (type2 == .Generic) {
        const genType = compInfo.getGeneric(type2.Generic);
        if (genType) |gType| {
            return matchTypes(allocator, compInfo, type1, gType.*, withGenDef);
        } else if (withGenDef) {
            return ScanError.EmptyGenericType;
        } else return true;
    }

    const res = switch (type1) {
        .String => type2 == .String,
        .Bool => type2 == .Bool,
        .Char => type2 == .Char,
        .Void => type2 == .Void,
        .Number => |num| type2 == .Number and @intFromEnum(num) == @intFromEnum(type2.Number),
        .DynamicArray => |arr| type2 == .DynamicArray and try matchTypes(allocator, compInfo, arr.*, type2.DynamicArray.*, withGenDef),
        .StaticArray => |arr| a: {
            if (type2 != .StaticArray) {
                return false;
            }

            const sizeType1 = try scanNode(allocator, compInfo, arr.size, withGenDef);
            const sizeType2 = try scanNode(allocator, compInfo, type2.StaticArray.size, withGenDef);
            defer free.freeStackType(allocator, &sizeType1);
            defer free.freeStackType(allocator, &sizeType2);

            if (!isInt(sizeType1) or !isInt(sizeType2)) {
                break :a false;
            }

            break :a try matchTypes(allocator, compInfo, arr.type.*, type2.StaticArray.type.*, withGenDef);
        },
        .Custom => |custom| a: {
            if (type2 != .Custom) return false;
            if (!string.compString(type1.Custom.name, type2.Custom.name)) return false;
            if (custom.generics.len != type2.Custom.generics.len) return false;

            for (custom.generics, 0..) |gen, index| {
                const genMatch = try matchTypes(allocator, compInfo, gen.*, type2.Custom.generics[index].*, withGenDef);
                if (!genMatch) return ScanError.CustomGenericMismatch;
            }

            break :a true;
        },
        .Error => |err| a: {
            switch (type2) {
                .Error => |err2| break :a string.compString(err, err2),
                .ErrorVariant => |err2| break :a string.compString(err, err2.from),
                else => break :a false,
            }
        },
        .ErrorVariant => |err| a: {
            switch (type2) {
                .Error => |err2| break :a string.compString(err.from, err2),
                .ErrorVariant => |err2| break :a string.compString(err.from, err2.from) and string.compString(err.variant, err2.variant),
                else => break :a false,
            }
        },
        else => false,
    };

    return res;
}

fn isPrimary(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
        .String, .Bool, .Char, .Number => true,
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

fn inferStaticArrType(allocator: Allocator, compInfo: *CompInfo, arr: []*const blitzAst.AstNode, withGenDef: bool) !blitzAst.AstTypes {
    if (arr.len == 0) return .Void;

    const firstType = try scanNode(allocator, compInfo, arr[0], withGenDef);

    for (arr[1..]) |item| {
        const exprType = try scanNode(allocator, compInfo, item, withGenDef);
        defer free.freeStackType(allocator, &exprType);

        if (!try matchTypes(allocator, compInfo, exprType, firstType, false)) {
            return ScanError.StaticArrayTypeMismatch;
        }
    }

    return firstType;
}
