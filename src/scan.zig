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

pub const ScanError = error{
    StaticArrayTypeMismatch,
    InvalidCast,
    ExpectedBooleanBang,
    VariableAnnotationMismatch,
    UndefinedOrUnknownVariableType,
    ExpectedBooleanIfCondition,
    FunctionReturnTypeMismatch,
    VariableAlreadyExists,
    FunctionCallParamTypeMismatch,
    FunctionCallParamCountMismatch,
    VoidVariableDec,
    ExpectedFunctionReturn,
    StructInitGenericCountMismatch,
    StructInitAttributeCountMismatch,
    StructInitMemberTypeMismatch,
    StructInitAttributeNotFound,
    InvalidProperty,
    UnsupportedFeature,
    InvalidPropertySource,
    IdentifierNotAFunction,
    CannotCallNonFunctionNode,
    StaticAccessFromStructInstance,
    NonStaticAccessFromStaticStructReference,
    UnexpectedDeriveType,
    SelfUsedOutsideStruct,
    UndefinedStruct,
    RestrictedPropertyAccess,
    MathOpOnNonNumberType,
    MathOpTypeMismatch,
    ExpectedUSizeForIndex,
    ExpectedArrayForIndexTarget,
    EmptyGenericType,
    CustomGenericMismatch,
};

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, compInfo: *CompInfo) !void {
    while (compInfo.variableScopes.items.len > 1) compInfo.popScope();

    try scanNodes(allocator, compInfo, ast.root.nodes, false);
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
        .NoOp => return .Void,

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

            if (indexType == .Number and indexType.Number != .USize) {
                return ScanError.ExpectedUSizeForIndex;
            }

            return switch (valueType) {
                .StaticArray => |arr| try clone.cloneAstTypes(allocator, compInfo, arr.type.*, false),
                .DynamicArray => |arr| try clone.cloneAstTypes(allocator, compInfo, arr.*, false),
                else => return ScanError.ExpectedArrayForIndexTarget,
            };
        },
        .MathOp => |op| {
            const left = try scanNode(allocator, compInfo, op.left, withGenDef);
            defer free.freeStackType(allocator, &left);
            const right = try scanNode(allocator, compInfo, op.right, withGenDef);
            defer free.freeStackType(allocator, &right);

            if (left != .Number or right != .Number) return ScanError.MathOpOnNonNumberType;
            if (!number.sameType(left.Number, right.Number)) return ScanError.MathOpTypeMismatch;

            return clone.cloneAstTypes(allocator, compInfo, left, false);
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
                    const propType = try validateCustomProps(allocator, compInfo, custom, access.property);
                    if (propType) |t| {
                        return t;
                    }

                    break :a false;
                },
                .StaticStructInstance => |name| a: {
                    const propType = try validateStaticStructProps(allocator, compInfo, name, access.property);
                    if (propType) |t| {
                        return t;
                    }

                    break :a false;
                },
                else => false,
            };

            if (!valid) return ScanError.InvalidProperty;

            // TODO - update with builtin prop types
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
        .Variable => |v| {
            if (compInfo.isInStructMethod() and string.compString(v.name, "self")) {
                return .{
                    .StaticStructInstance = v.name,
                };
            }

            const varType = compInfo.getVariableType(v.name);

            if (varType) |t| {
                return try clone.cloneAstTypes(allocator, compInfo, t, false);
            }

            return ScanError.UndefinedOrUnknownVariableType;
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
            const scanRes = try scanFuncBodyAndReturn(allocator, compInfo, func, withGenDef);
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

            if (func.params.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            for (func.params, 0..) |param, index| {
                const paramType = try scanNode(allocator, compInfo, call.params[index], withGenDef);
                var isGeneric = false;

                switch (param.type.*) {
                    .Generic => |generic| {
                        const typePtr = try create(blitzAst.AstTypes, allocator, paramType);
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
            const structDec = compInfo.getStructDec(init.name).?;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.StructInitGenericCountMismatch;
            }

            try scanGenerics(init.generics, structDec.generics);

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

                // TODO - check something for generics
                if (!try matchTypes(allocator, compInfo, attr.attr.Member.*, attrType, false)) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            return .{
                .Custom = .{
                    // TODO - prob do some generic thing
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

// TODO - review for generic replacing
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
            if (string.compString(attr.name, prop)) {
                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
            }
        }

        if (dec.deriveType) |derived| {
            // TODO - handle litterally anything else
            return validateSelfProps(allocator, compInfo, derived.StaticStructInstance, prop);
        }
    } else {
        return ScanError.UndefinedStruct;
    }

    return null;
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

fn validateCustomProps(allocator: Allocator, compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8) !?blitzAst.AstTypes {
    const dec = compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (attr.static) continue;

            if (string.compString(attr.name, prop)) {
                if (attr.visibility != .Public) return null;

                return try clone.cloneStructAttributeUnionType(allocator, compInfo, attr.attr, false);
            }
        }

        if (structDec.deriveType) |deriveType| {
            switch (deriveType.*) {
                .Custom => |c| return try validateCustomProps(allocator, compInfo, c, prop),
                else => return ScanError.UnexpectedDeriveType,
            }
        }

        return null;
    }

    return ScanError.InvalidPropertySource;
}

fn scanGenerics(initGenerics: []*const blitzAst.AstTypes, decGenerics: []blitzAst.GenericType) !void {
    // TODO - fill in
    _ = initGenerics;
    _ = decGenerics;
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

                // TODO - do things with generics
                const bodyType = try scanNode(allocator, compInfo, func.body, false);
                free.freeStackType(allocator, &bodyType);
            },
        }
    }
}

fn matchNumber(num1: blitzAst.AstNumberVariants, num2: blitzAst.AstNumberVariants) bool {
    return switch (num1) {
        .U8 => num2 == .U8,
        .U16 => num2 == .U16,
        .U32 => num2 == .U32,
        .U64 => num2 == .U64,
        .U128 => num2 == .U128,
        .I8 => num2 == .I8,
        .I16 => num2 == .I16,
        .I32 => num2 == .I32,
        .I64 => num2 == .I64,
        .I128 => num2 == .I128,
        .F8 => num2 == .F8,
        .F16 => num2 == .F16,
        .F32 => num2 == .F32,
        .F64 => num2 == .F64,
        .F128 => num2 == .F128,
        .USize => num2 == .USize,
    };
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
        .Number => |num| type2 == .Number and matchNumber(num, type2.Number),
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

            for (custom.generics, 0..) |gen, index| {
                const genMatch = try matchTypes(allocator, compInfo, gen.*, type2.Custom.generics[index].*, withGenDef);
                if (!genMatch) return ScanError.CustomGenericMismatch;
            }

            break :a true;
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
