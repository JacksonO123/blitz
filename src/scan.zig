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
    StructInitGenericsCountMismatch,
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
};

pub fn typeScan(allocator: Allocator, ast: blitzAst.Ast, compInfo: *CompInfo) !void {
    try scanNodes(allocator, compInfo, ast.root.nodes);
}

pub fn scanNodes(allocator: Allocator, compInfo: *CompInfo, nodes: []*const blitzAst.AstNode) (ScanError || Allocator.Error)!void {
    for (nodes) |node| {
        try scanNode(allocator, compInfo, node);
    }
}

pub fn scanNode(allocator: Allocator, compInfo: *CompInfo, node: *const blitzAst.AstNode) (Allocator.Error || ScanError)!void {
    switch (node.*) {
        .NoOp, .Type, .Value, .Cast, .StaticStructInstance => {},
        .Add, .Sub, .Mult, .Div => |op| {
            const left = try getExpressionType(allocator, compInfo, op.left);
            defer free.freeStackType(allocator, &left);
            const right = try getExpressionType(allocator, compInfo, op.right);
            defer free.freeStackType(allocator, &right);

            if (left != .Number or right != .Number) return ScanError.MathOpOnNonNumberType;
            if (!number.sameType(left.Number, right.Number)) return ScanError.MathOpTypeMismatch;
        },
        .FuncReference => |ref| {
            if (!compInfo.hasFunctionName(ref)) return ScanError.IdentifierNotAFunction;
        },
        .PropertyAccess => |access| {
            const res = try getExpressionType(allocator, compInfo, access.value);
            defer free.freeStackType(allocator, &res);

            const validProp = switch (res) {
                .DynamicArray => builtins.validateDynamicArrayProps(access.property),
                .StaticArray => builtins.validateStaticArrayProps(access.property),
                .String => builtins.validateStringProps(access.property),
                .Custom => |custom| try validateCustomProps(compInfo, custom, access.property),
                .StaticStructInstance => |name| try validateStaticStructProps(compInfo, name, access.property),
                else => false,
            };

            if (!validProp) {
                return ScanError.InvalidProperty;
            }
        },
        .ReturnNode => |ret| {
            try scanNode(allocator, compInfo, ret);
        },
        .Seq => |seq| {
            try scanNodes(allocator, compInfo, seq.nodes);
        },
        .VarDec => |dec| {
            if (compInfo.getVariableType(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            try scanNode(allocator, compInfo, dec.setNode);

            const setType = try getExpressionType(allocator, compInfo, dec.setNode);

            if (setType == .Void) {
                return ScanError.VoidVariableDec;
            }

            const setPtr = try create(blitzAst.AstTypes, allocator, setType);

            if (dec.annotation) |annotation| {
                if (try matchTypes(allocator, compInfo, annotation.*, setType)) {
                    try compInfo.setVariableType(dec.name, setPtr);
                } else {
                    return ScanError.VariableAnnotationMismatch;
                }
            } else {
                try compInfo.setVariableType(dec.name, setPtr);
            }

            // TODO: remove variable types when out of scope
        },
        .Variable => |v| {
            const varType = compInfo.getVariableType(v.name);
            if (varType == null) return ScanError.UndefinedOrUnknownVariableType;
        },
        .StructDec => |dec| {
            try compInfo.addCurrentStruct(dec.name);

            for (dec.generics) |generic| {
                try compInfo.addGeneric(generic.name);
            }

            compInfo.enteringStruct();
            try scanAttributes(allocator, compInfo, dec.attributes);
            compInfo.exitingStruct();

            for (dec.generics) |generic| {
                compInfo.removeGeneric(generic.name);
            }

            _ = compInfo.popCurrentStruct();
        },
        .IfStatement => |statement| {
            const conditionType = try getExpressionType(allocator, compInfo, statement.condition);
            defer free.freeStackType(allocator, &conditionType);
            if (conditionType != .Bool) return ScanError.ExpectedBooleanIfCondition;
        },
        .FuncDec => |name| {
            const dec = compInfo.getFunction(name).?;
            try scanNode(allocator, compInfo, dec.body);

            switch (dec.body.*) {
                .Seq => |seq| {
                    if (seq.nodes.len == 0) {
                        if (dec.returnType.* == .Void) {
                            return;
                        } else {
                            return ScanError.ExpectedFunctionReturn;
                        }
                    }

                    const last = seq.nodes[seq.nodes.len - 1];
                    const lastType = try getExpressionType(allocator, compInfo, last);
                    defer free.freeStackType(allocator, &lastType);

                    if (last.* == .ReturnNode or dec.returnType.* != .Void) {
                        if (!try matchTypes(allocator, compInfo, dec.returnType.*, lastType)) {
                            return ScanError.FunctionReturnTypeMismatch;
                        }
                    }
                },
                .ReturnNode => |ret| {
                    const retType = try getExpressionType(allocator, compInfo, ret);
                    defer free.freeStackType(allocator, &retType);

                    if (!try matchTypes(allocator, compInfo, dec.returnType.*, retType)) {
                        return ScanError.FunctionReturnTypeMismatch;
                    }
                },
                else => if (dec.returnType.* != .Void) return ScanError.FunctionReturnTypeMismatch,
            }
        },
        .FuncCall => |call| {
            try scanNode(allocator, compInfo, call.func);
            const dec = try getExpressionType(allocator, compInfo, call.func);

            switch (dec) {
                .Function => |func| {
                    if (func.params.len != call.params.len) {
                        return ScanError.FunctionCallParamCountMismatch;
                    }

                    for (func.params, 0..) |param, index| {
                        const paramType = try getExpressionType(allocator, compInfo, call.params[index]);
                        defer free.freeStackType(allocator, &paramType);

                        if (!try matchTypes(allocator, compInfo, param.type.*, paramType)) {
                            return ScanError.FunctionCallParamTypeMismatch;
                        }
                    }
                },
                else => return ScanError.CannotCallNonFunctionNode,
            }
        },
        .StructInit => |init| {
            const structDec = compInfo.getStructDec(init.name).?;

            if (init.generics.len != structDec.generics.len) {
                return ScanError.StructInitGenericsCountMismatch;
            }

            try scanGenerics(init.generics, structDec.generics);

            var numMembers: usize = 0;
            for (structDec.attributes) |attr| {
                if (attr.static) continue;
                if (attr.attr == .Function) continue;
                numMembers += 1;

                var attrNode: ?*const blitzAst.AstNode = null;
                for (init.attributes) |initAttr| {
                    if (string.compString(initAttr.name, attr.name)) {
                        attrNode = initAttr.value;
                    }
                }

                if (attrNode == null) {
                    std.debug.print("cant find {s}\n", .{attr.name});
                    return ScanError.StructInitAttributeNotFound;
                }

                // TODO this is not ideal for generics, which should have
                // the ability to define ambiguity in their restrictions
                // which tighten when initialized, but for now whatever
                // ig generics dont exist

                const attrType = try getExpressionType(allocator, compInfo, attrNode.?);
                defer free.freeStackType(allocator, &attrType);

                if (!try matchTypes(allocator, compInfo, attr.attr.Member.*, attrType)) {
                    return ScanError.StructInitMemberTypeMismatch;
                }
            }

            // if (numMembers != init.attributes.len) {
            //     return ScanError.StructInitAttributeCountMismatch;
            // }
        },
        .Bang => |bang| {
            const bangType = try getExpressionType(allocator, compInfo, bang);
            defer free.freeStackType(allocator, &bangType);
            if (bangType != .Bool) return ScanError.ExpectedBooleanBang;
        },
    }
}

fn validateSelfProps(compInfo: *CompInfo, name: []u8, prop: []u8) !bool {
    const structDec = compInfo.getStructDec(name);

    if (structDec) |dec| {
        for (dec.attributes) |attr| {
            if (string.compString(attr.name, prop)) {
                return true;
            }
        }

        if (dec.deriveType) |derived| {
            // TODO - handle litterally anything else
            return validateSelfProps(compInfo, derived.StaticStructInstance, prop);
        }
    } else return ScanError.UndefinedStruct;

    return false;
}

fn validateStaticStructProps(compInfo: *CompInfo, name: []u8, prop: []u8) !bool {
    if (string.compString(name, "self")) {
        const currentStruct = compInfo.getCurrentStruct();
        if (currentStruct) |current| {
            return validateSelfProps(compInfo, current, prop);
        } else return ScanError.SelfUsedOutsideStruct;
    }

    const dec = compInfo.getStructDec(name).?;

    for (dec.attributes) |attr| {
        if (!string.compString(attr.name, prop)) continue;
        if (!attr.static) return ScanError.NonStaticAccessFromStaticStructReference;
        if (attr.visibility != .Public) return ScanError.RestrictedPropertyAccess;

        return true;
    }

    return ScanError.InvalidProperty;
}

fn validateCustomProps(compInfo: *CompInfo, custom: blitzAst.CustomType, prop: []u8) !bool {
    const dec = compInfo.getStructDec(custom.name);
    if (dec) |structDec| {
        for (structDec.attributes) |attr| {
            if (attr.static) continue;

            if (string.compString(attr.name, prop)) {
                if (attr.visibility != .Public) return false;
                return true;
            }
        }

        if (structDec.deriveType) |deriveType| {
            switch (deriveType.*) {
                .Custom => |c| return try validateCustomProps(compInfo, c, prop),
                else => return ScanError.UnexpectedDeriveType,
            }
        }

        return false;
    }

    return ScanError.InvalidPropertySource;
}

fn scanGenerics(initGenerics: []*const blitzAst.AstTypes, decGenerics: []blitzAst.GenericType) !void {
    _ = initGenerics;
    _ = decGenerics;
}

fn scanAttributes(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.StructAttribute) !void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => {},
            .Function => |func| {
                for (func.params) |param| {
                    try compInfo.setVariableType(param.name, param.type);
                }

                try scanNode(allocator, compInfo, func.body);

                for (func.params) |param| {
                    compInfo.removeVariableType(param.name);
                }
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

fn matchTypes(allocator: Allocator, compInfo: *CompInfo, type1: blitzAst.AstTypes, type2: blitzAst.AstTypes) !bool {
    switch (type1) {
        .String => return type2 == .String,
        .Bool => return type2 == .Bool,
        .Char => return type2 == .Char,
        .Void => return type2 == .Void,
        .Number => |num| return type2 == .Number and matchNumber(num, type2.Number),
        .DynamicArray => |arr| return type2 == .DynamicArray and try matchTypes(allocator, compInfo, arr.*, type2.DynamicArray.*),
        .StaticArray => |arr| {
            if (type2 != .StaticArray) {
                return false;
            }

            const sizeType1 = try getExpressionType(allocator, compInfo, arr.size);
            const sizeType2 = try getExpressionType(allocator, compInfo, type2.StaticArray.size);
            defer free.freeStackType(allocator, &sizeType1);
            defer free.freeStackType(allocator, &sizeType2);

            if (!isInt(sizeType1) or !isInt(sizeType2)) {
                return false;
            }

            return try matchTypes(allocator, compInfo, arr.type.*, type2.StaticArray.type.*);
        },
        else => {
            return true;
        },
    }

    return false;
}

fn isPrimary(astType: blitzAst.AstTypes) bool {
    return switch (astType) {
        .String, .Bool, .Char, .Number => true,
        else => false,
    };
}

fn getExpressionType(allocator: Allocator, compInfo: *CompInfo, expr: *const blitzAst.AstNode) (ScanError || Allocator.Error)!blitzAst.AstTypes {
    return switch (expr.*) {
        .NoOp => .Void,
        .Seq => .Void,
        .VarDec => .Void,
        .StructDec => .Void,
        .IfStatement => .Void,

        .Add, .Sub, .Mult, .Div => |op| {
            try scanNode(allocator, compInfo, expr);

            const leftType = try getExpressionType(allocator, compInfo, op.left);
            defer free.freeStackType(allocator, &leftType);
            const rightType = try getExpressionType(allocator, compInfo, op.right);
            defer free.freeStackType(allocator, &rightType);

            if (leftType != .Number or rightType != .Number) return ScanError.MathOpOnNonNumberType;
            if (!number.sameType(leftType.Number, rightType.Number)) return ScanError.MathOpTypeMismatch;

            return .{
                .Number = number.largestNumType(leftType.Number, rightType.Number),
            };
        },

        .FuncReference => |ref| {
            try scanNode(allocator, compInfo, expr);
            const dec = compInfo.getFunction(ref).?;
            return .{
                .Function = dec,
            };
        },
        .ReturnNode => |ret| getExpressionType(allocator, compInfo, ret),
        .FuncDec => |name| {
            const dec = compInfo.getFunction(name).?;
            return .{ .Function = dec };
        },
        .StaticStructInstance => |inst| .{ .StaticStructInstance = inst },
        .Type => |t| t,
        .Value => |val| switch (val) {
            .String => .String,
            .Bool => .Bool,
            .Char => .Char,
            .Number => |num| .{ .Number = num.type },
            .StaticArray => |arr| a: {
                const buf = try std.fmt.allocPrint(allocator, "{}", .{arr.len});

                break :a .{
                    .StaticArray = .{
                        .type = try create(blitzAst.AstTypes, allocator, try inferStaticArrType(allocator, compInfo, arr)),
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
        },
        .Cast => |cast| {
            const nodeType = try getExpressionType(allocator, compInfo, cast.node);
            defer free.freeStackType(allocator, &nodeType);

            if (isPrimary(nodeType) and isPrimary(cast.toType.*)) {
                return cast.toType.*;
            }

            return ScanError.InvalidCast;
        },
        .Variable => |v| {
            if (compInfo.isInStructMethod() and string.compString(v.name, "self")) {
                return .{
                    .StaticStructInstance = v.name,
                };
            }

            const varType = compInfo.getVariableType(v.name);
            if (varType) |astType| {
                return try clone.cloneAstTypes(allocator, astType);
            }

            return ScanError.UndefinedOrUnknownVariableType;
        },
        .StructInit => |init| {
            return .{
                .Custom = .{
                    .generics = init.generics,
                    .name = try string.cloneString(allocator, init.name),
                },
            };
        },
        .FuncCall => |call| {
            try scanNode(allocator, compInfo, expr);

            const dec = try getExpressionType(allocator, compInfo, call.func);
            defer free.freeStackType(allocator, &dec);

            return try clone.cloneAstTypes(allocator, dec.Function.returnType.*);
        },
        .Bang => |node| {
            const nodeType = try getExpressionType(allocator, compInfo, node);
            defer free.freeStackType(allocator, &nodeType);

            if (!try matchTypes(allocator, compInfo, nodeType, .Bool)) {
                return ScanError.ExpectedBooleanBang;
            }

            return .Bool;
        },
        .PropertyAccess => |access| {
            const source = try getExpressionType(allocator, compInfo, access.value);
            defer free.freeStackType(allocator, &source);
            return try getPropertyType(allocator, compInfo, source, access.property);
        },
    };
}

fn getPropertyType(allocator: Allocator, compInfo: *CompInfo, source: blitzAst.AstTypes, prop: []u8) !blitzAst.AstTypes {
    return switch (source) {
        .StaticStructInstance => |inst| try getStructPropType(compInfo, false, inst, prop),
        .StaticArray => try builtins.getStaticArrayPropTypes(allocator, prop),
        .String => try builtins.getStringPropTypes(allocator, prop),
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

            switch (attr.attr) {
                .Member => |member| return try clone.cloneAstTypes(allocator, member.*),
                .Function => |func| return .{ .Function = func },
            }
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

fn inferStaticArrType(allocator: Allocator, compInfo: *CompInfo, arr: []*const blitzAst.AstNode) !blitzAst.AstTypes {
    if (arr.len == 0) return .Void;

    const item0Type = try getExpressionType(allocator, compInfo, arr[0]);

    for (arr[1..]) |item| {
        const exprType = try getExpressionType(allocator, compInfo, item);
        defer free.freeStackType(allocator, &exprType);

        if (!try matchTypes(allocator, compInfo, exprType, item0Type)) {
            return ScanError.StaticArrayTypeMismatch;
        }
    }

    return item0Type;
}
