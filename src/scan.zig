const std = @import("std");
const astMod = @import("ast.zig");
const utils = @import("utils.zig");
const free = @import("free.zig");
const Allocator = std.mem.Allocator;
const Ast = astMod.Ast;
const AstNode = astMod.AstNode;
const AstTypes = astMod.AstTypes;
const StructAttribute = astMod.StructAttribute;
const CompInfo = utils.CompInfo;
const AstNumberVariants = astMod.AstNumberVariants;
const ArrayList = std.ArrayList;
const numberLength = utils.numberLength;
const create = utils.create;
const toSlice = utils.toSlice;
const Case = std.fmt.Case;
const freeStackType = free.freeStackType;

// debug
const debug = @import("debug.zig");
const printNode = debug.printNode;
const printType = debug.printType;

const ScanError = error{
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
};

pub fn typeScan(allocator: Allocator, ast: Ast, compInfo: *CompInfo) !void {
    try scanNodes(allocator, compInfo, ast.root.nodes);
}

fn scanNodes(allocator: Allocator, compInfo: *CompInfo, nodes: []*const AstNode) (ScanError || Allocator.Error)!void {
    for (nodes) |node| {
        try scanNode(allocator, compInfo, node);
    }
}

fn scanNode(allocator: Allocator, compInfo: *CompInfo, node: *const AstNode) (Allocator.Error || ScanError)!void {
    switch (node.*) {
        .NoOp, .Type, .Value, .Cast, .ReturnNode => {},

        .Seq => |seq| {
            try scanNodes(allocator, compInfo, seq.nodes);
        },
        .VarDec => |dec| {
            if (compInfo.getVariableType(dec.name) != null) {
                return ScanError.VariableAlreadyExists;
            }

            try scanNode(allocator, compInfo, dec.setNode);

            const setType = try getExpressionType(allocator, compInfo, dec.setNode);

            if (setType == AstTypes.Void) {
                return ScanError.VoidVariableDec;
            }

            const setPtr = try create(AstTypes, allocator, setType);

            if (dec.annotation) |annotation| {
                if (try matchTypes(allocator, compInfo, annotation.*, setType)) {
                    try compInfo.setVariableType(dec.name, setPtr);
                } else {
                    return ScanError.VariableAnnotationMismatch;
                }
            } else {
                try compInfo.setVariableType(dec.name, setPtr);
            }
        },
        .Variable => |v| {
            const varType = compInfo.getVariableType(v.name);
            if (varType == null) return ScanError.UndefinedOrUnknownVariableType;
        },
        .StructDec => |dec| {
            for (dec.generics) |generic| {
                try compInfo.addGeneric(generic.name);
            }

            try scanAttributes(allocator, compInfo, dec.attributes);

            for (dec.generics) |generic| {
                compInfo.removeGeneric(generic.name);
            }
        },
        .IfStatement => |statement| {
            const conditionType = try getExpressionType(allocator, compInfo, statement.condition);
            defer freeStackType(allocator, &conditionType);
            if (conditionType != AstTypes.Bool) return ScanError.ExpectedBooleanIfCondition;
        },
        .FuncDec => |dec| {
            switch (dec.body.*) {
                .Seq => |seq| {
                    if (seq.nodes.len == 0) {
                        if (dec.returnType.* == AstTypes.Void) {
                            return;
                        } else {
                            return ScanError.ExpectedFunctionReturn;
                        }
                    }

                    const last = seq.nodes[seq.nodes.len - 1];
                    const lastType = try getExpressionType(allocator, compInfo, last);
                    defer freeStackType(allocator, &lastType);
                    if (last.* == AstNode.ReturnNode or dec.returnType.* != AstTypes.Void) {
                        if (!try matchTypes(allocator, compInfo, dec.returnType.*, lastType)) {
                            return ScanError.FunctionReturnTypeMismatch;
                        }
                    }
                },
                .ReturnNode => |ret| {
                    const retType = try getExpressionType(allocator, compInfo, ret);
                    defer freeStackType(allocator, &retType);
                    if (!try matchTypes(allocator, compInfo, dec.returnType.*, retType)) {
                        return ScanError.FunctionReturnTypeMismatch;
                    }
                },
                else => if (dec.returnType.* != AstTypes.Void) return ScanError.FunctionReturnTypeMismatch,
            }
        },
        .FuncCall => |call| {
            if (call.func.params.len != call.params.len) {
                return ScanError.FunctionCallParamCountMismatch;
            }

            for (call.func.params, 0..) |param, index| {
                const paramType = try getExpressionType(allocator, compInfo, call.params[index]);
                if (!try matchTypes(allocator, compInfo, param.type.*, paramType)) {
                    return ScanError.FunctionCallParamTypeMismatch;
                }
            }
        },
        // TODO
        .StructInit => {},
        .Bang => |bang| {
            const bangType = try getExpressionType(allocator, compInfo, bang);
            defer freeStackType(allocator, &bangType);
            if (bangType != AstTypes.Bool) return ScanError.ExpectedBooleanBang;
        },
    }
}

fn scanAttributes(allocator: Allocator, compInfo: *CompInfo, attrs: []StructAttribute) !void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => |member| {
                const varType = try create(AstTypes, allocator, member.type.*);
                try compInfo.setVariableType(member.name, varType);
            },
            .Function => |function| {
                try scanNode(allocator, compInfo, function.func.body);
            },
        }
    }
}

fn matchNumber(num1: AstNumberVariants, num2: AstNumberVariants) bool {
    return switch (num1) {
        .U8 => num2 == AstNumberVariants.U8,
        .U16 => num2 == AstNumberVariants.U16,
        .U32 => num2 == AstNumberVariants.U32,
        .U64 => num2 == AstNumberVariants.U64,
        .U128 => num2 == AstNumberVariants.U128,
        .I8 => num2 == AstNumberVariants.I8,
        .I16 => num2 == AstNumberVariants.I16,
        .I32 => num2 == AstNumberVariants.I32,
        .I64 => num2 == AstNumberVariants.I64,
        .I128 => num2 == AstNumberVariants.I128,
        .F8 => num2 == AstNumberVariants.F8,
        .F16 => num2 == AstNumberVariants.F16,
        .F32 => num2 == AstNumberVariants.F32,
        .F64 => num2 == AstNumberVariants.F64,
        .F128 => num2 == AstNumberVariants.F128,
        .USize => num2 == AstNumberVariants.USize,
    };
}

fn isNumber(astType: AstTypes) bool {
    return switch (astType) {
        .Number => true,
        else => false,
    };
}

fn isUnsignedInt(astType: AstTypes) bool {
    return switch (astType) {
        .Number => |num| switch (num) {
            .U8, .U16, .U32, .U64, .U128, .USize => true,
            else => false,
        },
        else => false,
    };
}

fn isInt(astType: AstTypes) bool {
    return switch (astType) {
        .Number => |num| switch (num) {
            .U8, .U16, .U32, .U64, .U128, .USize => true,
            .I8, .I16, .I32, .I64, .I128 => true,
            else => false,
        },
        else => false,
    };
}

fn isFloat(astType: AstTypes) bool {
    return switch (astType) {
        .Number => |num| switch (num) {
            .F8, .F16, .F32, .F64, .F128 => true,
            else => false,
        },
        else => false,
    };
}

fn matchTypes(allocator: Allocator, compInfo: *CompInfo, type1: AstTypes, type2: AstTypes) !bool {
    switch (type1) {
        .String => return type2 == AstTypes.String,
        .Bool => return type2 == AstTypes.Bool,
        .Char => return type2 == AstTypes.Char,
        .Void => return type2 == AstTypes.Void,
        .Number => |num| return type2 == AstTypes.Number and matchNumber(num, type2.Number),
        .DynamicArray => |arr| return type2 == AstTypes.DynamicArray and try matchTypes(allocator, compInfo, arr.*, type2.DynamicArray.*),
        .StaticArray => |arr| {
            if (type2 != AstTypes.StaticArray) {
                return false;
            }

            const sizeType1 = try getExpressionType(allocator, compInfo, arr.size);
            const sizeType2 = try getExpressionType(allocator, compInfo, type2.StaticArray.size);
            defer freeStackType(allocator, &sizeType1);
            defer freeStackType(allocator, &sizeType2);

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

fn isPrimary(astType: AstTypes) bool {
    return switch (astType) {
        .String, .Bool, .Char, .Number => true,
        else => false,
    };
}

fn getExpressionType(allocator: Allocator, compInfo: *CompInfo, expr: *const AstNode) (ScanError || Allocator.Error)!AstTypes {
    return switch (expr.*) {
        .NoOp => AstTypes.Void,
        .Seq => AstTypes.Void,
        .VarDec => AstTypes.Void,
        .StructDec => AstTypes.Void,
        .IfStatement => AstTypes.Void,
        .ReturnNode => |ret| getExpressionType(allocator, compInfo, ret),
        .FuncDec => |func| AstTypes{ .Function = func },

        .Type => |t| t,
        .Value => |val| switch (val) {
            .String => AstTypes.String,
            .Bool => AstTypes.Bool,
            .Char => AstTypes.Char,
            .Number => |num| AstTypes{ .Number = num.type },
            .StaticArray => |arr| a: {
                const buf = try std.fmt.allocPrint(allocator, "{}", .{arr.len});

                break :a AstTypes{
                    .StaticArray = .{
                        .type = try create(AstTypes, allocator, try inferStaticArrType(allocator, compInfo, arr)),
                        .size = try create(AstNode, allocator, .{
                            .Value = .{
                                .Number = .{
                                    .type = AstNumberVariants.USize,
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
            defer freeStackType(allocator, &nodeType);

            if (isPrimary(nodeType) and isPrimary(cast.toType.*)) {
                return cast.toType.*;
            }

            return ScanError.InvalidCast;
        },
        .Variable => |v| {
            const varType = compInfo.getVariableType(v.name);
            if (varType) |astType| {
                return astType;
            }

            return ScanError.UndefinedOrUnknownVariableType;
        },
        // TODO
        .StructInit => AstTypes.Void,
        .FuncCall => |call| call.func.returnType.*,
        .Bang => |node| {
            const nodeType = try getExpressionType(allocator, compInfo, node);
            defer freeStackType(allocator, &nodeType);

            if (!try matchTypes(allocator, compInfo, nodeType, AstTypes.Bool)) {
                return ScanError.ExpectedBooleanBang;
            }

            return AstTypes.Bool;
        },
    };
}

fn inferStaticArrType(allocator: Allocator, compInfo: *CompInfo, arr: []*const AstNode) !AstTypes {
    if (arr.len == 0) return AstTypes.Void;

    const item0Type = try getExpressionType(allocator, compInfo, arr[0]);

    for (arr[1..]) |item| {
        const exprType = try getExpressionType(allocator, compInfo, item);
        defer freeStackType(allocator, &exprType);

        if (!try matchTypes(allocator, compInfo, exprType, item0Type)) {
            return ScanError.StaticArrayTypeMismatch;
        }
    }

    return item0Type;
}
