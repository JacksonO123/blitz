const std = @import("std");
const astUtils = @import("ast.zig");
const Ast = astUtils.Ast;
const AstNode = astUtils.AstNode;
const AstTypes = astUtils.AstTypes;
const AstValues = astUtils.AstValues;
const AstNumberVariants = astUtils.AstNumberVariants;
const RegisteredStruct = astUtils.RegisteredStruct;
const GenericType = astUtils.GenericType;
const Parameter = astUtils.Parameter;
const StructAttribute = astUtils.StructAttribute;
const FuncDecNode = astUtils.FuncDecNode;

pub fn printAst(ast: Ast) void {
    printNodes(ast.root.nodes);
}

fn printType(typeNode: *const AstTypes) void {
    return switch (typeNode.*) {
        .Void => {
            std.debug.print("void", .{});
        },
        .String => {
            std.debug.print("string", .{});
        },
        .Char => {
            std.debug.print("char", .{});
        },
        .Bool => {
            std.debug.print("bool", .{});
        },
        .DynamicArray => |*arr| {
            std.debug.print("DynamicArray<", .{});
            printType(arr.type);
            std.debug.print(">", .{});
        },
        .StaticArray => |*arr| {
            std.debug.print("StaticArray<", .{});
            printNode(arr.size);
            std.debug.print(", ", .{});
            printType(arr.type);
            std.debug.print(">", .{});
        },
        .Nullable => |n| {
            std.debug.print("?", .{});
            printType(n);
        },
        .Number => |*num| {
            std.debug.print("{s}", .{numberTypeToString(num.*)});
        },
        .Custom => |*custom| {
            std.debug.print("{s}", .{custom.name});
            if (custom.generics.len > 0) {
                std.debug.print("<", .{});
            }

            for (custom.generics, 0..) |generic, index| {
                printType(generic);

                if (index < custom.generics.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            if (custom.generics.len > 0) {
                std.debug.print(">", .{});
            }
        },
        .Union => |typeUnion| {
            std.debug.print("(", .{});

            for (typeUnion, 0..) |t, index| {
                printType(t);

                if (index < typeUnion.len - 1) {
                    std.debug.print(" | ", .{});
                }
            }

            std.debug.print(")", .{});
        },
    };
}

fn numberTypeToString(numType: AstNumberVariants) [*:0]const u8 {
    return switch (numType) {
        .U16 => "u16",
        .U32 => "u32",
        .U64 => "u64",
        .U128 => "u128",
        .I16 => "i16",
        .I32 => "i32",
        .I64 => "i64",
        .I128 => "i128",
        .F16 => "f16",
        .F32 => "f32",
        .F64 => "f64",
        .F128 => "f128",
    };
}

fn printValue(value: *const AstValues) void {
    switch (value.*) {
        .StaticArray => |arr| {
            std.debug.print("([", .{});

            for (arr, 0..) |val, index| {
                printNode(val);

                if (index < arr.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print("])", .{});
        },
        .Number => |*n| {
            std.debug.print("[{s}]({s})", .{ numberTypeToString(n.type), n.value });
        },
        .String => |*s| {
            std.debug.print("[string](\"{s}\")", .{s});
        },
        .Char => |*c| {
            std.debug.print("[char]({c})", .{c});
        },
        .Bool => |b| {
            std.debug.print("[bool]({s})", .{if (b) "true" else "false"});
        },
    }
}

fn printNode(node: *const AstNode) void {
    switch (node.*) {
        .VarDec => |*dec| {
            std.debug.print("declare ({s}) ({s}) = ", .{
                if (dec.isConst) "const" else "mutable",
                dec.name,
            });

            printNode(dec.setNode);

            if (dec.annotation != null) {
                std.debug.print(" with annotation: ", .{});
                printType(dec.annotation.?);
            }
        },
        .Value => |*val| {
            printValue(val);
        },
        .Type => |*t| {
            printType(t);
        },
        .Seq => |*seq| {
            if (seq.nodes.len == 0) {
                std.debug.print("(empty seq)", .{});
            } else {
                printNodes(seq.nodes);
            }
        },
        .Cast => |*cast| {
            std.debug.print("cast ", .{});
            printNode(cast.node);
            std.debug.print(" to ", .{});
            printType(cast.toType);
        },
        .Variable => |*variable| {
            std.debug.print("[variable: ({s})]", .{variable.name});
        },
        .StructDec => |*dec| {
            std.debug.print("declare struct ({s})", .{dec.name});

            if (dec.generics.len > 0) {
                std.debug.print(" with generics [", .{});
                printGenerics(dec.generics);
                std.debug.print("]", .{});
            }

            if (dec.attributes.len > 0) {
                std.debug.print(" with attributes [", .{});
                printAttributes(dec.attributes);
                std.debug.print("]", .{});
            }
        },
        .IfStatement => |*statement| {
            std.debug.print("if ", .{});
            printNode(statement.condition);
            std.debug.print(" then -- body --\n", .{});
            printNode(statement.body);
            std.debug.print("-- body end --\n", .{});
        },
        .NoOp => {
            std.debug.print("(noop)", .{});
        },
        .FuncDec => |func| {
            printFuncDec(func);
        },
    }
}

fn printFuncDec(func: FuncDecNode) void {
    std.debug.print("declare function [", .{});
    printType(func.returnType);
    std.debug.print("] ({s})", .{func.name});
    if (func.generics) |generics| {
        std.debug.print(" with generics [", .{});
        printGenerics(generics);
        std.debug.print("]", .{});
    }

    std.debug.print(" with params [", .{});
    printParams(func.params);

    std.debug.print("] -- body --\n", .{});
    printNode(func.body);
    std.debug.print("-- body end --\n", .{});
}

fn printAttributes(attrs: []StructAttribute) void {
    for (attrs) |attr| {
        switch (attr) {
            .Function => {
                std.debug.print("{s} ({s}) ", .{ attr.Function.visibility.toString(), attr.Function.name });
                printFuncDec(attr.Function.func);
            },
            .Member => {
                std.debug.print("{s} ({s}) with type ", .{ attr.Member.visibility.toString(), attr.Member.name });
                printType(attr.Member.type);
            },
        }
    }
}

fn printParams(params: []Parameter) void {
    if (params.len == 0) {
        std.debug.print("(no params)", .{});
        return;
    }

    for (params, 0..) |param, index| {
        std.debug.print("[", .{});
        printType(param.type);
        std.debug.print("]({s})", .{param.name});

        if (index < params.len - 1) {
            std.debug.print(", ", .{});
        }
    }
}

fn printGenerics(generics: []GenericType) void {
    for (generics, 0..) |generic, index| {
        std.debug.print("[", .{});

        if (generic.restriction) |restriction| {
            printType(restriction);
        } else {
            std.debug.print("any", .{});
        }

        std.debug.print("]({s})", .{generic.name});

        if (index < generics.len - 1) {
            std.debug.print(", ", .{});
        }
    }
}

fn printNodes(nodes: []*const AstNode) void {
    for (nodes) |node| {
        printNode(node);
        std.debug.print("\n", .{});
    }
}

pub fn printRegisteredStructs(structs: []RegisteredStruct) void {
    std.debug.print("--- structs ---\n", .{});
    for (structs) |s| {
        std.debug.print("{s}{s}\n", .{ s.name, if (s.numGenerics > 0) " : (generic)" else "" });
    }
}
