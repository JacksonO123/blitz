const std = @import("std");
const astUtils = @import("ast.zig");
const utils = @import("utils.zig");
const Ast = astUtils.Ast;
const AstNode = astUtils.AstNode;
const AstTypes = astUtils.AstTypes;
const AstValues = astUtils.AstValues;
const AstNumberVariants = astUtils.AstNumberVariants;
const GenericType = astUtils.GenericType;
const Parameter = astUtils.Parameter;
const StructAttribute = astUtils.StructAttribute;
const FuncDecNode = astUtils.FuncDecNode;
const StructDecNode = astUtils.StructDecNode;
const CompInfo = utils.CompInfo;

pub fn printAst(compInfo: *CompInfo, ast: Ast) void {
    printNodes(compInfo, ast.root.nodes);
}

pub fn printStructNames(names: [][]u8) void {
    std.debug.print("------------\n", .{});
    std.debug.print("structs:\n", .{});
    for (names) |name| {
        std.debug.print("{s}\n", .{name});
    }
    std.debug.print("------------\n", .{});
}

pub fn printType(compInfo: *CompInfo, typeNode: *const AstTypes) void {
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
        .DynamicArray => |arr| {
            std.debug.print("DynamicArray<", .{});
            printType(compInfo, arr);
            std.debug.print(">", .{});
        },
        .StaticArray => |*arr| {
            std.debug.print("StaticArray<", .{});
            printNode(compInfo, arr.size);
            std.debug.print(", ", .{});
            printType(compInfo, arr.type);
            std.debug.print(">", .{});
        },
        .Nullable => |n| {
            std.debug.print("?", .{});
            printType(compInfo, n);
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
                printType(compInfo, generic);

                if (index < custom.generics.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            if (custom.generics.len > 0) {
                std.debug.print(">", .{});
            }
        },
        .Generic => |gen| {
            std.debug.print("[generic]({s})", .{gen});
        },
        .Function => |func| {
            std.debug.print("[function](\"{s}\"", .{func.name});

            if (func.generics) |generics| {
                printGenerics(compInfo, generics);
            }

            std.debug.print(" (", .{});

            for (func.params, 0..) |param, index| {
                std.debug.print("({s})[", .{param.name});
                printType(compInfo, param.type);
                std.debug.print("]", .{});

                if (index < func.params.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print(" ", .{});
            printType(compInfo, func.returnType);

            std.debug.print(")", .{});
        },
        .StaticStructInstance => |inst| {
            std.debug.print("[static struct instance]({s})", .{inst});
        },
    };
}

fn numberTypeToString(numType: AstNumberVariants) [*:0]const u8 {
    return switch (numType) {
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
        .F8 => "f8",
        .F16 => "f16",
        .F32 => "f32",
        .F64 => "f64",
        .F128 => "f128",
        .USize => "usize",
    };
}

fn printValue(compInfo: *CompInfo, value: *const AstValues) void {
    switch (value.*) {
        .StaticArray => |arr| {
            std.debug.print("([", .{});

            for (arr, 0..) |val, index| {
                printNode(compInfo, val);

                if (index < arr.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print("])", .{});
        },
        .Number => |*n| {
            std.debug.print("[{s}]({s})", .{ numberTypeToString(n.type), n.value });
        },
        .String => |s| {
            std.debug.print("[string](\"{s}\")", .{s});
        },
        .Char => |c| {
            std.debug.print("[char]({c})", .{c});
        },
        .Bool => |b| {
            std.debug.print("[bool]({s})", .{if (b) "true" else "false"});
        },
    }
}

pub fn printNode(compInfo: *CompInfo, node: *const AstNode) void {
    switch (node.*) {
        .Add => |op| {
            std.debug.print("(", .{});
            printNode(compInfo, op.left);
            std.debug.print(") +ADD+ (", .{});
            printNode(compInfo, op.right);
            std.debug.print(")", .{});
        },
        .Sub => |op| {
            std.debug.print("(", .{});
            printNode(compInfo, op.left);
            std.debug.print(") -SUB- (", .{});
            printNode(compInfo, op.right);
            std.debug.print(")", .{});
        },
        .Mult => |op| {
            std.debug.print("(", .{});
            printNode(compInfo, op.left);
            std.debug.print(") *MULT* (", .{});
            printNode(compInfo, op.right);
            std.debug.print(")", .{});
        },
        .Div => |op| {
            std.debug.print("(", .{});
            printNode(compInfo, op.left);
            std.debug.print(") /DIV/ (", .{});
            printNode(compInfo, op.right);
            std.debug.print(")", .{});
        },
        .FuncReference => |ref| {
            std.debug.print("function ({s})", .{ref});
        },
        .StaticStructInstance => |inst| {
            std.debug.print("static struct ({s})", .{inst});
        },
        .PropertyAccess => |access| {
            std.debug.print("accessing {s} from ", .{access.property});
            printNode(compInfo, access.value);
        },
        .VarDec => |*dec| {
            std.debug.print("declare ({s}) ({s}) = ", .{
                if (dec.isConst) "const" else "mutable",
                dec.name,
            });

            printNode(compInfo, dec.setNode);

            if (dec.annotation != null) {
                std.debug.print(" with annotation: ", .{});
                printType(compInfo, dec.annotation.?);
            }
        },
        .Value => |*val| {
            printValue(compInfo, val);
        },
        .Type => |*t| {
            printType(compInfo, t);
        },
        .Seq => |*seq| {
            if (seq.nodes.len == 0) {
                std.debug.print("(empty seq)", .{});
            } else {
                printNodes(compInfo, seq.nodes);
            }
        },
        .Cast => |*cast| {
            std.debug.print("cast ", .{});
            printNode(compInfo, cast.node);
            std.debug.print(" to ", .{});
            printType(compInfo, cast.toType);
        },
        .Variable => |*variable| {
            std.debug.print("[variable: ({s})]", .{variable.name});
        },
        .StructDec => |dec| {
            std.debug.print("declare struct ({s})", .{dec.name});

            if (dec.generics.len > 0) {
                std.debug.print(" with generics [", .{});
                printGenerics(compInfo, dec.generics);
                std.debug.print("]", .{});
            }

            if (dec.attributes.len > 0) {
                std.debug.print(" with attributes [", .{});
                printAttributes(compInfo, dec.attributes);
                std.debug.print("]", .{});
            }
        },
        .IfStatement => |*statement| {
            std.debug.print("if ", .{});
            printNode(compInfo, statement.condition);
            std.debug.print(" then -- body --\n", .{});
            printNode(compInfo, statement.body);
            std.debug.print("-- body end --\n", .{});
        },
        .NoOp => {
            std.debug.print("(noop)", .{});
        },
        .FuncDec => |name| {
            const dec = compInfo.getFunction(name).?;
            printFuncDec(compInfo, dec);
        },
        .FuncCall => |call| {
            std.debug.print("calling ", .{});
            printNode(compInfo, call.func);
            std.debug.print(" with params [", .{});

            for (call.params, 0..) |param, index| {
                printNode(compInfo, param);
                if (index < call.params.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print("]", .{});
        },
        .ReturnNode => |ret| {
            std.debug.print("return ", .{});
            printNode(compInfo, ret);
        },
        .StructInit => |init| {
            std.debug.print("initializing ({s})[", .{init.name});

            for (init.generics, 0..) |generic, index| {
                printType(compInfo, generic);

                if (index < init.generics.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print("] with {{", .{});

            for (init.attributes, 0..) |attr, index| {
                std.debug.print("{s}: ", .{attr.name});
                printNode(compInfo, attr.value);
                if (index < init.attributes.len - 1) {
                    std.debug.print(", ", .{});
                }
            }
            std.debug.print("}}", .{});
        },
        .Bang => |bang| {
            std.debug.print("[bang]!", .{});
            printNode(compInfo, bang);
        },
    }
}

pub fn printFuncDec(compInfo: *CompInfo, func: *const FuncDecNode) void {
    std.debug.print("declare function [", .{});
    printType(compInfo, func.returnType);
    std.debug.print("] ({s})", .{func.name});
    if (func.generics) |generics| {
        std.debug.print(" with generics [", .{});
        printGenerics(compInfo, generics);
        std.debug.print("]", .{});
    }

    std.debug.print(" with params [", .{});
    printParams(compInfo, func.params);

    std.debug.print("] -- body --\n", .{});
    printNode(compInfo, func.body);
    std.debug.print("-- body end --\n", .{});
}

fn printAttributes(compInfo: *CompInfo, attrs: []StructAttribute) void {
    for (attrs, 0..) |attr, index| {
        if (attr.static) {
            std.debug.print("(static) ", .{});
        }

        std.debug.print("{s} ({s}) ", .{ attr.visibility.toString(), attr.name });

        switch (attr.attr) {
            .Function => |func| printFuncDec(compInfo, func),
            .Member => |mem| printType(compInfo, mem),
        }

        if (index < attrs.len - 1) {
            std.debug.print(", ", .{});
        }
    }
}

fn printParams(compInfo: *CompInfo, params: []Parameter) void {
    if (params.len == 0) {
        std.debug.print("(no params)", .{});
        return;
    }

    for (params, 0..) |param, index| {
        std.debug.print("[", .{});
        printType(compInfo, param.type);
        std.debug.print("]({s})", .{param.name});

        if (index < params.len - 1) {
            std.debug.print(", ", .{});
        }
    }
}

fn printGenerics(compInfo: *CompInfo, generics: []GenericType) void {
    for (generics, 0..) |generic, index| {
        std.debug.print("[", .{});

        if (generic.restriction) |restriction| {
            printType(compInfo, restriction);
        } else {
            std.debug.print("any", .{});
        }

        std.debug.print("]({s})", .{generic.name});

        if (index < generics.len - 1) {
            std.debug.print(", ", .{});
        }
    }
}

fn printNodes(compInfo: *CompInfo, nodes: []*const AstNode) void {
    for (nodes) |node| {
        printNode(compInfo, node);
        std.debug.print("\n", .{});
    }
}

pub fn printRegisteredStructs(compInfo: *CompInfo, structs: [](*const StructDecNode)) void {
    std.debug.print("--- structs ---\n", .{});
    for (structs) |s| {
        std.debug.print("declaring {s}", .{s.name});

        if (s.deriveType) |derived| {
            std.debug.print(" extending ", .{});
            printType(compInfo, derived);
        }

        if (s.generics.len > 0) {
            std.debug.print(" with generics [", .{});
            printGenerics(compInfo, s.generics);
            std.debug.print("]", .{});
        }

        std.debug.print(" with attributes [", .{});
        printAttributes(compInfo, s.attributes);
        std.debug.print("]", .{});

        std.debug.print("\n", .{});
    }
}

pub fn printTokens(tokens: anytype) void {
    for (tokens) |token| {
        std.debug.print("{any}", .{token.type});
        if (token.string != null) {
            std.debug.print(" : {s}\n", .{token.string.?});
        } else {
            std.debug.print("\n", .{});
        }
    }
}
