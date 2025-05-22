const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const CompInfo = utils.CompInfo;
const print = std.debug.print;

pub fn printAst(compInfo: *CompInfo, ast: blitzAst.Ast) void {
    printNode(compInfo, ast.root);
}

pub fn printStructAndErrorNames(names: blitzAst.StructAndErrorNames) void {
    print("------------\n", .{});
    print("structs:\n", .{});
    for (names.structNames) |name| {
        print("{s}\n", .{name});
    }

    print("errors:\n", .{});
    for (names.errorNames) |name| {
        print("{s}\n", .{name});
    }
    print("------------\n", .{});
}

pub fn printType(compInfo: *CompInfo, typeNode: *const blitzAst.AstTypes) void {
    return switch (typeNode.*) {
        .Void => {
            print("void", .{});
        },
        .String => {
            print("string", .{});
        },
        .Char => {
            print("char", .{});
        },
        .Bool => {
            print("bool", .{});
        },
        .DynamicArray => |arr| {
            print("DynamicArray<", .{});
            printType(compInfo, arr);
            print(">", .{});
        },
        .StaticArray => |*arr| {
            print("StaticArray<", .{});
            printNode(compInfo, arr.size);
            print(", ", .{});
            printType(compInfo, arr.type);
            print(">", .{});
        },
        .Nullable => |n| {
            print("?", .{});
            printType(compInfo, n);
        },
        .Number => |*num| {
            print("{s}", .{numberTypeToString(num.*)});
        },
        .Custom => |*custom| {
            print("{s}", .{custom.name});
            if (custom.generics.len > 0) {
                print("<", .{});
            }

            for (custom.generics, 0..) |generic, index| {
                printType(compInfo, generic);

                if (index < custom.generics.len - 1) {
                    print(", ", .{});
                }
            }

            if (custom.generics.len > 0) {
                print(">", .{});
            }
        },
        .Generic => |gen| {
            print("[generic]({s})", .{gen});
        },
        .Function => |func| {
            print("[function](\"{s}\"", .{func.name});

            if (func.generics) |generics| {
                printGenerics(compInfo, generics);
            }

            print(" (", .{});

            for (func.params, 0..) |param, index| {
                print("({s})[", .{param.name});
                printType(compInfo, param.type);
                print("]", .{});

                if (index < func.params.len - 1) {
                    print(", ", .{});
                }
            }

            print(" ", .{});
            printType(compInfo, func.returnType);

            print(")", .{});
        },
        .StaticStructInstance => |inst| {
            print("[static struct instance]({s})", .{inst});
        },
        .Error => |err| {
            print("error ({s})", .{err});
        },
        .ErrorVariant => |err| {
            print("variant [{s}] from ({s})", .{ err.variant, err.from });
        },
    };
}

fn numberTypeToString(numType: blitzAst.AstNumberVariants) [*:0]const u8 {
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

fn printValue(compInfo: *CompInfo, value: *const blitzAst.AstValues) void {
    switch (value.*) {
        .StaticArray => |arr| {
            print("([", .{});

            for (arr, 0..) |val, index| {
                printNode(compInfo, val);

                if (index < arr.len - 1) {
                    print(", ", .{});
                }
            }

            print("])", .{});
        },
        .Number => |*n| {
            print("[{s}]({s})", .{ numberTypeToString(n.type), n.value });
        },
        .String => |s| {
            print("[string](\"{s}\")", .{s});
        },
        .Char => |c| {
            print("[char]({c})", .{c});
        },
        .Bool => |b| {
            print("[bool]({s})", .{if (b) "true" else "false"});
        },
    }
}

pub fn printNode(compInfo: *CompInfo, node: *const blitzAst.AstNode) void {
    switch (node.*) {
        .IndexValue => |index| {
            print("indexing ", .{});
            printNode(compInfo, index.value);
            print(" with ", .{});
            printNode(compInfo, index.index);
        },
        .OpExpr => |op| {
            print("(", .{});
            printNode(compInfo, op.left);
            print(") ", .{});

            switch (op.type) {
                .BitAnd => print("&BitAnd&", .{}),
                .BitOr => print("|BitOr|", .{}),
                .And => print("&&AND&&", .{}),
                .Or => print("||OR||", .{}),
                .Add => print("+ADD+", .{}),
                .Sub => print("-SUB-", .{}),
                .Mult => print("*MULT*", .{}),
                .Div => print("/DIV/", .{}),
            }

            print(" (", .{});
            printNode(compInfo, op.right);
            print(")", .{});
        },
        .FuncReference => |ref| {
            print("function ({s})", .{ref});
        },
        .StaticStructInstance => |inst| {
            print("static struct ({s})", .{inst});
        },
        .PropertyAccess => |access| {
            print("accessing {s} from ", .{access.property});
            printNode(compInfo, access.value);
        },
        .VarDec => |*dec| {
            print("declare ({s}) ({s}) = ", .{
                if (dec.isConst) "const" else "mutable",
                dec.name,
            });

            printNode(compInfo, dec.setNode);

            if (dec.annotation != null) {
                print(" with annotation: ", .{});
                printType(compInfo, dec.annotation.?);
            }
        },
        .VarSet => |set| {
            print("set {s} to ", .{set.variable});
            printNode(compInfo, set.setNode);
        },
        .Value => |*val| {
            printValue(compInfo, val);
        },
        .Type => |*t| {
            printType(compInfo, t);
        },
        .Seq => |*seq| {
            if (seq.nodes.len == 0) {
                print("(empty seq)", .{});
            } else {
                printNodes(compInfo, seq.nodes);
            }
        },
        .Cast => |*cast| {
            print("cast ", .{});
            printNode(compInfo, cast.node);
            print(" to ", .{});
            printType(compInfo, cast.toType);
        },
        .Variable => |*variable| {
            print("[variable: ({s})]", .{variable});
        },
        .StructDec => |dec| {
            print("declare struct ({s})", .{dec.name});

            if (dec.generics.len > 0) {
                print(" with generics [", .{});
                printGenerics(compInfo, dec.generics);
                print("]", .{});
            }

            if (dec.attributes.len > 0) {
                print(" with attributes [", .{});
                printAttributes(compInfo, dec.attributes);
                print("]", .{});
            }
        },
        .IfStatement => |*statement| {
            print("if ", .{});
            printNode(compInfo, statement.condition);
            print(" then -- body --\n", .{});
            printNode(compInfo, statement.body);
            print("-- body end --\n", .{});
        },
        .NoOp => {
            print("(noop)", .{});
        },
        .FuncDec => |name| {
            const dec = compInfo.getFunction(name).?;
            printFuncDec(compInfo, dec);
        },
        .FuncCall => |call| {
            print("calling ", .{});
            printNode(compInfo, call.func);
            print(" with params [", .{});

            for (call.params, 0..) |param, index| {
                printNode(compInfo, param);
                if (index < call.params.len - 1) {
                    print(", ", .{});
                }
            }

            print("]", .{});
        },
        .ReturnNode => |ret| {
            print("return ", .{});
            printNode(compInfo, ret);
        },
        .StructInit => |init| {
            print("initializing ({s})", .{init.name});

            if (init.generics.len > 0) {
                print("[generics: ", .{});

                for (init.generics, 0..) |generic, index| {
                    printType(compInfo, generic);

                    if (index < init.generics.len - 1) {
                        print(", ", .{});
                    }
                }

                print("]", .{});
            }

            print(" with {{", .{});

            for (init.attributes, 0..) |attr, index| {
                print("{s}: ", .{attr.name});
                printNode(compInfo, attr.value);
                if (index < init.attributes.len - 1) {
                    print(", ", .{});
                }
            }
            print("}}", .{});
        },
        .Bang => |bang| {
            print("[bang]!", .{});
            printNode(compInfo, bang);
        },
        .ErrorDec => |def| printRegisteredError(def),
        .Error => |err| {
            print("error type ({s})", .{err});
        },
    }
}

pub fn printFuncDec(compInfo: *CompInfo, func: *const blitzAst.FuncDecNode) void {
    print("declare function [", .{});
    printType(compInfo, func.returnType);
    print("] ({s})", .{func.name});
    if (func.generics) |generics| {
        print(" with generics [", .{});
        printGenerics(compInfo, generics);
        print("]", .{});
    }

    print(" with params [", .{});
    printParams(compInfo, func.params);

    print("] -- body --\n", .{});
    printNode(compInfo, func.body);
    print("-- body end --\n", .{});
}

fn printAttributes(compInfo: *CompInfo, attrs: []blitzAst.StructAttribute) void {
    for (attrs, 0..) |attr, index| {
        if (attr.static) {
            print("(static) ", .{});
        }

        print("{s} ({s}) ", .{ attr.visibility.toString(), attr.name });

        switch (attr.attr) {
            .Function => |func| printFuncDec(compInfo, func),
            .Member => |mem| printType(compInfo, mem),
        }

        if (index < attrs.len - 1) {
            print(", ", .{});
        }
    }
}

fn printParams(compInfo: *CompInfo, params: []blitzAst.Parameter) void {
    if (params.len == 0) {
        print("(no params)", .{});
        return;
    }

    for (params, 0..) |param, index| {
        print("[", .{});
        printType(compInfo, param.type);
        print("]({s})", .{param.name});

        if (index < params.len - 1) {
            print(", ", .{});
        }
    }
}

pub fn printGenerics(compInfo: *CompInfo, generics: []blitzAst.GenericType) void {
    for (generics, 0..) |generic, index| {
        print("[", .{});

        if (generic.restriction) |restriction| {
            printType(compInfo, restriction);
        } else {
            print("any", .{});
        }

        print("]({s})", .{generic.name});

        if (index < generics.len - 1) {
            print(", ", .{});
        }
    }
}

fn printNodes(compInfo: *CompInfo, nodes: []*const blitzAst.AstNode) void {
    for (nodes) |node| {
        printNode(compInfo, node);
        print("\n", .{});
    }
}

fn printRegisteredError(err: *const blitzAst.ErrorDecNode) void {
    print("defining error: {s} with variants [ ", .{err.name});

    for (err.variants, 0..) |variant, index| {
        print("{s}", .{variant});
        if (index < err.variants.len - 1) {
            print(", ", .{});
        }
    }
}

pub fn printRegisteredErrors(errors: []*const blitzAst.ErrorDecNode) void {
    print("--- errors ---\n", .{});
    for (errors) |err| {
        printRegisteredError(err);
        print(" ]\n", .{});
    }
}

pub fn printRegisteredStructs(compInfo: *CompInfo, structs: [](*blitzAst.StructDecNode)) void {
    print("--- structs ---\n", .{});
    for (structs) |s| {
        print("declaring {s}", .{s.name});

        if (s.deriveType) |derived| {
            print(" extending ", .{});
            printType(compInfo, derived);
        }

        if (s.generics.len > 0) {
            print(" with generics [", .{});
            printGenerics(compInfo, s.generics);
            print("]", .{});
        }

        print(" with attributes [", .{});
        printAttributes(compInfo, s.attributes);
        print("]", .{});

        print("\n", .{});
    }
}

pub fn printToken(token: tokenizer.Token) void {
    print("{any}", .{token.type});
    if (token.string != null) {
        print(" : {s}\n", .{token.string.?});
    } else {
        print("\n", .{});
    }
}

pub fn printTokens(tokens: anytype) void {
    for (tokens) |token| {
        printToken(token);
    }
}
