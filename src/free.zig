const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;

// DEBUG
const debug = @import("debug.zig");
const printType = debug.printType;

pub fn freeAst(allocator: Allocator, ast: blitzAst.Ast) void {
    freeNodes(allocator, ast.root.nodes);
    allocator.free(ast.root.nodes);
}

pub fn freeCompInfo(allocator: Allocator, compInfo: *CompInfo) void {
    compInfo.generics.deinit();

    var variableIt = compInfo.variableTypes.valueIterator();
    while (variableIt.next()) |valuePtr| {
        freeType(allocator, valuePtr.*);
    }

    var functionIt = compInfo.functions.valueIterator();
    while (functionIt.next()) |f| {
        freeFuncDec(allocator, f.*);
    }

    var structsIt = compInfo.structs.valueIterator();
    while (structsIt.next()) |dec| {
        freeStructDec(allocator, dec.*);
        allocator.destroy(dec.*);
    }

    freeStructNames(allocator, compInfo.structNames);

    for (compInfo.currentStructs.items) |item| {
        allocator.free(item);
    }

    compInfo.variableTypes.deinit();
    compInfo.functions.deinit();
    compInfo.structs.deinit();
    compInfo.currentStructs.deinit();
    compInfo.distFromStructMethod.deinit();
}

pub fn freeStructNames(allocator: Allocator, structNames: [][]u8) void {
    for (structNames) |name| {
        allocator.free(name);
    }

    allocator.free(structNames);
}

pub fn freeFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    allocator.free(func.name);

    for (func.params) |param| {
        freeType(allocator, param.type);
        allocator.free(param.name);
    }

    allocator.free(func.params);

    if (func.generics) |generics| {
        for (generics) |generic| {
            allocator.free(generic.name);

            if (generic.restriction) |rest| {
                freeType(allocator, rest);
            }
        }

        allocator.free(generics);
    }

    freeNode(allocator, func.body);
    freeType(allocator, func.returnType);

    allocator.destroy(func);
}

pub fn freeAttr(allocator: Allocator, attr: blitzAst.StructAttribute) void {
    allocator.free(attr.name);

    switch (attr.attr) {
        .Member => |mem| freeType(allocator, mem),
        .Function => |func| freeFuncDec(allocator, func),
    }
}

pub fn freeValueNode(allocator: Allocator, node: *const blitzAst.AstValues) void {
    switch (node.*) {
        .StaticArray => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        .Number => |num| {
            allocator.free(num.value);
        },
        .String => |string| {
            allocator.free(string);
        },
        else => {},
    }
}

pub fn freeNode(allocator: Allocator, node: *const blitzAst.AstNode) void {
    switch (node.*) {
        .Add, .Sub, .Mult, .Div => |op| {
            freeNode(allocator, op.left);
            freeNode(allocator, op.right);
        },
        .FuncReference => |ref| {
            allocator.free(ref);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        .PropertyAccess => |access| {
            freeNode(allocator, access.value);
        },
        .VarDec => |*dec| {
            freeNode(allocator, dec.setNode);

            if (dec.annotation) |annotation| {
                freeType(allocator, annotation);
            }

            allocator.free(dec.name);
        },
        .Seq => |*seq| {
            for (seq.nodes) |seqNode| {
                freeNode(allocator, seqNode);
            }
            allocator.free(seq.nodes);
        },
        .Type => |*t| {
            freeType(allocator, t);
        },
        .Value => |*val| {
            freeValueNode(allocator, val);
        },
        .Cast => |*cast| {
            freeNode(allocator, cast.node);
            freeType(allocator, cast.toType);
        },
        .Variable => |*variable| {
            allocator.free(variable.name);
        },
        .StructDec => |dec| {
            freeStructDec(allocator, dec);
        },
        .IfStatement => |*statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
        },
        .NoOp => {},
        .FuncDec => |name| {
            allocator.free(name);
        },
        .FuncCall => |call| {
            freeNode(allocator, call.func);

            for (call.params) |param| {
                freeNode(allocator, param);
            }

            allocator.free(call.params);
        },
        .ReturnNode => |ret| {
            freeNode(allocator, ret);
        },
        .StructInit => |init| {
            allocator.free(init.name);

            for (init.attributes) |attr| {
                allocator.free(attr.name);
                freeNode(allocator, attr.value);
            }

            for (init.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(init.attributes);
            allocator.free(init.generics);
        },
        .Bang => |bang| {
            freeNode(allocator, bang);
        },
    }

    allocator.destroy(node);
}

fn freeStructDec(allocator: Allocator, dec: *const blitzAst.StructDecNode) void {
    allocator.free(dec.name);

    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            freeType(allocator, restriction);
        }
        allocator.free(generic.name);
    }

    for (dec.attributes) |attr| {
        freeAttr(allocator, attr);
    }

    allocator.free(dec.attributes);
    allocator.free(dec.generics);

    if (dec.deriveType) |derived| {
        freeType(allocator, derived);
    }
}

pub fn freeNodes(allocator: Allocator, nodes: []*const blitzAst.AstNode) void {
    for (nodes) |node| {
        freeNode(allocator, node);
    }
}

pub fn freeStackType(allocator: Allocator, node: *const blitzAst.AstTypes) void {
    switch (node.*) {
        .DynamicArray => |arr| {
            freeType(allocator, arr);
        },
        .StaticArray => |arr| {
            freeType(allocator, arr.type);
            freeNode(allocator, arr.size);
        },
        .Nullable => |nullable| {
            freeType(allocator, nullable);
        },
        .Custom => |custom| {
            for (custom.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(custom.generics);
            allocator.free(custom.name);
        },
        .Generic => |gen| {
            allocator.free(gen);
        },
        else => {},
    }
}

pub fn freeType(allocator: Allocator, node: *const blitzAst.AstTypes) void {
    freeStackType(allocator, node);
    allocator.destroy(node);
}

pub fn freeToken(allocator: Allocator, token: tokenizer.Token) void {
    if (token.string) |str| {
        allocator.free(str);
    }
}

pub fn freeTokens(allocator: Allocator, tokens: []tokenizer.Token) void {
    for (tokens) |token| {
        freeToken(allocator, token);
    }

    allocator.free(tokens);
}

pub fn freeTokenArr(allocator: Allocator, tokens: []tokenizer.Token) void {
    for (tokens.*) |token| {
        if (token.string) |str| {
            allocator.free(str);
        }
    }
}
