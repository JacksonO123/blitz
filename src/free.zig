const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const builtins = blitz.builtins;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;

// DEBUG
const debug = @import("debug.zig");
const printType = debug.printType;

pub fn freeNestedSlice(comptime T: type, allocator: Allocator, slices: [][]T) void {
    for (slices) |name| {
        allocator.free(name);
    }

    allocator.free(slices);
}

pub fn shallowFreeFuncDecParams(allocator: Allocator, params: []blitzAst.Parameter) void {
    for (params) |param| {
        allocator.free(param.name);
    }
}

pub fn freeBuiltinFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    freeFuncDecUtil(allocator, func, true);
}

pub fn freeFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    freeFuncDecUtil(allocator, func, false);
}

pub fn freeFuncDecUtil(allocator: Allocator, func: *const blitzAst.FuncDecNode, builtin: bool) void {
    allocator.free(func.name);
    if (builtin) {
        shallowFreeFuncDecParams(allocator, func.params);
    } else {
        for (func.params) |param| {
            freeType(allocator, param.type);
            allocator.free(param.name);
        }
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

    if (func.capturedValues) |captured| {
        utils.freeVariableCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    if (func.capturedTypes) |captured| {
        utils.freeGenericCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    allocator.destroy(func);
}

pub fn freeAttrs(allocator: Allocator, attrs: []blitzAst.StructAttribute) void {
    for (attrs) |attr| {
        freeAttr(allocator, attr);
    }
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
        .GeneralArray => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        .String => |string| {
            allocator.free(string);
        },
        .RawNumber => |num| {
            allocator.free(num);
        },
        else => {},
    }
}

pub fn freeNode(allocator: Allocator, node: *const blitzAst.AstNode) void {
    switch (node.*) {
        .NoOp, .StructPlaceholder => {},
        .IndexValue => |index| {
            freeNode(allocator, index.index);
            freeNode(allocator, index.value);
        },
        .OpExpr => |op| {
            freeNode(allocator, op.left);
            freeNode(allocator, op.right);
        },
        .IncOne,
        .DecOne,
        .Bang,
        .ReturnNode,
        .Group,
        .Scope,
        => |val| {
            freeNode(allocator, val);
        },
        .FuncReference => |ref| {
            allocator.free(ref);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        .PropertyAccess => |access| {
            freeNode(allocator, access.value);
            allocator.free(access.property);
        },
        .VarDec => |dec| {
            freeNode(allocator, dec.setNode);

            if (dec.annotation) |annotation| {
                freeType(allocator, annotation);
            }

            allocator.free(dec.name);
        },
        .ValueSet => |set| {
            freeNode(allocator, set.value);
            freeNode(allocator, set.setNode);
        },
        .VarEqOp => |op| {
            freeNode(allocator, op.value);
            allocator.free(op.variable);
        },
        .Seq => |seq| {
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
        .Cast => |cast| {
            freeNode(allocator, cast.node);
            freeType(allocator, cast.toType);
        },
        .Variable => |variable| {
            allocator.free(variable);
        },
        .StructDec => |dec| {
            freeStructDec(allocator, dec);
        },
        .IfStatement => |statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
            if (statement.fallback) |fallback| {
                freeIfFallback(allocator, fallback);
            }
        },
        .ForLoop => |loop| {
            if (loop.initNode) |init| {
                freeNode(allocator, init);
            }

            freeNode(allocator, loop.condition);
            freeNode(allocator, loop.incNode);
            freeNode(allocator, loop.body);
        },
        .WhileLoop => |loop| {
            freeNode(allocator, loop.condition);
            freeNode(allocator, loop.body);
        },
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
        .ErrorDec => |dec| {
            freeErrorDec(allocator, dec);
        },
        .Error => |err| {
            allocator.free(err);
        },
    }

    allocator.destroy(node);
}

fn freeIfFallback(allocator: Allocator, fallback: *const blitzAst.IfFallback) void {
    if (fallback.condition) |condition| {
        freeNode(allocator, condition);
    }

    if (fallback.fallback) |innerFallback| {
        freeIfFallback(allocator, innerFallback);
    }

    freeNode(allocator, fallback.body);
    allocator.destroy(fallback);
}

pub fn freeErrorDec(allocator: Allocator, dec: *const blitzAst.ErrorDecNode) void {
    allocator.free(dec.name);

    if (dec.variants) |variants| {
        for (variants) |variant| {
            allocator.free(variant);
        }

        allocator.free(variants);
    }
}

pub fn freeStructDec(allocator: Allocator, dec: *const blitzAst.StructDecNode) void {
    allocator.free(dec.name);

    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            freeType(allocator, restriction);
        }
        allocator.free(generic.name);
    }

    freeAttrs(allocator, dec.attributes);

    allocator.free(dec.attributes);
    allocator.free(dec.generics);
    allocator.free(dec.totalMemberList);

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
        .GeneralArray => |arr| {
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
        .Error => |err| {
            allocator.free(err.name);
            if (err.payload) |payload| {
                freeType(allocator, payload);
            }
        },
        .ErrorVariant => |err| {
            allocator.free(err.from);
            allocator.free(err.variant);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        else => {},
    }
}

pub fn freeAstTypeInfo(allocator: Allocator, info: blitzAst.AstTypeInfo) void {
    freeType(allocator, info.astType);
}

pub fn freeType(allocator: Allocator, typeNode: *const blitzAst.AstTypes) void {
    freeStackType(allocator, typeNode);
    allocator.destroy(typeNode);
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

pub fn freeBuiltins(allocator: Allocator, memos: builtins.BuiltinFuncMemo) void {
    const dyn = memos.dynArr;
    const fns = .{
        dyn.push,
        dyn.pop,
        dyn.pushFront,
        dyn.popFront,
    };

    inline for (fns) |func| {
        if (func) |dec| {
            freeBuiltinFuncDec(allocator, dec);
        }
    }
}
