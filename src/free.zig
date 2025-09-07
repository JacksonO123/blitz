const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const builtins = blitz.builtins;
const compInfo = blitz.compInfo;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;

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
            freeAstTypeInfo(allocator, param.type);
            allocator.free(param.name);
        }
    }
    allocator.free(func.params);

    if (func.generics) |generics| {
        for (generics) |generic| {
            allocator.free(generic.name);

            if (generic.restriction) |rest| {
                freeAstTypeInfo(allocator, rest);
            }
        }

        allocator.free(generics);
    }

    freeNode(allocator, func.body);
    freeAstTypeInfo(allocator, func.returnType);

    if (func.capturedValues) |captured| {
        freeVariableCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    if (func.capturedTypes) |captured| {
        freeGenericCaptures(allocator, captured);
        captured.deinit();
        allocator.destroy(captured);
    }

    if (func.capturedFuncs) |captured| {
        captured.deinit();
        allocator.destroy(captured);
    }

    for (func.toScanTypes.items) |rels| {
        for (rels) |item| {
            freeAstTypeInfo(allocator, item.info);
        }
        allocator.free(rels);
    }

    func.toScanTypes.deinit();
    allocator.destroy(func.toScanTypes);
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
        .Member => |mem| freeAstTypeInfo(allocator, mem),
        .Function => |func| freeFuncDec(allocator, func),
    }
}

pub fn freeValueNode(allocator: Allocator, node: *const blitzAst.AstValues) void {
    switch (node.*) {
        .ArraySlice => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        .String => |string| {
            allocator.free(string);
        },
        .RawNumber => |num| {
            allocator.free(num.digits);
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
        .Dereference,
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
                freeAstTypeInfo(allocator, annotation);
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
            freeAstTypeInfo(allocator, cast.toType);
        },
        .Variable => |variable| {
            allocator.free(variable);
        },
        .Pointer => |ptr| {
            freeNode(allocator, ptr.node);
        },
        .HeapAlloc => |alloc| {
            if (alloc.allocType) |allocType| {
                freeAstTypeInfo(allocator, allocType);
            }

            freeNode(allocator, alloc.node);
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
                freeAstTypeInfo(allocator, generic);
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
        .ArrayInit => |init| {
            allocator.free(init.size);
            freeNode(allocator, init.initNode);
            freeAstTypeInfo(allocator, init.initType);
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
            freeAstTypeInfo(allocator, restriction);
        }
        allocator.free(generic.name);
    }

    freeAttrs(allocator, dec.attributes);

    allocator.free(dec.attributes);
    allocator.free(dec.generics);
    allocator.free(dec.totalMemberList);

    if (dec.deriveType) |derived| {
        freeAstTypeInfo(allocator, derived);
    }
}

pub fn freeNodes(allocator: Allocator, nodes: []*blitzAst.AstNode) void {
    for (nodes) |node| {
        freeNode(allocator, node);
    }
}

pub fn freeStackType(allocator: Allocator, node: *const blitzAst.AstTypes) void {
    switch (node.*) {
        .ArraySlice => |arr| {
            freeAstTypeInfo(allocator, arr.type);
            if (arr.size) |size| {
                freeNode(allocator, size);
            }
        },
        .Nullable => |nullable| {
            freeAstTypeInfo(allocator, nullable);
        },
        .Pointer => |ptr| {
            freeAstTypeInfo(allocator, ptr);
        },
        .Custom => |custom| {
            for (custom.generics) |generic| {
                freeAstTypeInfo(allocator, generic);
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
                freeAstTypeInfo(allocator, payload);
            }
        },
        .ErrorVariant => |err| {
            allocator.free(err.from);
            allocator.free(err.variant);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        .VarInfo => |info| {
            freeType(allocator, info.astType);
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

pub fn freeTokenArr(allocator: Allocator, tokens: []tokenizer.Token) void {
    for (tokens.*) |token| {
        if (token.string) |str| {
            allocator.free(str);
        }
    }
}

pub fn freeBuiltins(allocator: Allocator, memos: builtins.BuiltinFuncMemo) void {
    _ = allocator;
    _ = memos;
}

pub fn freeVariableScope(allocator: Allocator, scope: *compInfo.VarScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |v| {
        freeAstTypeInfo(allocator, v.*);
    }
}

pub fn freeVariableCaptures(allocator: Allocator, scope: *compInfo.CaptureScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |v| {
        freeAstTypeInfo(allocator, v.*);
    }
}

pub const freeGenericCaptures = freeGenericScope;

pub fn freeScopedFunctionScope(allocator: Allocator, scope: *compInfo.StringListScope) void {
    for (scope.items) |item| {
        allocator.free(item);
    }
}

pub fn freeGenericScope(allocator: Allocator, scope: *compInfo.TypeScope) void {
    var genericIt = scope.valueIterator();
    while (genericIt.next()) |gen| {
        freeAstTypeInfo(allocator, gen.*);
    }
}

pub fn freeStringListScope(allocator: Allocator, scope: *compInfo.StringListScope) void {
    _ = allocator;
    _ = scope;
    // no operation needed
}
