const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const builtins = blitz.builtins;
const compInfo = blitz.compInfo;
const Allocator = std.mem.Allocator;

pub fn freeBuiltinFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    freeFuncDecUtil(allocator, func, true);
}

pub fn freeFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    freeFuncDecUtil(allocator, func, false);
}

pub fn freeFuncDecUtil(
    allocator: Allocator,
    func: *const blitzAst.FuncDecNode,
    builtin: bool,
) void {
    if (!builtin) {
        for (func.params) |param| {
            freeAstTypeInfo(allocator, param.type);
        }
    }
    allocator.free(func.params);

    if (func.generics) |generics| {
        for (generics) |generic| {
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
        allocator.destroy(captured);
    }

    if (func.capturedTypes) |captured| {
        freeGenericCaptures(allocator, captured);
        allocator.destroy(captured);
    }

    if (func.capturedFuncs) |captured| {
        captured.deinit(allocator);
        allocator.destroy(captured);
    }

    for (func.toScanTypes.items) |rels| {
        freeGenInfoRels(allocator, rels);
    }

    func.toScanTypes.deinit(allocator);
    allocator.destroy(func.toScanTypes);
    allocator.destroy(func);
}

pub fn freeGenInfoRels(allocator: Allocator, rels: []blitzAst.StrToTypeInfoRel) void {
    for (rels) |item| {
        freeAstTypeInfo(allocator, item.info);
    }
    allocator.free(rels);
}

pub fn freeAttrs(allocator: Allocator, attrs: []blitzAst.StructAttribute) void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Member => |mem| freeAstTypeInfo(allocator, mem),
            .Function => |func| freeFuncDec(allocator, func),
        }
    }
}

pub fn freeValueNode(allocator: Allocator, node: *const blitzAst.AstValues) void {
    switch (node.*) {
        .ArraySlice => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        else => {},
    }
}

pub fn freeNode(allocator: Allocator, node: *const blitzAst.AstNode) void {
    switch (node.*) {
        .NoOp, .StructPlaceholder, .Break, .Continue => {},
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
        .HeapFree,
        => |val| {
            freeNode(allocator, val);
        },
        .PropertyAccess => |access| {
            freeNode(allocator, access.value);
        },
        .VarDec => |dec| {
            freeNode(allocator, dec.setNode);

            if (dec.annotation) |annotation| {
                freeAstTypeInfo(allocator, annotation);
            }
        },
        .ValueSet => |set| {
            freeNode(allocator, set.value);
            freeNode(allocator, set.setNode);
        },
        .VarEqOp => |op| {
            freeNode(allocator, op.value);
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
        .FuncCall => |call| {
            freeNode(allocator, call.func);

            for (call.params) |param| {
                freeNode(allocator, param);
            }

            allocator.free(call.params);
        },
        .StructInit => |init| {
            for (init.attributes) |attr| {
                freeNode(allocator, attr.value);
            }

            for (init.generics) |generic| {
                freeAstTypeInfo(allocator, generic);
            }

            allocator.free(init.attributes);
            allocator.free(init.generics);
        },
        .ErrorDec => |dec| {
            allocator.free(dec.variants);
        },
        .ArrayInit => |init| {
            freeNode(allocator, init.initNode);
            freeAstTypeInfo(allocator, init.initType);
        },
        else => {},
    }

    // allocator.destroy(node);
}

fn freeIfFallback(allocator: Allocator, fallback: blitzAst.FallbackInfo) void {
    freeNode(allocator, fallback.node);
}

pub fn freeStructDec(allocator: Allocator, dec: *const blitzAst.StructDecNode) void {
    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            freeAstTypeInfo(allocator, restriction);
        }
    }

    freeAttrs(allocator, dec.attributes);

    allocator.free(dec.attributes);
    allocator.free(dec.generics);
    allocator.free(dec.totalMemberList);

    if (dec.deriveType) |derived| {
        freeAstTypeInfo(allocator, derived);
    }

    for (dec.toScanTypes.items) |rels| {
        freeGenInfoRels(allocator, rels);
    }

    dec.toScanTypes.deinit(allocator);
    allocator.destroy(dec.toScanTypes);
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
        },
        .Error => |err| {
            if (err.payload) |payload| {
                freeAstTypeInfo(allocator, payload);
            }
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
    scope.deinit();
}

pub fn freeVariableCaptures(allocator: Allocator, scope: *compInfo.CaptureScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |v| {
        freeAstTypeInfo(allocator, v.*);
    }
    scope.deinit();
}

pub const freeGenericCaptures = freeGenericScope;

pub fn freeGenericScope(allocator: Allocator, scope: *compInfo.TypeScope) void {
    var genericIt = scope.valueIterator();
    while (genericIt.next()) |gen| {
        freeAstTypeInfo(allocator, gen.*);
    }
    scope.deinit();
}

pub fn deinitScope(allocator: Allocator, scope: *compInfo.StringListScope) void {
    scope.deinit(allocator);
}
