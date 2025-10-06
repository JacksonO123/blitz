const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const builtins = blitz.builtins;
const compInfo = blitz.compInfo;
const blitzContext = blitz.context;
const Allocator = std.mem.Allocator;
const Context = blitzContext.Context;

pub fn freeFuncDec(
    allocator: Allocator,
    context: *Context,
    func: *const blitzAst.FuncDecNode,
) void {
    allocator.free(func.params);

    if (func.generics) |generics| {
        allocator.free(generics);
    }

    if (func.capturedValues) |captured| {
        freeVariableCaptures(allocator, context, captured);
        allocator.destroy(captured);
    }

    if (func.capturedTypes) |captured| {
        freeGenericCaptures(allocator, context, captured);
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
    allocator.free(rels);
}

pub fn freeAttrs(allocator: Allocator, context: *Context, attrs: []blitzAst.StructAttribute) void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Function => |func| freeFuncDec(allocator, context, func),
            else => {},
        }
    }
}

pub fn freeStructDec(
    allocator: Allocator,
    context: *Context,
    dec: *const blitzAst.StructDecNode,
) void {
    freeAttrs(allocator, context, dec.attributes);

    allocator.free(dec.attributes);
    allocator.free(dec.generics);
    allocator.free(dec.totalMemberList);

    for (dec.toScanTypes.items) |rels| {
        freeGenInfoRels(allocator, rels);
    }

    dec.toScanTypes.deinit(allocator);
    allocator.destroy(dec.toScanTypes);
}

pub fn freeBuiltins(allocator: Allocator, memos: builtins.BuiltinFuncMemo) void {
    _ = allocator;
    _ = memos;
}

pub fn freeVariableScope(allocator: Allocator, context: *Context, scope: *compInfo.VarScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |val| {
        if (val.allocState == .Allocated) {
            recursiveReleaseType(allocator, context, val.info.astType);
        }
    }
    scope.deinit();
}

pub fn freeVariableCaptures(allocator: Allocator, context: *Context, scope: *compInfo.CaptureScope) void {
    _ = allocator;
    _ = context;
    scope.deinit();
}

pub const freeGenericCaptures = freeGenericScope;

pub fn freeGenericScope(allocator: Allocator, context: *Context, scope: *compInfo.TypeScope) void {
    _ = allocator;
    _ = context;
    scope.deinit();
}

pub fn deinitScope(allocator: Allocator, context: *Context, scope: *compInfo.StringListScope) void {
    _ = context;
    scope.deinit(allocator);
}

pub fn recursiveReleaseNode(allocator: Allocator, context: *Context, ptr: *blitzAst.AstNode) void {
    recursiveReleaseNodeUtil(allocator, context, ptr, .Allocated);
}

pub fn recursiveReleaseNodeAll(
    allocator: Allocator,
    context: *Context,
    ptr: *blitzAst.AstNode,
) void {
    recursiveReleaseNodeUtil(allocator, context, ptr, .All);
}

const ReleaseType = enum {
    All,
    Allocated,
};

pub fn recursiveReleaseNodeUtil(
    allocator: Allocator,
    context: *Context,
    ptr: *blitzAst.AstNode,
    releaseType: ReleaseType,
) void {
    context.pools.nodes.release(ptr);

    switch (ptr.*) {
        .ReturnNode,
        .Bang,
        .Group,
        .Scope,
        .IncOne,
        .DecOne,
        .Dereference,
        .HeapFree,
        => |node| {
            recursiveReleaseNodeUtil(allocator, context, node, releaseType);
        },
        .Seq => |seq| {
            for (seq) |node| {
                recursiveReleaseNodeUtil(allocator, context, node, releaseType);
            }
        },
        .ArrayInit => |init| {
            recursiveReleaseNodeUtil(allocator, context, init.initNode, releaseType);
            recursiveReleaseTypeUtil(allocator, context, init.initType.astType, releaseType);
        },
        .VarDec => |dec| {
            recursiveReleaseNodeUtil(allocator, context, dec.setNode, releaseType);
            if (dec.annotation) |annotation| {
                recursiveReleaseTypeUtil(allocator, context, annotation.astType, releaseType);
            }
        },
        .ValueSet => |set| {
            recursiveReleaseNodeUtil(allocator, context, set.value, releaseType);
            recursiveReleaseNodeUtil(allocator, context, set.setNode, releaseType);
        },
        .VarEqOp => |eqOp| {
            recursiveReleaseNodeUtil(allocator, context, eqOp.value, releaseType);
        },
        .Value => |val| {
            if (val != .ArraySlice) return;
            for (val.ArraySlice) |item| {
                recursiveReleaseNodeUtil(allocator, context, item, releaseType);
            }
        },
        .Cast => |cast| {
            recursiveReleaseNodeUtil(allocator, context, cast.node, releaseType);
            recursiveReleaseTypeUtil(allocator, context, cast.toType.astType, releaseType);
        },
        .IfStatement => |statement| {
            recursiveReleaseNodeUtil(allocator, context, statement.body, releaseType);
            recursiveReleaseNodeUtil(allocator, context, statement.condition, releaseType);
            if (statement.fallback) |fallback| {
                recursiveReleaseNodeUtil(allocator, context, fallback.node, releaseType);
            }
        },
        .FuncCall => |call| {
            recursiveReleaseNodeUtil(allocator, context, call.func, releaseType);
            for (call.params) |param| {
                recursiveReleaseNodeUtil(allocator, context, param, releaseType);
            }
        },
        .StructInit => |init| {
            for (init.generics) |generic| {
                recursiveReleaseTypeUtil(allocator, context, generic.astType, releaseType);
            }

            for (init.attributes) |attr| {
                recursiveReleaseNodeUtil(allocator, context, attr.value, releaseType);
            }
        },
        .PropertyAccess => |access| {
            recursiveReleaseNodeUtil(allocator, context, access.value, releaseType);
        },
        .OpExpr => |expr| {
            recursiveReleaseNodeUtil(allocator, context, expr.left, releaseType);
            recursiveReleaseNodeUtil(allocator, context, expr.right, releaseType);
        },
        .IndexValue => |index| {
            recursiveReleaseNodeUtil(allocator, context, index.value, releaseType);
            recursiveReleaseNodeUtil(allocator, context, index.index, releaseType);
        },
        .ForLoop => |loop| {
            if (loop.initNode) |init| {
                recursiveReleaseNodeUtil(allocator, context, init, releaseType);
            }

            recursiveReleaseNodeUtil(allocator, context, loop.condition, releaseType);
            recursiveReleaseNodeUtil(allocator, context, loop.body, releaseType);
            recursiveReleaseNodeUtil(allocator, context, loop.incNode, releaseType);
        },
        .WhileLoop => |loop| {
            recursiveReleaseNodeUtil(allocator, context, loop.condition, releaseType);
            recursiveReleaseNodeUtil(allocator, context, loop.body, releaseType);
        },
        .Pointer => |ptrNode| {
            recursiveReleaseNodeUtil(allocator, context, ptrNode.node, releaseType);
        },
        .HeapAlloc => |alloc| {
            recursiveReleaseNodeUtil(allocator, context, alloc.node, releaseType);
        },
        else => {},
    }
}

pub fn recursiveReleaseType(
    allocator: Allocator,
    context: *Context,
    astType: *blitzAst.AstTypes,
) void {
    recursiveReleaseTypeUtil(allocator, context, astType, .Allocated);
}

pub fn recursiveReleaseTypeUtil(
    allocator: Allocator,
    context: *Context,
    astType: *blitzAst.AstTypes,
    releaseType: ReleaseType,
) void {
    if (!context.constTypeInfos.isStatic(astType)) {
        context.pools.types.release(astType);
    }

    switch (astType.*) {
        .Nullable => |info| {
            recursiveReleaseType(allocator, context, info.astType);
        },
        .VarInfo, .Pointer => |info| {
            if (info.allocState == .Allocated) {
                recursiveReleaseType(allocator, context, info.info.astType);
            }
        },
        .ArraySlice => |slice| {
            if (slice.size) |size| {
                recursiveReleaseNodeUtil(allocator, context, size, releaseType);
            }

            if (slice.type.allocState == .Allocated) {
                recursiveReleaseType(allocator, context, slice.type.info.astType);
            }
        },
        .Custom => |custom| {
            for (custom.generics) |generic| {
                recursiveReleaseType(allocator, context, generic.astType);
            }
        },
        .Error => |err| {
            if (err.payload) |payload| {
                recursiveReleaseType(allocator, context, payload.astType);
            }
        },
        else => {},
    }
}
