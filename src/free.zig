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
    func: *const blitzAst.FuncDecNode,
) void {
    allocator.free(func.params);

    if (func.generics) |generics| {
        allocator.free(generics);
    }

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
    allocator.free(rels);
}

pub fn freeAttrs(allocator: Allocator, attrs: []blitzAst.StructAttribute) void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Function => |func| freeFuncDec(allocator, func),
            else => {},
        }
    }
}

pub fn freeStructDec(allocator: Allocator, dec: *const blitzAst.StructDecNode) void {
    freeAttrs(allocator, dec.attributes);

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

pub fn freeVariableScope(allocator: Allocator, scope: *compInfo.VarScope) void {
    _ = allocator;
    scope.deinit();
}

pub fn freeVariableCaptures(allocator: Allocator, scope: *compInfo.CaptureScope) void {
    _ = allocator;
    scope.deinit();
}

pub const freeGenericCaptures = freeGenericScope;

pub fn freeGenericScope(allocator: Allocator, scope: *compInfo.TypeScope) void {
    _ = allocator;
    scope.deinit();
}

pub fn deinitScope(allocator: Allocator, scope: *compInfo.StringListScope) void {
    scope.deinit(allocator);
}

pub fn recursiveReleaseNode(allocator: Allocator, context: *Context, ptr: *blitzAst.AstNode) void {
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
            recursiveReleaseNode(allocator, context, node);
        },
        .Seq => |seq| {
            for (seq) |node| {
                recursiveReleaseNode(allocator, context, node);
            }
        },
        .ArrayInit => |init| {
            recursiveReleaseNode(allocator, context, init.initNode);
            recursiveReleaseType(allocator, context, init.initType.astType);
        },
        .VarDec => |dec| {
            recursiveReleaseNode(allocator, context, dec.setNode);
            if (dec.annotation) |annotation| {
                recursiveReleaseType(allocator, context, annotation.astType);
            }
        },
        .ValueSet => |set| {
            recursiveReleaseNode(allocator, context, set.value);
            recursiveReleaseNode(allocator, context, set.setNode);
        },
        .VarEqOp => |eqOp| {
            recursiveReleaseNode(allocator, context, eqOp.value);
        },
        .Value => |val| {
            if (val != .ArraySlice) return;
            for (val.ArraySlice) |item| {
                recursiveReleaseNode(allocator, context, item);
            }
        },
        .Cast => |cast| {
            recursiveReleaseNode(allocator, context, cast.node);
            recursiveReleaseType(allocator, context, cast.toType.astType);
        },
        .IfStatement => |statement| {
            recursiveReleaseNode(allocator, context, statement.body);
            recursiveReleaseNode(allocator, context, statement.condition);
            if (statement.fallback) |fallback| {
                recursiveReleaseNode(allocator, context, fallback.node);
            }
        },
        .FuncCall => |call| {
            recursiveReleaseNode(allocator, context, call.func);
            for (call.params) |param| {
                recursiveReleaseNode(allocator, context, param);
            }
        },
        .StructInit => |init| {
            for (init.generics) |generic| {
                recursiveReleaseType(allocator, context, generic.astType);
            }

            for (init.attributes) |attr| {
                recursiveReleaseNode(allocator, context, attr.value);
            }
        },
        .PropertyAccess => |access| {
            recursiveReleaseNode(allocator, context, access.value);
        },
        .OpExpr => |expr| {
            recursiveReleaseNode(allocator, context, expr.left);
            recursiveReleaseNode(allocator, context, expr.right);
        },
        .IndexValue => |index| {
            recursiveReleaseNode(allocator, context, index.value);
            recursiveReleaseNode(allocator, context, index.index);
        },
        .ForLoop => |loop| {
            if (loop.initNode) |init| {
                recursiveReleaseNode(allocator, context, init);
            }

            recursiveReleaseNode(allocator, context, loop.condition);
            recursiveReleaseNode(allocator, context, loop.body);
            recursiveReleaseNode(allocator, context, loop.incNode);
        },
        .WhileLoop => |loop| {
            recursiveReleaseNode(allocator, context, loop.condition);
            recursiveReleaseNode(allocator, context, loop.body);
        },
        .Pointer => |ptrNode| {
            recursiveReleaseNode(allocator, context, ptrNode.node);
        },
        .HeapAlloc => |alloc| {
            recursiveReleaseNode(allocator, context, alloc.node);
        },
        else => {},
    }
}

pub fn recursiveReleaseType(
    allocator: Allocator,
    context: *Context,
    astType: *blitzAst.AstTypes,
) void {
    context.pools.types.release(astType);

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
                recursiveReleaseNode(allocator, context, size);
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
