const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const builtins = blitz.builtins;
const compInfo = blitz.compInfo;
const blitzContext = blitz.context;
const Allocator = std.mem.Allocator;
const Context = blitzContext.Context;

pub const ReleaseType = enum {
    All,
    Allocated,
};

pub fn releaseFuncDec(
    context: *Context,
    func: *const ast.FuncDecNode,
) void {
    recursiveReleaseNodeAll(context, func.body);

    for (func.params) |param| {
        recursiveReleaseTypeAll(context, param.type.astType);
    }

    if (func.generics) |generics| {
        for (generics) |generic| {
            if (generic.restriction) |restriction| {
                recursiveReleaseTypeAll(context, restriction.astType);
            }
        }
    }

    if (func.capturedValues) |captured| {
        freeVariableCaptures(context, captured, .All);
    }

    if (func.capturedTypes) |captured| {
        freeGenericCaptures(context, captured, .All);
    }

    for (func.toScanTypes.items) |rels| {
        for (rels) |rel| {
            recursiveReleaseTypeAll(context, rel.info.astType);
        }
    }

    recursiveReleaseTypeAll(context, func.returnType.astType);
}

pub fn freeStructAttrs(context: *Context, attrs: []ast.StructAttribute) void {
    for (attrs) |attr| {
        switch (attr.attr) {
            .Function => |func| releaseFuncDec(context, func),
            .Member => |member| recursiveReleaseTypeAll(context, member.astType),
        }
    }
}

pub fn releaseStructDec(
    context: *Context,
    dec: *ast.StructDecNode,
) void {
    freeStructAttrs(context, dec.attributes);

    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            recursiveReleaseTypeAll(context, restriction.astType);
        }
    }
}

pub fn freeVariableScope(
    context: *Context,
    scope: *compInfo.VarScope,
    releaseType: ReleaseType,
) void {
    var scopeIt = scope.iterator();
    while (scopeIt.next()) |val| {
        const astType = val.value_ptr.lastUsedNode;

        if (astType) |lastNode| {
            if (lastNode.variant == .Variable or lastNode.variant == .VarDec) {
                lastNode.typeInfo.lastVarUse = true;
            }
        }

        if (val.value_ptr.varTypeAndAllocInfo.allocState == .Allocated) {
            recursiveReleaseType(context, val.value_ptr.varTypeAndAllocInfo.info.astType);
        } else if (releaseType == .All) {
            recursiveReleaseTypeAll(context, val.value_ptr.varTypeAndAllocInfo.info.astType);
        }
    }
    scope.deinit();
}

pub const freeGenericCaptures = freeGenericScope;

pub fn freeVariableCaptures(
    context: *Context,
    scope: *compInfo.CaptureScope,
    releaseType: ReleaseType,
) void {
    var scopeIt = scope.iterator();
    while (scopeIt.next()) |val| {
        if (val.value_ptr.allocState == .Allocated) {
            recursiveReleaseType(context, val.value_ptr.info.astType);
        } else if (releaseType == .All) {
            recursiveReleaseTypeAll(context, val.value_ptr.info.astType);
        }
    }
    scope.deinit();
}

pub fn freeGenericScope(
    context: *Context,
    scope: *compInfo.TypeScope,
    releaseType: ReleaseType,
) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |val| {
        if (val.allocState == .Allocated) {
            recursiveReleaseType(context, val.info.astType);
        } else if (releaseType == .All) {
            recursiveReleaseTypeAll(context, val.info.astType);
        }
    }
    scope.deinit();
}

pub fn NoopDeinitScope(
    context: *Context,
    scope: *compInfo.StringListScope,
    releaseType: ReleaseType,
) void {
    _ = scope;
    _ = context;
    _ = releaseType;
}

pub fn recursiveReleaseNode(context: *Context, ptr: *ast.AstNode) void {
    recursiveReleaseNodeUtil(context, ptr, .Allocated);
}

pub fn recursiveReleaseNodeAll(
    context: *Context,
    ptr: *ast.AstNode,
) void {
    recursiveReleaseNodeUtil(context, ptr, .All);
}

pub fn recursiveReleaseNodeUtil(
    context: *Context,
    ptr: *ast.AstNode,
    releaseType: ReleaseType,
) void {
    switch (ptr.variant) {
        .ReturnNode,
        .Bang,
        .Group,
        .Scope,
        .IncOne,
        .DecOne,
        .Dereference,
        .HeapFree,
        => |node| {
            recursiveReleaseNodeUtil(context, node, releaseType);
        },
        .Seq => |seq| {
            for (seq) |node| {
                recursiveReleaseNodeUtil(context, node, releaseType);
            }
        },
        .ArrayInit => |init| {
            recursiveReleaseNodeUtil(context, init.initNode, releaseType);
            recursiveReleaseTypeUtil(context, init.initType.astType, releaseType);
        },
        .VarDec => |dec| {
            recursiveReleaseNodeUtil(context, dec.setNode, releaseType);

            if (dec.annotation) |annotation| {
                recursiveReleaseTypeUtil(context, annotation.astType, releaseType);
            }
        },
        .ValueSet => |set| {
            recursiveReleaseNodeUtil(context, set.value, releaseType);
            recursiveReleaseNodeUtil(context, set.setNode, releaseType);
        },
        .VarEqOp => |eqOp| {
            recursiveReleaseNodeUtil(context, eqOp.variable, releaseType);
            recursiveReleaseNodeUtil(context, eqOp.value, releaseType);
        },
        .Value => |val| {
            if (val == .ArrayDec) {
                for (val.ArrayDec) |item| {
                    recursiveReleaseNodeUtil(context, item, releaseType);
                }
            }
        },
        .Cast => |cast| {
            recursiveReleaseNodeUtil(context, cast.node, releaseType);
            recursiveReleaseTypeUtil(context, cast.toType.astType, releaseType);
        },
        .IfStatement => |statement| {
            recursiveReleaseNodeUtil(context, statement.body, releaseType);
            recursiveReleaseNodeUtil(context, statement.condition, releaseType);
            if (statement.fallback) |fallback| {
                recursiveReleaseNodeUtil(context, fallback.node, releaseType);
            }
        },
        .FuncCall => |call| {
            recursiveReleaseNodeUtil(context, call.func, releaseType);

            for (call.params) |param| {
                recursiveReleaseNodeUtil(context, param, releaseType);
            }

            if (call.callGenerics) |generics| {
                for (generics) |gen| {
                    recursiveReleaseTypeUtil(context, gen.astType, releaseType);
                }
            }
        },
        .StructInit => |init| {
            for (init.generics) |generic| {
                recursiveReleaseTypeUtil(context, generic.astType, releaseType);
            }

            for (init.attributes) |attr| {
                recursiveReleaseNodeUtil(context, attr.value, releaseType);
            }
        },
        .PropertyAccess => |access| {
            recursiveReleaseNodeUtil(context, access.value, releaseType);
        },
        .OpExpr => |expr| {
            recursiveReleaseNodeUtil(context, expr.left, releaseType);
            recursiveReleaseNodeUtil(context, expr.right, releaseType);
        },
        .IndexValue => |index| {
            recursiveReleaseNodeUtil(context, index.target, releaseType);
            recursiveReleaseNodeUtil(context, index.index, releaseType);
        },
        .ForLoop => |loop| {
            if (loop.initNode) |init| {
                recursiveReleaseNodeUtil(context, init, releaseType);
            }

            recursiveReleaseNodeUtil(context, loop.condition, releaseType);
            recursiveReleaseNodeUtil(context, loop.body, releaseType);
            recursiveReleaseNodeUtil(context, loop.incNode, releaseType);
        },
        .WhileLoop => |loop| {
            recursiveReleaseNodeUtil(context, loop.condition, releaseType);
            recursiveReleaseNodeUtil(context, loop.body, releaseType);
        },
        .Pointer => |ptrNode| {
            recursiveReleaseNodeUtil(context, ptrNode.node, releaseType);
        },
        .HeapAlloc => |alloc| {
            recursiveReleaseNodeUtil(context, alloc.node, releaseType);
        },
        .StructDec => |dec| {
            if (releaseType == .All) {
                if (dec.deriveType) |derive| {
                    recursiveReleaseTypeAll(context, derive.astType);
                }
            }
        },
        else => {},
    }

    if (!context.staticPtrs.isStaticPtr(ptr)) {
        context.releasePoolNode(ptr);
    }
}

pub fn recursiveReleaseType(
    context: *Context,
    astType: *ast.AstTypes,
) void {
    recursiveReleaseTypeUtil(context, astType, .Allocated);
}

pub fn recursiveReleaseTypeAll(
    context: *Context,
    astType: *ast.AstTypes,
) void {
    recursiveReleaseTypeUtil(context, astType, .All);
}

pub fn recursiveReleaseTypeUtil(
    context: *Context,
    astType: *ast.AstTypes,
    releaseType: ReleaseType,
) void {
    switch (astType.*) {
        .Nullable => |info| {
            recursiveReleaseType(context, info.astType);
        },
        .VarInfo, .Pointer => |info| {
            if (info.allocState == .Allocated or releaseType == .All) {
                recursiveReleaseType(context, info.info.astType);
            }
        },
        .ArrayDec => |slice| {
            if (slice.size) |size| {
                recursiveReleaseNodeUtil(context, size, releaseType);
            }

            if (slice.type.allocState == .Allocated or releaseType == .All) {
                recursiveReleaseType(context, slice.type.info.astType);
            }
        },
        .Custom => |custom| {
            for (custom.generics) |generic| {
                recursiveReleaseType(context, generic.astType);
            }
        },
        .Error => |err| {
            if (err.payload) |payload| {
                recursiveReleaseType(context, payload.astType);
            }
        },
        else => {},
    }

    if (!context.staticPtrs.isStaticPtr(astType)) {
        context.releasePoolType(astType);
    }
}

pub fn releaseStructsAndErrors(
    context: *Context,
    structsAndErrors: ast.RegisterStructsAndErrorsResult,
) void {
    for (structsAndErrors.structs) |def| {
        recursiveReleaseNodeAll(context, def);
    }

    for (structsAndErrors.errors) |err| {
        recursiveReleaseNodeAll(context, err);
    }
}
