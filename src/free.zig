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

pub fn freeRecurseAstNode(allocator: Allocator, context: *Context, ptr: *blitzAst.AstNode) void {
    context.pools.nodes.free(ptr);

    _ = allocator;
    unreachable;
}

pub fn freeRecurseType(allocator: Allocator, context: *Context, ptr: *blitzAst.AstTypes) void {
    _ = allocator;
    context.pools.types.free(ptr);

    // TODO
}
