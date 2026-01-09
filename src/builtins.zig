const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const scanner = blitz.scanner;
const clone = blitz.clone;
const free = blitz.free;
const blitzContext = blitz.context;
const Allocator = std.mem.Allocator;
const ScanError = scanner.ScanError;
const createMut = utils.createMut;
const create = utils.create;
const Context = blitzContext.Context;

pub const BuiltinFuncMemo = struct {};

const PropTypeMap = struct {
    prop: []const u8,
    type: *ast.AstTypes,
    mutState: scanner.MutState,
};

pub fn getStringPropType(context: *Context, prop: []const u8) !ast.AstTypeInfo {
    const props = &[_]PropTypeMap{
        .{
            .prop = "len",
            .type = context.staticPtrs.types.u64Type.astType,
            .mutState = .Const,
        },
    };

    return try getPropType(context, props, prop);
}

pub fn getArraySlicePropType(context: *Context, prop: []const u8) !ast.AstTypeInfo {
    const props = &[_]PropTypeMap{
        .{
            .prop = "len",
            .type = context.staticPtrs.types.u64Type.astType,
            .mutState = .Const,
        },
    };

    return try getPropType(context, props, prop);
}

fn getPropType(
    context: *Context,
    props: []const PropTypeMap,
    prop: []const u8,
) !ast.AstTypeInfo {
    for (props) |item| {
        if (utils.compString(item.prop, prop)) {
            return .{
                .astType = try context.pools.newType(.{
                    .VarInfo = item.type.toAllocInfo(
                        item.mutState,
                        .Recycled,
                    ),
                }),
                .mutState = item.mutState,
            };
        }
    }

    return ScanError.InvalidProperty;
}
