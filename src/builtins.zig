const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const scanner = blitz.scanner;
const vmInfo = blitz.vmInfo;
const ScanError = scanner.ScanError;
const Context = blitz.context.Context;

const PropTypeMap = struct {
    prop: []const u8,
    type: *ast.AstTypes,
    mutState: scanner.MutState,
};

pub fn getArrayDecPropType(context: *Context, prop: []const u8) !ast.AstTypeInfo {
    const props = &[_]PropTypeMap{
        .{
            .prop = "len",
            .type = context.staticPtrs.types.u64Type.astType,
            .mutState = .Const,
        },
    };

    return try getPropType(context, props, prop);
}

const SlicePropLocation = struct {
    prop: []const u8,
    location: u64,
};

const slicePropLocations = &[_]SlicePropLocation{
    .{
        .prop = "len",
        .location = vmInfo.POINTER_SIZE,
    },
};

pub fn getSlicePropLocations(prop: []const u8) ?u64 {
    for (slicePropLocations) |rel| {
        if (std.mem.eql(u8, rel.prop, prop)) {
            return rel.location;
        }
    }

    return null;
}

fn getPropType(
    context: *Context,
    props: []const PropTypeMap,
    prop: []const u8,
) !ast.AstTypeInfo {
    for (props) |item| {
        if (utils.compString(item.prop, prop)) {
            return .{
                .astType = try context.pools.newType(context, .{
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
