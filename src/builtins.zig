const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const scanner = blitz.scanner;
const vmInfo = blitz.vmInfo;
const ScanError = scanner.ScanError;
const Context = blitz.context.Context;
const identStore = blitz.identStore;

const PropTypeMap = struct {
    propIdentId: identStore.IdentId,
    type: *ast.AstTypes,
    mutState: scanner.MutState,
};

pub fn getArrayDecPropType(context: *Context, propIdentId: identStore.IdentId) !ast.AstTypeInfo {
    const props = &[_]PropTypeMap{
        .{
            .propIdentId = identStore.KNOWN_IDENT_IDS.len,
            .type = context.staticPtrs.types.u64Type.astType,
            .mutState = .Const,
        },
    };

    return try getPropType(context, props, propIdentId);
}

const SlicePropLocation = struct {
    prop: identStore.IdentId,
    location: u64,
};

const slicePropLocations = &[_]SlicePropLocation{
    .{
        .prop = identStore.KNOWN_IDENT_IDS.ptr,
        .location = 0,
    },
    .{
        .prop = identStore.KNOWN_IDENT_IDS.len,
        .location = vmInfo.POINTER_SIZE,
    },
};

pub fn getSlicePropLocations(propIdentId: identStore.IdentId) ?u64 {
    for (slicePropLocations) |rel| {
        if (rel.prop == propIdentId) return rel.location;
    }

    return null;
}

fn getPropType(
    context: *Context,
    props: []const PropTypeMap,
    propIdentId: identStore.IdentId,
) !ast.AstTypeInfo {
    for (props) |item| {
        if (item.propIdentId == propIdentId) {
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
