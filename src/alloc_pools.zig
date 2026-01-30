const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const blitzContext = blitz.context;
const debug = blitz.debug;
const compInfo = blitz.compInfo;
const Allocator = std.mem.Allocator;
const Context = blitzContext.Context;
const Writer = std.Io.Writer;
const MemPool = std.heap.MemoryPool;

const POOL_SIZE = 1024 * 64;

const NodePool = MemPool(ast.AstNode);
const TypePool = MemPool(ast.AstTypes);

pub const Pools = struct {
    const Self = @This();

    nodes: *NodePool,
    types: *TypePool,

    pub fn init(allocator: Allocator) !Self {
        const nodePool = try NodePool.initPreheated(allocator, POOL_SIZE);
        const nodePoolPtr = try utils.createMut(NodePool, allocator, nodePool);
        const typePool = try TypePool.initPreheated(allocator, POOL_SIZE);
        const typePoolPtr = try utils.createMut(TypePool, allocator, typePool);

        return .{
            .nodes = nodePoolPtr,
            .types = typePoolPtr,
        };
    }

    pub fn newType(self: Self, context: *Context, data: ast.AstTypes) !*ast.AstTypes {
        const ptr = try self.newTypeUntracked(data);
        if (context.settings.debug.trackPoolMem) {
            try context.utils.reserveTypeAddress(ptr);
        }
        return ptr;
    }

    pub fn newNode(self: Self, context: *Context, data: ast.AstNode) !*ast.AstNode {
        const ptr = try self.newNodeUntracked(data);
        if (context.settings.debug.trackPoolMem) {
            try context.utils.reserveNodeAddress(ptr);
        }
        return ptr;
    }

    pub fn newTypeUntracked(self: Self, data: ast.AstTypes) !*ast.AstTypes {
        const ptr = try self.types.create();
        ptr.* = data;
        return ptr;
    }

    pub fn newNodeUntracked(self: Self, data: ast.AstNode) !*ast.AstNode {
        const ptr = try self.nodes.create();
        ptr.* = data;
        return ptr;
    }

    pub fn writeStats(self: Self, context: *Context, verbose: bool, writer: *Writer) !void {
        const nodesCapacity = self.nodes.arena.queryCapacity();
        const typesCapacity = self.types.arena.queryCapacity();
        const usedNodeCount = context.utils.usedNodes.count();
        const usedTypesCount = context.utils.usedTypes.count();

        const floatAvailableNodes: f32 = @floatFromInt(usedNodeCount);
        const floatNodeCapacity: f32 = @floatFromInt(nodesCapacity);
        const percentFreeNodes: f32 = (floatAvailableNodes / floatNodeCapacity) * 100;

        const floatAvailableTypes: f32 = @floatFromInt(usedTypesCount);
        const floatTypeCapacity: f32 = @floatFromInt(typesCapacity);
        const percentFreeTypes: f32 = (floatAvailableTypes / floatTypeCapacity) * 100;

        if (verbose) {
            try writer.writeAll("node stats:\n");
            try writer.printInt(context.utils.usedNodes.count(), 10, .lower, .{});
            try writer.writeAll("/");
            try writer.printInt(nodesCapacity, 10, .lower, .{});
            try writer.writeAll(" : ");
            try writer.printFloat(percentFreeNodes, .{});
            try writer.writeAll("%\nused nodes:\n");

            var usedNodeIt = context.utils.usedNodes.keyIterator();
            while (usedNodeIt.next()) |item| {
                try writer.writeAll("|-- ");
                try debug.printNode(context, item.*, writer);
                try writer.print(" 0x{x}", .{@intFromPtr(item.*)});
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("LEAKED NODES: ");
        try writer.printInt(context.utils.usedNodes.count(), 10, .lower, .{});
        try writer.writeAll("\n\n");

        if (verbose) {
            try writer.writeAll("type stats:\n");
            try writer.printInt(context.utils.usedTypes.count(), 10, .lower, .{});
            try writer.writeAll("/");
            try writer.printInt(typesCapacity, 10, .lower, .{});
            try writer.writeAll(" : ");
            try writer.printFloat(percentFreeTypes, .{});
            try writer.writeAll("%\nused types:\n");

            var usedTypesIt = context.utils.usedTypes.keyIterator();
            while (usedTypesIt.next()) |item| {
                try writer.writeAll("|-- ");
                try debug.printType(context, item.*, writer);
                try writer.print(" 0x{x}", .{@intFromPtr(item.*)});
                try writer.writeAll("\n");
            }
        }

        try writer.writeAll("LEAKED TYPES: ");
        try writer.printInt(context.utils.usedTypes.count(), 10, .lower, .{});
        try writer.writeAll("\n");
    }
};

pub const ReleaseType = enum {
    All,
    Allocated,
};

pub fn releaseFuncDec(
    context: *Context,
    func: *const ast.FuncDecNode,
) void {
    recursiveReleaseNodeAll(context, func.body);

    for (func.params.params) |param| {
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
        releaseVariableCaptures(context, captured, .All);
    }

    if (func.capturedTypes) |captured| {
        releaseGenericCaptures(context, captured, .All);
    }

    for (func.toScanTypes.items) |rels| {
        for (rels) |rel| {
            recursiveReleaseTypeAll(context, rel.info.astType);
        }
    }

    recursiveReleaseTypeAll(context, func.returnType.astType);
}

pub fn releaseStructAttrs(context: *Context, attrs: []ast.StructAttribute) void {
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
    releaseStructAttrs(context, dec.attributes);

    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            recursiveReleaseTypeAll(context, restriction.astType);
        }
    }
}

pub fn releaseVariableScope(
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
}

pub const releaseGenericCaptures = releaseGenericScope;

pub fn releaseVariableCaptures(
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
}

pub fn releaseGenericScope(
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
}

pub fn NoopReleaseScope(
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

pub fn releaseHoistedNodes(
    context: *Context,
    hoistedNodes: ast.HoistedNodes,
) void {
    for (hoistedNodes.structs) |def| {
        recursiveReleaseNodeAll(context, def);
    }

    for (hoistedNodes.errors) |err| {
        recursiveReleaseNodeAll(context, err);
    }

    for (hoistedNodes.enums) |dec| {
        recursiveReleaseNodeAll(context, dec);
    }
}
