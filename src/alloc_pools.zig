const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const blitzContext = blitz.context;
const debug = blitz.debug;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
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

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.nodes.deinit();
        allocator.destroy(self.nodes);

        self.types.deinit();
        allocator.destroy(self.types);
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

    pub fn releaseType(self: Self, context: *Context, ptr: *ast.AstTypes) void {
        if (context.settings.debug.trackPoolMem) {
            context.utils.releaseTypeAddress(ptr);
        }

        self.types.destroy(ptr);
    }

    pub fn releaseNode(self: Self, context: *Context, ptr: *ast.AstNode) void {
        if (context.settings.debug.trackPoolMem) {
            context.utils.releaseNodeAddress(ptr);
        }
        self.nodes.destroy(ptr);
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
