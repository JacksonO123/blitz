const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const POOL_SIZE = 1024 * 64;

const NodePool = AllocPool(blitzAst.AstNode, POOL_SIZE);
const TypePool = AllocPool(blitzAst.AstTypes, POOL_SIZE);

pub const Pools = struct {
    const Self = @This();

    allocator: Allocator,
    nodes: *NodePool,
    types: *TypePool,

    pub fn init(allocator: Allocator) !Self {
        const nodePool = try NodePool.init(allocator);
        const nodePoolPtr = try utils.createMut(NodePool, allocator, nodePool);
        const typePool = try TypePool.init(allocator);
        const typePoolPtr = try utils.createMut(TypePool, allocator, typePool);

        return .{
            .allocator = allocator,
            .nodes = nodePoolPtr,
            .types = typePoolPtr,
        };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit();
        self.allocator.destroy(self.nodes);

        self.types.deinit();
        self.allocator.destroy(self.types);
    }
};

fn AllocPoolChunk(comptime T: type) type {
    return struct {
        nodes: []T,
        next: ?*AllocPoolChunk(T),
    };
}

fn AllocPool(comptime T: type, comptime size: comptime_int) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        root: *AllocPoolChunk(T),
        last: *AllocPoolChunk(T),
        freeNodes: *ArrayList(*T),

        fn newChunk(allocator: Allocator) !*AllocPoolChunk(T) {
            const nodes = try allocator.alloc(T, size);
            const chunk = utils.createMut(AllocPoolChunk(T), allocator, .{
                .nodes = nodes,
                .next = null,
            });
            return chunk;
        }

        pub fn init(allocator: Allocator) !Self {
            const chunk = try newChunk(allocator);
            const stack = try ArrayList(*T).initCapacity(allocator, size);
            var i: usize = 0;
            while (i < stack.items.len) : (i += 1) {
                stack.items[i] = &chunk.nodes[i];
            }
            const stackPtr = try utils.createMut(ArrayList(*T), allocator, stack);

            return .{
                .allocator = allocator,
                .root = chunk,
                .last = chunk,
                .freeNodes = stackPtr,
            };
        }

        pub fn deinit(self: *Self) void {
            var current: ?*AllocPoolChunk(T) = self.root;
            while (current) |chunk| {
                self.allocator.free(chunk.nodes);
                self.allocator.destroy(chunk);
                current = chunk.next;
            }

            self.freeNodes.deinit(self.allocator);
            self.allocator.destroy(self.freeNodes);
        }

        pub fn new(self: *Self, val: T) !*T {
            const freePtr = self.freeNodes.pop();
            if (freePtr) |ptr| {
                ptr.* = val;
                return ptr;
            }

            try self.appendChunk();
            return try self.new(val);
        }

        fn appendChunk(self: *Self) !void {
            const chunk = try newChunk(self.allocator);
            self.last.next = chunk;
            self.last = chunk;

            var ptrs: [size]*T = undefined;
            var i: usize = 0;
            while (i < ptrs.len) : (i += 1) {
                ptrs[i] = &chunk.nodes[i];
            }

            try self.freeNodes.appendSlice(self.allocator, &ptrs);
        }
    };
}
