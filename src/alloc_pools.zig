const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const blitzContext = blitz.context;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Context = blitzContext.Context;
const Writer = std.Io.Writer;

const POOL_SIZE = 1024 * 64;

const NodePool = AllocPool(blitzAst.AstNode, POOL_SIZE, "node_pool");
const TypePool = AllocPool(blitzAst.AstTypes, POOL_SIZE, "type_pool");

pub const Pools = struct {
    const Self = @This();

    allocator: Allocator,
    nodes: *NodePool,
    types: *TypePool,

    pub fn init(allocator: Allocator, context: *Context) !Self {
        const nodePool = try NodePool.init(allocator, context, &free.recursiveReleaseNode);
        const nodePoolPtr = try utils.createMut(NodePool, allocator, nodePool);
        const typePool = try TypePool.init(allocator, context, &free.recursiveReleaseType);
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

    pub fn writeStats(self: Self, writer: *Writer) !void {
        try self.nodes.writeStats(writer);
        try self.types.writeStats(writer);
    }
};

fn AllocPoolChunk(comptime T: type) type {
    return struct {
        items: []T,
        next: ?*AllocPoolChunk(T),
    };
}

fn FreeFn(comptime T: type) type {
    return fn (allocator: Allocator, context: *Context, *T) void;
}

fn AllocPool(comptime T: type, comptime size: comptime_int, comptime name: []const u8) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        context: *Context,
        root: *AllocPoolChunk(T),
        last: *AllocPoolChunk(T),
        availableItems: *ArrayList(*T),
        freeFn: *const FreeFn(T),
        numChunks: u32,

        fn newChunk(allocator: Allocator) !*AllocPoolChunk(T) {
            const items = try allocator.alloc(T, size);
            const chunk = utils.createMut(AllocPoolChunk(T), allocator, .{
                .items = items,
                .next = null,
            });
            return chunk;
        }

        pub fn init(
            allocator: Allocator,
            context: *Context,
            freeFn: *const FreeFn(T),
        ) !Self {
            const chunk = try newChunk(allocator);
            var stack = try ArrayList(*T).initCapacity(allocator, size);
            for (chunk.items) |*item| {
                try stack.append(allocator, item);
            }
            const stackPtr = try utils.createMut(ArrayList(*T), allocator, stack);

            return .{
                .allocator = allocator,
                .context = context,
                .root = chunk,
                .last = chunk,
                .availableItems = stackPtr,
                .freeFn = freeFn,
                .numChunks = 1,
            };
        }

        pub fn deinit(self: *Self) void {
            var current: ?*AllocPoolChunk(T) = self.root;
            while (current) |chunk| {
                self.allocator.free(chunk.items);
                self.allocator.destroy(chunk);
                current = chunk.next;
            }

            self.availableItems.deinit(self.allocator);
            self.allocator.destroy(self.availableItems);
        }

        pub fn new(self: *Self, val: T) !*T {
            const freePtr = self.availableItems.pop();
            if (freePtr) |ptr| {
                ptr.* = val;
                return ptr;
            }

            try self.appendChunk();
            return try self.new(val);
        }

        pub fn release(self: *Self, ptr: *T) void {
            self.availableItems.append(self.allocator, ptr) catch @panic("Out of memory");
        }

        pub fn releaseRecurse(self: *Self, ptr: *T) void {
            self.freeFn(self.allocator, self.context, ptr);
        }

        fn appendChunk(self: *Self) !void {
            const chunk = try newChunk(self.allocator);
            self.last.next = chunk;
            self.last = chunk;

            var ptrs: [size]*T = undefined;
            var i: usize = 0;
            while (i < ptrs.len) : (i += 1) {
                ptrs[i] = &chunk.items[i];
            }

            try self.availableItems.appendSlice(self.allocator, &ptrs);
            self.numChunks += 1;
        }

        pub fn writeStats(self: Self, writer: *Writer) !void {
            const availableItems = self.availableItems.items.len;
            const totalItems = self.numChunks * size;
            const floatAvailable: f32 = @floatFromInt(totalItems - availableItems);
            const percentFree: f32 = (floatAvailable / @as(f32, @floatFromInt(totalItems))) * 100;

            try writer.writeAll(name);
            try writer.writeAll(" stats:\n(");
            try writer.printInt(self.numChunks, 10, .lower, .{});
            try writer.writeAll(" chunks) ");
            try writer.printInt(totalItems - availableItems, 10, .lower, .{});
            try writer.writeAll("/");
            try writer.printInt(totalItems, 10, .lower, .{});
            try writer.writeAll(" : ");
            try writer.printFloat(percentFree, .{});
            try writer.writeAll("%\n");
        }
    };
}
