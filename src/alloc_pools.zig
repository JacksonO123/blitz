const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const blitzContext = blitz.context;
const debug = blitz.debug;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Context = blitzContext.Context;
const Writer = std.Io.Writer;

const POOL_SIZE = 1024 * 64;

const NodePool = AllocPool(
    blitzAst.AstNode,
    POOL_SIZE,
    "node_pool",
    free.recursiveReleaseNode,
    debug.printNode,
);
const TypePool = AllocPool(
    blitzAst.AstTypes,
    POOL_SIZE,
    "type_pool",
    free.recursiveReleaseType,
    debug.printType,
);

pub const Pools = struct {
    const Self = @This();

    allocator: Allocator,
    nodes: *NodePool,
    types: *TypePool,

    pub fn init(allocator: Allocator, context: *Context) !Self {
        const nodePool = try NodePool.init(allocator, context);
        const nodePoolPtr = try utils.createMut(NodePool, allocator, nodePool);
        const typePool = try TypePool.init(allocator, context);
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
    return fn (Allocator, *Context, *T) void;
}

fn PrintFn(comptime T: type) type {
    return fn (*Context, *T, *Writer) anyerror!void;
}

fn AllocPool(
    comptime T: type,
    comptime size: comptime_int,
    comptime name: []const u8,
    comptime freeFn: FreeFn(T),
    comptime printFn: PrintFn(T),
) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        context: *Context,
        root: *AllocPoolChunk(T),
        last: *AllocPoolChunk(T),
        available: *ArrayList(*T),
        used: *ArrayList(*T),
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
        ) !Self {
            const chunk = try newChunk(allocator);
            var availableList = try ArrayList(*T).initCapacity(allocator, size);
            for (chunk.items) |*item| {
                try availableList.append(allocator, item);
            }
            const availableListPtr = try utils.createMut(ArrayList(*T), allocator, availableList);

            const usedList = try ArrayList(*T).initCapacity(allocator, size);
            const usedListPtr = try utils.createMut(ArrayList(*T), allocator, usedList);

            return .{
                .allocator = allocator,
                .context = context,
                .root = chunk,
                .last = chunk,
                .available = availableListPtr,
                .used = usedListPtr,
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

            self.available.deinit(self.allocator);
            self.allocator.destroy(self.available);

            self.used.deinit(self.allocator);
            self.allocator.destroy(self.used);
        }

        fn getAvailablePtr(self: *Self) !*T {
            const freePtr = self.available.pop();
            if (freePtr) |ptr| {
                return ptr;
            }

            try self.appendChunk();
            return try self.getAvailablePtr();
        }

        pub fn new(self: *Self, val: T) !*T {
            const ptr = try self.getAvailablePtr();
            ptr.* = val;
            try self.used.append(self.allocator, ptr);
            return ptr;
        }

        pub fn release(self: *Self, ptr: *T) void {
            self.available.append(self.allocator, ptr) catch @panic("Out of memory");
        }

        pub fn releaseRecurse(self: *Self, ptr: *T) void {
            freeFn(self.allocator, self.context, ptr);
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

            try self.available.appendSlice(self.allocator, &ptrs);
            self.numChunks += 1;
        }

        pub fn writeStats(self: Self, writer: *Writer) !void {
            const availableItems = self.available.items.len;
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
            try writer.writeAll("%\nused items:\n");
            for (self.used.items) |item| {
                try writer.writeAll("|-- ");
                try printFn(self.context, item, writer);
                try writer.writeAll("\n");
            }
        }
    };
}
