const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const logger = blitz.logger;
const tokenizer = blitz.tokenizer;
const compInfo = blitz.compInfo;
const scanner = blitz.scanner;
const codegen = blitz.codegen;
const allocPools = blitz.allocPools;
const utils = blitz.utils;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Context = struct {
    const Self = @This();

    pools: *allocPools.Pools,
    logger: *logger.Logger,
    tokens: *tokenizer.TokenUtil,
    compInfo: *compInfo.CompInfo,
    scanInfo: *scanner.ScanInfo,
    genInfo: *codegen.GenInfo,
    deferCleanup: *DeferCleanup,
    code: []u8,

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }
};

pub const DeferCleanup = struct {
    const Self = @This();

    allocator: Allocator,
    slices: struct {
        strings: DeferedSlice([]u8),
        nodeSlices: DeferedSlice([]*blitzAst.AstNode),
        typeInfoSlices: DeferedSlice([]blitzAst.AstTypeInfo),
        genericTypeSlices: DeferedSlice([]blitzAst.GenericType),
    },

    pub fn init(allocator: Allocator) !Self {
        return .{
            .allocator = allocator,
            .strings = try DeferedSlice([]u8).init(allocator),
            .nodeSlices = try DeferedSlice([]*blitzAst.AstNode),
            .typeInfoSlices = try DeferedSlice([]blitzAst.AstTypeInfo),
            .genericTypeSlices = try DeferedSlice([]blitzAst.GenericType),
        };
    }

    pub fn deinit(self: *Self) void {
        self.slices.strings.deinit();
        self.slices.nodeSlices.deinit();
        self.slices.typeInfoSlices.deinit();
        self.slices.genericTypeSlices.deinit();
    }

    pub fn appendString(self: *Self, str: []const u8) !void {
        try self.slices.strings.append(self.allocator, str);
    }

    pub fn appendNodeSlice(self: *Self, slice: []*blitzAst.AstNode) !void {
        try self.slices.nodeSlice.append(self.allocator, slice);
    }

    pub fn appendTypeInfoSlice(self: *Self, slice: []blitzAst.AstTypeInfo) !void {
        try self.slices.typeInfoSlice.append(self.allocator, slice);
    }

    pub fn appendGenericTypeSlice(self: *Self, slice: []blitzAst.GenericType) !void {
        try self.slices.genericTypeSlice.append(self.allocator, slice);
    }
};

fn DeferedSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        slices: *ArrayList([]T),

        pub fn init(allocator: Allocator) !Self {
            const list: ArrayList([]T) = .empty;
            const ptr = try utils.createMut(ArrayList([]T), allocator, list);

            return .{
                .allocator = allocator,
                .slices = ptr,
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.slices.items) |slice| {
                self.allocator.free(slice);
            }
            self.slices.deinit();
            self.allocator.destroy(self.slices);
        }
    };
}
