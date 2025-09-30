const std = @import("std");
const blitz = @import("blitz.zig");
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
    strings: *ArrayList([]u8),

    pub fn init(allocator: Allocator) !Self {
        const list: ArrayList([]u8) = .empty;
        const listPtr = try utils.createMut(ArrayList([]u8), allocator, list);

        return .{
            .allocator = allocator,
            .strings = listPtr,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.strings.items) |str| {
            self.allocator.free(str);
        }

        self.strings.deinit(self.allocator);
        self.allocator.destroy(self.strings);
    }

    pub fn appendString(self: *Self, str: []const u8) !void {
        try self.strings.append(self.allocator, str);
    }
};
