const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const logger = blitz.logger;
const tokenizer = blitz.tokenizer;
const blitzCompInfo = blitz.compInfo;
const scanner = blitz.scanner;
const codegen = blitz.codegen;
const allocPools = blitz.allocPools;
const utils = blitz.utils;
const debug = blitz.debug;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;

pub const Context = struct {
    const Self = @This();

    allocator: Allocator,
    tokens: []tokenizer.Token,
    pools: *allocPools.Pools,
    logger: *logger.Logger,
    tokenUtil: *tokenizer.TokenUtil,
    compInfo: *blitzCompInfo.CompInfo,
    scanInfo: *scanner.ScanInfo,
    genInfo: *codegen.GenInfo,
    deferCleanup: *DeferCleanup,
    constTypeInfos: *ConstTypeInfos,
    code: []const u8,

    pub fn init(allocator: Allocator, code: []const u8, writer: *Writer) !*Self {
        // NOTE - context pointer is passed around before initialization
        // this is intentional, these utility structs need to store the memory
        // location which is allocated before initialization
        // to make this pattern safe, all of these util init functions must not
        // use context properties, as they are undefined
        const context = try allocator.create(Self);

        const pools = try allocPools.Pools.init(allocator, context);
        const poolsPtr = try utils.createMut(allocPools.Pools, allocator, pools);

        const tokens = try tokenizer.tokenize(allocator, code, writer);

        const names = try blitzAst.findStructsAndErrors(allocator, tokens, code);
        try debug.printStructAndErrorNames(names, writer);

        const tokenUtil = tokenizer.TokenUtil.init(tokens);
        const tokenUtilPtr = try utils.createMut(tokenizer.TokenUtil, allocator, tokenUtil);
        const loggerUtil = logger.Logger.init(tokenUtilPtr, code, writer);
        const loggerUtilPtr = try utils.createMut(logger.Logger, allocator, loggerUtil);

        const compInfo = try blitzCompInfo.CompInfo.init(allocator, context, names);
        const compInfoPtr = try utils.createMut(blitzCompInfo.CompInfo, allocator, compInfo);

        const scanInfoPtr = try utils.createMut(scanner.ScanInfo, allocator, .{});

        const genInfo = try codegen.GenInfo.init(allocator);
        const genInfoPtr = try utils.createMut(codegen.GenInfo, allocator, genInfo);
        genInfoPtr.vmInfo.stackStartSize = compInfoPtr.stackSizeEstimate;

        const deferCleanup = try DeferCleanup.init(allocator);
        const deferCleanupPtr = try utils.createMut(DeferCleanup, allocator, deferCleanup);

        const constTypeInfos = try ConstTypeInfos.init(poolsPtr);
        const constTypeInfosPtr = try utils.createMut(ConstTypeInfos, allocator, constTypeInfos);

        context.* = .{
            .allocator = allocator,
            .pools = poolsPtr,
            .logger = loggerUtilPtr,
            .tokens = tokens,
            .tokenUtil = tokenUtilPtr,
            .compInfo = compInfoPtr,
            .scanInfo = scanInfoPtr,
            .genInfo = genInfoPtr,
            .deferCleanup = deferCleanupPtr,
            .constTypeInfos = constTypeInfosPtr,
            .code = code,
        };

        return context;
    }

    pub fn deinit(self: *Self) void {
        self.pools.deinit();
        self.allocator.free(self.tokens);
        self.compInfo.deinit();
        self.genInfo.deinit();
        self.deferCleanup.deinit();
    }

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }
};

pub const ConstTypeInfos = struct {
    const Self = @This();

    voidType: blitzAst.AstTypeInfo,
    boolType: blitzAst.AstTypeInfo,
    anyType: blitzAst.AstTypeInfo,
    f32Type: blitzAst.AstTypeInfo,
    u64Type: blitzAst.AstTypeInfo,

    pub fn init(pools: *allocPools.Pools) !Self {
        return .{
            .voidType = utils.astTypesPtrToInfo(try pools.types.new(.Void), true),
            .boolType = utils.astTypesPtrToInfo(try pools.types.new(.Bool), false),
            .anyType = utils.astTypesPtrToInfo(try pools.types.new(.Any), false),
            .f32Type = utils.astTypesPtrToInfo(try pools.types.new(.{ .Number = .F32 }), false),
            .u64Type = utils.astTypesPtrToInfo(try pools.types.new(.{ .Number = .U64 }), false),
        };
    }
};

pub const DeferCleanup = struct {
    const Self = @This();

    allocator: Allocator,
    slices: struct {
        strings: DeferedSlice([]const u8),
        nodeSlices: DeferedSlice([]*blitzAst.AstNode),
        typeInfoSlices: DeferedSlice([]blitzAst.AstTypeInfo),
        genericTypeSlices: DeferedSlice([]blitzAst.GenericType),
        attrDefSlices: DeferedSlice([]blitzAst.AttributeDefinition),
    },

    pub fn init(allocator: Allocator) !Self {
        return .{
            .allocator = allocator,
            .slices = .{
                .strings = try DeferedSlice([]const u8).init(allocator),
                .nodeSlices = try DeferedSlice([]*blitzAst.AstNode).init(allocator),
                .typeInfoSlices = try DeferedSlice([]blitzAst.AstTypeInfo).init(allocator),
                .genericTypeSlices = try DeferedSlice([]blitzAst.GenericType).init(allocator),
                .attrDefSlices = try DeferedSlice([]blitzAst.AttributeDefinition).init(allocator),
            },
        };
    }

    pub fn deinit(self: *Self) void {
        self.slices.strings.deinit();
        self.slices.nodeSlices.deinit();
        self.slices.typeInfoSlices.deinit();
        self.slices.genericTypeSlices.deinit();
    }
};

fn DeferedSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        slices: *ArrayList(T),

        pub fn init(allocator: Allocator) !Self {
            const list: ArrayList(T) = .empty;
            const ptr = try utils.createMut(ArrayList(T), allocator, list);

            return .{
                .allocator = allocator,
                .slices = ptr,
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.slices.items) |slice| {
                self.allocator.free(slice);
            }
            self.slices.deinit(self.allocator);
            self.allocator.destroy(self.slices);
        }

        pub fn append(self: *Self, slice: T) !void {
            try self.slices.append(self.allocator, slice);
        }
    };
}
