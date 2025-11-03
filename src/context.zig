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
    staticPtrs: *StaticPtrs,
    code: []const u8,

    pub fn init(allocator: Allocator, code: []const u8, writer: *Writer) !*Self {
        // NOTE - context pointer is passed around before initialization
        // this is intentional, these utility structs need to store the memory
        // location which is allocated before initialization
        // to make this pattern safe, all of these util init functions must not
        // use context properties, as they are undefined
        const context = try allocator.create(Context);

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

        const genInfo = try codegen.GenInfo.init(allocator, context);
        const genInfoPtr = try utils.createMut(codegen.GenInfo, allocator, genInfo);
        genInfoPtr.vmInfo.stackStartSize = compInfoPtr.stackSizeEstimate;

        const deferCleanup = try DeferCleanup.init(allocator);
        const deferCleanupPtr = try utils.createMut(DeferCleanup, allocator, deferCleanup);

        const constTypeInfos = try StaticPtrs.init(poolsPtr);
        const constTypeInfosPtr = try utils.createMut(StaticPtrs, allocator, constTypeInfos);

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
            .staticPtrs = constTypeInfosPtr,
            .code = code,
        };

        return context;
    }

    pub fn clear(self: *Self) void {
        self.compInfo.clearPoolMem();
        // self.genInfo.cleanup();
        // self.deferCleanup.clear();
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
        self.compInfo.deinit();
        self.genInfo.deinit();
        self.deferCleanup.deinit();
        self.staticPtrs.deinit(self.pools);
        self.pools.deinit();

        self.allocator.destroy(self.pools);
        self.allocator.destroy(self.tokenUtil);
        self.allocator.destroy(self.logger);
        self.allocator.destroy(self.compInfo);
        self.allocator.destroy(self.scanInfo);
        self.allocator.destroy(self.genInfo);
        self.allocator.destroy(self.deferCleanup);
        self.allocator.destroy(self.staticPtrs);
    }

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }
};

pub const StaticPtrs = struct {
    const Self = @This();

    types: struct {
        voidType: blitzAst.AstTypeInfo,
        boolType: blitzAst.AstTypeInfo,
        anyType: blitzAst.AstTypeInfo,
        f32Type: blitzAst.AstTypeInfo,
        u64Type: blitzAst.AstTypeInfo,
        undefType: blitzAst.AstTypeInfo,
    },
    nodes: struct {
        noOp: *blitzAst.AstNode,
        structPlaceholder: *blitzAst.AstNode,
        breakNode: *blitzAst.AstNode,
        continueNode: *blitzAst.AstNode,
    },

    pub fn init(pools: *allocPools.Pools) !Self {
        return .{
            .types = .{
                .voidType = (try pools.types.new(.Void)).toTypeInfo(.Const),
                .boolType = (try pools.types.new(.Bool)).toTypeInfo(.Mut),
                .anyType = (try pools.types.new(.Any)).toTypeInfo(.Mut),
                .f32Type = (try pools.types.new(.{ .Number = .F32 })).toTypeInfo(.Mut),
                .u64Type = (try pools.types.new(.{ .Number = .U64 })).toTypeInfo(.Mut),
                .undefType = (try pools.types.new(.{ .Undef = {} })).toTypeInfo(.Mut),
            },
            .nodes = .{
                .noOp = try pools.nodes.new((blitzAst.AstNodeUnion{ .NoOp = {} }).toAstNode()),
                .structPlaceholder = try pools.nodes.new((blitzAst.AstNodeUnion{
                    .StructPlaceholder = {},
                }).toAstNode()),
                .breakNode = try pools.nodes.new((blitzAst.AstNodeUnion{
                    .Break = {},
                }).toAstNode()),
                .continueNode = try pools.nodes.new((blitzAst.AstNodeUnion{
                    .Continue = {},
                }).toAstNode()),
            },
        };
    }

    pub fn deinit(self: *Self, pools: *allocPools.Pools) void {
        const allPtrs1 = .{
            self.types.voidType,
            self.types.boolType,
            self.types.anyType,
            self.types.f32Type,
            self.types.u64Type,
            self.types.undefType,
        };

        const allPtrs2 = .{
            self.nodes.noOp,
            self.nodes.structPlaceholder,
            self.nodes.breakNode,
            self.nodes.continueNode,
        };

        inline for (allPtrs1) |ptr| {
            pools.types.release(ptr.astType);
        }

        inline for (allPtrs2) |ptr| {
            pools.nodes.release(ptr);
        }
    }

    pub fn isStaticPtr(self: Self, ptr: anytype) bool {
        if (@TypeOf(ptr) == *blitzAst.AstTypes) {
            const staticTypes = .{
                self.types.voidType,
                self.types.boolType,
                self.types.anyType,
                self.types.f32Type,
                self.types.u64Type,
                self.types.undefType,
            };

            inline for (staticTypes) |t| {
                if (t.astType == ptr) return true;
            }
        } else if (@TypeOf(ptr) == *blitzAst.AstNode) {
            const staticNodes = .{
                self.nodes.noOp,
                self.nodes.structPlaceholder,
                self.nodes.breakNode,
                self.nodes.continueNode,
            };

            inline for (staticNodes) |t| {
                if (t == ptr) return true;
            }
        } else {
            @compileError("Expected *AstTypes or *AstNode in 'isStaticPtr'");
        }

        return false;
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

    pub fn clear(self: *Self) void {
        self.slices.strings.cleanup();
        self.slices.nodeSlices.cleanup();
        self.slices.typeInfoSlices.cleanup();
        self.slices.genericTypeSlices.cleanup();
        self.slices.attrDefSlices.cleanup();
    }

    pub fn deinit(self: *Self) void {
        self.slices.strings.deinit();
        self.slices.nodeSlices.deinit();
        self.slices.typeInfoSlices.deinit();
        self.slices.genericTypeSlices.deinit();
        self.slices.attrDefSlices.deinit();
    }
};

fn DeferedSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        slices: *ArrayList(T),

        pub fn init(allocator: Allocator) !Self {
            const ptr = try utils.createMut(ArrayList(T), allocator, .empty);

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

        pub fn cleanup(self: *Self) void {
            for (self.slices.items) |slice| {
                self.allocator.free(slice);
            }
            self.slices.clearRetainingCapacity();
        }

        pub fn append(self: *Self, slice: T) !void {
            try self.slices.append(self.allocator, slice);
        }
    };
}
