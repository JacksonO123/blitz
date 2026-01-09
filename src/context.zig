const std = @import("std");
const builtin = @import("builtin");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
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

const ContextSettings = struct {
    debug: struct {
        printAddresses: bool = false,
        trackPoolMem: bool = builtin.mode == .Debug,
    } = .{},
};

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
    utils: *ContextUtils,
    settings: ContextSettings,

    pub fn init(
        allocator: Allocator,
        code: []const u8,
        writer: *Writer,
        settings: ContextSettings,
    ) !*Self {
        // NOTE - context pointer is passed around before initialization
        // this is intentional, these utility structs need to store the memory
        // location which is allocated before initialization
        // to make this pattern safe, all of these util init functions must not
        // use context properties, as they are undefined
        const context = try allocator.create(Context);

        const pools = try allocPools.Pools.init(allocator, context);
        const poolsPtr = try utils.createMut(allocPools.Pools, allocator, pools);

        const tokens = try tokenizer.tokenize(allocator, code, writer);

        const names = try ast.findStructsAndErrors(allocator, tokens, code);

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

        const constTypeInfos = try StaticPtrs.init(poolsPtr);
        const constTypeInfosPtr = try utils.createMut(StaticPtrs, allocator, constTypeInfos);

        const contextUtils = try ContextUtils.init(allocator, context);
        const contextUtilsPtr = try utils.createMut(ContextUtils, allocator, contextUtils);

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
            .utils = contextUtilsPtr,
            .settings = settings,
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
        self.utils.deinit();

        self.allocator.destroy(self.pools);
        self.allocator.destroy(self.tokenUtil);
        self.allocator.destroy(self.logger);
        self.allocator.destroy(self.compInfo);
        self.allocator.destroy(self.scanInfo);
        self.allocator.destroy(self.genInfo);
        self.allocator.destroy(self.deferCleanup);
        self.allocator.destroy(self.staticPtrs);
        self.allocator.destroy(self.utils);
    }

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }
};

pub const ContextUtils = struct {
    const Self = @This();
    const UsedNodeAddresses = std.AutoHashMap(*ast.AstNode, void);
    const UsedTypeAddresses = std.AutoHashMap(*ast.AstTypes, void);

    allocator: Allocator,
    context: *Context,
    usedNodes: *UsedNodeAddresses,
    usedTypes: *UsedTypeAddresses,

    pub fn init(allocator: Allocator, context: *Context) !Self {
        const usedNodes = UsedNodeAddresses.init(allocator);
        const usedTypes = UsedTypeAddresses.init(allocator);

        return .{
            .allocator = allocator,
            .context = context,
            .usedNodes = try utils.createMut(UsedNodeAddresses, allocator, usedNodes),
            .usedTypes = try utils.createMut(UsedTypeAddresses, allocator, usedTypes),
        };
    }

    pub fn deinit(self: *Self) void {
        self.usedNodes.deinit();
        self.allocator.destroy(self.usedNodes);

        self.usedTypes.deinit();
        self.allocator.destroy(self.usedTypes);
    }

    pub fn reserveNodeAddress(self: *Self, addr: *ast.AstNode) !void {
        try self.usedNodes.put(addr, {});
    }

    pub fn reserveTypeAddress(self: *Self, addr: *ast.AstTypes) !void {
        try self.usedTypes.put(addr, {});
    }

    pub fn releaseNodeAddress(self: *Self, addr: *ast.AstNode) void {
        _ = self.usedNodes.remove(addr);
    }

    pub fn releaseTypeAddress(self: *Self, addr: *ast.AstTypes) void {
        _ = self.usedTypes.remove(addr);
    }
};

pub const StaticPtrs = struct {
    const Self = @This();

    types: struct {
        voidType: ast.AstTypeInfo,
        boolType: ast.AstTypeInfo,
        anyType: ast.AstTypeInfo,
        f32Type: ast.AstTypeInfo,
        u64Type: ast.AstTypeInfo,
        undefType: ast.AstTypeInfo,
        charType: ast.AstTypeInfo,
    },
    nodes: struct {
        noOp: *ast.AstNode,
        structPlaceholder: *ast.AstNode,
        breakNode: *ast.AstNode,
        continueNode: *ast.AstNode,
    },

    pub fn init(pools: *allocPools.Pools) !Self {
        // called before context is initialized, so any calls to reserve types
        // or nodes must not use context

        return .{
            .types = .{
                .voidType = (try pools.newTypeUntracked(.Void)).toTypeInfo(.Const),
                .boolType = (try pools.newTypeUntracked(.Bool)).toTypeInfo(.Mut),
                .anyType = (try pools.newTypeUntracked(.Any)).toTypeInfo(.Mut),
                .f32Type = (try pools.newTypeUntracked(.{ .Number = .F32 })).toTypeInfo(.Mut),
                .u64Type = (try pools.newTypeUntracked(.{ .Number = .U64 })).toTypeInfo(.Mut),
                .undefType = (try pools.newTypeUntracked(.{ .Undef = {} })).toTypeInfo(.Mut),
                .charType = (try pools.newTypeUntracked(.{ .Char = {} })).toTypeInfo(.Mut),
            },
            .nodes = .{
                .noOp = try pools.newNodeUntracked((ast.AstNodeUnion{
                    .NoOp = {},
                }).toAstNode()),
                .structPlaceholder = try pools.newNodeUntracked((ast.AstNodeUnion{
                    .StructPlaceholder = {},
                }).toAstNode()),
                .breakNode = try pools.newNodeUntracked((ast.AstNodeUnion{
                    .Break = {},
                }).toAstNode()),
                .continueNode = try pools.newNodeUntracked((ast.AstNodeUnion{
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
            pools.types.destroy(ptr.astType);
        }

        inline for (allPtrs2) |ptr| {
            pools.nodes.destroy(ptr);
        }
    }

    pub fn isStaticPtr(self: Self, ptr: anytype) bool {
        if (@TypeOf(ptr) == *ast.AstTypes) {
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
        } else if (@TypeOf(ptr) == *ast.AstNode) {
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
        nodeSlices: DeferedSlice([]*ast.AstNode),
        typeInfoSlices: DeferedSlice([]ast.AstTypeInfo),
        genericTypeSlices: DeferedSlice([]ast.GenericType),
        attrDefSlices: DeferedSlice([]ast.AttributeDefinition),
    },

    pub fn init(allocator: Allocator) !Self {
        return .{
            .allocator = allocator,
            .slices = .{
                .strings = try DeferedSlice([]const u8).init(allocator),
                .nodeSlices = try DeferedSlice([]*ast.AstNode).init(allocator),
                .typeInfoSlices = try DeferedSlice([]ast.AstTypeInfo).init(allocator),
                .genericTypeSlices = try DeferedSlice([]ast.GenericType).init(allocator),
                .attrDefSlices = try DeferedSlice([]ast.AttributeDefinition).init(allocator),
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
