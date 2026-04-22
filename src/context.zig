const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;
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

const ContextSettings = struct {
    debug: struct {
        printAddresses: bool = false,
        trackPoolMem: bool = builtin.mode == .Debug,

        // printLabels: bool = false,
        // printNoOps: bool = false,
        // printSkippedInstrs: bool = false,
        // printInsertedTag: bool = false,
        printLabels: bool = true,
        printNoOps: bool = true,
        printSkippedInstrs: bool = true,
        printInsertedTag: bool = true,

        allocateRegisters: bool = true,
        // allocateRegisters: bool = false,
    } = .{},
};

pub const Context = struct {
    const Self = @This();

    tokens: []tokenizer.Token,
    pools: allocPools.Pools,
    logger: logger.Logger,
    tokenUtil: tokenizer.TokenUtil,
    compInfo: blitzCompInfo.CompInfo,
    scanBehavior: scanner.ScanBehavior = .{},
    genInfo: codegen.GenInfo,
    deferCleanup: DeferCleanup,
    staticPtrs: StaticPtrs,
    code: []const u8,
    utils: ContextUtils,
    settings: ContextSettings,

    pub inline fn init(
        allocator: Allocator,
        code: []const u8,
        writer: *Writer,
        settings: ContextSettings,
    ) !Self {
        var pools = try allocPools.Pools.init(allocator);
        const tokens = try tokenizer.tokenize(allocator, code, writer);
        const names = try ast.findHoistedInfo(allocator, tokens, code);

        const tokenUtil = tokenizer.TokenUtil.init(tokens);
        const loggerUtil = logger.Logger.init(tokenUtil, code);

        const compInfo = try blitzCompInfo.CompInfo.init(allocator, names);

        var genInfo = try codegen.GenInfo.init(allocator);
        genInfo.vmInfo.stackStartSize = compInfo.stackSizeEstimate;

        const constTypeInfos = try StaticPtrs.init(&pools);
        const contextUtils = ContextUtils.init(allocator);

        return .{
            .pools = pools,
            .logger = loggerUtil,
            .tokens = tokens,
            .tokenUtil = tokenUtil,
            .compInfo = compInfo,
            .genInfo = genInfo,
            .deferCleanup = .empty,
            .staticPtrs = constTypeInfos,
            .code = code,
            .utils = contextUtils,
            .settings = settings,
        };
    }

    /// call to free unused pool mem
    pub fn clearPoolMem(self: *Self) void {
        self.compInfo.clearPoolMem(self);
    }

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }

    pub fn releasePoolType(self: *Self, ptr: *ast.AstTypes) void {
        if (self.settings.debug.trackPoolMem) {
            self.utils.releaseTypeAddress(ptr);
        }
    }

    pub fn releasePoolNode(self: *Self, ptr: *ast.AstNode) void {
        if (self.settings.debug.trackPoolMem) {
            self.utils.releaseNodeAddress(ptr);
        }
    }
};

pub const ContextUtils = struct {
    const Self = @This();
    const UsedNodeAddresses = std.AutoHashMap(*ast.AstNode, void);
    const UsedTypeAddresses = std.AutoHashMap(*ast.AstTypes, void);

    usedNodes: UsedNodeAddresses,
    usedTypes: UsedTypeAddresses,

    pub inline fn init(allocator: Allocator) Self {
        const usedNodes = UsedNodeAddresses.init(allocator);
        const usedTypes = UsedTypeAddresses.init(allocator);

        return .{
            .usedNodes = usedNodes,
            .usedTypes = usedTypes,
        };
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
        return .{
            .types = .{
                .voidType = (try pools.newTypeUntracked(.Void)).toTypeInfo(.Const),
                .boolType = (try pools.newTypeUntracked(.Bool)).toTypeInfo(.Mut),
                .anyType = (try pools.newTypeUntracked(.Any)).toTypeInfo(.Mut),
                .f32Type = (try pools.newTypeUntracked(
                    .{ .Number = .F32 },
                )).toTypeInfo(.Mut),
                .u64Type = (try pools.newTypeUntracked(
                    .{ .Number = .U64 },
                )).toTypeInfo(.Mut),
                .undefType = (try pools.newTypeUntracked(
                    .{ .Undef = {} },
                )).toTypeInfo(.Mut),
                .charType = (try pools.newTypeUntracked(
                    .{ .Char = {} },
                )).toTypeInfo(.Mut),
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

    strings: ArrayList([]const u8),
    nodeSlices: ArrayList([]*ast.AstNode),
    typeInfoSlices: ArrayList([]ast.AstTypeInfo),
    genericTypeSlices: ArrayList([]ast.GenericType),
    attrDefSlices: ArrayList([]ast.AttributeDefinition),

    pub const empty: Self = .{
        .strings = .empty,
        .nodeSlices = .empty,
        .typeInfoSlices = .empty,
        .genericTypeSlices = .empty,
        .attrDefSlices = .empty,
    };
};
