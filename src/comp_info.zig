const std = @import("std");
const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const logger = blitz.logger;
const builtins = blitz.builtins;
const clone = blitz.clone;
const scanner = blitz.scanner;
const vmInfo = blitz.vmInfo;
const pools = blitz.allocPools;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;
const Writer = std.Io.Writer;

fn ScopeDeinitFn(comptime T: type) type {
    return fn (*Context, T, pools.ReleaseType) void;
}

fn initScopeUtil(
    comptime T: type,
    comptime Fn: ScopeDeinitFn(*T),
    allocator: Allocator,
) !*ScopeUtil(*T, Fn) {
    const baseScope = try utils.initMutPtrT(T, allocator);
    const scopeUtil = try ScopeUtil(*T, Fn).init(allocator);
    const scopePtr = try utils.createMut(ScopeUtil(*T, Fn), allocator, scopeUtil);
    try scopePtr.add(allocator, baseScope, false);
    return scopePtr;
}

fn initScopeUtilWithBase(
    comptime T: type,
    comptime Fn: ScopeDeinitFn(*T),
    allocator: Allocator,
    base: T,
) !*ScopeUtil(*T, Fn) {
    const baseScope = try utils.createMut(T, allocator, base);
    const scopeUtil = try ScopeUtil(*T, Fn).init(allocator);
    const scopePtr = try utils.createMut(ScopeUtil(*T, Fn), allocator, scopeUtil);
    try scopePtr.add(allocator, baseScope, false);
    return scopePtr;
}

const VarTypeAndUsedInfo = struct {
    varTypeAndAllocInfo: scanner.TypeAndAllocInfo,
    lastUsedNode: ?*ast.AstNode,
};

pub const VarScope = StringHashMap(VarTypeAndUsedInfo);
pub const CaptureScope = StringHashMap(scanner.TypeAndAllocInfo);
pub const TypeScope = StringHashMap(scanner.TypeAndAllocInfo);
pub const StringListScope = ArrayList([]const u8);

const ToScanItem = struct {
    func: *ast.FuncDecNode,
    genTypes: []ast.StrToTypeInfoRel,
    withGenDef: bool,
};
const ToScanStack = ArrayList(ToScanItem);

const ScopeType = enum {
    Normal,
    Loop,
    Function,
};

pub const CompInfo = struct {
    const Self = @This();

    hoistedDecs: struct {
        structs: *StringHashMap(?*ast.StructDecNode),
        errors: *StringHashMap(?*const ast.ErrorOrEnumDecNode),
        enums: *StringHashMap(?*const ast.ErrorOrEnumDecNode),
    },
    // variableScopes store VarInfo as allocated, but return as recycled to preserve source
    variableScopes: *ScopeUtil(*VarScope, pools.releaseVariableScope),
    variableCaptures: *ScopeUtil(*CaptureScope, pools.releaseVariableCaptures),
    genericCaptures: *ScopeUtil(*TypeScope, pools.releaseGenericCaptures),
    functionCaptures: *ScopeUtil(*StringListScope, pools.NoopReleaseScope),
    parsedGenerics: *ScopeUtil(*StringListScope, pools.NoopReleaseScope),
    scopeTypes: *ArrayList(ScopeType),
    functions: *StringHashMap(*ast.FuncDecNode),
    functionsInScope: *ScopeUtil(*StringListScope, pools.NoopReleaseScope),
    functionsToScan: *ToScanStack,
    genericScopes: *ScopeUtil(*TypeScope, pools.releaseGenericScope),
    returnInfo: *ReturnInfo,
    builtins: builtins.BuiltinFuncMemo,
    stackSizeEstimate: vmInfo.StartStackType,
    preAst: bool,

    pub fn init(
        allocator: Allocator,
        names: ast.HoistedNames,
    ) !Self {
        const functionsToScan = try utils.createMut(ToScanStack, allocator, .empty);
        const scopeTypesPtr = try utils.createMut(ArrayList(ScopeType), allocator, .empty);
        try scopeTypesPtr.append(allocator, .Normal);

        const functions = try utils.initMutPtrT(StringHashMap(*ast.FuncDecNode), allocator);

        const genericScopes = try initScopeUtil(
            TypeScope,
            pools.releaseGenericScope,
            allocator,
        );
        const variableScopes = try initScopeUtil(
            VarScope,
            pools.releaseVariableScope,
            allocator,
        );
        const variableCaptures = try initScopeUtil(
            CaptureScope,
            pools.releaseVariableCaptures,
            allocator,
        );
        const genericCaptures = try initScopeUtil(
            TypeScope,
            pools.releaseGenericCaptures,
            allocator,
        );
        const functionCaptures = try initScopeUtilWithBase(
            StringListScope,
            pools.NoopReleaseScope,
            allocator,
            .empty,
        );
        const parsedGenerics = try initScopeUtilWithBase(
            StringListScope,
            pools.NoopReleaseScope,
            allocator,
            .empty,
        );
        const functionsInScope = try initScopeUtilWithBase(
            StringListScope,
            pools.NoopReleaseScope,
            allocator,
            .empty,
        );

        const returnInfoUtil = try ReturnInfo.init(allocator);
        const returnInfo = try utils.createMut(ReturnInfo, allocator, returnInfoUtil);

        const hoistedStructNames = try utils.createMut(
            StringHashMap(?*ast.StructDecNode),
            allocator,
            StringHashMap(?*ast.StructDecNode).init(allocator),
        );
        const hoistedErrorNames = try utils.createMut(
            StringHashMap(?*const ast.ErrorOrEnumDecNode),
            allocator,
            StringHashMap(?*const ast.ErrorOrEnumDecNode).init(allocator),
        );
        const hoistedEnumNames = try utils.createMut(
            StringHashMap(?*const ast.ErrorOrEnumDecNode),
            allocator,
            StringHashMap(?*const ast.ErrorOrEnumDecNode).init(allocator),
        );

        for (names.structNames) |name| {
            try hoistedStructNames.put(name, null);
        }

        for (names.errorNames) |name| {
            try hoistedErrorNames.put(name, null);
        }

        for (names.enumNames) |name| {
            try hoistedEnumNames.put(name, null);
        }

        return Self{
            .hoistedDecs = .{
                .structs = hoistedStructNames,
                .errors = hoistedErrorNames,
                .enums = hoistedEnumNames,
            },
            .variableScopes = variableScopes,
            .variableCaptures = variableCaptures,
            .functionCaptures = functionCaptures,
            .genericCaptures = genericCaptures,
            .parsedGenerics = parsedGenerics,
            .scopeTypes = scopeTypesPtr,
            .functions = functions,
            .functionsInScope = functionsInScope,
            .functionsToScan = functionsToScan,
            .genericScopes = genericScopes,
            .preAst = true,
            .returnInfo = returnInfo,
            .builtins = .{},
            .stackSizeEstimate = 1024, // 1kb
        };
    }

    pub fn clearPoolMem(self: *Self, context: *Context) void {
        var functionIt = self.functions.valueIterator();
        while (functionIt.next()) |f| {
            pools.releaseFuncDec(context, f.*);
        }

        var structsIt = self.hoistedDecs.structs.valueIterator();
        while (structsIt.next()) |dec| {
            pools.releaseStructDec(context, dec.*.?);
        }

        self.variableScopes.clear(context);
        self.variableCaptures.clear(context);
        self.functionCaptures.clear(context);
        self.genericCaptures.clear(context);
        self.genericScopes.clear(context);

        self.returnInfo.clear(context);
    }

    pub fn getScopeDepth(self: Self) usize {
        return self.variableScopes.scopes.items.len;
    }

    pub fn pushScope(self: *Self, allocator: Allocator, leak: bool) !void {
        try self.pushScopeWithType(allocator, leak, .Normal);
    }

    pub fn pushScopeWithType(
        self: *Self,
        allocator: Allocator,
        leak: bool,
        scopeType: ScopeType,
    ) !void {
        const scope = try utils.initMutPtrT(VarScope, allocator);
        try self.variableScopes.add(allocator, scope, leak);
        try self.pushScopedFunctionScope(allocator, leak);
        try self.scopeTypes.append(allocator, scopeType);
    }

    pub fn popScope(self: *Self, context: *Context) void {
        self.variableScopes.pop(context);
        self.popScopedFunctionScope(context);
        _ = self.scopeTypes.pop();
    }

    pub fn addCaptureScope(self: *Self, allocator: Allocator) !void {
        try self.variableScopes.addCaptureIndex(allocator);
        try self.functionsInScope.addCaptureIndex(allocator);

        const newVarScope = try utils.initMutPtrT(CaptureScope, allocator);
        const newFuncScope = try utils.createMut(StringListScope, allocator, .empty);

        try self.variableCaptures.add(allocator, newVarScope, false);
        try self.functionCaptures.add(allocator, newFuncScope, false);
    }

    pub fn popCaptureScope(self: *Self, context: *Context) void {
        self.variableCaptures.pop(context);
        self.functionCaptures.pop(context);
    }

    pub fn addGenericCaptureScope(self: *Self, allocator: Allocator) !void {
        try self.genericScopes.addCaptureIndex(allocator);
        const newScope = try utils.initMutPtrT(TypeScope, allocator);
        try self.genericCaptures.add(allocator, newScope, false);
    }

    pub fn popGenericCaptureScope(self: *Self, context: *Context) void {
        self.genericCaptures.pop(context);
    }

    pub fn consumeVariableCaptures(self: *Self) ?*CaptureScope {
        return self.variableCaptures.release();
    }

    pub fn consumeGenericCaptures(self: *Self) ?*TypeScope {
        return self.genericCaptures.release();
    }

    pub fn consumeFunctionCaptures(self: *Self) ?*StringListScope {
        return self.functionCaptures.release();
    }

    pub fn pushScopedFunctionScope(self: *Self, allocator: Allocator, leak: bool) !void {
        const scope = try utils.createMut(StringListScope, allocator, .empty);
        try self.functionsInScope.add(allocator, scope, leak);
    }

    pub fn popScopedFunctionScope(self: *Self, context: *Context) void {
        self.functionsInScope.pop(context);
    }

    pub fn addScopedFunction(self: *Self, allocator: Allocator, name: []const u8) !void {
        const scope = self.functionsInScope.getCurrentScope();
        if (scope) |s| {
            try s.append(allocator, name);
        }
    }

    pub fn pushParsedGenericsScope(self: *Self, allocator: Allocator, leak: bool) !void {
        const newScope = try utils.createMut(StringListScope, allocator, .empty);
        try self.parsedGenerics.add(allocator, newScope, leak);
    }

    pub fn popParsedGenericsScope(self: *Self, context: *Context) void {
        self.parsedGenerics.pop(context);
    }

    pub fn addParsedGeneric(self: *Self, allocator: Allocator, gen: []const u8) !void {
        const scope = self.parsedGenerics.getCurrentScope();
        if (scope) |s| {
            try s.append(allocator, gen);
        }
    }

    pub fn hasParsedGeneric(self: Self, gen: []const u8) bool {
        var scope: ?*StringListScope = self.parsedGenerics.getCurrentScope();
        defer self.parsedGenerics.resetLeakIndex();

        while (scope) |s| {
            if (utils.inStringArr(s.items, gen)) {
                return true;
            }

            const nextLeak = self.parsedGenerics.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
            } else {
                scope = null;
            }
        }

        return false;
    }

    pub fn prepareForAst(self: *Self, allocator: Allocator, context: *Context, writer: *Writer) !void {
        self.preAst = false;

        {
            var structIt = self.hoistedDecs.structs.valueIterator();
            while (structIt.next()) |s| {
                const structPtr = s.*.?;
                const attributes = structPtr.*.attributes;
                var members = try ArrayList(ast.StructAttribute).initCapacity(
                    allocator,
                    attributes.len,
                );

                for (attributes) |attr| {
                    if (attr.attr != .Member) continue;
                    try members.append(allocator, attr);
                }

                const arr = try members.toOwnedSlice(allocator);

                structPtr.*.totalMemberList = arr;
            }
        }

        {
            var structIt = self.hoistedDecs.structs.valueIterator();
            while (structIt.next()) |s| {
                const structPtr = s.*.?;
                const attributes = structPtr.*.attributes;
                try self.pushParsedGenericsScope(allocator, false);
                defer self.popParsedGenericsScope(context);

                for (structPtr.*.generics) |g| {
                    try self.addParsedGeneric(allocator, g.name);
                }

                for (attributes) |attr| {
                    if (attr.attr != .Function) continue;

                    const f = attr.attr.Function;

                    if (f.generics) |funcGens| {
                        for (funcGens) |generic| {
                            try self.addParsedGeneric(allocator, generic.name);
                        }
                    }

                    const prevTokenUtil = context.tokenUtil;
                    const tempTokens = try utils.createMut(
                        tokenizer.TokenUtil,
                        allocator,
                        tokenizer.TokenUtil.init(f.bodyTokens),
                    );
                    context.tokenUtil = tempTokens;
                    pools.recursiveReleaseNodeAll(context, f.body);
                    f.body = ast.parseSequence(allocator, context, true) catch |e| {
                        logger.logParseError(context, e, writer);
                        return e;
                    };

                    context.tokenUtil = prevTokenUtil;
                    allocator.destroy(tempTokens);
                }
            }
        }

        context.tokenUtil.reset();
    }

    pub fn hasStruct(self: Self, name: []const u8) bool {
        return self.hoistedDecs.structs.contains(name);
    }

    pub fn hasError(self: Self, name: []const u8) bool {
        return self.hoistedDecs.errors.contains(name);
    }

    pub fn hasEnum(self: Self, name: []const u8) bool {
        return self.hoistedDecs.enums.contains(name);
    }

    pub fn pushGenScope(self: *Self, allocator: Allocator, leak: bool) !void {
        const genScope = try utils.initMutPtrT(TypeScope, allocator);
        try self.genericScopes.add(allocator, genScope, leak);
    }

    pub fn popGenScope(self: *Self, context: *Context) void {
        self.genericScopes.pop(context);
    }

    pub fn setGeneric(self: *Self, name: []const u8, gType: scanner.TypeAndAllocInfo) !void {
        if (gType.info.astType.* == .VarInfo) {
            return scanner.ScanError.CannotSetGenericToVarInfo;
        }

        const genScope = self.genericScopes.getCurrentScope();

        if (genScope) |scope| {
            try scope.put(name, gType);
        }
    }

    pub fn getGeneric(self: *Self, allocator: Allocator, context: *Context, name: []const u8) !?scanner.TypeAndAllocInfo {
        var genScope: ?*TypeScope = self.genericScopes.getCurrentScope();
        defer self.genericScopes.resetLeakIndex();
        var capture = false;

        while (genScope) |s| {
            if (s.get(name)) |t| {
                var copy = t;
                copy.allocState = .Recycled;

                if (!capture) {
                    return copy;
                }

                const captureScope = self.genericCaptures.getCurrentScope();
                if (captureScope) |capScope| {
                    const clonedType = try clone.cloneAstTypeInfo(
                        allocator,
                        context,
                        copy.info,
                        true,
                    );
                    try capScope.put(name, clonedType.toAllocInfo(.Recycled));
                }

                return copy;
            }

            const nextLeak = self.genericScopes.getNextInLeak();
            if (nextLeak) |next| {
                genScope = next.scope;
                capture = next.capture;
            } else {
                genScope = null;
            }
        }

        return null;
    }

    pub fn hasGeneric(self: Self, name: []const u8) bool {
        var scope: ?*TypeScope = self.genericScopes.getCurrentScope();
        defer self.genericScopes.resetLeakIndex();

        while (scope) |s| {
            if (s.get(name) != null) {
                return true;
            }

            const nextLeak = self.genericScopes.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
            } else {
                scope = null;
            }
        }

        return false;
    }

    pub fn setVariableType(
        self: *Self,
        context: *Context,
        name: []const u8,
        info: scanner.TypeAndAllocInfo,
        decNode: ?*ast.AstNode,
        mutState: scanner.MutState,
    ) !void {
        if (info.info.astType.* == .VarInfo) {
            return scanner.ScanError.NestedVarInfoDetected;
        }

        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            const varType = try context.pools.newType(context, .{
                .VarInfo = info,
            });
            const varInfo = varType.toAllocInfo(mutState, .Allocated);
            const varTypeAndUsedInfo = VarTypeAndUsedInfo{
                .varTypeAndAllocInfo = varInfo,
                .lastUsedNode = decNode,
            };
            try s.put(name, varTypeAndUsedInfo);
        }
    }

    fn getVariableTypeInfo(
        self: *Self,
        name: []const u8,
    ) !?struct { scope: *VarScope, shouldCapture: bool, varTypeUsedInfo: VarTypeAndUsedInfo } {
        var scope: ?*VarScope = self.variableScopes.getCurrentScope();
        defer self.variableScopes.resetLeakIndex();
        var capture = false;

        while (scope) |s| {
            if (s.get(name)) |t| {
                return .{
                    .scope = s,
                    .shouldCapture = capture,
                    .varTypeUsedInfo = t,
                };
            }

            const nextLeak = self.variableScopes.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
                capture = next.capture;
            } else {
                scope = null;
            }
        }

        return null;
    }

    pub fn getVariableType(
        self: *Self,
        allocator: Allocator,
        context: *Context,
        name: []const u8,
        replaceGenerics: bool,
    ) !?scanner.TypeAndAllocInfo {
        const varTypeInfo = (try self.getVariableTypeInfo(name)) orelse return null;

        var copy = varTypeInfo.varTypeUsedInfo;
        copy.varTypeAndAllocInfo.allocState = .Recycled;

        if (!varTypeInfo.shouldCapture) {
            return copy.varTypeAndAllocInfo;
        }

        const captureScope = self.variableCaptures.getCurrentScope();
        if (captureScope) |capScope| {
            const clonedType = try clone.cloneAstTypeInfo(
                allocator,
                context,
                copy.varTypeAndAllocInfo.info,
                replaceGenerics,
            );
            try capScope.put(name, clonedType.toAllocInfo(.Allocated));
        }

        return copy.varTypeAndAllocInfo;
    }

    pub fn setVariableLastUsedNode(self: *Self, name: []const u8, node: *ast.AstNode) !void {
        var varTypeInfo = (try self.getVariableTypeInfo(name)) orelse return;
        varTypeInfo.varTypeUsedInfo.lastUsedNode = node;
        try varTypeInfo.scope.put(name, varTypeInfo.varTypeUsedInfo);
    }

    pub fn isVariableInScope(self: *Self, name: []const u8) bool {
        var scope: ?*VarScope = self.variableScopes.getCurrentScope();
        defer self.variableScopes.resetLeakIndex();

        while (scope) |s| {
            if (s.contains(name)) {
                return true;
            }

            const nextLeak = self.variableScopes.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
            } else {
                scope = null;
            }
        }

        return false;
    }

    pub fn getVariableTypeFixed(self: *Self, name: []const u8) ?scanner.TypeAndAllocInfo {
        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            if (s.get(name)) |t| {
                return t.varTypeAndAllocInfo;
            }
        }

        return null;
    }

    pub fn addFunction(self: *Self, name: []const u8, dec: *ast.FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn addFuncToScan(
        self: *Self,
        allocator: Allocator,
        func: *ast.FuncDecNode,
        rels: []ast.StrToTypeInfoRel,
        withGenDef: bool,
    ) !void {
        try self.functionsToScan.append(allocator, .{
            .func = func,
            .genTypes = rels,
            .withGenDef = withGenDef,
        });
    }

    pub fn getFunction(self: Self, allocator: Allocator, name: []const u8) !?*ast.FuncDecNode {
        const func = self.getFunctionAsGlobal(name);
        const captureScope = self.functionCaptures.getCurrentScope();
        if (func) |funcDec| a: {
            if (!funcDec.globallyDefined) break :a;

            if (captureScope) |capScope| {
                if (!utils.inStringArr(capScope.items, name)) {
                    try capScope.append(allocator, name);
                }
            }

            return func;
        }

        var scope: ?*StringListScope = self.functionsInScope.getCurrentScope();
        defer self.functionsInScope.resetLeakIndex();
        var capture = false;

        while (scope) |s| {
            for (s.items) |item| {
                if (!utils.compString(item, name)) {
                    continue;
                }

                if (func) |funcDec| {
                    if (!capture) {
                        return func;
                    }

                    if (captureScope) |capScope| {
                        if (!utils.inStringArr(capScope.items, name)) {
                            try capScope.append(allocator, name);
                        }
                    }

                    return funcDec;
                }

                return null;
            }

            const nextLeak = self.functionsInScope.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
                capture = next.capture;
            } else {
                scope = null;
            }
        }

        return null;
    }

    pub fn getFunctionAsGlobal(self: Self, name: []const u8) ?*ast.FuncDecNode {
        return self.functions.get(name);
    }

    pub fn setHoistedNodes(self: *Self, hoistedNodes: ast.HoistedNodes) !void {
        for (hoistedNodes.structs) |node| {
            const structDec = node.variant.StructDec;
            try self.hoistedDecs.structs.put(structDec.name, structDec);
        }

        for (hoistedNodes.errors) |dec| {
            const errorDec = dec.variant.ErrorDec;
            try self.hoistedDecs.errors.put(errorDec.name, errorDec);
        }

        for (hoistedNodes.enums) |dec| {
            const enumDec = dec.variant.EnumDec;
            try self.hoistedDecs.enums.put(enumDec.name, enumDec);
        }
    }

    pub fn getStructDec(self: Self, name: []const u8) ?*const ast.StructDecNode {
        return self.hoistedDecs.structs.get(name) orelse null;
    }

    pub fn getErrorDec(self: Self, name: []const u8) ?*const ast.ErrorOrEnumDecNode {
        return self.hoistedDecs.errors.get(name) orelse null;
    }

    pub fn getEnumDec(self: Self, name: []const u8) ?*const ast.ErrorOrEnumDecNode {
        return self.hoistedDecs.enums.get(name) orelse null;
    }

    pub fn inLoopScope(self: Self) bool {
        const scopeTypes = self.scopeTypes.items;
        var current = scopeTypes.len - 1;
        while (true) : (current -= 1) {
            switch (scopeTypes[current]) {
                .Loop => return true,
                .Function => break,
                else => {},
            }

            if (current == 0) break;
        }

        return false;
    }
};

fn ScopeInfo(comptime T: type) type {
    return struct {
        scope: T,
        capture: bool,
    };
}

fn ScopeUtil(comptime T: type, freeFn: ScopeDeinitFn(T)) type {
    return struct {
        const Self = @This();

        scopes: *ArrayList(T),
        captureIndexes: *ArrayList(usize),
        leaks: *ArrayList(usize),
        current: usize,

        pub fn init(allocator: Allocator) !Self {
            const leaks = try utils.createMut(ArrayList(usize), allocator, .empty);
            const scopes = try utils.createMut(ArrayList(T), allocator, .empty);
            const captureIndexes = try utils.createMut(ArrayList(usize), allocator, .empty);

            return Self{
                .scopes = scopes,
                .leaks = leaks,
                .captureIndexes = captureIndexes,
                .current = 0,
            };
        }

        pub fn deinit(self: *Self, context: *Context) void {
            for (self.scopes.items) |item| {
                freeFn(context, item, .All);
            }
        }

        pub fn clear(self: *Self, context: *Context) void {
            for (self.scopes.items) |item| {
                freeFn(context, item, .Allocated);
            }
        }

        pub fn addCaptureIndex(self: *Self, allocator: Allocator) !void {
            const index = self.scopes.items.len;
            try self.captureIndexes.append(allocator, index);
        }

        pub fn add(self: *Self, allocator: Allocator, scope: T, leak: bool) !void {
            try self.scopes.append(allocator, scope);

            if (leak) {
                const index = self.leaks.items.len - 1;
                self.leaks.items[index] += 1;
            } else {
                try self.leaks.append(allocator, 0);
            }

            self.current = self.scopes.items.len;
        }

        pub fn release(self: *Self) ?T {
            if (self.scopes.items.len == 0) return null;

            while (self.captureIndexes.items.len > 0 and
                self.captureIndexes.getLast() >= self.scopes.items.len - 1)
            {
                _ = self.captureIndexes.pop();
            }

            const index = self.leaks.items.len - 1;
            if (self.leaks.items[index] == 0) {
                _ = self.leaks.pop();
            } else {
                self.leaks.items[index] -= 1;
            }

            self.current = self.scopes.items.len - 1;

            return self.scopes.pop();
        }

        pub fn pop(self: *Self, context: *Context) void {
            const last = self.release();

            if (last) |item| {
                freeFn(context, item, .Allocated);
            }
        }

        pub fn getCurrentScope(self: Self) ?T {
            return self.scopes.getLastOrNull();
        }

        pub fn getNextInLeak(self: *Self) ?ScopeInfo(T) {
            if (self.current == 0 or self.scopes.items.len == 0) return null;

            const leak = self.leaks.getLast();
            const diff = self.scopes.items.len - self.current;
            if (diff == leak + 1) return null;

            self.current -= 1;
            const res = self.scopes.items[self.current];

            const lastCapture = self.captureIndexes.getLastOrNull();
            const capture = if (lastCapture) |index| index > self.current else false;

            return .{
                .scope = res,
                .capture = capture,
            };
        }

        pub fn resetLeakIndex(self: *Self) void {
            self.current = self.scopes.items.len;
        }

        pub fn getLastLeak(self: Self) usize {
            return self.leaks.getLast();
        }
    };
}

pub const ReturnInfoData = struct {
    retType: ?scanner.TypeAndAllocInfo,
    inFunction: bool,
    exhaustive: bool,
    lockExhaustive: bool,
};

pub const ReturnInfo = struct {
    const Self = @This();

    info: *ReturnInfoData,

    pub fn init(allocator: Allocator) !Self {
        const info = try utils.createMut(ReturnInfoData, allocator, .{
            .retType = null,
            .inFunction = false,
            .exhaustive = true,
            .lockExhaustive = false,
        });

        return .{
            .info = info,
        };
    }

    pub fn clear(self: *Self, context: *Context) void {
        if (self.info.retType) |retType| {
            if (retType.allocState == .Allocated) {
                pools.recursiveReleaseType(context, retType.info.astType);
            }
        }
    }

    pub fn newInfo(self: *Self, allocator: Allocator, isFunction: bool) !*ReturnInfoData {
        const oldRetInfo = self.info;
        self.info = try utils.createMut(ReturnInfoData, allocator, .{
            .retType = null,
            .inFunction = oldRetInfo.inFunction or isFunction,
            .exhaustive = true,
            .lockExhaustive = false,
        });
        return oldRetInfo;
    }

    pub fn swapFree(self: *Self, allocator: Allocator, context: *Context, oldRetInfo: *ReturnInfoData) void {
        if (self.info.retType) |retType| {
            if (retType.allocState == .Allocated) {
                pools.recursiveReleaseType(context, retType.info.astType);
            }
        }

        allocator.destroy(self.info);
        self.info = oldRetInfo;
    }

    /// IMPORTANT - invalidates prev
    pub fn collapse(
        self: *Self,
        allocator: Allocator,
        context: *Context,
        prev: *ReturnInfoData,
        withGenDef: bool,
    ) !void {
        if (prev.retType) |retType| {
            if (self.info.retType) |firstRetType| {
                const matches = try scanner.matchTypes(
                    allocator,
                    context,
                    retType.info,
                    firstRetType.info,
                    withGenDef,
                );
                if (!matches) {
                    return scanner.ScanError.FunctionReturnsHaveDifferentTypes;
                }
            } else {
                self.info.retType = retType;
            }
        }

        self.info.exhaustive = self.info.exhaustive and prev.exhaustive;
        self.info.lockExhaustive = prev.lockExhaustive;
        allocator.destroy(prev);
    }

    pub fn setExhaustive(self: *Self, exhaustive: bool) void {
        if (!self.info.lockExhaustive) {
            self.info.exhaustive = exhaustive;
        }
    }

    pub fn setInFunction(self: *Self, inFunction: bool) bool {
        const prev = self.info.inFunction;
        self.info.inFunction = inFunction;
        return prev;
    }

    pub fn revertInFunction(self: *Self, inFunction: bool) void {
        self.info.inFunction = inFunction;
    }

    pub fn hasType(self: Self) bool {
        return self.info.retType != null;
    }
};
