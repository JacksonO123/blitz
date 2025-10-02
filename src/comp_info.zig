const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const free = blitz.free;
const tokenizer = blitz.tokenizer;
const logger = blitz.logger;
const builtins = blitz.builtins;
const string = blitz.string;
const clone = blitz.clone;
const scanner = blitz.scanner;
const vmInfo = blitz.vmInfo;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;

fn initScopeUtil(comptime T: type, allocator: Allocator) !*ScopeUtil(*T) {
    const baseScope = try utils.initMutPtrT(T, allocator);
    const scopeUtil = try ScopeUtil(*T).init(allocator);
    const scopePtr = try utils.createMut(ScopeUtil(*T), allocator, scopeUtil);
    try scopePtr.add(baseScope, false);
    return scopePtr;
}

fn initScopeUtilWithBase(comptime T: type, allocator: Allocator, base: T) !*ScopeUtil(*T) {
    const baseScope = try utils.createMut(T, allocator, base);
    const scopeUtil = try ScopeUtil(*T).init(allocator);
    const scopePtr = try utils.createMut(ScopeUtil(*T), allocator, scopeUtil);
    try scopePtr.add(baseScope, false);
    return scopePtr;
}

pub const VarScope = StringHashMap(blitzAst.AstTypeInfo);
pub const CaptureScope = StringHashMap(blitzAst.AstTypeInfo);
pub const TypeScope = StringHashMap(blitzAst.AstTypeInfo);
pub const StringListScope = ArrayList([]const u8);

const ToScanItem = struct {
    func: *blitzAst.FuncDecNode,
    genTypes: []blitzAst.StrToTypeInfoRel,
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

    allocator: Allocator,
    context: *Context = undefined, // to be set before usage in compiler.zig
    structNames: [][]const u8,
    errorNames: [][]const u8,
    variableScopes: *ScopeUtil(*VarScope),
    variableCaptures: *ScopeUtil(*CaptureScope),
    genericCaptures: *ScopeUtil(*TypeScope),
    functionCaptures: *ScopeUtil(*StringListScope),
    parsedGenerics: *ScopeUtil(*StringListScope),
    scopeTypes: *ArrayList(ScopeType),
    // how many scopes up the current scope has access
    functions: *StringHashMap(*blitzAst.FuncDecNode),
    functionsInScope: *ScopeUtil(*StringListScope),
    structDecs: *StringHashMap(*blitzAst.StructDecNode),
    errorDecs: *StringHashMap(*const blitzAst.ErrorDecNode),
    functionsToScan: *ToScanStack,
    // each number describes how far from
    // the struct method a child node is
    genericScopes: *ScopeUtil(*TypeScope),
    preAst: bool,
    returnInfo: *ReturnInfo,
    builtins: builtins.BuiltinFuncMemo,
    stackSizeEstimate: vmInfo.StartStackType,

    pub fn init(
        allocator: Allocator,
        names: blitzAst.HoistedNames,
    ) !Self {
        const functionsToScan = try utils.createMut(ToScanStack, allocator, .empty);
        const scopeTypesPtr = try utils.createMut(ArrayList(ScopeType), allocator, .empty);
        try scopeTypesPtr.append(allocator, .Normal);

        const functions = try utils.initMutPtrT(StringHashMap(*blitzAst.FuncDecNode), allocator);
        const structs = try utils.initMutPtrT(StringHashMap(*blitzAst.StructDecNode), allocator);
        const errors = try utils.initMutPtrT(
            StringHashMap(*const blitzAst.ErrorDecNode),
            allocator,
        );

        const genericScopes = try initScopeUtil(TypeScope, allocator);
        const variableScopes = try initScopeUtil(VarScope, allocator);
        const variableCaptures = try initScopeUtil(CaptureScope, allocator);
        const genericCaptures = try initScopeUtil(TypeScope, allocator);
        const functionCaptures = try initScopeUtilWithBase(StringListScope, allocator, .empty);
        const parsedGenerics = try initScopeUtilWithBase(StringListScope, allocator, .empty);
        const functionsInScope = try initScopeUtilWithBase(StringListScope, allocator, .empty);

        const returnInfoUtil = try ReturnInfo.init(allocator);
        const returnInfo = try utils.createMut(ReturnInfo, allocator, returnInfoUtil);

        return Self{
            .allocator = allocator,
            .structNames = names.structNames,
            .errorNames = names.errorNames,
            .variableScopes = variableScopes,
            .variableCaptures = variableCaptures,
            .functionCaptures = functionCaptures,
            .genericCaptures = genericCaptures,
            .parsedGenerics = parsedGenerics,
            .scopeTypes = scopeTypesPtr,
            .functions = functions,
            .functionsInScope = functionsInScope,
            .structDecs = structs,
            .errorDecs = errors,
            .functionsToScan = functionsToScan,
            .genericScopes = genericScopes,
            .preAst = true,
            .returnInfo = returnInfo,
            .builtins = .{},
            .stackSizeEstimate = 1024, // 1kb
        };
    }

    pub fn deinit(self: *Self) void {
        var functionIt = self.functions.valueIterator();
        while (functionIt.next()) |f| {
            free.freeFuncDec(self.allocator, f.*);
        }

        var structsIt = self.structDecs.valueIterator();
        while (structsIt.next()) |dec| {
            free.freeStructDec(self.allocator, dec.*);
            self.allocator.destroy(dec.*);
        }

        var errorsIt = self.errorDecs.valueIterator();
        while (errorsIt.next()) |err| {
            self.allocator.free(err.*.variants);
            self.allocator.destroy(err.*);
        }

        self.allocator.free(self.structNames);
        self.allocator.free(self.errorNames);

        free.freeBuiltins(self.allocator, self.builtins);

        self.returnInfo.deinit();
        self.allocator.destroy(self.returnInfo);

        self.variableScopes.deinit(free.freeVariableScope);
        self.allocator.destroy(self.variableScopes);

        self.variableCaptures.deinit(free.freeVariableCaptures);
        self.allocator.destroy(self.variableCaptures);

        self.genericCaptures.deinit(free.freeGenericCaptures);
        self.allocator.destroy(self.genericCaptures);

        self.functionCaptures.deinit(free.deinitScope);
        self.allocator.destroy(self.functionCaptures);

        self.parsedGenerics.deinit(free.deinitScope);
        self.allocator.destroy(self.parsedGenerics);

        self.scopeTypes.deinit(self.allocator);
        self.allocator.destroy(self.scopeTypes);

        self.functions.deinit();
        self.allocator.destroy(self.functions);

        self.structDecs.deinit();
        self.allocator.destroy(self.structDecs);

        self.errorDecs.deinit();
        self.allocator.destroy(self.errorDecs);

        self.functionsToScan.deinit(self.allocator);
        self.allocator.destroy(self.functionsToScan);

        self.genericScopes.deinit(free.freeGenericScope);
        self.allocator.destroy(self.genericScopes);

        self.functionsInScope.deinit(free.deinitScope);
        self.allocator.destroy(self.functionsInScope);
    }

    pub fn getScopeDepth(self: Self) usize {
        return self.variableScopes.scopes.items.len;
    }

    pub fn pushScope(self: *Self, leak: bool) !void {
        try self.pushScopeWithType(leak, .Normal);
    }

    pub fn pushScopeWithType(self: *Self, leak: bool, scopeType: ScopeType) !void {
        const scope = try utils.initMutPtrT(VarScope, self.allocator);
        try self.variableScopes.add(scope, leak);
        try self.pushScopedFunctionScope(leak);
        try self.scopeTypes.append(self.allocator, scopeType);
    }

    pub fn popScope(self: *Self) void {
        self.variableScopes.pop(free.freeVariableScope);
        self.popScopedFunctionScope();
        _ = self.scopeTypes.pop();
    }

    pub fn addCaptureScope(self: *Self) !void {
        try self.variableScopes.addCaptureIndex();
        try self.functionsInScope.addCaptureIndex();

        const newVarScope = try utils.initMutPtrT(CaptureScope, self.allocator);
        const newFuncScope = try utils.createMut(StringListScope, self.allocator, .empty);

        try self.variableCaptures.add(newVarScope, false);
        try self.functionCaptures.add(newFuncScope, false);
    }

    pub fn popCaptureScope(self: *Self) void {
        self.variableCaptures.pop(free.freeVariableCaptures);
        self.functionCaptures.pop(free.deinitScope);
    }

    pub fn addGenericCaptureScope(self: *Self) !void {
        try self.genericScopes.addCaptureIndex();
        const newScope = try utils.initMutPtrT(TypeScope, self.allocator);
        try self.genericCaptures.add(newScope, false);
    }

    pub fn popGenericCaptureScope(self: *Self) void {
        self.genericCaptures.pop(free.freeGenericCaptures);
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

    pub fn pushScopedFunctionScope(self: *Self, leak: bool) !void {
        const scope = try utils.createMut(StringListScope, self.allocator, .empty);
        try self.functionsInScope.add(scope, leak);
    }

    pub fn popScopedFunctionScope(self: *Self) void {
        self.functionsInScope.pop(free.deinitScope);
    }

    pub fn addScopedFunction(self: *Self, name: []const u8) !void {
        const scope = self.functionsInScope.getCurrentScope();
        if (scope) |s| {
            try s.append(self.allocator, name);
        }
    }

    pub fn pushParsedGenericsScope(self: *Self, leak: bool) !void {
        const newScope = try utils.createMut(StringListScope, self.allocator, .empty);
        try self.parsedGenerics.add(newScope, leak);
    }

    pub fn popParsedGenericsScope(self: *Self) void {
        self.parsedGenerics.pop(free.deinitScope);
    }

    pub fn addParsedGeneric(self: *Self, gen: []const u8) !void {
        const scope = self.parsedGenerics.getCurrentScope();
        if (scope) |s| {
            try s.append(self.allocator, gen);
        }
    }

    pub fn hasParsedGeneric(self: Self, gen: []const u8) bool {
        var scope: ?*StringListScope = self.parsedGenerics.getCurrentScope();
        defer self.parsedGenerics.resetLeakIndex();

        while (scope) |s| {
            if (string.inStringArr(s.items, gen)) {
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

    pub fn prepareForAst(self: *Self, context: *Context) !void {
        self.preAst = false;

        {
            var structIt = self.structDecs.valueIterator();
            while (structIt.next()) |s| {
                const attributes = s.*.attributes;
                const arr = if (s.*.deriveType) |derived| a: {
                    break :a try blitzAst.mergeMembers(
                        self.allocator,
                        context,
                        attributes,
                        derived,
                    );
                } else a: {
                    var members = try ArrayList(blitzAst.StructAttribute).initCapacity(
                        self.allocator,
                        attributes.len,
                    );

                    for (attributes) |attr| {
                        if (attr.attr != .Member) continue;
                        try members.append(self.allocator, attr);
                    }

                    break :a try members.toOwnedSlice(self.allocator);
                };

                s.*.totalMemberList = arr;
            }
        }

        {
            var structIt = self.structDecs.valueIterator();
            while (structIt.next()) |s| {
                const attributes = s.*.attributes;
                try self.pushParsedGenericsScope(false);
                defer self.popParsedGenericsScope();

                for (s.*.generics) |g| {
                    try self.addParsedGeneric(g.name);
                }

                for (attributes) |attr| {
                    if (attr.attr != .Function) continue;

                    const f = attr.attr.Function;
                    free.freeNode(self.allocator, f.body);

                    const prevTokens = context.tokens;
                    const tempTokens = try utils.createMut(
                        tokenizer.TokenUtil,
                        self.allocator,
                        tokenizer.TokenUtil.init(f.bodyTokens),
                    );
                    context.tokens = tempTokens;
                    f.body = blitzAst.parseSequence(self.allocator, context, true) catch |e| {
                        logger.logParseError(context, e);
                        return e;
                    };

                    context.tokens = prevTokens;
                    self.allocator.destroy(tempTokens);
                }
            }
        }

        context.tokens.reset();
    }

    pub fn hasStruct(self: Self, name: []const u8) bool {
        for (self.structNames) |structName| {
            if (string.compString(structName, name)) return true;
        }

        return false;
    }

    pub fn hasError(self: Self, name: []const u8) bool {
        for (self.errorNames) |errorName| {
            if (string.compString(errorName, name)) return true;
        }

        return false;
    }

    pub fn pushGenScope(self: *Self, leak: bool) !void {
        const genScope = try utils.initMutPtrT(TypeScope, self.allocator);
        try self.genericScopes.add(genScope, leak);
    }

    pub fn popGenScope(self: *Self) void {
        self.genericScopes.pop(free.freeGenericScope);
    }

    pub fn setGeneric(self: *Self, name: []const u8, gType: blitzAst.AstTypeInfo) !void {
        const genScope = self.genericScopes.getCurrentScope();

        if (genScope) |scope| {
            const value = scope.get(name);
            if (value) |genValue| {
                free.freeAstTypeInfo(self.allocator, genValue);
            }

            try scope.put(name, gType);
        }
    }

    pub fn getGeneric(self: *Self, name: []const u8) !?blitzAst.AstTypeInfo {
        var genScope: ?*TypeScope = self.genericScopes.getCurrentScope();
        defer self.genericScopes.resetLeakIndex();
        var capture = false;

        while (genScope) |s| {
            if (s.get(name)) |t| {
                if (!capture) return t;

                const captureScope = self.genericCaptures.getCurrentScope();
                if (captureScope) |capScope| {
                    const clonedType = try clone.cloneAstTypeInfo(
                        self.allocator,
                        self.context,
                        t,
                        true,
                    );
                    const existing = capScope.get(name);

                    if (existing) |exist| {
                        free.freeAstTypeInfo(self.allocator, exist);
                    }

                    try capScope.put(name, clonedType);
                }

                return t;
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
        name: []const u8,
        info: blitzAst.AstTypeInfo,
        isConst: bool,
    ) !void {
        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            const varInfo = try utils.astTypesToInfo(self.allocator, .{
                .VarInfo = info,
            }, isConst);
            try s.put(name, varInfo);
        }
    }

    pub fn removeVariableType(self: *Self, name: []const u8) void {
        const scope = self.variableScopes.getCurrentScope();
        if (scope) |s| {
            _ = s.remove(name);
        }
    }

    pub fn getVariableType(self: *Self, name: []const u8, replaceGenerics: bool) !?blitzAst.AstTypeInfo {
        var scope: ?*VarScope = self.variableScopes.getCurrentScope();
        defer self.variableScopes.resetLeakIndex();
        var capture = false;

        while (scope) |s| {
            if (s.get(name)) |t| {
                if (!capture) return t;

                const captureScope = self.variableCaptures.getCurrentScope();
                if (captureScope) |capScope| {
                    const clonedType = try clone.cloneAstTypeInfo(
                        self.allocator,
                        self.context,
                        t,
                        replaceGenerics,
                    );
                    const existing = capScope.get(name);

                    if (existing) |exist| {
                        free.freeAstTypeInfo(self.allocator, exist);
                    }

                    try capScope.put(name, clonedType);
                }

                return t;
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

    pub fn getVariableTypeFixed(self: *Self, name: []const u8) ?blitzAst.AstTypeInfo {
        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            if (s.get(name)) |t| {
                return t;
            }
        }

        return null;
    }

    pub fn addFunction(self: *Self, name: []const u8, dec: *blitzAst.FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn addFuncToScan(
        self: *Self,
        func: *blitzAst.FuncDecNode,
        rels: []blitzAst.StrToTypeInfoRel,
        withGenDef: bool,
    ) !void {
        try self.functionsToScan.append(self.allocator, .{
            .func = func,
            .genTypes = rels,
            .withGenDef = withGenDef,
        });
    }

    pub fn getFunction(self: Self, name: []const u8) !?*blitzAst.FuncDecNode {
        const func = self.getFunctionAsGlobal(name);
        const captureScope = self.functionCaptures.getCurrentScope();
        if (func) |funcDec| a: {
            if (!funcDec.globallyDefined) break :a;

            if (captureScope) |capScope| {
                if (!string.inStringArr(capScope.items, name)) {
                    try capScope.append(self.allocator, name);
                }
            }

            return func;
        }

        var scope: ?*StringListScope = self.functionsInScope.getCurrentScope();
        defer self.functionsInScope.resetLeakIndex();
        var capture = false;

        while (scope) |s| {
            for (s.items) |item| {
                if (!string.compString(item, name)) {
                    continue;
                }

                if (func) |funcDec| {
                    if (!capture) {
                        return func;
                    }

                    if (captureScope) |capScope| {
                        if (!string.inStringArr(capScope.items, name)) {
                            try capScope.append(self.allocator, name);
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

    pub fn getFunctionAsGlobal(self: Self, name: []const u8) ?*blitzAst.FuncDecNode {
        return self.functions.get(name);
    }

    pub fn setStructDec(self: *Self, name: []const u8, node: *blitzAst.StructDecNode) !void {
        try self.structDecs.put(name, node);
    }

    pub fn setStructDecs(self: *Self, nodes: []*blitzAst.StructDecNode) !void {
        for (nodes) |node| {
            try self.setStructDec(node.name, node);
        }
    }

    pub fn setErrorDecs(self: *Self, decs: []*const blitzAst.ErrorDecNode) !void {
        for (decs) |dec| {
            try self.errorDecs.put(dec.name, dec);
        }
    }

    pub fn getStructDec(self: Self, name: []const u8) ?*const blitzAst.StructDecNode {
        return self.structDecs.get(name);
    }

    pub fn getErrorDec(self: Self, name: []const u8) ?*const blitzAst.ErrorDecNode {
        return self.errorDecs.get(name);
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

fn ScopeUtil(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        scopes: *ArrayList(T),
        captureIndexes: *ArrayList(usize),
        leaks: *ArrayList(usize),
        current: usize,

        pub fn init(allocator: Allocator) !Self {
            const leaks = try utils.createMut(ArrayList(usize), allocator, .empty);
            const scopes = try utils.createMut(ArrayList(T), allocator, .empty);
            const captureIndexes = try utils.createMut(ArrayList(usize), allocator, .empty);

            return Self{
                .allocator = allocator,
                .scopes = scopes,
                .leaks = leaks,
                .captureIndexes = captureIndexes,
                .current = 0,
            };
        }

        pub fn deinit(self: *Self, freeFn: fn (Allocator, T) void) void {
            for (self.scopes.items) |item| {
                freeFn(self.allocator, item);
                self.allocator.destroy(item);
            }

            self.scopes.deinit(self.allocator);
            self.allocator.destroy(self.scopes);

            self.leaks.deinit(self.allocator);
            self.allocator.destroy(self.leaks);

            self.captureIndexes.deinit(self.allocator);
            self.allocator.destroy(self.captureIndexes);
        }

        pub fn addCaptureIndex(self: *Self) !void {
            const index = self.scopes.items.len;
            try self.captureIndexes.append(self.allocator, index);
        }

        pub fn add(self: *Self, scope: T, leak: bool) !void {
            try self.scopes.append(self.allocator, scope);

            if (leak) {
                const index = self.leaks.items.len - 1;
                self.leaks.items[index] += 1;
            } else {
                try self.leaks.append(self.allocator, 0);
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

            const last = self.scopes.pop();
            return last;
        }

        pub fn pop(self: *Self, freeFn: fn (Allocator, T) void) void {
            const last = self.release();

            if (last) |item| {
                freeFn(self.allocator, item);
                self.allocator.destroy(item);
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
    retType: ?blitzAst.AstTypeInfo,
    inFunction: bool,
    exhaustive: bool,
    lockExhaustive: bool,
};

pub const ReturnInfo = struct {
    const Self = @This();

    allocator: Allocator,
    info: *ReturnInfoData,

    pub fn init(allocator: Allocator) !Self {
        const info = try utils.createMut(ReturnInfoData, allocator, .{
            .retType = null,
            .inFunction = false,
            .exhaustive = true,
            .lockExhaustive = false,
        });

        return .{
            .allocator = allocator,
            .info = info,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self.info);
    }

    pub fn newInfo(self: *Self, isFunction: bool) !*ReturnInfoData {
        const oldRetInfo = self.info;
        self.info = try utils.createMut(ReturnInfoData, self.allocator, .{
            .retType = null,
            .inFunction = oldRetInfo.inFunction or isFunction,
            .exhaustive = true,
            .lockExhaustive = false,
        });
        return oldRetInfo;
    }

    pub fn swapFree(self: *Self, oldRetInfo: *ReturnInfoData) void {
        if (self.info.retType) |retType| {
            free.freeAstTypeInfo(self.allocator, retType);
        }
        self.allocator.destroy(self.info);
        self.info = oldRetInfo;
    }

    /// IMPORTANT - invalidates prev
    pub fn collapse(
        self: *Self,
        context: *Context,
        prev: *ReturnInfoData,
        withGenDef: bool,
    ) !void {
        if (prev.retType) |retType| {
            if (self.info.retType) |firstRetType| {
                const matches = try scanner.matchTypes(
                    self.allocator,
                    context,
                    retType,
                    firstRetType,
                    withGenDef,
                );
                if (!matches) {
                    return scanner.ScanError.FunctionReturnsHaveDifferentTypes;
                }

                free.freeAstTypeInfo(self.allocator, retType);
            } else {
                self.info.retType = retType;
            }
        }

        self.info.exhaustive = self.info.exhaustive and prev.exhaustive;
        self.info.lockExhaustive = prev.lockExhaustive;
        self.allocator.destroy(prev);
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
