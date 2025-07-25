const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const string = blitz.string;
const free = blitz.free;
const scanner = blitz.scanner;
const clone = blitz.clone;
const builtins = blitz.builtins;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Logger = blitz.logger.Logger;
const AstError = blitzAst.AstError;
const ScanError = scanner.ScanError;

pub inline fn create(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*const T {
    return createMut(T, allocator, obj);
}

pub inline fn createMut(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}

pub fn readRelativeFile(allocator: Allocator, path: []const u8) ![]u8 {
    const maxFileSize = 1028 * 4; // arbitrary
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, maxFileSize);
}

pub fn initMutPtrT(comptime T: type, allocator: Allocator) !*T {
    const data = T.init(allocator);
    return try createMut(T, allocator, data);
}

pub inline fn astTypesPtrToInfo(astType: *const blitzAst.AstTypes, isConst: bool) blitzAst.AstTypeInfo {
    return .{
        .isConst = isConst,
        .astType = astType,
    };
}

pub inline fn astTypesToInfo(allocator: Allocator, astType: blitzAst.AstTypes, isConst: bool) !blitzAst.AstTypeInfo {
    const ptr = try create(blitzAst.AstTypes, allocator, astType);
    return .{
        .isConst = isConst,
        .astType = ptr,
    };
}

/// IMPORTANT: supports values 0 - 16
pub fn intToHex(num: usize) u8 {
    return if (num < 10) ('0' + @as(u8, @intCast(num))) else ('a' + @as(u8, @intCast(num - 10)));
}

fn initScopeUtil(comptime T: type, allocator: Allocator) !*ScopeUtil(*T) {
    const baseScope = try initMutPtrT(T, allocator);
    const scopeUtil = try ScopeUtil(*T).init(allocator);
    const scopePtr = try createMut(ScopeUtil(*T), allocator, scopeUtil);
    try scopePtr.add(baseScope, false);
    return scopePtr;
}

const VariableInfo = struct {
    varType: *const blitzAst.AstTypes,
    isConst: bool,
};

pub const TypeScope = StringHashMap(*const blitzAst.AstTypes);
const VarScope = StringHashMap(VariableInfo);
pub const CaptureScope = StringHashMap(VariableInfo);
const StringListScope = ArrayList([]u8);

pub const CompInfo = struct {
    const Self = @This();

    allocator: Allocator,
    structNames: [][]u8,
    errorNames: [][]u8,
    variableScopes: *ScopeUtil(*VarScope),
    variableCaptures: *ScopeUtil(*CaptureScope),
    genericCaptures: *ScopeUtil(*TypeScope),
    parsedGenerics: *ScopeUtil(*StringListScope),
    // how many scopes up the current scope has access
    functions: *StringHashMap(*blitzAst.FuncDecNode),
    functionsInScope: *ScopeUtil(*StringListScope),
    structDecs: *StringHashMap(*blitzAst.StructDecNode),
    errorDecs: *StringHashMap(*const blitzAst.ErrorDecNode),
    currentStructs: *ArrayList([]u8),
    // each number describes how far from
    // the struct method a child node is
    distFromStructMethod: *ArrayList(u32),
    genericScopes: *ScopeUtil(*TypeScope),
    previousAccessedStruct: ?[]u8,
    preAst: bool,
    tokens: *TokenUtil,
    logger: *Logger,
    scanner: struct {
        allowErrorWithoutVariants: bool,
        allowStaticStructInstance: bool,
    },
    returnInfo: *ReturnInfo,
    builtins: builtins.BuiltinFuncMemo,

    pub fn init(allocator: Allocator, tokens: []tokenizer.Token, names: blitzAst.HoistedNames, code: []const u8) !Self {
        const loggerUtil = try allocator.create(Logger);
        const tokenUtil = try createMut(TokenUtil, allocator, try TokenUtil.init(allocator, loggerUtil, tokens));
        loggerUtil.* = Logger.init(allocator, tokenUtil, code);

        const currentStructs = try initMutPtrT(ArrayList([]u8), allocator);
        const distFromStructMethod = try initMutPtrT(ArrayList(u32), allocator);
        const functions = try initMutPtrT(StringHashMap(*blitzAst.FuncDecNode), allocator);
        const structs = try initMutPtrT(StringHashMap(*blitzAst.StructDecNode), allocator);

        const errors = try initMutPtrT(StringHashMap(*const blitzAst.ErrorDecNode), allocator);

        const genericScopes = try initScopeUtil(TypeScope, allocator);
        const functionsInScope = try initScopeUtil(StringListScope, allocator);
        const variableScopes = try initScopeUtil(VarScope, allocator);
        const variableCaptures = try initScopeUtil(CaptureScope, allocator);
        const genericCaptures = try initScopeUtil(TypeScope, allocator);
        const parsedGenerics = try initScopeUtil(StringListScope, allocator);

        const returnInfoUtil = try ReturnInfo.init(allocator);
        const returnInfo = try createMut(ReturnInfo, allocator, returnInfoUtil);

        return Self{
            .allocator = allocator,
            .structNames = names.structNames,
            .errorNames = names.errorNames,
            .variableScopes = variableScopes,
            .variableCaptures = variableCaptures,
            .genericCaptures = genericCaptures,
            .parsedGenerics = parsedGenerics,
            .functions = functions,
            .functionsInScope = functionsInScope,
            .structDecs = structs,
            .errorDecs = errors,
            .currentStructs = currentStructs,
            .distFromStructMethod = distFromStructMethod,
            .genericScopes = genericScopes,
            .previousAccessedStruct = null,
            .preAst = true,
            .tokens = tokenUtil,
            .logger = loggerUtil,
            .scanner = .{
                .allowErrorWithoutVariants = false,
                .allowStaticStructInstance = false,
            },
            .returnInfo = returnInfo,
            .builtins = .{
                .dynArr = .{
                    .push = null,
                    .pop = null,
                    .pushFront = null,
                    .popFront = null,
                },
            },
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
            free.freeErrorDec(self.allocator, err.*);
            self.allocator.destroy(err.*);
        }

        free.freeNestedSlice(u8, self.allocator, self.structNames);
        free.freeNestedSlice(u8, self.allocator, self.errorNames);

        for (self.currentStructs.items) |item| {
            self.allocator.free(item);
        }

        free.freeBuiltins(self.allocator, self.builtins);

        self.returnInfo.deinit();
        self.allocator.destroy(self.returnInfo);

        self.variableScopes.deinit(freeVariableScope);
        self.allocator.destroy(self.variableScopes);

        self.variableCaptures.deinit(freeVariableCaptures);
        self.allocator.destroy(self.variableCaptures);

        self.genericCaptures.deinit(freeGenericCaptures);
        self.allocator.destroy(self.genericCaptures);

        self.parsedGenerics.deinit(freeParsedGenerics);
        self.allocator.destroy(self.parsedGenerics);

        self.functions.deinit();
        self.allocator.destroy(self.functions);

        self.structDecs.deinit();
        self.allocator.destroy(self.structDecs);

        self.errorDecs.deinit();
        self.allocator.destroy(self.errorDecs);

        self.currentStructs.deinit();
        self.allocator.destroy(self.currentStructs);

        self.distFromStructMethod.deinit();
        self.allocator.destroy(self.distFromStructMethod);

        self.genericScopes.deinit(freeGenericScope);
        self.allocator.destroy(self.genericScopes);

        self.tokens.deinit();
        self.allocator.destroy(self.tokens);

        self.logger.deinit();
        self.allocator.destroy(self.logger);

        self.functionsInScope.deinit(freeScopedFunctionScope);
        self.allocator.destroy(self.functionsInScope);
    }

    pub fn getScopeDepth(self: Self) usize {
        return self.variableScopes.scopes.items.len;
    }

    pub fn pushScope(self: *Self, leak: bool) !void {
        const scope = try initMutPtrT(StringHashMap(VariableInfo), self.allocator);
        try self.variableScopes.add(scope, leak);
        try self.pushScopedFunctionScope(leak);
    }

    pub fn popScope(self: *Self) void {
        self.variableScopes.pop(freeVariableScope);
        self.popScopedFunctionScope();
    }

    pub fn addCaptureScope(self: *Self) !void {
        try self.variableScopes.addCaptureIndex();
        const newScope = try initMutPtrT(StringHashMap(VariableInfo), self.allocator);
        try self.variableCaptures.add(newScope, false);
    }

    pub fn popCaptureScope(self: *Self) void {
        self.variableCaptures.pop(freeVariableScope);
    }

    pub fn addGenericCaptureScope(self: *Self) !void {
        try self.genericScopes.addCaptureIndex();
        const newScope = try initMutPtrT(StringHashMap(*const blitzAst.AstTypes), self.allocator);
        try self.genericCaptures.add(newScope, false);
    }

    pub fn popGenericCaptureScope(self: *Self) void {
        self.genericCaptures.pop(freeGenericCaptures);
    }

    pub fn consumeVariableCaptures(self: *Self) ?*CaptureScope {
        return self.variableCaptures.release();
    }

    pub fn consumeGenericCaptures(self: *Self) ?*TypeScope {
        return self.genericCaptures.release();
    }

    pub fn pushScopedFunctionScope(self: *Self, leak: bool) !void {
        const scope = try initMutPtrT(StringListScope, self.allocator);
        try self.functionsInScope.add(scope, leak);
    }

    pub fn popScopedFunctionScope(self: *Self) void {
        self.functionsInScope.pop(freeScopedFunctionScope);
    }

    pub fn addScopedFunction(self: *Self, name: []u8) !void {
        const scope = self.functionsInScope.getCurrentScope();
        if (scope) |s| {
            try s.append(name);
        }
    }

    pub fn pushParsedGenericsScope(self: *Self, leak: bool) !void {
        const newScope = try initMutPtrT(StringListScope, self.allocator);
        try self.parsedGenerics.add(newScope, leak);
    }

    pub fn popParsedGenericsScope(self: *Self) void {
        self.parsedGenerics.pop(freeParsedGenerics);
    }

    pub fn addParsedGeneric(self: *Self, gen: []u8) !void {
        const scope = self.parsedGenerics.getCurrentScope();
        if (scope) |s| {
            try s.append(gen);
        }
    }

    pub fn hasParsedGeneric(self: Self, gen: []u8) bool {
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

    pub fn prepareForAst(self: *Self) !void {
        self.preAst = false;

        {
            var structIt = self.structDecs.valueIterator();
            while (structIt.next()) |s| {
                const attributes = s.*.attributes;
                const arr = if (s.*.deriveType) |derived| a: {
                    break :a try blitzAst.mergeMembers(self.allocator, self, attributes, derived);
                } else a: {
                    var members = try ArrayList(blitzAst.StructAttribute).initCapacity(self.allocator, attributes.len);

                    for (attributes) |attr| {
                        if (attr.attr != .Member) continue;
                        try members.append(attr);
                    }

                    break :a try members.toOwnedSlice();
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

                    const oldTokens = self.tokens;
                    self.tokens = try createMut(TokenUtil, self.allocator, try TokenUtil.init(self.allocator, self.logger, f.bodyTokens));
                    f.body = try blitzAst.parseSequence(self.allocator, self, true);

                    self.tokens.deinit();
                    self.allocator.destroy(self.tokens);
                    self.tokens = oldTokens;
                }
            }
        }

        self.tokens.reset();
    }

    pub fn setPreviousAccessedStruct(self: *Self, name: ?[]u8) void {
        self.previousAccessedStruct = name;
    }

    pub fn getPreviousAccessedStruct(self: Self) ?[]u8 {
        return self.previousAccessedStruct;
    }

    pub fn hasStruct(self: Self, name: []u8) bool {
        for (self.structNames) |structName| {
            if (string.compString(structName, name)) return true;
        }

        return false;
    }

    pub fn hasError(self: Self, name: []u8) bool {
        for (self.errorNames) |errorName| {
            if (string.compString(errorName, name)) return true;
        }

        return false;
    }

    pub fn pushGenScope(self: *Self, leak: bool) !void {
        const genScope = try initMutPtrT(StringHashMap(*const blitzAst.AstTypes), self.allocator);
        try self.genericScopes.add(genScope, leak);
    }

    pub fn popGenScope(self: *Self) void {
        self.genericScopes.pop(freeGenericScope);
    }

    pub fn setGeneric(self: *Self, name: []const u8, gType: *const blitzAst.AstTypes) !void {
        const genScope = self.genericScopes.getCurrentScope();

        if (genScope) |scope| {
            const value = scope.get(name);
            if (value) |genValue| {
                free.freeType(self.allocator, genValue);
            }

            try scope.put(name, gType);
        }
    }

    pub fn removeGeneric(self: *Self, name: []u8) void {
        const genScope = self.getCurrentGenScope();
        const gType = genScope.get(name);

        if (gType) |generic| {
            free.freeNode(self.allocator, generic);
        }

        self.genericScopes.remove(name);
    }

    pub fn getGeneric(self: *Self, name: []u8) !?*const blitzAst.AstTypes {
        var genScope: ?*TypeScope = self.genericScopes.getCurrentScope();
        defer self.genericScopes.resetLeakIndex();
        var capture = false;

        while (genScope) |s| {
            if (s.get(name)) |t| {
                if (!capture) return t;

                const captureScope = self.genericCaptures.getCurrentScope();
                if (captureScope) |capScope| {
                    const clonedType = try clone.cloneAstTypesPtr(self.allocator, self, t, true);
                    const existing = capScope.get(name);

                    if (existing) |exist| {
                        free.freeType(self.allocator, exist);
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

    pub fn hasGeneric(self: Self, name: []u8) bool {
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

    pub fn setVariableType(self: *Self, name: []const u8, astType: *const blitzAst.AstTypes, isConst: bool) !void {
        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            const varInfo = VariableInfo{
                .varType = astType,
                .isConst = isConst,
            };
            try s.put(name, varInfo);
        }
    }

    pub fn removeVariableType(self: *Self, name: []u8) void {
        const scope = self.variableScopes.getCurrentScope();
        if (scope) |s| {
            _ = s.remove(name);
        }
    }

    pub fn getVariableType(self: *Self, name: []u8, replaceGenerics: bool) !?VariableInfo {
        var scope: ?*VarScope = self.variableScopes.getCurrentScope();
        defer self.variableScopes.resetLeakIndex();
        var capture = false;

        while (scope) |s| {
            if (s.get(name)) |t| {
                if (!capture) return t;

                const captureScope = self.variableCaptures.getCurrentScope();
                if (captureScope) |capScope| {
                    const clonedType = try clone.cloneAstTypesPtr(self.allocator, self, t.varType, replaceGenerics);
                    const varInfo = VariableInfo{
                        .varType = clonedType,
                        .isConst = t.isConst,
                    };

                    const existing = capScope.get(name);

                    if (existing) |exist| {
                        free.freeType(self.allocator, exist.varType);
                    }

                    try capScope.put(name, varInfo);
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

    pub fn getVariableTypeFixed(self: *Self, name: []u8) ?VariableInfo {
        const scope = self.variableScopes.getCurrentScope();

        if (scope) |s| {
            if (s.get(name)) |t| {
                return t;
            }
        }

        return null;
    }

    pub fn popFunctionScope(self: *Self) void {
        if (self.functions.items.len == 1) return;
        const scope = self.functions.pop().?;
        const it = scope.valueIterator();
        while (it.next()) |f| {
            free.freeFuncDec(self.allocator, f.*);
        }
        scope.deinit();
        self.allocator.destroy(scope);
    }

    pub fn addFunction(self: *Self, name: []u8, dec: *blitzAst.FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn functionInScope(self: Self, name: []u8) bool {
        var scope: ?*StringListScope = self.functionsInScope.getCurrentScope();
        defer self.functionsInScope.resetLeakIndex();

        while (scope) |s| {
            for (s.items) |item| {
                if (string.compString(item, name)) return true;
            }

            const nextLeak = self.functionsInScope.getNextInLeak();
            if (nextLeak) |next| {
                scope = next.scope;
            } else {
                scope = null;
            }
        }

        return false;
    }

    pub fn getFunction(self: Self, name: []u8) !?*blitzAst.FuncDecNode {
        if (!self.functionInScope(name)) {
            return ScanError.FunctionNotInScope;
        }

        return self.getFunctionAsGlobal(name);
    }

    pub fn getFunctionAsGlobal(self: Self, name: []u8) ?*blitzAst.FuncDecNode {
        return self.functions.get(name);
    }

    pub fn hasFunctionName(self: Self, name: []u8) bool {
        return self.functions.getLast().contains(name);
    }

    pub fn setStructDec(self: *Self, name: []u8, node: *blitzAst.StructDecNode) !void {
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

    pub fn getStructDec(self: Self, name: []u8) ?*const blitzAst.StructDecNode {
        return self.structDecs.get(name);
    }

    pub fn getErrorDec(self: Self, name: []u8) ?*const blitzAst.ErrorDecNode {
        return self.errorDecs.get(name);
    }

    pub fn addCurrentStruct(self: *Self, name: []u8) !void {
        try self.currentStructs.append(name);
        try self.distFromStructMethod.append(0);
    }

    pub fn getCurrentStruct(self: Self) ?[]u8 {
        return self.currentStructs.getLastOrNull();
    }

    pub fn popCurrentStruct(self: *Self) ?[]u8 {
        _ = self.distFromStructMethod.pop();
        return self.currentStructs.pop();
    }

    pub fn enteringStruct(self: *Self) void {
        const len = self.distFromStructMethod.items.len;
        if (len > 0) {
            self.distFromStructMethod.items[len - 1] += 1;
        }
    }

    pub fn exitingStruct(self: *Self) void {
        const len = self.distFromStructMethod.items.len;
        if (len > 0) {
            self.distFromStructMethod.items[len - 1] -= 1;
        }
    }

    pub fn isInStructMethod(self: Self) bool {
        const len = self.distFromStructMethod.items.len;
        return self.getCurrentStruct() != null and len > 0 and self.distFromStructMethod.items[len - 1] > 0;
    }
};

pub const TokenUtil = struct {
    const Self = @This();

    allocator: Allocator,
    index: usize,
    currentLine: usize,
    currentLineToken: usize,
    tokens: []tokenizer.Token,
    windows: *ArrayList(usize),
    logger: *Logger,

    pub fn init(allocator: Allocator, logger: *Logger, tokens: []tokenizer.Token) !Self {
        const windows = try initMutPtrT(ArrayList(usize), allocator);

        return Self{
            .allocator = allocator,
            .index = 0,
            .currentLine = 0,
            .currentLineToken = 0,
            .tokens = tokens,
            .windows = windows,
            .logger = logger,
        };
    }

    pub fn deinit(self: *Self) void {
        self.windows.deinit();
        self.allocator.destroy(self.windows);
    }

    pub fn reset(self: *Self) void {
        self.index = 0;
        self.currentLine = 0;
        self.currentLineToken = 0;
        self.windows.clearRetainingCapacity();
    }

    pub fn take(self: *Self) !tokenizer.Token {
        const res = try self.takeFixed();

        if (res.type == .NewLine) {
            return try self.take();
        }

        return res;
    }

    pub fn takeFixed(self: *Self) !tokenizer.Token {
        if (self.index >= self.tokens.len) {
            return self.logger.logError(AstError.ExpectedTokenFoundNothing);
        }

        const res = self.tokens[self.index];
        self.index += 1;
        self.currentLineToken += 1;

        if (res.type == .NewLine) {
            self.currentLine += 1;
            self.currentLineToken = 0;
        }

        return res;
    }

    pub fn peakFixed(self: Self) !tokenizer.Token {
        if (self.index >= self.tokens.len) {
            return AstError.ExpectedTokenFoundNothing;
        }

        return self.tokens[self.index];
    }

    pub fn peak(self: *Self) !tokenizer.Token {
        const res = try self.peakFixed();

        if (res.type == .NewLine) {
            const prevCurrentLineToken = self.currentLineToken;

            self.index += 1;
            self.currentLine += 1;
            self.currentLineToken = 0;

            const newRes = self.peak();

            self.index -= 1;
            self.currentLine -= 1;
            self.currentLineToken = prevCurrentLineToken;

            return newRes;
        }

        return res;
    }

    pub fn returnToken(self: *Self) void {
        self.index -= 1;
        self.currentLineToken -= 1;
    }

    pub fn expectToken(self: *Self, tokenType: tokenizer.TokenType) !void {
        const token = try self.take();
        if (token.type != tokenType) {
            return self.logger.logError(AstError.UnexpectedToken);
        }
    }

    pub fn hasNextFixed(self: Self) bool {
        if (self.index < self.tokens.len) return true;
        return false;
    }

    pub fn hasNext(self: *Self) bool {
        _ = self.peak() catch {
            return false;
        };

        return true;
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
            const leaks = try initMutPtrT(ArrayList(usize), allocator);
            const scopes = try initMutPtrT(ArrayList(T), allocator);
            const captureIndexes = try initMutPtrT(ArrayList(usize), allocator);

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
                item.deinit();
                self.allocator.destroy(item);
            }

            self.scopes.deinit();
            self.allocator.destroy(self.scopes);

            self.leaks.deinit();
            self.allocator.destroy(self.leaks);

            self.captureIndexes.deinit();
            self.allocator.destroy(self.captureIndexes);
        }

        pub fn addCaptureIndex(self: *Self) !void {
            const index = self.scopes.items.len;
            try self.captureIndexes.append(index);
        }

        pub fn add(self: *Self, scope: T, leak: bool) !void {
            try self.scopes.append(scope);

            if (leak) {
                const index = self.leaks.items.len - 1;
                self.leaks.items[index] += 1;
            } else {
                try self.leaks.append(0);
            }

            self.current = self.scopes.items.len;
        }

        pub fn release(self: *Self) ?T {
            if (self.scopes.items.len == 0) return null;

            while (self.captureIndexes.items.len > 0 and self.captureIndexes.getLast() >= self.scopes.items.len - 1) {
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
                item.deinit();
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
            const capture = lastCapture != null;

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
    retType: ?*const blitzAst.AstTypes,
    inFunction: bool,
    exhaustive: bool,
    lockExhaustive: bool,
};

pub const ReturnInfo = struct {
    const Self = @This();

    allocator: Allocator,
    info: *ReturnInfoData,

    pub fn init(allocator: Allocator) !Self {
        const info = try createMut(ReturnInfoData, allocator, .{
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
        self.info = try createMut(ReturnInfoData, self.allocator, .{
            .retType = null,
            .inFunction = oldRetInfo.inFunction or isFunction,
            .exhaustive = true,
            .lockExhaustive = false,
        });
        return oldRetInfo;
    }

    pub fn swapFree(self: *Self, oldRetInfo: *ReturnInfoData) void {
        if (self.info.retType) |retType| {
            free.freeType(self.allocator, retType);
        }
        self.allocator.destroy(self.info);
        self.info = oldRetInfo;
    }

    /// IMPORTANT - invalidates prev
    pub fn collapse(self: *Self, compInfo: *CompInfo, prev: *ReturnInfoData, withGenDef: bool) !void {
        if (prev.retType) |retType| {
            if (self.info.retType) |firstRetType| {
                if (!(try scanner.matchTypes(self.allocator, compInfo, retType, firstRetType, withGenDef))) {
                    return ScanError.FunctionReturnsHaveDifferentTypes;
                }

                free.freeType(self.allocator, retType);
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

fn freeVariableScope(allocator: Allocator, scope: *VarScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |v| {
        free.freeType(allocator, v.varType);
    }
}

pub const freeVariableCaptures = freeVariableScope;

pub const freeGenericCaptures = freeGenericScope;

fn freeScopedFunctionScope(allocator: Allocator, scope: *StringListScope) void {
    for (scope.items) |item| {
        allocator.free(item);
    }
}

fn freeGenericScope(allocator: Allocator, scope: *TypeScope) void {
    var genericIt = scope.valueIterator();
    while (genericIt.next()) |gen| {
        free.freeType(allocator, gen.*);
    }
}

fn freeParsedGenerics(allocator: Allocator, scope: *StringListScope) void {
    _ = allocator;
    _ = scope;
    // no operation needed
}
