const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const string = blitz.string;
const free = blitz.free;
const scanner = blitz.scanner;
const Logger = blitz.logger.Logger;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
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

fn initMutPtrT(comptime T: type, allocator: Allocator) !*T {
    const data = T.init(allocator);
    return try createMut(T, allocator, data);
}

const VariableInfo = struct {
    varType: *const blitzAst.AstTypes,
    isConst: bool,
};

const TypeScope = StringHashMap(*const blitzAst.AstTypes);
const VarScope = StringHashMap(VariableInfo);

pub const CompInfo = struct {
    const Self = @This();

    allocator: Allocator,
    structNames: [][]u8,
    errorNames: [][]u8,
    availableGenerics: *ArrayList(*ArrayList([]u8)),
    variableScopes: *ScopeUtil(*VarScope),
    // how many scopes up the current scope has access
    functions: *StringHashMap(*const blitzAst.FuncDecNode),
    functionsInScope: *ScopeUtil(*ArrayList([]u8)),
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

    pub fn init(allocator: Allocator, tokens: []tokenizer.Token, names: blitzAst.HoistedNames, code: []const u8) !Self {
        const loggerUtil = try allocator.create(Logger);
        const tokenUtil = try createMut(TokenUtil, allocator, try TokenUtil.init(allocator, loggerUtil, tokens));
        loggerUtil.* = Logger.init(allocator, tokenUtil, code);

        const availableGenerics = try initMutPtrT(ArrayList(*ArrayList([]u8)), allocator);
        const availableGenScope = try initMutPtrT(ArrayList([]u8), allocator);
        try availableGenerics.append(availableGenScope);

        const currentStructs = try initMutPtrT(ArrayList([]u8), allocator);
        const distFromStructMethod = try initMutPtrT(ArrayList(u32), allocator);
        const functions = try initMutPtrT(StringHashMap(*const blitzAst.FuncDecNode), allocator);
        const structs = try initMutPtrT(StringHashMap(*blitzAst.StructDecNode), allocator);

        const errors = try initMutPtrT(StringHashMap(*const blitzAst.ErrorDecNode), allocator);

        const genericMap = try initMutPtrT(StringHashMap(*const blitzAst.AstTypes), allocator);
        const genericScopesUtil = try ScopeUtil(*TypeScope).init(allocator);
        const genericScopes = try createMut(ScopeUtil(*TypeScope), allocator, genericScopesUtil);
        try genericScopes.add(genericMap, false);

        const baseFunctionsInScope = try initMutPtrT(ArrayList([]u8), allocator);
        const functionsInScopeUtil = try ScopeUtil(*ArrayList([]u8)).init(allocator);
        const functionsInScope = try createMut(ScopeUtil(*ArrayList([]u8)), allocator, functionsInScopeUtil);
        try functionsInScope.add(baseFunctionsInScope, false);

        const baseScope = try initMutPtrT(VarScope, allocator);
        const variableScopesUtil = try ScopeUtil(*VarScope).init(allocator);
        const variableScopes = try createMut(ScopeUtil(*VarScope), allocator, variableScopesUtil);
        try variableScopes.add(baseScope, false);

        return Self{
            .allocator = allocator,
            .structNames = names.structNames,
            .errorNames = names.errorNames,
            .availableGenerics = availableGenerics,
            .variableScopes = variableScopes,
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
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.availableGenerics.items) |gens| {
            gens.deinit();
            self.allocator.destroy(gens);
        }

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

        self.availableGenerics.deinit();
        self.allocator.destroy(self.availableGenerics);

        self.variableScopes.deinit(freeVariableScope);
        self.allocator.destroy(self.variableScopes);

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

    pub fn pushScope(self: *Self, leak: bool) !void {
        const scope = try initMutPtrT(StringHashMap(VariableInfo), self.allocator);
        try self.variableScopes.add(scope, leak);
        try self.pushScopedFunctionScope(leak);
    }

    pub fn popScope(self: *Self) void {
        self.variableScopes.pop(freeVariableScope);
        self.popScopedFunctionScope();
    }

    pub fn pushScopedFunctionScope(self: *Self, leak: bool) !void {
        const scope = try initMutPtrT(ArrayList([]u8), self.allocator);
        try self.functionsInScope.add(scope, leak);
    }

    pub fn popScopedFunctionScope(self: *Self) void {
        self.functionsInScope.pop(freeScopedFunctionScope);
    }

    pub fn addScopedFunction(self: *Self, name: []u8) !void {
        const scope = self.functionsInScope.getCurrentScope();
        try scope.append(name);
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
                for (s.*.generics) |gen| {
                    try self.addAvailableGeneric(gen.name);
                }

                defer {
                    for (s.*.generics) |gen| {
                        self.removeAvailableGeneric(gen.name);
                    }
                }

                const attributes = s.*.attributes;
                for (attributes) |attr| {
                    if (attr.attr != .Function) continue;

                    const f = attr.attr.Function;
                    free.freeNode(self.allocator, f.body);

                    const oldTokens = self.tokens;
                    self.tokens = try createMut(TokenUtil, self.allocator, try TokenUtil.init(self.allocator, self.logger, f.bodyTokens));
                    f.body = try blitzAst.parseSequence(self.allocator, self);

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

    pub fn setGeneric(self: *Self, name: []u8, gType: *const blitzAst.AstTypes) !void {
        const genScope = self.genericScopes.getCurrentScope();

        const value = genScope.get(name);
        if (value) |genValue| {
            free.freeType(self.allocator, genValue);
        }

        try genScope.put(name, gType);
    }

    pub fn removeGeneric(self: *Self, name: []u8) void {
        const genScope = self.getCurrentGenScope();
        const gType = genScope.get(name);

        if (gType) |generic| {
            free.freeNode(self.allocator, generic);
        }

        self.genericScopes.remove(name);
    }

    pub fn getGeneric(self: *Self, name: []u8) ?*const blitzAst.AstTypes {
        var genScope: ?*TypeScope = self.genericScopes.getCurrentScope();
        defer self.genericScopes.resetLeakIndex();

        while (genScope) |s| {
            if (s.get(name)) |t| {
                return t;
            }

            genScope = self.genericScopes.getNextInLeak();
        }

        return null;
    }

    pub fn getCurrentRegGenScope(self: Self) *ArrayList([]u8) {
        return self.availableGenerics.getLast();
    }

    pub fn addAvailableGeneric(self: *Self, name: []u8) !void {
        const genScope = self.getCurrentRegGenScope();
        try genScope.append(name);
    }

    pub fn removeAvailableGeneric(self: *Self, name: []u8) void {
        const genScope = self.getCurrentRegGenScope();
        for (genScope.items, 0..) |gen, index| {
            if (string.compString(gen, name)) {
                _ = genScope.swapRemove(index);
            }
        }
    }

    pub fn hasRegGeneric(self: Self, name: []u8) bool {
        const genScope = self.getCurrentRegGenScope();

        for (genScope.items) |gen| {
            if (string.compString(gen, name)) {
                return true;
            }
        }

        return false;
    }

    pub fn hasGeneric(self: Self, name: []u8) bool {
        const genScope = self.getCurrentGenScope();
        return genScope.contains(name);
    }

    pub fn pushRegGenScope(self: *Self) !void {
        const newScope = try initMutPtrT(ArrayList([]u8), self.allocator);
        try self.availableGenerics.append(newScope);
    }

    pub fn popRegGenScope(self: *Self) void {
        if (self.availableGenerics.items.len == 1) return;

        const genScope = self.getCurrentRegGenScope();

        genScope.deinit();
        self.allocator.destroy(genScope);
        _ = self.availableGenerics.pop();
    }

    pub fn setVariableType(self: *Self, name: []u8, astType: *const blitzAst.AstTypes, isConst: bool) !void {
        const scope = self.variableScopes.getCurrentScope();
        const varInfo = VariableInfo{
            .varType = astType,
            .isConst = isConst,
        };
        try scope.put(name, varInfo);
    }

    pub fn removeVariableType(self: *Self, name: []u8) void {
        const scope = self.variableScopes.getCurrentScope();
        _ = scope.remove(name);
    }

    pub fn getVariableType(self: *Self, name: []u8) ?VariableInfo {
        var scope: ?*VarScope = self.variableScopes.getCurrentScope();
        defer self.variableScopes.resetLeakIndex();

        while (scope) |s| {
            if (s.get(name)) |t| {
                return t;
            }

            scope = self.variableScopes.getNextInLeak();
        }

        return null;
    }

    pub fn getVariableTypeFixed(self: *Self, name: []u8) ?VariableInfo {
        var scope = self.variableScopes.getCurrentScope();

        if (scope.get(name)) |t| {
            return t;
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

    pub fn addFunction(self: *Self, name: []u8, dec: *const blitzAst.FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn functionInScope(self: Self, name: []u8) bool {
        var scope: ?*ArrayList([]u8) = self.functionsInScope.getCurrentScope();
        defer self.functionsInScope.resetLeakIndex();

        while (scope) |s| {
            for (s.items) |item| {
                if (string.compString(item, name)) return true;
            }

            scope = self.functionsInScope.getNextInLeak();
        }

        return false;
    }

    pub fn getFunction(self: Self, name: []u8) !?*const blitzAst.FuncDecNode {
        if (!self.functionInScope(name)) {
            return ScanError.FunctionNotInScope;
        }

        return self.getFunctionAsGlobal(name);
    }

    pub fn getFunctionAsGlobal(self: Self, name: []u8) ?*const blitzAst.FuncDecNode {
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

    pub fn addWindow(self: *Self) !void {
        try self.windows.append(0);
    }

    fn incWindow(self: *Self) void {
        if (self.windows.items.len == 0) return;

        const index = self.windows.items.len - 1;
        self.windows.items[index] += 1;
    }

    pub fn collapseWindow(self: *Self) void {
        if (self.windows.items.len == 0) return;

        const index = self.windows.items.len - 1;
        self.index -= self.windows.items[index];
        _ = self.windows.pop();
    }

    pub fn getWindowSize(self: Self) usize {
        if (self.windows.items.len == 0) return 0;
        const index = self.windows.items.len - 1;
        return self.windows.items[index];
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
        self.incWindow();
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

fn ScopeUtil(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        scopes: *ArrayList(T),
        leaks: *ArrayList(usize),
        current: usize,

        pub fn init(allocator: Allocator) !Self {
            const leaks = try initMutPtrT(ArrayList(usize), allocator);
            const scopes = try initMutPtrT(ArrayList(T), allocator);

            return Self{
                .allocator = allocator,
                .scopes = scopes,
                .leaks = leaks,
                .current = 0,
            };
        }

        pub fn deinit(self: *Self, freeFn: fn (Allocator, T) void) void {
            for (self.scopes.items) |item| {
                freeFn(self.allocator, item);
            }
            self.scopes.deinit();
            self.allocator.destroy(self.scopes);

            self.leaks.deinit();
            self.allocator.destroy(self.leaks);
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

        pub fn pop(self: *Self, freeFn: fn (Allocator, T) void) void {
            if (self.scopes.items.len == 1) return;

            const last = self.scopes.pop();
            if (last) |item| {
                freeFn(self.allocator, item);
            }

            const index = self.leaks.items.len - 1;
            if (self.leaks.items[index] == 0) {
                _ = self.leaks.pop();
            } else {
                self.leaks.items[index] -= 1;
            }

            self.current = self.scopes.items.len;
        }

        pub fn getCurrentScope(self: Self) T {
            return self.scopes.getLast();
        }

        pub fn getNextInLeak(self: *Self) ?T {
            if (self.current == 0) return null;
            const leak = self.leaks.getLast();
            const diff = self.scopes.items.len - self.current;
            if (diff == leak + 1) return null;

            self.current -= 1;
            const res = self.scopes.items[self.current];

            return res;
        }

        pub fn resetLeakIndex(self: *Self) void {
            self.current = self.scopes.items.len;
        }

        pub fn getIndex(self: Self, index: usize) T {
            return self.scopes.items[index];
        }

        pub fn count(self: Self) usize {
            return self.scopes.items.len;
        }

        pub fn getLastLeak(self: Self) usize {
            return self.leaks.getLast();
        }
    };
}

fn freeVariableScope(allocator: Allocator, scope: *VarScope) void {
    var scopeIt = scope.valueIterator();
    while (scopeIt.next()) |v| {
        free.freeType(allocator, v.*.varType);
    }
    scope.deinit();
    allocator.destroy(scope);
}

fn freeScopedFunctionScope(allocator: Allocator, scope: *ArrayList([]u8)) void {
    for (scope.items) |item| {
        allocator.free(item);
    }
    scope.deinit();
    allocator.destroy(scope);
}

fn freeGenericScope(allocator: Allocator, scope: *TypeScope) void {
    var genericIt = scope.valueIterator();
    while (genericIt.next()) |gen| {
        free.freeType(allocator, gen.*);
    }
    scope.deinit();
    allocator.destroy(scope);
}
