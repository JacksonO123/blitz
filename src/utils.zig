const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const string = blitz.string;
const free = blitz.free;
const Logger = blitz.logger.Logger;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const AstError = blitzAst.AstError;

pub inline fn create(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*const T {
    return createMut(T, allocator, obj);
}

pub inline fn createMut(comptime T: type, allocator: Allocator, obj: T) Allocator.Error!*T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}

pub fn readRelativeFile(allocator: Allocator, path: []u8) ![]u8 {
    const maxFileSize = 1028 * 4; // arbitrary
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, maxFileSize);
}

pub fn delimiterIndex(tokens: []tokenizer.Token, start: usize, delimiter: tokenizer.TokenType) !usize {
    var parens: u32 = 0;

    var i = start;
    while (i < tokens.len) : (i += 1) {
        if (parens == 0 and tokens[i].type == delimiter) return i;

        if (tokens[i].isOpenToken(true)) {
            parens += 1;
        } else if (tokens[i].isCloseToken(true)) {
            if (parens > 0) {
                parens -= 1;
            } else if (tokens[i].type != delimiter) return AstError.TokenNotFound;
        }
    }

    return AstError.TokenNotFound;
}

pub fn smartDelimiterIndex(tokens: []tokenizer.Token, compInfo: *CompInfo, start: usize, delimiter: tokenizer.TokenType) !usize {
    var current = start;
    var parens: u32 = 0;
    var inGeneric = false;
    var genericStart: u32 = 0;

    while (current < tokens.len) : (current += 1) {
        if (parens == 0 and tokens[current].type == delimiter) return current;

        if (tokens[current].type == .RAngle and inGeneric) {
            parens -= 1;
            if (parens == genericStart) {
                genericStart = 0;
                inGeneric = false;
            }
            continue;
        }

        if (tokens[current].type == .LAngle and current > 0) {
            const prevToken = tokens[current - 1];
            if (prevToken.type == .Identifier and compInfo.hasStruct(prevToken.string.?)) {
                if (!inGeneric) {
                    genericStart = parens;
                    inGeneric = true;
                }

                parens += 1;
                continue;
            }
        }

        if (tokens[current].isOpenToken(false)) {
            parens += 1;
        } else if (tokens[current].isCloseToken(false)) {
            if (parens == 0) return AstError.TokenNotFound;
            parens -= 1;
        }
    }

    return AstError.TokenNotFound;
}

fn initPtrT(comptime T: type, allocator: Allocator) !*T {
    const data = T.init(allocator);
    return try createMut(T, allocator, data);
}

const Scope = *StringHashMap(*const blitzAst.AstTypes);

pub const CompInfo = struct {
    const Self = @This();

    allocator: Allocator,
    structNames: [][]u8,
    errorNames: [][]u8,
    availableGenerics: *ArrayList(*ArrayList([]u8)),
    variableScopes: *ArrayList(Scope),
    functions: *StringHashMap(*const blitzAst.FuncDecNode),
    structDecs: *StringHashMap(*blitzAst.StructDecNode),
    errorDecs: *StringHashMap(*const blitzAst.ErrorDecNode),
    currentStructs: *ArrayList([]u8),
    // each number describes how far from
    // the struct method a child node is
    distFromStructMethod: *ArrayList(u32),
    genericScopes: *ArrayList(Scope),
    previosAccessedStruct: ?[]u8,
    preAst: bool,
    tokens: *TokenUtil,
    logger: *Logger,

    pub fn init(allocator: Allocator, tokens: []tokenizer.Token, names: blitzAst.StructAndErrorNames, code: *const []u8) !Self {
        const loggerUtil = try allocator.create(Logger);
        const tokenUtil = try createMut(TokenUtil, allocator, try TokenUtil.init(allocator, loggerUtil, tokens));
        loggerUtil.* = Logger.init(allocator, tokenUtil, code);

        const availableGenerics = try initPtrT(ArrayList(*ArrayList([]u8)), allocator);
        const availableGenScope = try initPtrT(ArrayList([]u8), allocator);
        try availableGenerics.append(availableGenScope);

        const currentStructs = try initPtrT(ArrayList([]u8), allocator);
        const distFromStructMethod = try initPtrT(ArrayList(u32), allocator);
        const functions = try initPtrT(StringHashMap(*const blitzAst.FuncDecNode), allocator);
        const structs = try initPtrT(StringHashMap(*blitzAst.StructDecNode), allocator);

        const errors = try initPtrT(StringHashMap(*const blitzAst.ErrorDecNode), allocator);

        const genericMap = try initPtrT(StringHashMap(*const blitzAst.AstTypes), allocator);
        const genericScopes = try initPtrT(ArrayList(Scope), allocator);
        try genericScopes.append(genericMap);

        const baseScope = try initPtrT(StringHashMap(*const blitzAst.AstTypes), allocator);
        const variableScopes = try initPtrT(ArrayList(Scope), allocator);
        try variableScopes.append(baseScope);

        return Self{
            .allocator = allocator,
            .structNames = names.structNames,
            .errorNames = names.errorNames,
            .availableGenerics = availableGenerics,
            .variableScopes = variableScopes,
            .functions = functions,
            .structDecs = structs,
            .errorDecs = errors,
            .currentStructs = currentStructs,
            .distFromStructMethod = distFromStructMethod,
            .genericScopes = genericScopes,
            .previosAccessedStruct = null,
            .preAst = true,
            .tokens = tokenUtil,
            .logger = loggerUtil,
        };
    }

    pub fn deinit(self: *Self) void {
        while (self.variableScopes.items.len > 0) {
            self.popScopeInternal();
        }

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

        for (self.genericScopes.items) |gens| {
            var genericIt = gens.valueIterator();
            while (genericIt.next()) |gen| {
                free.freeType(self.allocator, gen.*);
            }
            self.allocator.destroy(gens);
        }

        free.freeNestedSlice(u8, self.allocator, self.structNames);
        free.freeNestedSlice(u8, self.allocator, self.errorNames);

        for (self.currentStructs.items) |item| {
            self.allocator.free(item);
        }

        self.availableGenerics.deinit();
        self.allocator.destroy(self.availableGenerics);

        self.variableScopes.deinit();
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

        self.genericScopes.deinit();
        self.allocator.destroy(self.genericScopes);

        self.tokens.deinit();
        self.allocator.destroy(self.tokens);

        self.logger.deinit();
        self.allocator.destroy(self.logger);
    }

    pub fn pushScope(self: *Self) !void {
        const newScope = try initPtrT(StringHashMap(*const blitzAst.AstTypes), self.allocator);
        try self.variableScopes.append(newScope);
    }

    /// no guard on empty scope array
    fn popScopeInternal(self: *Self) void {
        const scope = self.getCurrentScope();

        var scopeIt = scope.valueIterator();
        while (scopeIt.next()) |v| {
            free.freeType(self.allocator, v.*);
        }

        scope.deinit();
        self.allocator.destroy(scope);
        _ = self.variableScopes.pop();
    }

    pub fn popScope(self: *Self) void {
        if (self.variableScopes.items.len == 1) return;
        self.popScopeInternal();
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

        // {
        //     var structIt = self.structDecs.valueIterator();
        //     while (structIt.next()) |s| {
        //         for (s.*.generics) |gen| {
        //             try self.addAvailableGeneric(gen.name);
        //         }
        //
        //         defer {
        //             for (s.*.generics) |gen| {
        //                 self.removeAvailableGeneric(gen.name);
        //             }
        //         }
        //
        //         const attributes = s.*.attributes;
        //         for (attributes) |attr| {
        //             if (attr.attr != .Function) continue;
        //
        //             const f = attr.attr.Function;
        //             free.freeNode(self.allocator, f.body);
        //             f.body = (try blitzAst.createAstNode(self.allocator, self, f.bodyTokens)).node;
        //         }
        //     }
        // }
    }

    pub fn setPreviousAccessedStruct(self: *Self, name: ?[]u8) void {
        self.previosAccessedStruct = name;
    }

    pub fn getPreviousAccessedStruct(self: Self) ?[]u8 {
        return self.previosAccessedStruct;
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

    pub fn getCurrentGenScope(self: Self) Scope {
        return self.genericScopes.getLast();
    }

    pub fn pushGenScope(self: *Self) !void {
        const genScope = try initPtrT(StringHashMap(*const blitzAst.AstTypes), self.allocator);
        try self.genericScopes.append(genScope);
    }

    pub fn popGenScope(self: *Self) void {
        if (self.genericScopes.items.len == 1) return;

        const genScope = self.getCurrentGenScope();

        var genScopeIt = genScope.valueIterator();
        while (genScopeIt.next()) |item| {
            free.freeType(self.allocator, item.*);
        }

        genScope.deinit();
        self.allocator.destroy(genScope);
        _ = self.genericScopes.pop();
    }

    pub fn setGeneric(self: *Self, name: []u8, gType: *const blitzAst.AstTypes) !void {
        const genScope = self.getCurrentGenScope();

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
        const genScope = self.getCurrentGenScope();
        return genScope.get(name);
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
        const newScope = try initPtrT(ArrayList([]u8), self.allocator);
        try self.availableGenerics.append(newScope);
    }

    pub fn popRegGenScope(self: *Self) void {
        if (self.availableGenerics.items.len == 1) return;

        const genScope = self.getCurrentRegGenScope();

        genScope.deinit();
        self.allocator.destroy(genScope);
        _ = self.availableGenerics.pop();
    }

    pub fn getCurrentScope(self: Self) Scope {
        return self.variableScopes.getLast();
    }

    pub fn getPrevScope(self: *Self, current: Scope) ?Scope {
        var i: usize = self.variableScopes.items.len - 1;

        while (i > 0) {
            if (self.variableScopes.items[i] == current) {
                return self.variableScopes.items[i - 1];
            }

            i -= 1;
        }

        return null;
    }

    pub fn setVariableType(self: *Self, name: []u8, astType: *const blitzAst.AstTypes) !void {
        const scope = self.getCurrentScope();
        try scope.put(name, astType);
    }

    pub fn removeVariableType(self: *Self, name: []u8) void {
        const scope = self.getCurrentScope();
        _ = scope.remove(name);
    }

    pub fn getVariableType(self: *Self, name: []u8) ?blitzAst.AstTypes {
        var scope: ?Scope = self.getCurrentScope();

        while (scope) |s| {
            if (s.get(name)) |t| {
                return t.*;
            }

            scope = self.getPrevScope(s);
        }

        return null;
    }

    pub fn addFunction(self: *Self, name: []u8, dec: *const blitzAst.FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn getFunction(self: Self, name: []u8) ?*const blitzAst.FuncDecNode {
        return self.functions.get(name);
    }

    pub fn hasFunctionName(self: Self, name: []u8) bool {
        return self.functions.contains(name);
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
        const windows = try initPtrT(ArrayList(usize), allocator);

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

    // fn incWindow(self: *Self) void {
    //     if (self.windows.items.len == 0) return;
    //
    //     const index = self.windows.items.len - 1;
    //     self.windows.items[index] += 1;
    // }
    //
    // pub fn collapseWindow(self: *Self) void {
    //     if (self.windows.items.len == 0) return;
    //
    //     const index = self.windows.items.len - 1;
    //     self.index -= self.windows.items[index];
    //     self.windows.pop();
    // }

    pub fn take(self: *Self) !tokenizer.Token {
        if (self.index == self.tokens.len) {
            return self.logger.logError(AstError.ExpectedTokenFoundNothing);
        }

        const res = self.tokens[self.index];
        // self.incWindow();
        self.index += 1;
        self.currentLineToken += 1;

        if (res.type == .NewLine) {
            self.currentLine += 1;
            self.currentLineToken = 0;
            if (self.hasNext()) return self.take();
            return res;
        }
        return res;
    }

    pub fn peak(self: Self) !tokenizer.Token {
        if (self.index < self.tokens.len) {
            return self.tokens[self.index];
        }

        return AstError.ExpectedTokenFoundNothing;
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

    pub fn hasNext(self: Self) bool {
        if (self.index < self.tokens.len) return true;
        return false;
    }
};
