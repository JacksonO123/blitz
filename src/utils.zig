const std = @import("std");
const blitz = @import("root").blitz;
const tokenizer = blitz.tokenizer;
const blitzAst = blitz.ast;
const string = blitz.string;
const free = blitz.free;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const AstError = blitzAst.AstError;

pub inline fn create(comptime T: type, allocator: Allocator, obj: anytype) Allocator.Error!*const T {
    return createMut(T, allocator, obj);
}

pub inline fn createMut(comptime T: type, allocator: Allocator, obj: anytype) Allocator.Error!*T {
    const ptr = try allocator.create(T);
    ptr.* = obj;
    return ptr;
}

pub fn toSlice(comptime T: type, allocator: Allocator, data: anytype) ![]T {
    var list = ArrayList(T).init(allocator);
    defer list.deinit();
    try list.resize(data.len);
    std.mem.copyForwards(T, list.items, data);
    const res = try allocator.dupe(T, list.items);
    return res;
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

        if (tokenizer.isOpenToken(tokens[i].type, true)) {
            parens += 1;
        } else if (tokenizer.isCloseToken(tokens[i].type, true)) {
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

        if (tokenizer.isOpenToken(tokens[current].type, false)) {
            parens += 1;
        } else if (tokenizer.isCloseToken(tokens[current].type, false)) {
            if (parens == 0) return AstError.TokenNotFound;
            parens -= 1;
        }
    }

    return AstError.TokenNotFound;
}

pub const CompInfo = struct {
    const Self = @This();

    structNames: [][]u8,
    generics: *ArrayList([]u8),
    variableTypes: *StringHashMap(*const blitzAst.AstTypes),
    functions: *StringHashMap(*const blitzAst.FuncDecNode),
    structs: *StringHashMap(*const blitzAst.StructDecNode),
    currentStructs: *ArrayList([]u8),
    // each number describes how far from
    // the struct method a child node is
    distFromStructMethod: *ArrayList(u32),
    preAst: bool,
    allocator: Allocator,

    pub inline fn init(allocator: Allocator, structNames: [][]u8) Self {
        var genericsList = ArrayList([]u8).init(allocator);
        var currentStructs = ArrayList([]u8).init(allocator);
        var distFromStructMethod = ArrayList(u32).init(allocator);
        var functions = StringHashMap(*const blitzAst.FuncDecNode).init(allocator);
        var variableTypes = StringHashMap(*const blitzAst.AstTypes).init(allocator);
        var structs = StringHashMap(*const blitzAst.StructDecNode).init(allocator);

        return Self{
            .structNames = structNames,
            .generics = &genericsList,
            .variableTypes = &variableTypes,
            .functions = &functions,
            .structs = &structs,
            .currentStructs = &currentStructs,
            .distFromStructMethod = &distFromStructMethod,
            .preAst = true,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        var variableIt = self.variableTypes.valueIterator();
        while (variableIt.next()) |valuePtr| {
            free.freeType(self.allocator, valuePtr.*);
        }

        var functionIt = self.functions.valueIterator();
        while (functionIt.next()) |f| {
            free.freeFuncDec(self.allocator, f.*);
        }

        var structsIt = self.structs.valueIterator();
        while (structsIt.next()) |dec| {
            free.freeStructDec(self.allocator, dec.*);
            self.allocator.destroy(dec.*);
        }

        free.freeStructNames(self.allocator, self.structNames);

        for (self.currentStructs.items) |item| {
            self.allocator.free(item);
        }

        self.generics.deinit();
        self.variableTypes.deinit();
        self.functions.deinit();
        self.structs.deinit();
        self.currentStructs.deinit();
        self.distFromStructMethod.deinit();
    }

    pub fn prepareForAst(self: *Self) void {
        self.preAst = false;
    }

    pub fn hasStruct(self: Self, name: []u8) bool {
        for (self.structNames) |structName| {
            if (string.compString(structName, name)) return true;
        }

        return false;
    }

    pub fn addGeneric(self: *Self, name: []u8) !void {
        try self.generics.append(name);
    }

    pub fn removeGeneric(self: *Self, name: []u8) void {
        for (self.generics.items, 0..) |item, index| {
            if (string.compString(item, name)) {
                _ = self.generics.swapRemove(index);
            }
        }
    }

    pub fn hasGeneric(self: Self, name: []u8) bool {
        for (self.generics.items) |item| {
            if (string.compString(item, name)) {
                return true;
            }
        }

        return false;
    }

    pub fn setVariableType(self: *Self, name: []u8, astType: *const blitzAst.AstTypes) !void {
        try self.variableTypes.put(name, astType);
    }

    pub fn removeVariableType(self: *Self, name: []u8) void {
        _ = self.variableTypes.remove(name);
    }

    pub fn getVariableType(self: *Self, name: []u8) ?blitzAst.AstTypes {
        const res = self.variableTypes.get(name);

        if (res) |t| {
            return t.*;
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

    pub fn setStructDec(self: *Self, name: []u8, node: *const blitzAst.StructDecNode) !void {
        try self.structs.put(name, node);
    }

    pub fn setStructDecs(self: *Self, nodes: []*const blitzAst.StructDecNode) !void {
        for (nodes) |node| {
            try self.setStructDec(node.name, node);
        }
    }

    pub fn getStructDec(self: Self, name: []u8) ?*const blitzAst.StructDecNode {
        return self.structs.get(name);
    }

    pub fn addCurrentStruct(self: *Self, name: []u8) !void {
        try self.currentStructs.append(name);
        try self.distFromStructMethod.append(0);
    }

    pub fn getCurrentStruct(self: Self) ?[]u8 {
        return self.currentStructs.getLastOrNull();
    }

    pub fn popCurrentStruct(self: *Self) ?[]u8 {
        _ = self.distFromStructMethod.popOrNull();
        return self.currentStructs.popOrNull();
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
