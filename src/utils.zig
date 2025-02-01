const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const astMod = @import("ast.zig");
const free = @import("free.zig");
const AstTypes = astMod.AstTypes;
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const FuncDecNode = astMod.FuncDecNode;
const StructDecNode = astMod.StructDecNode;
const freeCompInfo = free.freeCompInfo;

pub fn findChar(items: []const u8, start: usize, item: u8) ?usize {
    var i = start;

    while (i < items.len) : (i += 1) {
        if (items[i] == item) return i;
    }

    return null;
}

pub fn create(comptime T: type, allocator: Allocator, obj: anytype) Allocator.Error!*const T {
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

pub fn numberLength(num: usize) usize {
    if (num == 0) return 1;
    const float: f64 = @floatFromInt(num);
    return @as(usize, @intFromFloat(@floor(@log10(@abs(float))) + 1));
}

pub fn cloneString(allocator: Allocator, string: []u8) ![]u8 {
    return try allocator.dupe(u8, string);
}

pub inline fn compString(str1: []const u8, str2: []const u8) bool {
    return std.mem.eql(u8, str1, str2);
}

pub const CompInfo = struct {
    const Self = @This();

    structNames: [][]u8,
    generics: *ArrayList([]u8),
    variableTypes: *StringHashMap(*const AstTypes),
    functions: *StringHashMap(*const FuncDecNode),
    structs: *StringHashMap(*const StructDecNode),
    currentStructs: *ArrayList([]u8),
    // each number describes how far from
    // the struct method a child node is
    distFromStructMethod: *ArrayList(u32),
    preAst: bool,

    pub fn prepareForAst(self: *Self) void {
        self.preAst = false;
    }

    pub fn hasStruct(self: Self, name: []u8) bool {
        for (self.structNames) |structName| {
            if (compString(structName, name)) return true;
        }

        return false;
    }

    pub fn addGeneric(self: *Self, name: []u8) !void {
        try self.generics.append(name);
    }

    pub fn removeGeneric(self: *Self, name: []u8) void {
        for (self.generics.items, 0..) |item, index| {
            if (compString(item, name)) {
                _ = self.generics.swapRemove(index);
            }
        }
    }

    pub fn hasGeneric(self: Self, name: []u8) bool {
        for (self.generics.items) |item| {
            if (compString(item, name)) {
                return true;
            }
        }

        return false;
    }

    pub fn setVariableType(self: *Self, name: []u8, astType: *const AstTypes) !void {
        try self.variableTypes.put(name, astType);
    }

    pub fn removeVariableType(self: *Self, name: []u8) void {
        _ = self.variableTypes.remove(name);
    }

    pub fn getVariableType(self: *Self, name: []u8) ?AstTypes {
        const res = self.variableTypes.get(name);

        if (res) |t| {
            return t.*;
        }

        return null;
    }

    pub fn addFunction(self: *Self, name: []u8, dec: *const FuncDecNode) !void {
        try self.functions.put(name, dec);
    }

    pub fn getFunction(self: Self, name: []u8) ?*const FuncDecNode {
        return self.functions.get(name);
    }

    pub fn hasFunctionName(self: Self, name: []u8) bool {
        return self.functions.contains(name);
    }

    pub fn setStructDec(self: *Self, name: []u8, node: *const StructDecNode) !void {
        try self.structs.put(name, node);
    }

    pub fn setStructDecs(self: *Self, nodes: []*const StructDecNode) !void {
        for (nodes) |node| {
            try self.setStructDec(node.name, node);
        }
    }

    pub fn getStructDec(self: Self, name: []u8) ?*const StructDecNode {
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
