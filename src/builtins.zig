const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const scanner = blitz.scanner;
const string = blitz.string;
const Allocator = std.mem.Allocator;
const ScanError = scanner.ScanError;

const PropTypeMap = struct {
    prop: []u8,
    type: blitzAst.AstTypes,
};

pub fn validateDynamicArrayProps(str: []u8) bool {
    const sz = 5;
    const validProps: *const [sz][]const u8 = &[_][]const u8{
        @as([]const u8, "len"),
        @as([]const u8, "push"),
        @as([]const u8, "pop"),
        @as([]const u8, "pushFront"),
        @as([]const u8, "popFront"),
    };
    return validateProps(sz, validProps, str);
}

pub fn validateStaticArrayProps(str: []u8) bool {
    const str2: []const u8 = "len";
    const sz = 1;
    const validProps: *const [sz][]const u8 = &[_][]const u8{str2};
    return validateProps(sz, validProps, str);
}

pub fn validateStringProps(str: []u8) bool {
    const sz = 4;
    const validProps: *const [sz][]const u8 = &[_][]const u8{
        @as([]const u8, "len"),
        @as([]const u8, "concat"),
        @as([]const u8, "indexOf"),
        @as([]const u8, "lastIndexOf"),
    };
    return validateProps(sz, validProps, str);
}

fn validateProps(comptime sz: comptime_int, props: *const [sz][]const u8, prop: []u8) bool {
    for (props) |p| {
        if (string.compString(p, prop)) return true;
    }

    return false;
}

pub fn getStringPropTypes(allocator: Allocator, prop: []u8) !blitzAst.AstTypes {
    const typeMap = &[_]PropTypeMap{.{
        .prop = try utils.toSlice(u8, allocator, "len"),
        .type = .{
            .Number = blitzAst.AstNumberVariants.USize,
        },
    }};

    return getPropType(typeMap, prop);
}

pub fn getStaticArrayPropTypes(allocator: Allocator, prop: []u8) !blitzAst.AstTypes {
    const typeMap = &[_]PropTypeMap{.{
        .prop = try utils.toSlice(u8, allocator, "len"),
        .type = .{
            .Number = blitzAst.AstNumberVariants.USize,
        },
    }};

    return try getPropType(typeMap, prop);
}

fn getPropType(typeMap: []const PropTypeMap, prop: []u8) !blitzAst.AstTypes {
    for (typeMap) |item| {
        if (string.compString(item.prop, prop)) return item.type;
    }

    return ScanError.InvalidProperty;
}
