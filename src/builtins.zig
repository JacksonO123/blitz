const std = @import("std");
const astMod = @import("ast.zig");
const tokenizer = @import("tokenizer.zig");
const utils = @import("utils.zig");
const scanner = @import("scan.zig");
const AstTypes = astMod.AstTypes;
const ScanError = scanner.ScanError;
const AstNumberVariants = astMod.AstNumberVariants;
const toSlice = utils.toSlice;
const Allocator = std.mem.Allocator;
const compString = utils.compString;

const PropTypeMap = struct {
    prop: []u8,
    type: AstTypes,
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
        if (compString(p, prop)) return true;
    }

    return false;
}

pub fn getStringPropTypes(allocator: Allocator, prop: []u8) !AstTypes {
    const typeMap = &[_]PropTypeMap{.{
        .prop = try toSlice(u8, allocator, "len"),
        .type = .{
            .Number = AstNumberVariants.USize,
        },
    }};

    return getPropType(typeMap, prop);
}

pub fn getStaticArrayPropTypes(allocator: Allocator, prop: []u8) !AstTypes {
    const typeMap = &[_]PropTypeMap{.{
        .prop = try toSlice(u8, allocator, "len"),
        .type = .{
            .Number = AstNumberVariants.USize,
        },
    }};

    return try getPropType(typeMap, prop);
}

fn getPropType(typeMap: []const PropTypeMap, prop: []u8) !AstTypes {
    for (typeMap) |item| {
        if (compString(item.prop, prop)) return item.type;
    }

    return ScanError.InvalidProperty;
}
