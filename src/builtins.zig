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
    return validateProps(.{ "len", "push", "pop", "pushFront", "popFront" }, str);
}

pub fn validateStaticArrayProps(str: []u8) bool {
    return validateProps(.{"len"}, str);
}

pub fn validateStringProps(str: []u8) bool {
    return validateProps(.{
        "len",
        "concat",
        "indexOf",
        "lastIndexOf",
    }, str);
}

fn validateProps(comptime props: anytype, prop: []u8) bool {
    inline for (props) |p| {
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
