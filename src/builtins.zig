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
    prop: *const []u8,
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

pub fn getStringPropTypes(prop: []u8) !blitzAst.AstTypes {
    const props = .{"len"};
    const types = &[_]blitzAst.AstTypes{
        .{
            .Number = blitzAst.AstNumberVariants.USize,
        },
    };

    const index = try getPropType(props, prop);
    return types[index];
}

pub fn getStaticArrayPropTypes(prop: []u8) !blitzAst.AstTypes {
    const props = .{"len"};
    const types = &[_]blitzAst.AstTypes{
        .{
            .Number = blitzAst.AstNumberVariants.USize,
        },
    };

    const index = try getPropType(props, prop);
    return types[index];
}

fn getPropType(comptime props: anytype, prop: []u8) !usize {
    inline for (props, 0..) |item, index| {
        if (string.compString(item, prop)) return index;
    }

    return ScanError.InvalidProperty;
}
