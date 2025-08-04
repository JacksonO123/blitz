const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const scanner = blitz.scanner;
const string = blitz.string;
const clone = blitz.clone;
const free = blitz.free;
const Allocator = std.mem.Allocator;
const ScanError = scanner.ScanError;
const CompInfo = utils.CompInfo;
const createMut = utils.createMut;
const create = utils.create;

pub const BuiltinFuncMemo = struct {};

const PropTypeMap = struct {
    prop: *const []u8,
    type: blitzAst.AstTypes,
};

fn toNumSig(allocator: Allocator, num: blitzAst.AstNumberVariants) !*const blitzAst.AstTypes {
    return try create(blitzAst.AstTypes, allocator, .{ .Number = num });
}

fn paramsFromTypes(allocator: Allocator, paramTypes: []const blitzAst.AstTypeInfo) ![]blitzAst.Parameter {
    var res = try allocator.alloc(blitzAst.Parameter, paramTypes.len);

    for (paramTypes, 0..) |param, index| {
        const hex = utils.intToHex(index);
        res[index] = .{
            .name = try string.cloneString(allocator, &[_]u8{hex}),
            .type = param,
        };
    }

    return res;
}

fn toFuncSignature(
    allocator: Allocator,
    compInfo: *CompInfo,
    name: []const u8,
    paramTypes: []const blitzAst.AstTypeInfo,
    returnType: blitzAst.AstTypeInfo,
    withGenDef: bool,
) !*blitzAst.FuncDecNode {
    const params = try paramsFromTypes(allocator, paramTypes);

    return try createMut(blitzAst.FuncDecNode, allocator, .{
        .name = try string.cloneString(allocator, name),
        .generics = null,
        .params = params,
        .body = try create(blitzAst.AstNode, allocator, .NoOp),
        .bodyTokens = &[_]tokenizer.Token{},
        .returnType = try clone.cloneAstTypeInfo(allocator, compInfo, returnType, withGenDef),
        .capturedValues = null,
        .capturedTypes = null,
        .builtin = true,
    });
}

fn updateBuiltinFn(
    allocator: Allocator,
    compInfo: *CompInfo,
    builtinFn: *?*blitzAst.FuncDecNode,
    params: []const blitzAst.AstTypeInfo,
    returnType: blitzAst.AstTypeInfo,
    withGenDef: bool,
) !*const blitzAst.AstTypes {
    if (builtinFn.*) |builtin| {
        free.shallowFreeFuncDecParams(allocator, builtin.params);
        builtin.params = try paramsFromTypes(allocator, params);
        builtin.returnType = returnType;
    } else {
        builtinFn.* = try toFuncSignature(allocator, compInfo, "push", params, returnType, withGenDef);
    }

    return try create(blitzAst.AstTypes, allocator, .{ .Function = builtinFn.*.? });
}

const PropInfo = struct {
    name: []const u8,
    signature: *const blitzAst.AstTypes,
    isFunc: bool,
};

pub fn validateArraySliceProps(str: []u8) bool {
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

fn getPropSignature(allocator: Allocator, props: []const PropInfo, prop: []u8) !*const blitzAst.AstTypes {
    var res: ?*const blitzAst.AstTypes = null;

    for (props) |p| {
        if (string.compString(p.name, prop)) {
            res = p.signature;
        } else if (p.isFunc) {
            allocator.destroy(p.signature);
        } else {
            free.freeType(allocator, p.signature);
        }
    }

    if (res) |propType| {
        return propType;
    }

    return ScanError.InvalidProperty;
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

pub fn getArraySlicePropTypes(prop: []u8) !blitzAst.AstTypes {
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
