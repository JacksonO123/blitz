const std = @import("std");
const astUtils = @import("ast.zig");
const Ast = astUtils.Ast;
const AstNode = astUtils.AstNode;
const AstTypes = astUtils.AstTypes;
const AstValues = astUtils.AstValues;
const AstNumberVariants = astUtils.AstNumberVariants;
const RegisteredStruct = astUtils.RegisteredStruct;

pub fn printAst(ast: Ast) void {
    printNodes(ast.root.nodes);
}

fn printType(typeNode: *const AstTypes) void {
    return switch (typeNode.*) {
        .String => {
            std.debug.print("string", .{});
        },
        .Char => {
            std.debug.print("char", .{});
        },
        .Bool => {
            std.debug.print("bool", .{});
        },
        .DynamicArray => |*arr| {
            std.debug.print("DynamicArray<", .{});
            printType(arr.type);
            std.debug.print(">", .{});
        },
        .StaticArray => |*arr| {
            std.debug.print("StaticArray<", .{});
            printNode(arr.size);
            std.debug.print(", ", .{});
            printType(arr.type);
            std.debug.print(">", .{});
        },
        .Nullable => |*n| {
            std.debug.print("?", .{});
            printType(n.type);
        },
        .Number => |*num| {
            std.debug.print("{s}", .{numberTypeToString(num.*)});
        },
    };
}

fn numberTypeToString(numType: AstNumberVariants) [*:0]const u8 {
    return switch (numType) {
        .U16 => "u16",
        .U32 => "u32",
        .U64 => "u64",
        .U128 => "u128",
        .I16 => "i16",
        .I32 => "i32",
        .I64 => "i64",
        .I128 => "i128",
        .F16 => "f16",
        .F32 => "f32",
        .F64 => "f64",
        .F128 => "f128",
    };
}

fn printValue(value: *const AstValues) void {
    switch (value.*) {
        .StaticArray => |arr| {
            std.debug.print("([", .{});

            for (arr, 0..) |val, index| {
                printNode(val);

                if (index < arr.len - 1) {
                    std.debug.print(", ", .{});
                }
            }

            std.debug.print("])", .{});
        },
        .Number => |*n| {
            std.debug.print("[{s}]({s})", .{ numberTypeToString(n.type), n.value });
        },
        .String => |*s| {
            std.debug.print("[string](\"{s}\")", .{s});
        },
        .Char => |*c| {
            std.debug.print("[char]({c})", .{c});
        },
        .Bool => |*b| {
            std.debug.print("[bool]({b})", .{b});
        },
    }
}

fn printNode(node: *const AstNode) void {
    switch (node.*) {
        .VarDec => |*dec| {
            std.debug.print("declare ({s}) ({s}) = ", .{
                if (dec.isConst) "const" else "mutable",
                dec.name,
            });
            printNode(dec.setNode);
            if (dec.annotation != null) {
                std.debug.print(" with type: ", .{});
                printType(dec.annotation.?);
            }
        },
        .Value => |*val| {
            printValue(val);
        },
        .Type => |*t| {
            printType(t);
        },
        .Seq => |*seq| {
            printNodes(seq.nodes);
        },
    }
}

fn printNodes(nodes: []*const AstNode) void {
    for (nodes) |node| {
        printNode(node);
        std.debug.print("\n", .{});
    }
}

pub fn printRegisteredStructs(structs: []RegisteredStruct) void {
    std.debug.print("registered structs:\n", .{});
    for (structs) |s| {
        std.debug.print("{s}{s}\n", .{ s.name, if (s.isGeneric) " : (generic)" else "" });
    }
    std.debug.print("--- end ---\n", .{});
}
