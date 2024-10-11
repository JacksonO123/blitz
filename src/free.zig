const std = @import("std");
const astMod = @import("ast.zig");
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Ast = astMod.Ast;
const AstTypes = astMod.AstTypes;
const AstNode = astMod.AstNode;
const AstValues = astMod.AstValues;
const StructAttribute = astMod.StructAttribute;
const FuncDecNode = astMod.FuncDecNode;
const RegisteredStruct = astMod.RegisteredStruct;
const CompInfo = utils.CompInfo;

pub fn freeAst(allocator: Allocator, ast: Ast) void {
    freeNodes(allocator, ast.root.nodes);
    allocator.free(ast.root.nodes);
}

pub fn freeRegisteredStructs(allocator: Allocator, structs: []RegisteredStruct) void {
    for (structs) |s| {
        allocator.free(s.name);
    }

    allocator.free(structs);
}

pub fn freeCompInfo(allocator: Allocator, compInfo: *CompInfo) void {
    compInfo.generics.deinit();

    var it = compInfo.variableTypes.valueIterator();
    while (it.next()) |valuePtr| {
        freeType(allocator, valuePtr.*);
    }

    compInfo.variableTypes.deinit();
}

pub fn freeFuncDec(allocator: Allocator, func: FuncDecNode) void {
    allocator.free(func.name);

    for (func.params) |param| {
        freeType(allocator, param.type);
        allocator.free(param.name);
    }
    allocator.free(func.params);

    if (func.generics) |generics| {
        allocator.free(generics);
    }
    freeNode(allocator, func.body);
    freeType(allocator, func.returnType);
}

pub fn freeAttr(allocator: Allocator, attr: StructAttribute) void {
    switch (attr.attr) {
        .Member => {
            allocator.free(attr.attr.Member.name);
            freeType(allocator, attr.attr.Member.type);
        },
        .Function => {
            allocator.free(attr.attr.Function.name);
            freeFuncDec(allocator, attr.attr.Function.func);
        },
    }
}

pub fn freeValueNode(allocator: Allocator, node: *const AstValues) void {
    switch (node.*) {
        .StaticArray => |arr| {
            freeNodes(allocator, arr);
            allocator.free(arr);
        },
        .Number => |num| {
            allocator.free(num.value);
        },
        .String => |string| {
            allocator.free(string);
        },
        else => {},
    }
}

pub fn freeNode(allocator: Allocator, node: *const AstNode) void {
    switch (node.*) {
        .VarDec => |*dec| {
            freeNode(allocator, dec.setNode);

            if (dec.annotation) |annotation| {
                freeType(allocator, annotation);
            }

            allocator.free(dec.name);
        },
        .Seq => |*seq| {
            for (seq.nodes) |seqNode| {
                freeNode(allocator, seqNode);
            }
            allocator.free(seq.nodes);
        },
        .Type => |*t| {
            freeType(allocator, t);
        },
        .Value => |*val| {
            freeValueNode(allocator, val);
        },
        .Cast => |*cast| {
            freeNode(allocator, cast.node);
            freeType(allocator, cast.toType);
        },
        .Variable => |*variable| {
            allocator.free(variable.name);
        },
        .StructDec => |*dec| {
            allocator.free(dec.name);

            for (dec.generics) |generic| {
                if (generic.restriction) |restriction| {
                    freeType(allocator, restriction);
                }
                allocator.free(generic.name);
            }

            for (dec.attributes) |attr| {
                freeAttr(allocator, attr);
            }

            allocator.free(dec.attributes);
            allocator.free(dec.generics);

            if (dec.deriveType) |derived| {
                freeType(allocator, derived);
            }
        },
        .IfStatement => |*statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
        },
        .NoOp => {},
        .FuncDec => |func| {
            freeFuncDec(allocator, func);
        },
        .ReturnNode => |ret| {
            freeNode(allocator, ret);
        },
        .StructInit => |init| {
            allocator.free(init.name);

            for (init.attributes) |attr| {
                allocator.free(attr.name);
                freeNode(allocator, attr.value);
            }

            for (init.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(init.attributes);
            allocator.free(init.generics);
        },
        .Bang => |bang| {
            freeNode(allocator, bang);
        },
    }

    allocator.destroy(node);
}

pub fn freeNodes(allocator: Allocator, nodes: []*const AstNode) void {
    for (nodes) |node| {
        freeNode(allocator, node);
    }
}

pub fn freeStackType(allocator: Allocator, node: *const AstTypes) void {
    switch (node.*) {
        .DynamicArray => |arr| {
            freeType(allocator, arr);
        },
        .StaticArray => |arr| {
            freeType(allocator, arr.type);
            freeNode(allocator, arr.size);
        },
        .Nullable => |nullable| {
            freeType(allocator, nullable);
        },
        .Custom => |custom| {
            for (custom.generics) |generic| {
                freeType(allocator, generic);
            }

            allocator.free(custom.generics);
            allocator.free(custom.name);
        },
        else => {},
    }
}

pub fn freeType(allocator: Allocator, node: *const AstTypes) void {
    freeStackType(allocator, node);
    allocator.destroy(node);
}
