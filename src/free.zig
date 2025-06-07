const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const tokenizer = blitz.tokenizer;
const utils = blitz.utils;
const Allocator = std.mem.Allocator;
const CompInfo = utils.CompInfo;

// DEBUG
const debug = @import("debug.zig");
const printType = debug.printType;

pub fn freeNestedSlice(comptime T: type, allocator: Allocator, slices: [][]T) void {
    for (slices) |name| {
        allocator.free(name);
    }

    allocator.free(slices);
}

pub fn freeFuncDec(allocator: Allocator, func: *const blitzAst.FuncDecNode) void {
    allocator.free(func.name);

    for (func.params) |param| {
        freeType(allocator, param.type);
        allocator.free(param.name);
    }

    allocator.free(func.params);

    if (func.generics) |generics| {
        for (generics) |generic| {
            allocator.free(generic.name);

            if (generic.restriction) |rest| {
                freeType(allocator, rest);
            }
        }

        allocator.free(generics);
    }

    freeNode(allocator, func.body);
    freeType(allocator, func.returnType);

    allocator.destroy(func);
}

pub fn freeAttrs(allocator: Allocator, attrs: []blitzAst.StructAttribute) void {
    for (attrs) |attr| {
        freeAttr(allocator, attr);
    }
}

pub fn freeAttr(allocator: Allocator, attr: blitzAst.StructAttribute) void {
    allocator.free(attr.name);

    switch (attr.attr) {
        .Member => |mem| freeType(allocator, mem),
        .Function => |func| freeFuncDec(allocator, func),
    }
}

pub fn freeValueNode(allocator: Allocator, node: *const blitzAst.AstValues) void {
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

pub fn freeNode(allocator: Allocator, node: *const blitzAst.AstNode) void {
    switch (node.*) {
        .IndexValue => |index| {
            freeNode(allocator, index.index);
            freeNode(allocator, index.value);
        },
        .OpExpr => |op| {
            freeNode(allocator, op.left);
            freeNode(allocator, op.right);
        },
        .FuncReference => |ref| {
            allocator.free(ref);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        .PropertyAccess => |access| {
            freeNode(allocator, access.value);
            allocator.free(access.property);
        },
        .VarDec => |dec| {
            freeNode(allocator, dec.setNode);

            if (dec.annotation) |annotation| {
                freeType(allocator, annotation);
            }

            allocator.free(dec.name);
        },
        .VarSet => |set| {
            freeNode(allocator, set.setNode);
            allocator.free(set.variable);
        },
        .VarEqOp => |op| {
            freeNode(allocator, op.value);
            allocator.free(op.variable);
        },
        .Seq => |seq| {
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
        .Cast => |cast| {
            freeNode(allocator, cast.node);
            freeType(allocator, cast.toType);
        },
        .Variable => |variable| {
            allocator.free(variable);
        },
        .StructDec => |dec| {
            freeStructDec(allocator, dec);
        },
        .IfStatement => |*statement| {
            freeNode(allocator, statement.condition);
            freeNode(allocator, statement.body);
        },
        .NoOp => {},
        .FuncDec => |name| {
            allocator.free(name);
        },
        .FuncCall => |call| {
            freeNode(allocator, call.func);

            for (call.params) |param| {
                freeNode(allocator, param);
            }

            allocator.free(call.params);
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
        .ErrorDec => |dec| {
            freeErrorDec(allocator, dec);
        },
        .Error => |err| {
            allocator.free(err);
        },
        .Group => |group| {
            freeNode(allocator, group);
        },
        .Scope => |scope| {
            freeNode(allocator, scope);
        },
    }

    allocator.destroy(node);
}

pub fn freeErrorDec(allocator: Allocator, dec: *const blitzAst.ErrorDecNode) void {
    allocator.free(dec.name);

    if (dec.variants) |variants| {
        for (variants) |variant| {
            allocator.free(variant);
        }

        allocator.free(variants);
    }
}

pub fn freeStructDec(allocator: Allocator, dec: *const blitzAst.StructDecNode) void {
    allocator.free(dec.name);

    for (dec.generics) |generic| {
        if (generic.restriction) |restriction| {
            freeType(allocator, restriction);
        }
        allocator.free(generic.name);
    }

    freeAttrs(allocator, dec.attributes);

    allocator.free(dec.attributes);
    allocator.free(dec.generics);
    allocator.free(dec.totalMemberList);

    if (dec.deriveType) |derived| {
        freeType(allocator, derived);
    }
}

pub fn freeNodes(allocator: Allocator, nodes: []*const blitzAst.AstNode) void {
    for (nodes) |node| {
        freeNode(allocator, node);
    }
}

pub fn freeStackType(allocator: Allocator, node: *const blitzAst.AstTypes) void {
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
        .Generic => |gen| {
            allocator.free(gen);
        },
        .Error => |err| {
            allocator.free(err.name);
            if (err.payload) |payload| {
                freeType(allocator, payload);
            }
        },
        .ErrorVariant => |err| {
            allocator.free(err.from);
            allocator.free(err.variant);
        },
        .StaticStructInstance => |inst| {
            allocator.free(inst);
        },
        else => {},
    }
}

pub fn freeType(allocator: Allocator, typeNode: *const blitzAst.AstTypes) void {
    freeStackType(allocator, typeNode);
    allocator.destroy(typeNode);
}

pub fn freeToken(allocator: Allocator, token: tokenizer.Token) void {
    if (token.string) |str| {
        allocator.free(str);
    }
}

pub fn freeTokens(allocator: Allocator, tokens: []tokenizer.Token) void {
    for (tokens) |token| {
        freeToken(allocator, token);
    }

    allocator.free(tokens);
}

pub fn freeTokenArr(allocator: Allocator, tokens: []tokenizer.Token) void {
    for (tokens.*) |token| {
        if (token.string) |str| {
            allocator.free(str);
        }
    }
}
