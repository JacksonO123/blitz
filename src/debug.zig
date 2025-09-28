const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const vmInfo = blitz.vmInfo;
const CompInfo = blitzCompInfo.CompInfo;
const GenInfo = codegen.GenInfo;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

pub fn printAst(compInfo: *CompInfo, ast: blitzAst.Ast, writer: *Writer) !void {
    try printNode(compInfo, ast.root, writer);
}

pub fn printStructAndErrorNames(names: blitzAst.HoistedNames, writer: *Writer) !void {
    try writer.writeAll("------------\nstructs:\n");
    for (names.structNames) |name| {
        try writer.writeAll(name);
        try writer.writeAll("\n");
    }

    try writer.writeAll("errors:\n");
    for (names.errorNames) |name| {
        try writer.writeAll(name);
        try writer.writeAll("\n");
    }
    try writer.writeAll("------------\n");
}

pub fn printTypeInfo(compInfo: *CompInfo, info: blitzAst.AstTypeInfo, writer: *Writer) !void {
    if (!info.isConst) {
        try writer.writeAll("mut ");
    }
    try printType(compInfo, info.astType, writer);
}

pub fn printType(
    compInfo: *CompInfo,
    typeNode: *const blitzAst.AstTypes,
    writer: *Writer,
) anyerror!void {
    return switch (typeNode.*) {
        .Any => try writer.writeAll("any"),
        .Null => try writer.writeAll("null"),
        .Void => try writer.writeAll("void"),
        .String => try writer.writeAll("string"),
        .Char => try writer.writeAll("char"),
        .Bool => try writer.writeAll("bool"),
        .VarInfo => |info| {
            try writer.writeAll("var info ");
            try printTypeInfo(compInfo, info, writer);
        },
        .ArraySlice => |arr| {
            try writer.writeAll("ArraySlice<");
            if (arr.size) |size| {
                try printNode(compInfo, size, writer);
            } else {
                try writer.writeAll("unknown");
            }
            try writer.writeAll(", ");
            try printTypeInfo(compInfo, arr.type, writer);
            try writer.writeAll(">");
        },
        .Pointer => |ptr| {
            try writer.writeAll("*");
            try printTypeInfo(compInfo, ptr, writer);
        },
        .Nullable => |n| {
            try writer.writeAll("?");
            try printTypeInfo(compInfo, n, writer);
        },
        .Number => |num| {
            try writer.writeAll(num.toString());
        },
        .RawNumber => |digits| {
            try writer.writeAll("[RawNumber ");
            try writer.writeAll(digits);
            try writer.writeAll("]");
        },
        .Custom => |*custom| {
            try writer.writeAll(custom.name);
            if (custom.generics.len > 0) {
                try writer.writeAll("<");
            }

            for (custom.generics, 0..) |generic, index| {
                try printTypeInfo(compInfo, generic, writer);

                if (index < custom.generics.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            if (custom.generics.len > 0) {
                try writer.writeAll(">");
            }
        },
        .Generic => |gen| {
            try writer.writeAll("[generic](");
            try writer.writeAll(gen);
            try writer.writeAll(")");
        },
        .Function => |func| {
            try writer.writeAll("[function](\"");
            try writer.writeAll("func.name");
            try writer.writeAll("\"");

            if (func.generics) |generics| {
                try printGenerics(compInfo, generics, writer);
            }

            try writer.writeAll(" (");

            for (func.params, 0..) |param, index| {
                try writer.writeAll("(");
                try writer.writeAll(param.name);
                try writer.writeAll(")[");
                try printTypeInfo(compInfo, param.type, writer);
                try writer.writeAll("]");

                if (index < func.params.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeAll(" ");
            try printTypeInfo(compInfo, func.returnType, writer);

            try writer.writeAll(")");
        },
        .StaticStructInstance => |inst| {
            try writer.writeAll("[static struct instance](");
            try writer.writeAll(inst);
            try writer.writeAll(")");
        },
        .Error => |err| {
            try writer.writeAll("error (");
            try writer.writeAll(err.name);
            try writer.writeAll(")");
            if (err.payload) |payload| {
                try writer.writeAll("[");
                try printTypeInfo(compInfo, payload, writer);
                try writer.writeAll("]");
            }
        },
        .ErrorVariant => |err| {
            try writer.writeAll("variant [");
            try writer.writeAll(err.variant);
            try writer.writeAll("] from (");
            try writer.writeAll(if (err.from) |from| from else "unknown");
            try writer.writeAll(")");
        },
    };
}

fn printValue(
    compInfo: *CompInfo,
    value: *const blitzAst.AstValues,
    writer: *Writer,
) anyerror!void {
    switch (value.*) {
        .Null => {
            try writer.writeAll("null");
        },
        .ArraySlice => |arr| {
            try writer.writeAll("[ArraySlice]([");

            for (arr, 0..) |val, index| {
                try printNode(compInfo, val, writer);

                if (index < arr.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeAll("])");
        },
        .Number => |n| try printAstNumber(n, writer),
        .RawNumber => |num| {
            try writer.writeAll("[RawNumber:");
            try writer.writeAll(num.numType.toString());
            try writer.writeAll("](");
            try writer.writeAll(num.digits);
            try writer.writeAll(")");
        },
        .String => |str| {
            try writer.writeAll("[string](\"");
            try writer.writeAll(str);
            try writer.writeAll("\")");
        },
        .Char => |ch| {
            try writer.writeAll("[char](");
            try writer.writeAll(&[_]u8{ch});
            try writer.writeAll(")");
        },
        .Bool => |b| {
            try writer.writeAll("[bool](");
            try writer.writeAll(if (b) "true" else "false");
            try writer.writeAll(")");
        },
    }
}

fn printAstNumberUtil(val: anytype, num: blitzAst.AstNumber, writer: *Writer) !void {
    try writer.writeAll("[");
    try writer.print("{d}", .{val});
    try writer.writeAll("](");
    try writer.writeAll(num.toString());
    try writer.writeAll(")");
}

fn printAstNumber(num: blitzAst.AstNumber, writer: *Writer) !void {
    try switch (num) {
        .Char, .U8 => |val| printAstNumberUtil(val, num, writer),
        .U16 => |val| printAstNumberUtil(val, num, writer),
        .U32 => |val| printAstNumberUtil(val, num, writer),
        .U64 => |val| printAstNumberUtil(val, num, writer),
        .U128 => |val| printAstNumberUtil(val, num, writer),
        .I8 => |val| printAstNumberUtil(val, num, writer),
        .I16 => |val| printAstNumberUtil(val, num, writer),
        .I32 => |val| printAstNumberUtil(val, num, writer),
        .I64 => |val| printAstNumberUtil(val, num, writer),
        .I128 => |val| printAstNumberUtil(val, num, writer),
        .F32 => |val| printAstNumberUtil(val, num, writer),
        .F64 => |val| printAstNumberUtil(val, num, writer),
        .F128 => |val| printAstNumberUtil(val, num, writer),
    };
}

pub fn printNode(compInfo: *CompInfo, node: *blitzAst.AstNode, writer: *Writer) anyerror!void {
    switch (node.*) {
        .IndexValue => |index| {
            try writer.writeAll("indexing ");
            try printNode(compInfo, index.value, writer);
            try writer.writeAll(" with ");
            try printNode(compInfo, index.index, writer);
        },
        .OpExpr => |op| {
            try writer.writeAll("(");
            try printNode(compInfo, op.left, writer);
            try writer.writeAll(") (");

            const opString = switch (op.type) {
                .BitAnd => "&BitAnd&",
                .BitOr => "|BitOr|",
                .And => "&&AND&&",
                .Or => "||OR||",
                .Add => "+ADD+",
                .Sub => "-SUB-",
                .Mult => "*MULT*",
                .Div => "/DIV/",
                .LessThan => "<LT<",
                .GreaterThan => ">GT>",
                .LessThanEq => "<=LTE<=",
                .GreaterThanEq => ">=GTE>=",
                .Equal => "==EQUAL==",
                .NotEqual => "!=EQUAL!=",
            };
            try writer.writeAll(opString);
            try writer.writeAll(")");

            try writer.writeAll(" (");
            try printNode(compInfo, op.right, writer);
            try writer.writeAll(")");
        },
        .IncOne => |val| {
            try writer.writeAll("(");
            try printNode(compInfo, val, writer);
            try writer.writeAll("++)");
        },
        .DecOne => |val| {
            try writer.writeAll("(");
            try printNode(compInfo, val, writer);
            try writer.writeAll("--)");
        },
        .VarEqOp => |op| {
            try writer.writeAll("set ");
            try writer.writeAll(op.variable);
            try writer.writeAll(" to result of (");
            try writer.writeAll(op.variable);
            try writer.writeAll(" ");
            const opString = switch (op.opType) {
                .BitOrEq => "|BitOr|",
                .BitAndEq => "&BitAnd&",
                .AddEq => "+ADD+",
                .SubEq => "-SUB-",
                .MultEq => "*MULT*",
                .DivEq => "/DIV/",
                .AndEq => "*MULT*",
                .OrEq => "/DIV/",
            };
            try writer.writeAll(opString);
            try writer.writeAll(" ");
            try printNode(compInfo, op.value, writer);
            try writer.writeAll(")");
        },
        .FuncReference => |ref| {
            try writer.writeAll("function (");
            try writer.writeAll(ref);
            try writer.writeAll(")");
        },
        .StaticStructInstance => |inst| {
            try writer.writeAll("static struct (");
            try writer.writeAll(inst);
            try writer.writeAll(")");
        },
        .PropertyAccess => |access| {
            try writer.writeAll("accessing ");
            try writer.writeAll(access.property);
            try writer.writeAll(" from ");
            try printNode(compInfo, access.value, writer);
        },
        .VarDec => |*dec| {
            try writer.writeAll("declare (");
            try writer.writeAll(if (dec.isConst) "const" else "mutable");
            try writer.writeAll(") (");
            try writer.writeAll(dec.name);
            try writer.writeAll(") = ");

            try printNode(compInfo, dec.setNode, writer);

            if (dec.annotation != null) {
                try writer.writeAll(" with annotation: ");
                try printTypeInfo(compInfo, dec.annotation.?, writer);
            }
        },
        .ValueSet => |set| {
            try writer.writeAll("set ");
            try printNode(compInfo, set.value, writer);
            try writer.writeAll(" to ");
            try printNode(compInfo, set.setNode, writer);
        },
        .Value => |*val| {
            try printValue(compInfo, val, writer);
        },
        .Type => |*t| {
            try printType(compInfo, t, writer);
        },
        .Seq => |*seq| {
            if (seq.nodes.len == 0) {
                try writer.writeAll("(empty seq)");
            } else {
                try printNodes(compInfo, seq.nodes, writer);
            }
        },
        .Cast => |*cast| {
            try writer.writeAll("cast ");
            try printNode(compInfo, cast.node, writer);
            try writer.writeAll(" to ");
            try printTypeInfo(compInfo, cast.toType, writer);
        },
        .Variable => |variable| {
            try writer.writeAll("[variable: (");
            try writer.writeAll(variable);
            try writer.writeAll(")]");
        },
        .Pointer => |ptr| {
            if (!ptr.isConst) {
                try writer.writeAll("mut ");
            }
            try writer.writeAll("pointer to ");
            try printNode(compInfo, ptr.node, writer);
        },
        .Dereference => |deref| {
            try writer.writeAll("dereference ");
            try printNode(compInfo, deref, writer);
        },
        .HeapAlloc => |alloc| {
            try writer.writeAll("heap alloc ");
            try printNode(compInfo, alloc.node, writer);
        },
        .HeapFree => |val| {
            try writer.writeAll("heap free ");
            try printNode(compInfo, val, writer);
        },
        .StructPlaceholder => {
            try writer.writeAll("struct dec");
        },
        .StructDec => |dec| {
            try writer.writeAll("declare struct (");
            try writer.writeAll(dec.name);
            try writer.writeAll(")");

            if (dec.generics.len > 0) {
                try writer.writeAll(" with generics [");
                try printGenerics(compInfo, dec.generics, writer);
                try writer.writeAll("]");
            }

            if (dec.attributes.len > 0) {
                try writer.writeAll(" with attributes [");
                try printAttributes(compInfo, dec.attributes, writer);
                try writer.writeAll("]");
            }
        },
        .IfStatement => |statement| {
            try writer.writeAll("if ");
            try printNode(compInfo, statement.condition, writer);
            try writer.writeAll(" then -- body --\n");
            try printNode(compInfo, statement.body, writer);
            try writer.writeAll(" -- body end --\n");

            if (statement.fallback) |fallback| {
                try printIfFallback(compInfo, fallback, writer);
            }
        },
        .ForLoop => |loop| {
            try writer.writeAll("for loop with");

            if (loop.initNode) |init| {
                try writer.writeAll(" init ");
                try printNode(compInfo, init, writer);
            }

            try writer.writeAll(" with condition ");
            try printNode(compInfo, loop.condition, writer);

            try writer.writeAll(" with inc ");
            try printNode(compInfo, loop.incNode, writer);

            try writer.writeAll(" -- body --\n");
            try printNode(compInfo, loop.body, writer);
            try writer.writeAll(" -- body end --\n");
        },
        .WhileLoop => |loop| {
            try writer.writeAll("while ");
            try printNode(compInfo, loop.condition, writer);
            try writer.writeAll(" -- body --\n");
            try printNode(compInfo, loop.body, writer);
            try writer.writeAll(" -- body end --\n");
        },
        .NoOp => {
            try writer.writeAll("(noop)");
        },
        .FuncDec => |name| {
            const dec = compInfo.getFunctionAsGlobal(name).?;
            try printFuncDec(compInfo, dec, writer);
        },
        .FuncCall => |call| {
            try writer.writeAll("calling ");
            try printNode(compInfo, call.func, writer);
            try writer.writeAll(" with params [");

            for (call.params, 0..) |param, index| {
                try printNode(compInfo, param, writer);
                if (index < call.params.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeAll("]");
        },
        .ReturnNode => |ret| {
            try writer.writeAll("return ");
            try printNode(compInfo, ret, writer);
        },
        .StructInit => |init| {
            try writer.writeAll("initializing (");
            try writer.writeAll(init.name);
            try writer.writeAll(")");

            if (init.generics.len > 0) {
                try writer.writeAll("[generics: ");

                for (init.generics, 0..) |generic, index| {
                    try printTypeInfo(compInfo, generic, writer);

                    if (index < init.generics.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll("]");
            }

            try writer.writeAll(" with {{");

            for (init.attributes, 0..) |attr, index| {
                try writer.writeAll(attr.name);
                try writer.writeAll(": ");
                try printNode(compInfo, attr.value, writer);
                if (index < init.attributes.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll("}}");
        },
        .Bang => |bang| {
            try writer.writeAll("[bang]!");
            try printNode(compInfo, bang, writer);
        },
        .ErrorDec => |def| try printRegisteredError(def, writer),
        .Error => |err| {
            try writer.writeAll("error type (");
            try writer.writeAll(err);
            try writer.writeAll(")");
        },
        .Group => |group| {
            try writer.writeAll("[group](");
            try printNode(compInfo, group, writer);
            try writer.writeAll(")");
        },
        .Scope => |scope| {
            try writer.writeAll("\n--- entering scope ---\n");
            try printNode(compInfo, scope, writer);
            try writer.writeAll("\n--- exiting scope ---\n");
        },
        .ArrayInit => |init| {
            try writer.writeAll("init array [");
            try writer.writeAll(init.size);
            try writer.writeAll("]");
            try printTypeInfo(compInfo, init.initType, writer);
            try writer.writeAll(" with initializer ");
            try printNode(compInfo, init.initNode, writer);
        },
        .InferErrorVariant => |variant| {
            try writer.writeAll("infer error from variant ");
            try writer.writeAll(variant);
        },
        .Continue => {
            try writer.writeAll("continue");
        },
        .Break => {
            try writer.writeAll("break");
        },
    }
}

fn printIfFallback(
    compInfo: *CompInfo,
    fallback: *const blitzAst.IfFallback,
    writer: *Writer,
) anyerror!void {
    try writer.writeAll("else ");

    if (fallback.condition) |condition| {
        try writer.writeAll(" if ");
        try printNode(compInfo, condition, writer);
    }

    try writer.writeAll("-- body --\n");
    try printNode(compInfo, fallback.body, writer);
    try writer.writeAll(" -- body end --\n");

    if (fallback.fallback) |innerFallback| {
        try printIfFallback(compInfo, innerFallback, writer);
    }
}

pub fn printFuncDec(
    compInfo: *CompInfo,
    func: *const blitzAst.FuncDecNode,
    writer: *Writer,
) !void {
    try writer.writeAll("declare function [");
    try printTypeInfo(compInfo, func.returnType, writer);
    try writer.writeAll("] (");
    try writer.writeAll(func.name);
    try writer.writeAll(")");
    if (func.generics) |generics| {
        try writer.writeAll(" with generics [");
        try printGenerics(compInfo, generics, writer);
        try writer.writeAll("]");
    }

    try writer.writeAll(" with params [");
    try printParams(compInfo, func.params, writer);

    if (func.capturedValues) |captured| {
        try writer.writeAll("] capturing [");
        var captureIt = captured.iterator();
        while (captureIt.next()) |item| {
            try writer.writeAll("(");
            try writer.writeAll(item.key_ptr.*);
            try writer.writeAll(": ");
            try printTypeInfo(compInfo, item.value_ptr.*, writer);
            try writer.writeAll(")");
        }
    }

    try writer.writeAll("] -- body --\n");
    try printNode(compInfo, func.body, writer);
    try writer.writeAll(" -- body end --\n");
}

pub fn printAttributes(
    compInfo: *CompInfo,
    attrs: []blitzAst.StructAttribute,
    writer: *Writer,
) !void {
    for (attrs, 0..) |attr, index| {
        if (attr.static) {
            try writer.writeAll("(static) ");
        }

        try writer.writeAll(attr.visibility.toString());
        try writer.writeAll(" (");
        try writer.writeAll(attr.name);
        try writer.writeAll(") ");

        try switch (attr.attr) {
            .Function => |func| printFuncDec(compInfo, func, writer),
            .Member => |mem| printTypeInfo(compInfo, mem, writer),
        };

        if (index < attrs.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

fn printParams(compInfo: *CompInfo, params: []blitzAst.Parameter, writer: *Writer) !void {
    if (params.len == 0) {
        try writer.writeAll("(no params)");
        return;
    }

    for (params, 0..) |param, index| {
        try writer.writeAll("[");
        try printTypeInfo(compInfo, param.type, writer);
        try writer.writeAll("](");
        try writer.writeAll(param.name);
        try writer.writeAll(")");

        if (index < params.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

pub fn printGenerics(
    compInfo: *CompInfo,
    generics: []blitzAst.GenericType,
    writer: *Writer,
) !void {
    for (generics, 0..) |generic, index| {
        try writer.writeAll("[");

        if (generic.restriction) |restriction| {
            try printTypeInfo(compInfo, restriction, writer);
        } else {
            try writer.writeAll("any");
        }

        try writer.writeAll("](");
        try writer.writeAll(generic.name);
        try writer.writeAll(")");

        if (index < generics.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

fn printNodes(compInfo: *CompInfo, nodes: []*blitzAst.AstNode, writer: *Writer) anyerror!void {
    for (nodes) |node| {
        try printNode(compInfo, node, writer);
        try writer.writeAll("\n");
    }
}

pub fn printRegisteredError(err: *const blitzAst.ErrorDecNode, writer: *Writer) !void {
    try writer.writeAll("defining error: {s} with variants [ ");

    for (err.variants, 0..) |variant, index| {
        try writer.writeAll(variant);
        if (index < err.variants.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

pub fn printRegisteredErrors(errors: []*const blitzAst.ErrorDecNode, writer: *Writer) !void {
    try writer.writeAll("--- errors ---\n");
    for (errors) |err| {
        try printRegisteredError(err, writer);
        try writer.writeAll(" ]\n");
    }
}

pub fn printRegisteredStructs(
    compInfo: *CompInfo,
    structs: []*blitzAst.StructDecNode,
    writer: *Writer,
) !void {
    try writer.writeAll("--- structs ---\n");
    for (structs, 0..) |s, index| {
        try writer.writeAll("declaring {s}");

        if (s.deriveType) |derived| {
            try writer.writeAll(" extending ");
            try printTypeInfo(compInfo, derived, writer);
        }

        if (s.generics.len > 0) {
            try writer.writeAll(" with generics [");
            try printGenerics(compInfo, s.generics, writer);
            try writer.writeAll("]");
        }

        try writer.writeAll(" with attributes [");
        try printAttributes(compInfo, s.attributes, writer);
        try writer.writeAll("]");

        try writer.writeAll("\n");

        if (index < structs.len - 1) {
            try writer.writeAll("\n");
        }
    }
}

pub fn printToken(token: tokenizer.Token, code: []u8, writer: *Writer) !void {
    const active = std.meta.activeTag(token.type);
    const tagName = @tagName(active);
    try writer.writeAll(tagName);
    if (token.start != token.end) {
        try writer.writeAll(" : ");
        try writer.writeAll(token.strFromCode(code));
        try writer.writeAll("\n");
    } else {
        try writer.writeAll("\n");
    }
}

pub fn printTokens(tokens: []const tokenizer.Token, code: []u8, writer: *Writer) !void {
    for (tokens) |token| {
        try printToken(token, code, writer);
    }
}

pub fn printBytecodeChunks(genInfo: *const GenInfo, writer: *Writer) !void {
    const rootChunk = genInfo.instructionList;

    if (rootChunk) |chunk| {
        try writer.writeAll("MakeStack ");
        try writeHexDecNumber(vmInfo.StartStackType, genInfo.vmInfo.stackStartSize, writer);
        try writer.writeAll("\n");

        var byteCounter: usize = 0;
        var next: ?*codegen.InstrChunk = chunk;
        while (next) |nextChunk| {
            const chunkLen = nextChunk.data.getInstrLen();
            try writer.writeAll("[");
            try writer.printInt(byteCounter, 10, .lower, .{});
            try writer.writeAll("] (");
            try writer.printInt(chunkLen, 10, .lower, .{});
            try writer.writeAll(") ");

            try printChunk(nextChunk, writer);
            byteCounter += chunkLen;
            next = nextChunk.next;
        }

        try writer.print("total bytes: {d}\n", .{genInfo.byteCounter});
    }
}

fn printChunk(chunk: *codegen.InstrChunk, writer: *Writer) !void {
    try writer.writeAll(chunk.data.toString());

    switch (chunk.data) {
        .SetReg64 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u64, inst.data, writer);
        },
        .SetReg32 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u32, inst.data, writer);
        },
        .SetReg16 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u16, inst.data, writer);
        },
        .SetReg8 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u8, inst.data, writer);
        },
        .CmpConstByte => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u8, inst.data, writer);
        },
        .Cmp => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg1, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg2, 10, .lower, .{});
        },
        .CmpSetRegEQ,
        .CmpSetRegNE,
        .CmpSetRegGT,
        .CmpSetRegLT,
        .CmpSetRegGTE,
        .CmpSetRegLTE,
        .Xor,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg1, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg2, 10, .lower, .{});
        },
        .Add,
        .Sub,
        .Mult,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg1, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg2, 10, .lower, .{});
        },
        .Jump,
        .JumpEQ,
        .JumpNE,
        .JumpGT,
        .JumpLT,
        .JumpGTE,
        .JumpLTE,
        .JumpBack,
        .JumpBackEQ,
        .JumpBackNE,
        .JumpBackGT,
        .JumpBackLT,
        .JumpBackGTE,
        .JumpBackLTE,
        => |inst| {
            try writer.writeAll(" ");
            try writeHexDecNumber(u16, inst.amount, writer);
        },
        .IncConstByte,
        .DecConstByte,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u8, inst.data, writer);
        },
        .Mov => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.src, 10, .lower, .{});
        },
        .XorConstByte => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u8, inst.byte, writer);
        },
        .AddSp,
        .SubSp,
        => |inst| {
            try writer.writeAll(" ");
            try writeHexDecNumber(u32, inst.amount, writer);
        },
        .AddSpReg,
        .SubSpReg,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.amount, 10, .lower, .{});
        },
        .StoreOffsetByte => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.fromReg, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.fromReg, 10, .lower, .{});
            try writer.writeAll(" ");
            try writeHexDecNumber(u8, inst.offset, writer);
        },
    }

    try writer.writeAll("\n");
}

fn printInstName(inst: u8, writer: *Writer) !void {
    const name = @tagName(@as(codegen.Instr, @enumFromInt(inst)));
    try writer.writeAll(name);
}

fn writeHexDecNumber(comptime T: type, num: T, writer: *Writer) !void {
    try writer.writeAll("0x");
    try writer.printInt(num, 16, .lower, .{
        .width = @sizeOf(T) * 2,
        .fill = '0',
    });
    try writer.writeAll("(");
    try writer.printInt(num, 10, .lower, .{});
    try writer.writeAll(")");
}
