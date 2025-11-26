const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const vmInfo = blitz.vmInfo;
const GenInfo = codegen.GenInfo;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const Context = blitz.context.Context;

pub fn printAst(context: *Context, ast: blitzAst.Ast, writer: *Writer) !void {
    try printNode(context, ast.root, writer);
}

pub fn printStructAndErrorNames(names: blitzAst.HoistedNames, writer: *Writer) !void {
    try writer.writeAll("------------\nstructs:\n");
    for (names.structNames) |name| {
        try writer.writeAll(name);
        try writer.writeByte('\n');
    }

    try writer.writeAll("errors:\n");
    for (names.errorNames) |name| {
        try writer.writeAll(name);
        try writer.writeByte('\n');
    }
    try writer.writeAll("------------\n");
}

pub fn printTypeInfo(context: *Context, info: blitzAst.AstTypeInfo, writer: *Writer) !void {
    if (info.mutState == .Mut) {
        try writer.writeAll("mut ");
    }
    try printType(context, info.astType, writer);
}

pub fn printType(
    context: *Context,
    typeNode: *const blitzAst.AstTypes,
    writer: *Writer,
) anyerror!void {
    if (context.settings.debug.printAddresses) {
        try writer.print("<0x{x}> ", .{@intFromPtr(typeNode)});
    }

    return switch (typeNode.*) {
        .Any => try writer.writeAll("any"),
        .Null => try writer.writeAll("null"),
        .Void => try writer.writeAll("void"),
        .String => try writer.writeAll("string"),
        .Char => try writer.writeAll("char"),
        .Bool => try writer.writeAll("bool"),
        .VarInfo => |info| {
            try writer.writeAll("var info ");
            try printTypeInfo(context, info.info, writer);
        },
        .ArraySlice => |arr| {
            try writer.writeAll("ArraySlice<");
            if (arr.size) |size| {
                try printNode(context, size, writer);
            } else {
                try writer.writeAll("unknown");
            }
            try writer.writeAll(", ");
            try printTypeInfo(context, arr.type.info, writer);
            try writer.writeByte('>');
        },
        .Pointer => |ptr| {
            try writer.writeByte('*');
            try printTypeInfo(context, ptr.info, writer);
        },
        .Nullable => |n| {
            try writer.writeByte('?');
            try printTypeInfo(context, n, writer);
        },
        .Number => |num| {
            try writer.writeAll(num.toString());
        },
        .RawNumber => |digits| {
            try writer.writeAll("[RawNumber ");
            try writer.writeAll(digits);
            try writer.writeByte(']');
        },
        .Custom => |*custom| {
            try writer.writeAll(custom.name);
            if (custom.generics.len > 0) {
                try writer.writeByte('<');
            }

            for (custom.generics, 0..) |generic, index| {
                try printTypeInfo(context, generic, writer);

                if (index < custom.generics.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            if (custom.generics.len > 0) {
                try writer.writeByte('>');
            }
        },
        .Generic => |gen| {
            try writer.writeAll("[generic](");
            try writer.writeAll(gen);
            try writer.writeByte(')');
        },
        .Function => |func| {
            try writer.writeAll("[function](\"");
            try writer.writeAll("func.name");
            try writer.writeByte('\"');

            if (func.generics) |generics| {
                try printGenerics(context, generics, writer);
            }

            try writer.writeAll(" (");

            for (func.params, 0..) |param, index| {
                try writer.writeByte('(');
                try writer.writeAll(param.name);
                try writer.writeAll(")[");
                try printTypeInfo(context, param.type, writer);
                try writer.writeByte(']');

                if (index < func.params.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeByte(' ');
            try printTypeInfo(context, func.returnType, writer);

            try writer.writeByte(')');
        },
        .StaticStructInstance => |inst| {
            try writer.writeAll("[static struct instance](");
            try writer.writeAll(inst);
            try writer.writeByte(')');
        },
        .Error => |err| {
            try writer.writeAll("error (");
            try writer.writeAll(err.name);
            try writer.writeByte(')');
            if (err.payload) |payload| {
                try writer.writeByte('[');
                try printTypeInfo(context, payload, writer);
                try writer.writeByte(']');
            }
        },
        .ErrorVariant => |err| {
            try writer.writeAll("variant [");
            try writer.writeAll(err.variant);
            try writer.writeAll("] from (");
            try writer.writeAll(if (err.from) |from| from else "unknown");
            try writer.writeByte(')');
        },
        .Undef => {
            try writer.writeAll("undef");
        },
    };
}

fn printValue(
    context: *Context,
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
                try printNode(context, val, writer);

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
            try writer.writeByte(')');
        },
        .String => |str| {
            try writer.writeAll("[string](");
            try writer.writeAll(str);
            try writer.writeByte(')');
        },
        .Char => |ch| {
            try writer.writeAll("[char](");
            try writer.writeByte(ch);
            try writer.writeByte(')');
        },
        .Bool => |b| {
            try writer.writeAll("[bool](");
            try writer.writeAll(if (b) "true" else "false");
            try writer.writeByte(')');
        },
    }
}

fn printAstNumberUtil(val: anytype, num: blitzAst.AstNumber, writer: *Writer) !void {
    try writer.writeByte('[');
    try writer.print("{d}", .{val});
    try writer.writeAll("](");
    try writer.writeAll(num.toString());
    try writer.writeByte(')');
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
    };
}

pub fn printNode(context: *Context, node: *blitzAst.AstNode, writer: *Writer) anyerror!void {
    if (context.settings.debug.printAddresses) {
        try writer.print("<0x{x}> ", .{@intFromPtr(node)});
    }

    switch (node.variant) {
        .IndexValue => |index| {
            try writer.writeAll("indexing ");
            try printNode(context, index.target, writer);
            try writer.writeAll(" with ");
            try printNode(context, index.index, writer);
        },
        .OpExpr => |op| {
            try writer.writeByte('(');
            try printNode(context, op.left, writer);
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
            try writer.writeByte(')');

            try writer.writeAll(" (");
            try printNode(context, op.right, writer);
            try writer.writeByte(')');
        },
        .IncOne => |val| {
            try writer.writeByte('(');
            try printNode(context, val, writer);
            try writer.writeAll("++)");
        },
        .DecOne => |val| {
            try writer.writeByte('(');
            try printNode(context, val, writer);
            try writer.writeAll("--)");
        },
        .VarEqOp => |op| {
            try writer.writeAll("set ");
            try printNode(context, op.value, writer);
            try writer.writeAll(" to result of (");
            try printNode(context, op.variable, writer);
            try writer.writeByte(' ');
            const opString = switch (op.opType) {
                .BitOr => "|BitOr|",
                .BitAnd => "&BitAnd&",
                .Add => "+ADD+",
                .Sub => "-SUB-",
                .Mult => "*MULT*",
                .Div => "/DIV/",
                .And => "*MULT*",
                .Or => "/DIV/",
                else => "invalid op",
            };
            try writer.writeAll(opString);
            try writer.writeByte(' ');
            try printNode(context, op.value, writer);
            try writer.writeByte(')');
        },
        .FuncReference => |ref| {
            try writer.writeAll("function (");
            try writer.writeAll(ref);
            try writer.writeByte(')');
        },
        .StaticStructInstance => |inst| {
            try writer.writeAll("static struct (");
            try writer.writeAll(inst);
            try writer.writeByte(')');
        },
        .PropertyAccess => |access| {
            try writer.writeAll("accessing ");
            try writer.writeAll(access.property);
            try writer.writeAll(" from ");
            try printNode(context, access.value, writer);
        },
        .VarDec => |dec| {
            try writer.writeAll("declare (");
            try writer.writeAll(if (dec.mutState == .Const) "const" else "mutable");
            try writer.writeAll(") (");
            try writer.writeAll(dec.name);
            try writer.writeAll(") = ");

            try printNode(context, dec.setNode, writer);

            if (dec.annotation != null) {
                try writer.writeAll(" with annotation: ");
                try printTypeInfo(context, dec.annotation.?, writer);
            }
        },
        .ValueSet => |set| {
            try writer.writeAll("set ");
            try printNode(context, set.value, writer);
            try writer.writeAll(" to ");
            try printNode(context, set.setNode, writer);
        },
        .Value => |*val| {
            try printValue(context, val, writer);
        },
        .Seq => |seq| {
            if (seq.len == 0) {
                try writer.writeAll("(empty seq)");
            } else {
                try printNodes(context, seq, writer);
            }
        },
        .Cast => |cast| {
            try writer.writeAll("cast ");
            try printNode(context, cast.node, writer);
            try writer.writeAll(" to ");
            try printTypeInfo(context, cast.toType, writer);
        },
        .Variable => |variable| {
            try writer.writeAll("[variable: (");
            try writer.writeAll(variable);
            try writer.writeAll(")]");
        },
        .Pointer => |ptr| {
            if (ptr.mutState == .Mut) {
                try writer.writeAll("mut ");
            }
            try writer.writeAll("pointer to ");
            try printNode(context, ptr.node, writer);
        },
        .Dereference => |deref| {
            try writer.writeAll("dereference ");
            try printNode(context, deref, writer);
        },
        .HeapAlloc => |alloc| {
            try writer.writeAll("heap alloc ");
            try printNode(context, alloc.node, writer);
        },
        .HeapFree => |val| {
            try writer.writeAll("heap free ");
            try printNode(context, val, writer);
        },
        .StructPlaceholder => {
            try writer.writeAll("struct dec");
        },
        .StructDec => |dec| {
            try writer.writeAll("declare struct (");
            try writer.writeAll(dec.name);
            try writer.writeByte(')');

            if (dec.generics.len > 0) {
                try writer.writeAll(" with generics [");
                try printGenerics(context, dec.generics, writer);
                try writer.writeByte(']');
            }

            if (dec.attributes.len > 0) {
                try writer.writeAll(" with attributes [");
                try printAttributes(context, dec.attributes, writer);
                try writer.writeByte(']');
            }
        },
        .IfStatement => |statement| {
            try writer.writeAll("if ");
            try printNode(context, statement.condition, writer);
            try writer.writeAll(" then -- body --\n");
            try printNode(context, statement.body, writer);
            try writer.writeAll(" -- body end --\n");

            if (statement.fallback) |fallback| {
                try printIfFallback(context, fallback, writer);
            }
        },
        .ForLoop => |loop| {
            try writer.writeAll("for loop with");

            if (loop.initNode) |init| {
                try writer.writeAll(" init ");
                try printNode(context, init, writer);
            }

            try writer.writeAll(" with condition ");
            try printNode(context, loop.condition, writer);

            try writer.writeAll(" with inc ");
            try printNode(context, loop.incNode, writer);

            try writer.writeAll(" -- body --\n");
            try printNode(context, loop.body, writer);
            try writer.writeAll(" -- body end --\n");
        },
        .WhileLoop => |loop| {
            try writer.writeAll("while ");
            try printNode(context, loop.condition, writer);
            try writer.writeAll(" -- body --\n");
            try printNode(context, loop.body, writer);
            try writer.writeAll(" -- body end --\n");
        },
        .NoOp => {
            try writer.writeAll("(noop)");
        },
        .FuncDec => |name| {
            const dec = context.compInfo.getFunctionAsGlobal(name).?;
            try printFuncDec(context, dec, writer);
        },
        .FuncCall => |call| {
            try writer.writeAll("calling ");
            try printNode(context, call.func, writer);
            try writer.writeAll(" with params [");

            for (call.params, 0..) |param, index| {
                try printNode(context, param, writer);
                if (index < call.params.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeByte(']');
        },
        .ReturnNode => |ret| {
            try writer.writeAll("return ");
            try printNode(context, ret, writer);
        },
        .StructInit => |init| {
            try writer.writeAll("initializing (");
            try writer.writeAll(init.name);
            try writer.writeByte(')');

            if (init.generics.len > 0) {
                try writer.writeAll("[generics: ");

                for (init.generics, 0..) |generic, index| {
                    try printTypeInfo(context, generic, writer);

                    if (index < init.generics.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeByte(']');
            }

            try writer.writeAll(" with {{");

            for (init.attributes, 0..) |attr, index| {
                try writer.writeAll(attr.name);
                try writer.writeAll(": ");
                try printNode(context, attr.value, writer);
                if (index < init.attributes.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll("}}");
        },
        .Bang => |bang| {
            try writer.writeAll("(bang)!");
            try printNode(context, bang, writer);
        },
        .ErrorDec => |def| try printRegisteredError(def, writer),
        .Error => |err| {
            try writer.writeAll("error type (");
            try writer.writeAll(err);
            try writer.writeByte(')');
        },
        .Group => |group| {
            try writer.writeAll("[group](");
            try printNode(context, group, writer);
            try writer.writeByte(')');
        },
        .Scope => |scope| {
            try writer.writeAll("\n--- entering scope ---\n");
            try printNode(context, scope, writer);
            try writer.writeAll("\n--- exiting scope ---\n");
        },
        .ArrayInit => |init| {
            try writer.writeAll("init array [");
            try writer.writeAll(init.size);
            try writer.writeByte(']');
            try printTypeInfo(context, init.initType, writer);
            try writer.writeAll(" with initializer ");
            try printNode(context, init.initNode, writer);
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
        .UndefValue => {
            try writer.writeAll("undef");
        },
    }
}

fn printIfFallback(
    context: *Context,
    fallback: blitzAst.FallbackInfo,
    writer: *Writer,
) anyerror!void {
    try writer.writeAll("else ");
    try printNode(context, fallback.node, writer);
}

pub fn printFuncDec(
    context: *Context,
    func: *const blitzAst.FuncDecNode,
    writer: *Writer,
) !void {
    try writer.writeAll("declare function [");
    try printTypeInfo(context, func.returnType, writer);
    try writer.writeAll("] (");
    try writer.writeAll(func.name);
    try writer.writeByte(')');
    if (func.generics) |generics| {
        try writer.writeAll(" with generics [");
        try printGenerics(context, generics, writer);
        try writer.writeByte(']');
    }

    try writer.writeAll(" with params [");
    try printParams(context, func.params, writer);

    if (func.capturedValues) |captured| {
        try writer.writeAll("] capturing [");
        var captureIt = captured.iterator();
        while (captureIt.next()) |item| {
            try writer.writeByte('(');
            try writer.writeAll(item.key_ptr.*);
            try writer.writeAll(": ");
            try printTypeInfo(context, item.value_ptr.info, writer);
            try writer.writeByte(')');
        }
    }

    try writer.writeAll("] -- body --\n");
    try printNode(context, func.body, writer);
    try writer.writeAll(" -- body end --\n");
}

pub fn printAttributes(
    context: *Context,
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
            .Function => |func| printFuncDec(context, func, writer),
            .Member => |mem| printTypeInfo(context, mem, writer),
        };

        if (index < attrs.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

fn printParams(context: *Context, params: []blitzAst.Parameter, writer: *Writer) !void {
    if (params.len == 0) {
        try writer.writeAll("(no params)");
        return;
    }

    for (params, 0..) |param, index| {
        try writer.writeByte('[');
        try printTypeInfo(context, param.type, writer);
        try writer.writeAll("](");
        try writer.writeAll(param.name);
        try writer.writeByte(')');

        if (index < params.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

pub fn printGenerics(
    context: *Context,
    generics: []blitzAst.GenericType,
    writer: *Writer,
) !void {
    for (generics, 0..) |generic, index| {
        try writer.writeByte('[');

        if (generic.restriction) |restriction| {
            try printTypeInfo(context, restriction, writer);
        } else {
            try writer.writeAll("any");
        }

        try writer.writeAll("](");
        try writer.writeAll(generic.name);
        try writer.writeByte(')');

        if (index < generics.len - 1) {
            try writer.writeAll(", ");
        }
    }
}

fn printNodes(context: *Context, nodes: []*blitzAst.AstNode, writer: *Writer) anyerror!void {
    for (nodes) |node| {
        try printNode(context, node, writer);
        try writer.writeByte('\n');
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

pub fn printRegisteredErrors(errors: []*blitzAst.AstNode, writer: *Writer) !void {
    try writer.writeAll("--- errors ---\n");
    for (errors) |err| {
        try printRegisteredError(err.variant.ErrorDec, writer);
        try writer.writeAll(" ]\n");
    }
}

pub fn printRegisteredStructs(
    context: *Context,
    structs: []*blitzAst.AstNode,
    writer: *Writer,
) !void {
    try writer.writeAll("--- structs ---\n");
    for (structs, 0..) |node, index| {
        const dec = node.variant.StructDec;

        try writer.writeAll("declaring ");
        try writer.writeAll(dec.name);

        if (dec.deriveType) |derived| {
            try writer.writeAll(" extending ");
            try printTypeInfo(context, derived, writer);
        }

        if (dec.generics.len > 0) {
            try writer.writeAll(" with generics [");
            try printGenerics(context, dec.generics, writer);
            try writer.writeByte(']');
        }

        try writer.writeAll(" with attributes [");
        try printAttributes(context, dec.attributes, writer);
        try writer.writeByte(']');

        try writer.writeByte('\n');

        if (index < structs.len - 1) {
            try writer.writeByte('\n');
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
        try writer.writeByte('\n');
    } else {
        try writer.writeByte('\n');
    }
}

pub fn printTokens(tokens: []const tokenizer.Token, code: []u8, writer: *Writer) !void {
    for (tokens) |token| {
        try printToken(token, code, writer);
    }
}

pub fn printBytecodeChunks(context: *const Context, writer: *Writer) !void {
    const chunk = context.genInfo.chunks.listStart orelse return;

    try writer.writeAll("MakeStack ");
    try writeHexDecNumber(vmInfo.StartStackType, context.genInfo.vmInfo.stackStartSize, writer);
    try writer.writeByte('\n');

    const numDigits = utils.getNumberDigitCount(u64, context.genInfo.byteCounter);
    const numInstrLenDigits = utils.getNumberDigitCount(u8, codegen.Instr.maxInstrSize());

    var byteCounter: usize = 0;
    var next: ?*codegen.InstrChunk = chunk;
    while (next) |nextChunk| {
        const chunkLen = nextChunk.data.getInstrLen();
        try writer.writeByte('[');
        try writer.printInt(byteCounter, 10, .lower, .{ .width = numDigits, .fill = '.' });
        try writer.writeAll("] (");
        try writer.printInt(chunkLen, 10, .lower, .{ .width = numInstrLenDigits, .fill = '.' });
        try writer.writeAll(") ");

        try printChunk(nextChunk, writer);
        byteCounter += chunkLen;
        next = nextChunk.next;
    }

    try writer.print("total bytes: {d} (+ {d} vm info)\n", .{
        context.genInfo.byteCounter, vmInfo.VM_INFO_BYTECODE_LEN,
    });
}

fn printChunk(chunk: *codegen.InstrChunk, writer: *Writer) !void {
    try writer.writeAll(chunk.data.toString());

    switch (chunk.data) {
        .SetReg64 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u64, inst.data, writer);
        },
        .SetReg32 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u32, inst.data, writer);
        },
        .SetReg16 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.data, writer);
        },
        .SetReg8 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u8, inst.data, writer);
        },
        .CmpConst8 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
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
        .Add8, .Sub8 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u8, inst.data, writer);
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
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.amount, writer);
        },
        .IncConst8,
        .DecConst8,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u8, inst.data, writer);
        },
        .Mov => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.src, 10, .lower, .{});
        },
        .MovSp => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst, 10, .lower, .{});
        },
        .MovSpNegOffset16 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.offset, writer);
        },
        .XorConst8 => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u8, inst.byte, writer);
        },
        .AddSp16, .SubSp16 => |inst| {
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst, writer);
        },
        .Store64AtRegPostInc16,
        .Store32AtRegPostInc16,
        .Store16AtRegPostInc16,
        .Store8AtRegPostInc16,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.fromReg, 10, .lower, .{});
            try writer.writeAll(" r");
            try writer.printInt(inst.toRegPtr, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.inc, writer);
        },
        .StoreSpAtSpNegOffset16 => |inst| {
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.offset, writer);
        },
        .StoreSpSub16AtSpNegOffset16 => |inst| {
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.sub, writer);
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.offset, writer);
        },
        .Store64AtSpNegOffset16,
        .Store32AtSpNegOffset16,
        .Store16AtSpNegOffset16,
        .Store8AtSpNegOffset16,
        => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.reg, 10, .lower, .{});
            try writer.writeByte(' ');
            try writeHexDecNumber(u16, inst.offset, writer);
        },
        .Load64AtReg, .Load32AtReg, .Load16AtReg, .Load8AtReg => |inst| {
            try writer.writeAll(" r");
            try writer.printInt(inst.dest, 10, .lower, .{});
            try writer.writeAll(" [r");
            try writer.printInt(inst.fromRegPtr, 10, .lower, .{});
            try writer.writeByte(']');
        },
    }

    try writer.writeByte('\n');
}

fn printInstName(inst: u8, writer: *Writer) !void {
    const name = @tagName(@as(codegen.Instr, @enumFromInt(inst)));
    try writer.writeAll(name);
}

pub fn writeHexDecNumber(comptime T: type, num: T, writer: *Writer) !void {
    try writer.writeAll("0x");
    try writer.printInt(num, 16, .lower, .{
        .width = @sizeOf(T) * 2,
        .fill = '0',
    });
    try writer.writeByte('(');
    try writer.printInt(num, 10, .lower, .{});
    try writer.writeByte(')');
}
