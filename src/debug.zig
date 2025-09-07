const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const tokenizer = blitz.tokenizer;
const codegen = blitz.codegen;
const blitzCompInfo = blitz.compInfo;
const CompInfo = blitzCompInfo.CompInfo;
const GenInfo = codegen.GenInfo;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

pub fn printAst(compInfo: *CompInfo, ast: blitzAst.Ast) void {
    printNode(compInfo, ast.root);
}

pub fn printStructAndErrorNames(names: blitzAst.HoistedNames) void {
    print("------------\n", .{});
    print("structs:\n", .{});
    for (names.structNames) |name| {
        print("{s}\n", .{name});
    }

    print("errors:\n", .{});
    for (names.errorNames) |name| {
        print("{s}\n", .{name});
    }
    print("------------\n", .{});
}

pub fn printTypeInfo(compInfo: *CompInfo, info: blitzAst.AstTypeInfo) void {
    if (!info.isConst) {
        print("mut ", .{});
    }
    printType(compInfo, info.astType);
}

pub fn printType(compInfo: *CompInfo, typeNode: *const blitzAst.AstTypes) void {
    return switch (typeNode.*) {
        .Any => print("any", .{}),
        .Null => print("null", .{}),
        .Void => print("void", .{}),
        .String => print("string", .{}),
        .Char => print("char", .{}),
        .Bool => print("bool", .{}),
        .VarInfo => |info| {
            print("var info ", .{});
            printTypeInfo(compInfo, info);
        },
        .ArraySlice => |arr| {
            print("ArraySlice<", .{});
            if (arr.size) |size| {
                printNode(compInfo, size);
            } else {
                print("unknown", .{});
            }
            print(", ", .{});
            printTypeInfo(compInfo, arr.type);
            print(">", .{});
        },
        .Pointer => |ptr| {
            print("*", .{});
            printTypeInfo(compInfo, ptr);
        },
        .Nullable => |n| {
            print("?", .{});
            printTypeInfo(compInfo, n);
        },
        .Number => |num| {
            print("{s}", .{num.toString()});
        },
        .RawNumber => |digits| {
            print("[RawNumber {s}]", .{digits});
        },
        .Custom => |*custom| {
            print("{s}", .{custom.name});
            if (custom.generics.len > 0) {
                print("<", .{});
            }

            for (custom.generics, 0..) |generic, index| {
                printTypeInfo(compInfo, generic);

                if (index < custom.generics.len - 1) {
                    print(", ", .{});
                }
            }

            if (custom.generics.len > 0) {
                print(">", .{});
            }
        },
        .Generic => |gen| {
            print("[generic]({s})", .{gen});
        },
        .Function => |func| {
            print("[function](\"{s}\"", .{func.name});

            if (func.generics) |generics| {
                printGenerics(compInfo, generics);
            }

            print(" (", .{});

            for (func.params, 0..) |param, index| {
                print("({s})[", .{param.name});
                printTypeInfo(compInfo, param.type);
                print("]", .{});

                if (index < func.params.len - 1) {
                    print(", ", .{});
                }
            }

            print(" ", .{});
            printTypeInfo(compInfo, func.returnType);

            print(")", .{});
        },
        .StaticStructInstance => |inst| {
            print("[static struct instance]({s})", .{inst});
        },
        .Error => |err| {
            print("error ({s})", .{err.name});
            if (err.payload) |payload| {
                print("[", .{});
                printTypeInfo(compInfo, payload);
                print("]", .{});
            }
        },
        .ErrorVariant => |err| {
            print("variant [{s}] from ({s})", .{ err.variant, err.from });
        },
    };
}

fn printValue(compInfo: *CompInfo, value: *const blitzAst.AstValues) void {
    switch (value.*) {
        .Null => {
            print("null", .{});
        },
        .ArraySlice => |arr| {
            print("[ArraySlice]([", .{});

            for (arr, 0..) |val, index| {
                printNode(compInfo, val);

                if (index < arr.len - 1) {
                    print(", ", .{});
                }
            }

            print("])", .{});
        },
        .Number => |n| {
            printAstNumber(n);
        },
        .RawNumber => |num| {
            print("[RawNumber:{s}]({s})", .{ num.numType.toString(), num.digits });
        },
        .String => |s| {
            print("[string](\"{s}\")", .{s});
        },
        .Char => |c| {
            print("[char]({c})", .{c});
        },
        .Bool => |b| {
            print("[bool]({s})", .{if (b) "true" else "false"});
        },
    }
}

fn printAstNumberUtil(val: anytype, num: blitzAst.AstNumber) void {
    print("[{d}]({s})", .{ val, num.toString() });
}

fn printAstNumber(num: blitzAst.AstNumber) void {
    switch (num) {
        .Char, .U8 => |val| printAstNumberUtil(val, num),
        .U16 => |val| printAstNumberUtil(val, num),
        .U32 => |val| printAstNumberUtil(val, num),
        .U64 => |val| printAstNumberUtil(val, num),
        .U128 => |val| printAstNumberUtil(val, num),
        .USize => |val| printAstNumberUtil(val, num),
        .I8 => |val| printAstNumberUtil(val, num),
        .I16 => |val| printAstNumberUtil(val, num),
        .I32 => |val| printAstNumberUtil(val, num),
        .I64 => |val| printAstNumberUtil(val, num),
        .I128 => |val| printAstNumberUtil(val, num),
        .ISize => |val| printAstNumberUtil(val, num),
        .F32 => |val| printAstNumberUtil(val, num),
        .F64 => |val| printAstNumberUtil(val, num),
        .F128 => |val| printAstNumberUtil(val, num),
    }
}

pub fn printNode(compInfo: *CompInfo, node: *blitzAst.AstNode) void {
    switch (node.*) {
        .IndexValue => |index| {
            print("indexing ", .{});
            printNode(compInfo, index.value);
            print(" with ", .{});
            printNode(compInfo, index.index);
        },
        .OpExpr => |op| {
            print("(", .{});
            printNode(compInfo, op.left);
            print(") ", .{});

            print("({s})", .{switch (op.type) {
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
            }});

            print(" (", .{});
            printNode(compInfo, op.right);
            print(")", .{});
        },
        .IncOne => |val| {
            print("(", .{});
            printNode(compInfo, val);
            print("++)", .{});
        },
        .DecOne => |val| {
            print("(", .{});
            printNode(compInfo, val);
            print("--)", .{});
        },
        .VarEqOp => |op| {
            print("set {s} to result of ({s} {s} ", .{
                op.variable, op.variable, switch (op.opType) {
                    .BitOrEq => "|BitOr|",
                    .BitAndEq => "&BitAnd&",
                    .AddEq => "+ADD+",
                    .SubEq => "-SUB-",
                    .MultEq => "*MULT*",
                    .DivEq => "/DIV/",
                    .AndEq => "*MULT*",
                    .OrEq => "/DIV/",
                },
            });
            printNode(compInfo, op.value);
            print(")", .{});
        },
        .FuncReference => |ref| {
            print("function ({s})", .{ref});
        },
        .StaticStructInstance => |inst| {
            print("static struct ({s})", .{inst});
        },
        .PropertyAccess => |access| {
            print("accessing {s} from ", .{access.property});
            printNode(compInfo, access.value);
        },
        .VarDec => |*dec| {
            print("declare ({s}) ({s}) = ", .{
                if (dec.isConst) "const" else "mutable",
                dec.name,
            });

            printNode(compInfo, dec.setNode);

            if (dec.annotation != null) {
                print(" with annotation: ", .{});
                printTypeInfo(compInfo, dec.annotation.?);
            }
        },
        .ValueSet => |set| {
            print("set ", .{});
            printNode(compInfo, set.value);
            print(" to ", .{});
            printNode(compInfo, set.setNode);
        },
        .Value => |*val| {
            printValue(compInfo, val);
        },
        .Type => |*t| {
            printType(compInfo, t);
        },
        .Seq => |*seq| {
            if (seq.nodes.len == 0) {
                print("(empty seq)", .{});
            } else {
                printNodes(compInfo, seq.nodes);
            }
        },
        .Cast => |*cast| {
            print("cast ", .{});
            printNode(compInfo, cast.node);
            print(" to ", .{});
            printTypeInfo(compInfo, cast.toType);
        },
        .Variable => |variable| {
            print("[variable: ({s})]", .{variable});
        },
        .Pointer => |ptr| {
            if (!ptr.isConst) {
                print("mut ", .{});
            }
            print("pointer to ", .{});
            printNode(compInfo, ptr.node);
        },
        .Dereference => |deref| {
            print("dereference ", .{});
            printNode(compInfo, deref);
        },
        .HeapAlloc => |alloc| {
            print("heap alloc ", .{});
            printNode(compInfo, alloc.node);
        },
        .StructPlaceholder => {
            print("struct dec", .{});
        },
        .StructDec => |dec| {
            print("declare struct ({s})", .{dec.name});

            if (dec.generics.len > 0) {
                print(" with generics [", .{});
                printGenerics(compInfo, dec.generics);
                print("]", .{});
            }

            if (dec.attributes.len > 0) {
                print(" with attributes [", .{});
                printAttributes(compInfo, dec.attributes);
                print("]", .{});
            }
        },
        .IfStatement => |statement| {
            print("if ", .{});
            printNode(compInfo, statement.condition);
            print(" then -- body --\n", .{});
            printNode(compInfo, statement.body);
            print(" -- body end --\n", .{});

            if (statement.fallback) |fallback| {
                printIfFallback(compInfo, fallback);
            }
        },
        .ForLoop => |loop| {
            print("for loop with", .{});

            if (loop.initNode) |init| {
                print(" init ", .{});
                printNode(compInfo, init);
            }

            print(" with condition ", .{});
            printNode(compInfo, loop.condition);

            print(" with inc ", .{});
            printNode(compInfo, loop.incNode);

            print(" -- body --\n", .{});
            printNode(compInfo, loop.body);
            print(" -- body end --\n", .{});
        },
        .WhileLoop => |loop| {
            print("while ", .{});
            printNode(compInfo, loop.condition);
            print(" -- body --\n", .{});
            printNode(compInfo, loop.body);
            print(" -- body end --\n", .{});
        },
        .NoOp => {
            print("(noop)", .{});
        },
        .FuncDec => |name| {
            const dec = compInfo.getFunctionAsGlobal(name).?;
            printFuncDec(compInfo, dec);
        },
        .FuncCall => |call| {
            print("calling ", .{});
            printNode(compInfo, call.func);
            print(" with params [", .{});

            for (call.params, 0..) |param, index| {
                printNode(compInfo, param);
                if (index < call.params.len - 1) {
                    print(", ", .{});
                }
            }

            print("]", .{});
        },
        .ReturnNode => |ret| {
            print("return ", .{});
            printNode(compInfo, ret);
        },
        .StructInit => |init| {
            print("initializing ({s})", .{init.name});

            if (init.generics.len > 0) {
                print("[generics: ", .{});

                for (init.generics, 0..) |generic, index| {
                    printTypeInfo(compInfo, generic);

                    if (index < init.generics.len - 1) {
                        print(", ", .{});
                    }
                }

                print("]", .{});
            }

            print(" with {{", .{});

            for (init.attributes, 0..) |attr, index| {
                print("{s}: ", .{attr.name});
                printNode(compInfo, attr.value);
                if (index < init.attributes.len - 1) {
                    print(", ", .{});
                }
            }
            print("}}", .{});
        },
        .Bang => |bang| {
            print("[bang]!", .{});
            printNode(compInfo, bang);
        },
        .ErrorDec => |def| printRegisteredError(def),
        .Error => |err| {
            print("error type ({s})", .{err});
        },
        .Group => |group| {
            print("[group](", .{});
            printNode(compInfo, group);
            print(")", .{});
        },
        .Scope => |scope| {
            print("\n--- entering scope ---\n", .{});
            printNode(compInfo, scope);
            print("\n--- exiting scope ---\n", .{});
        },
        .ArrayInit => |init| {
            print("init array [{s}]", .{init.size});
            printTypeInfo(compInfo, init.initType);
            print(" with initializer ", .{});
            printNode(compInfo, init.initNode);
        },
    }
}

fn printIfFallback(compInfo: *CompInfo, fallback: *const blitzAst.IfFallback) void {
    print("else ", .{});

    if (fallback.condition) |condition| {
        print(" if ", .{});
        printNode(compInfo, condition);
    }

    print("-- body --\n", .{});
    printNode(compInfo, fallback.body);
    print(" -- body end --\n", .{});

    if (fallback.fallback) |innerFallback| {
        printIfFallback(compInfo, innerFallback);
    }
}

pub fn printFuncDec(compInfo: *CompInfo, func: *const blitzAst.FuncDecNode) void {
    print("declare function [", .{});
    printTypeInfo(compInfo, func.returnType);
    print("] ({s})", .{func.name});
    if (func.generics) |generics| {
        print(" with generics [", .{});
        printGenerics(compInfo, generics);
        print("]", .{});
    }

    print(" with params [", .{});
    printParams(compInfo, func.params);

    if (func.capturedValues) |captured| {
        print("] capturing [", .{});
        var captureIt = captured.iterator();
        while (captureIt.next()) |item| {
            print("({s}: ", .{item.key_ptr.*});
            printTypeInfo(compInfo, item.value_ptr.*);
            print(")", .{});
        }
    }

    print("] -- body --\n", .{});
    printNode(compInfo, func.body);
    print(" -- body end --\n", .{});
}

pub fn printAttributes(compInfo: *CompInfo, attrs: []blitzAst.StructAttribute) void {
    for (attrs, 0..) |attr, index| {
        if (attr.static) {
            print("(static) ", .{});
        }

        print("{s} ({s}) ", .{ attr.visibility.toString(), attr.name });

        switch (attr.attr) {
            .Function => |func| printFuncDec(compInfo, func),
            .Member => |mem| printTypeInfo(compInfo, mem),
        }

        if (index < attrs.len - 1) {
            print(", ", .{});
        }
    }
}

fn printParams(compInfo: *CompInfo, params: []blitzAst.Parameter) void {
    if (params.len == 0) {
        print("(no params)", .{});
        return;
    }

    for (params, 0..) |param, index| {
        print("[", .{});
        printTypeInfo(compInfo, param.type);
        print("]({s})", .{param.name});

        if (index < params.len - 1) {
            print(", ", .{});
        }
    }
}

pub fn printGenerics(compInfo: *CompInfo, generics: []blitzAst.GenericType) void {
    for (generics, 0..) |generic, index| {
        print("[", .{});

        if (generic.restriction) |restriction| {
            printTypeInfo(compInfo, restriction);
        } else {
            print("any", .{});
        }

        print("]({s})", .{generic.name});

        if (index < generics.len - 1) {
            print(", ", .{});
        }
    }
}

fn printNodes(compInfo: *CompInfo, nodes: []*blitzAst.AstNode) void {
    for (nodes) |node| {
        printNode(compInfo, node);
        print("\n", .{});
    }
}

pub fn printRegisteredError(err: *const blitzAst.ErrorDecNode) void {
    print("defining error: {s} with variants [ ", .{err.name});

    if (err.variants) |variants| {
        for (variants, 0..) |variant, index| {
            print("{s}", .{variant});
            if (index < variants.len - 1) {
                print(", ", .{});
            }
        }
    }
}

pub fn printRegisteredErrors(errors: []*const blitzAst.ErrorDecNode) void {
    print("--- errors ---\n", .{});
    for (errors) |err| {
        printRegisteredError(err);
        print(" ]\n", .{});
    }
}

pub fn printRegisteredStructs(compInfo: *CompInfo, structs: [](*blitzAst.StructDecNode)) void {
    print("--- structs ---\n", .{});
    for (structs, 0..) |s, index| {
        print("declaring {s}", .{s.name});

        if (s.deriveType) |derived| {
            print(" extending ", .{});
            printTypeInfo(compInfo, derived);
        }

        if (s.generics.len > 0) {
            print(" with generics [", .{});
            printGenerics(compInfo, s.generics);
            print("]", .{});
        }

        print(" with attributes [", .{});
        printAttributes(compInfo, s.attributes);
        print("]", .{});

        print("\n", .{});

        if (index < structs.len - 1) {
            print("\n", .{});
        }
    }
}

pub fn printToken(token: tokenizer.Token, code: []u8) void {
    print("{any}", .{token.type});
    if (token.start != token.end) {
        print(" : {s}\n", .{token.strFromCode(code)});
    } else {
        print("\n", .{});
    }
}

pub fn printTokens(tokens: []const tokenizer.Token, code: []u8) void {
    for (tokens) |token| {
        printToken(token, code);
    }
}

pub fn printBytecodeChunks(genInfo: *const GenInfo, writer: anytype) !void {
    const rootChunk = genInfo.instructionList;

    if (rootChunk) |chunk| {
        const bytecode = chunk.chunk;
        const stackSizeType = @TypeOf(genInfo.stackStartSize);
        const stackSizeBufLen = @sizeOf(stackSizeType);

        const stackSizeBuf: *[stackSizeBufLen]u8 = bytecode[1 .. stackSizeBufLen + 1];
        try writer.writeAll("MakeStack ");
        try writeHexDecNumber(stackSizeBuf, writer);
        try writer.writeByte('\n');

        var byteCounter: usize = 0;
        var next = chunk.next;
        while (next) |nextChunk| {
            byteCounter += try printChunk(nextChunk.chunk, byteCounter, writer);
            next = nextChunk.next;
        }

        try writer.print("total bytes: {d}\n", .{genInfo.byteCounter});
    }
}

pub fn printBytecode(bytecode: []u8, writer: anytype) !void {
    try printVMStartInfo(bytecode[0..5], writer);
    var byteCounter: usize = 0;
    var current: usize = 5;
    while (current < bytecode.len) {
        const instr = @as(codegen.Instructions, @enumFromInt(bytecode[current]));
        const size = instr.getInstrLen();
        byteCounter += try printChunk(bytecode[current .. current + size], byteCounter, writer);
        current += size;
    }
}

pub fn printVMStartInfo(info: []u8, writer: anytype) !void {
    try writer.writeAll("blitz bytecode version ");
    try std.fmt.formatInt(info[0], 10, .lower, .{}, writer);
    try writer.writeAll("\nstarting stack size: ");
    const startStackSize = std.mem.readInt(u32, @ptrCast(info[1..5]), .big);
    try std.fmt.formatInt(startStackSize, 10, .lower, .{}, writer);
    try writer.writeByte('\n');
}

fn printChunk(bytecode: []u8, byteCounter: usize, writer: anytype) !usize {
    const inst = @as(codegen.Instructions, @enumFromInt(bytecode[0]));

    try writer.writeByte('[');
    try std.fmt.formatInt(byteCounter, 10, .lower, .{}, writer);
    try writer.writeAll("] (");
    try std.fmt.formatInt(bytecode.len, 10, .lower, .{}, writer);
    try writer.writeAll(") ");

    try writer.writeAll(inst.toString());

    switch (inst) {
        .SetRegHalf => {
            try writer.writeAll(" r");
            try std.fmt.formatInt(bytecode[1], 10, .lower, .{}, writer);
            try writer.writeAll(" ");
            const num = bytecode[2..6];
            try writeHexDecNumber(num, writer);
        },
        .SetRegByte, .CmpConstByte => {
            try writer.writeAll(" r");
            try std.fmt.formatInt(bytecode[1], 10, .lower, .{}, writer);
            try writer.writeAll(" ");
            try writeHexDecNumber(bytecode[2..3], writer);
        },
        .Cmp => {
            try writer.writeAll(" r");
            try writeByte(bytecode[1], writer);
            try writer.writeAll(" r");
            try writeByte(bytecode[2], writer);
        },
        .CmpSetReg => {
            try writer.writeAll(" r");
            try writeByte(bytecode[1], writer);
            try writer.writeAll(" r");
            try writeByte(bytecode[2], writer);
            try writer.writeAll(" r");
            try writeByte(bytecode[3], writer);
        },
        .Add,
        .Sub,
        .Mult,
        => try printBytecodeOpExpr(bytecode, writer),
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
        => {
            try writer.writeByte(' ');
            try writeHexDecNumber(bytecode[1..], writer);
        },
        .IncConstByte,
        .DecConstByte,
        => {
            try writer.writeAll(" r");
            try std.fmt.formatInt(bytecode[1], 10, .lower, .{}, writer);
            try writer.writeByte(' ');
            try writeHexDecNumber(bytecode[2..3], writer);
        },
        else => {
            try writer.writeAll(" (unknown_cmd) ");
        },
    }

    try writer.writeByte('\n');
    return bytecode.len;
}

fn writeByte(byte: u8, writer: anytype) !void {
    try std.fmt.formatInt(byte, 10, .lower, .{}, writer);
}

fn printBytecodeOpExpr(bytecode: []u8, writer: anytype) !void {
    try writer.writeAll(" r");
    try std.fmt.formatInt(bytecode[1], 10, .lower, .{}, writer);
    try writer.writeAll(" r");
    try std.fmt.formatInt(bytecode[2], 10, .lower, .{}, writer);
    try writer.writeAll(" r");
    try std.fmt.formatInt(bytecode[3], 10, .lower, .{}, writer);
}

fn printInstName(inst: u8, writer: anytype) !void {
    const name = @tagName(@as(codegen.Instructions, @enumFromInt(inst)));
    try writer.writeAll(name);
}

fn writeHexDecNumber(constStr: []u8, writer: anytype) !void {
    try writer.writeAll("0x");
    try formatHexByteSlice(constStr, writer);
    try writer.writeByte('(');
    try formatIntByteSlice(constStr, writer);
    try writer.writeByte(')');
}

fn formatIntByteSlice(slice: []u8, writer: anytype) !void {
    switch (slice.len) {
        1 => try formatIntByteSliceUtil(u8, slice, writer),
        2 => try formatIntByteSliceUtil(u16, slice, writer),
        4 => try formatIntByteSliceUtil(u32, slice, writer),
        else => unimplemented(),
    }
}

fn formatIntByteSliceUtil(comptime T: type, slice: []u8, writer: anytype) !void {
    var temp: T = 0;

    for (slice, 0..) |byte, index| {
        const shift = slice.len - index - 1;
        var byteTemp: T = byte;

        byteTemp = byteTemp << @intCast(shift * 8);
        temp += byteTemp;
    }

    try std.fmt.formatInt(temp, 10, .lower, .{}, writer);
}

fn formatHexByteSlice(slice: []u8, writer: anytype) !void {
    for (slice) |byte| {
        try std.fmt.formatInt(byte, 16, .lower, .{
            .width = 2,
            .fill = '0',
        }, writer);
    }
}

fn unimplemented() void {
    unreachable;
}
