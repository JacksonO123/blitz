const std = @import("std");
const findToken = @import("utils.zig").findToken;
const tokenizer = @import("tokenizer.zig");
const utils = @import("utils.zig");
const create = utils.create;
const Allocator = std.mem.Allocator;
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const ArrayList = std.ArrayList;

const SeqNode = struct {
    nodes: []*const AstNode,
};

const AstNumberType = enum {
    U16,
    U32,
    U64,
    U128,
};
const AstNumber = union(AstNumberType) {
    U16: u16,
    U32: u32,
    U64: u64,
    U128: u128,
};

const AstDynamicArray = struct { type: *const AstValueType };
const AstStaticArray = struct {
    type: *const AstValueType,
    size: *const AstNode,
};

const AstNullableType = struct { type: *const AstValueType };

const AstValueTypeVariants = enum {
    String,
    Bool,
    Char,
    Number,
    DynamicArray,
    StaticArray,
    Nullable,
};
const AstValueType = union(AstValueTypeVariants) {
    String,
    Bool,
    Char,
    Number: AstNumberType,
    DynamicArray: AstDynamicArray,
    StaticArray: AstStaticArray,
    Nullable: AstNullableType,
};

const VarDecNode = struct {
    name: []u8,
    isConst: bool,
    setNode: *const AstNode,
    annotation: ?*const AstValueType,
};

const AstNodeType = enum {
    Seq,
    VarDec,
    Number,
    Type,
};
const AstNode = union(AstNodeType) {
    Seq: SeqNode,
    VarDec: VarDecNode,
    Number: AstNumber,
    Type: AstValueType,
};

const AstError = error{
    UnexpectedToken,
    TokenNotFound,
    InvalidType,
    UnknownType,
};

// const Ast = struct {
//     root: SeqNode,
// };

const AstNodeOffsetData = struct {
    node: *const AstNode,
    offset: usize,
};

const AstTypeOffsetData = struct {
    type: *const AstValueType,
    offset: usize,
};

pub fn Ast() type {
    return struct {
        const Self = @This();

        root: SeqNode,
        allocator: Allocator,

        pub fn init(allocator: Allocator) Self {
            return Self{
                .root = SeqNode{
                    .nodes = &[_]*const AstNode{},
                },
                .allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            for (self.root.nodes) |node| {
                freeNode(self.allocator, node);
            }

            self.allocator.free(self.root.nodes);
        }

        pub fn fromTokens(self: *Self, tokens: []Token) !void {
            var currentToken: usize = 0;
            var seq = ArrayList(*const AstNode).init(self.allocator);
            defer seq.deinit();

            while (currentToken < tokens.len) {
                const node = try createAstNode(self.allocator, tokens, currentToken);
                currentToken += node.offset;
                try seq.append(node.node);
                break;
            }

            const astNodes = try self.allocator.dupe(*const AstNode, seq.items);
            self.deinit();
            self.root.nodes = astNodes;
        }
    };
}

pub fn createAst(allocator: Allocator, tokens: []Token) !Ast {
    var currentToken: usize = 0;
    var seq = ArrayList(*const AstNode).init(allocator);
    defer seq.deinit();

    while (currentToken < tokens.len) {
        const node = try createAstNode(allocator, tokens, currentToken);
        currentToken += node.offset;
        try seq.append(node.node);
        break;
    }

    const astNodes = try allocator.dupe(*const AstNode, seq.items);

    return Ast{ .root = SeqNode{ .nodes = astNodes } };
}

fn freeType(allocator: Allocator, node: *const AstValueType) void {
    switch (node.*) {
        .DynamicArray => |*arr| {
            freeType(allocator, arr.type);
        },
        .StaticArray => |*arr| {
            freeType(allocator, arr.type);
            freeNode(allocator, arr.size);
        },
        .Nullable => |*nullable| {
            freeType(allocator, nullable.type);
        },
        else => {},
    }

    allocator.destroy(node);
}

fn freeNode(allocator: Allocator, node: *const AstNode) void {
    switch (node.*) {
        .VarDec => |*dec| {
            freeNode(allocator, dec.setNode);
            if (dec.annotation != null) {
                freeType(allocator, dec.annotation.?);
            }
        },
        .Number => {},
        .Seq => |*seq| {
            for (seq.nodes) |seqNode| {
                freeNode(allocator, seqNode);
            }
        },
        .Type => |*t| {
            freeType(allocator, t);
        },
    }

    allocator.destroy(node);
}

pub fn createAstNode(allocator: Allocator, tokens: []Token, start: usize) !AstNodeOffsetData {
    const token = tokens[start];

    switch (token.type) {
        .Const => {
            return try createVarDecNode(allocator, tokens, start, false);
        },
        .Var => {
            return AstError.UnexpectedToken;
        },
        .Number => {
            const num = try std.fmt.parseInt(u32, token.string.?, 10);
            const node = try create(AstNode, allocator, .{
                .Number = .{ .U32 = num },
            });

            return .{
                .node = node,
                .offset = 1,
            };
        },
        else => {
            return AstError.UnexpectedToken;
        },
    }
}

fn createVarDecNode(allocator: Allocator, tokens: []Token, start: usize, isConst: bool) anyerror!AstNodeOffsetData {
    const name = tokens[start + 1];
    const hasType = switch (tokens[start + 2].type) {
        .EqSet => false,
        .Colon => true,
        else => return AstError.UnexpectedToken,
    };
    var annotation: ?*const AstValueType = null;
    var offset: usize = 3;

    if (hasType) {
        const searchStart = start + offset;
        const index = findToken(tokens, searchStart, TokenType.EqSet);

        if (index == null) {
            try expectTokenError(TokenType.EqSet);
        }

        annotation = try createTypeNode(allocator, tokens, searchStart, index.?);

        offset += index.? - searchStart + 1;
    }

    const setNode = try createAstNode(allocator, tokens, start + offset);
    const tokenOffset = offset + setNode.offset;

    if (name.type != TokenType.Identifier) return AstError.UnexpectedToken;
    const node = try create(AstNode, allocator, .{
        .VarDec = VarDecNode{
            .name = name.string.?,
            .isConst = isConst,
            .setNode = setNode.node,
            .annotation = annotation,
        },
    });

    return .{ .node = node, .offset = tokenOffset };
}

fn createTypeNode(allocator: Allocator, tokens: []Token, start: usize, end: usize) !*const AstValueType {
    if (tokens.len == 0) return AstError.InvalidType;

    const nullable = tokens[0].type == TokenType.QuestionMark;
    var current = start + if (nullable) @as(u32, 1) else @as(u32, 0);

    var res: *const AstValueType = try createAstType(allocator, tokens[current]);

    while (tokens[current + 1].type == TokenType.LBracket and current < end) : (current += 1) {
        if (tokens[current + 2].type != TokenType.RBracket) {
            const sizeNode = try createAstNode(allocator, tokens, current + 2);
            res = try create(AstValueType, allocator, .{
                .StaticArray = .{
                    .type = res,
                    .size = sizeNode.node,
                },
            });
        } else {
            res = try create(AstValueType, allocator, AstValueType{
                .DynamicArray = .{
                    .type = res,
                },
            });
        }
    }

    return res;
}

fn createAstType(allocator: Allocator, token: Token) !*const AstValueType {
    const val = switch (token.type) {
        .Bool => AstValueType.Bool,
        .String => AstValueType.String,
        .U16 => AstValueType{ .Number = AstNumberType.U16 },
        .U32 => AstValueType{ .Number = AstNumberType.U32 },
        .U64 => AstValueType{ .Number = AstNumberType.U64 },
        .U128 => AstValueType{ .Number = AstNumberType.U128 },
        .Char => AstValueType.Char,
        else => return AstError.UnknownType,
    };

    return create(AstValueType, allocator, val);
}

fn isClosingToken(token: Token) bool {
    return switch (token.type) {
        .RParen => true,
        .RBrace => true,
        .RBracket => true,
        else => false,
    };
}

fn isOpenToken(token: Token) bool {
    return switch (token.type) {
        .LParen => true,
        .LBrace => true,
        .LBracket => true,
        else => false,
    };
}

fn expectTokenError(tokenType: TokenType) !void {
    const stdout = std.io.getStdOut().writer();
    const str = tokenType.toString();
    try stdout.print("Token not found: {s}\n", .{str});

    return AstError.TokenNotFound;
}
