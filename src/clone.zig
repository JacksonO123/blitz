const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const Allocator = std.mem.Allocator;
const create = utils.create;
const createMut = utils.createMut;
const ArrayList = std.ArrayList;
const CompInfo = utils.CompInfo;

pub const CloneError = error{
    GenericNotFound,
};

pub fn cloneAstTypes(allocator: Allocator, compInfo: *CompInfo, types: blitzAst.AstTypes, replaceGenerics: bool) (Allocator.Error || CloneError)!blitzAst.AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number => return types,

        .DynamicArray => |arr| {
            const newPtr = try cloneAstTypesPtr(allocator, compInfo, arr, replaceGenerics);
            return .{ .DynamicArray = newPtr };
        },
        .StaticArray => |arr| {
            const clonedType = try cloneAstTypesPtr(allocator, compInfo, arr.type, replaceGenerics);
            const clonedNode = try cloneAstNodePtr(allocator, compInfo, arr.size, replaceGenerics);

            return .{
                .StaticArray = .{
                    .type = clonedType,
                    .size = clonedNode,
                },
            };
        },
        .StaticStructInstance => |name| {
            return .{
                .StaticStructInstance = try string.cloneString(allocator, name),
            };
        },
        .Nullable => |t| {
            return .{
                .Nullable = try cloneAstTypesPtr(allocator, compInfo, t, replaceGenerics),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneTypesArr(allocator, compInfo, custom.generics, replaceGenerics);

            return .{
                .Custom = .{
                    .name = try string.cloneString(allocator, custom.name),
                    .generics = genericsSlice,
                },
            };
        },
        .Generic => |generic| {
            if (replaceGenerics) {
                const genType = compInfo.getGeneric(generic);
                if (genType) |gType| return cloneAstTypes(allocator, compInfo, gType.*, replaceGenerics);
                return CloneError.GenericNotFound;
            }

            return .{
                .Generic = try string.cloneString(allocator, generic),
            };
        },
        .Function => |func| {
            var clonedGenerics: ?[]blitzAst.GenericType = null;

            if (func.generics) |generics| {
                clonedGenerics = try cloneGenerics(allocator, compInfo, generics, replaceGenerics);
            }

            const name = try string.cloneString(allocator, func.name);
            const params = try cloneParameters(allocator, compInfo, func.params, replaceGenerics);
            const returnType = try cloneAstTypesPtr(allocator, compInfo, func.returnType, replaceGenerics);
            const bodyPtr = try cloneAstNodePtr(allocator, compInfo, func.body, replaceGenerics);

            return .{
                .Function = try create(blitzAst.FuncDecNode, allocator, .{
                    .generics = clonedGenerics,
                    .name = name,
                    .params = params,
                    .returnType = returnType,
                    .body = bodyPtr,
                    .bodyTokens = func.bodyTokens,
                }),
            };
        },
        .Error => |err| {
            return .{
                .Error = try string.cloneString(allocator, err),
            };
        },
        .ErrorVariant => |err| {
            return .{
                .ErrorVariant = .{
                    .from = try string.cloneString(allocator, err.from),
                    .variant = try string.cloneString(allocator, err.variant),
                },
            };
        },
    }
}

fn cloneParameters(allocator: Allocator, compInfo: *CompInfo, params: []blitzAst.Parameter, replaceGenerics: bool) ![]blitzAst.Parameter {
    var parameters = ArrayList(blitzAst.Parameter).init(allocator);
    defer parameters.deinit();

    for (params) |param| {
        const typePtr = try cloneAstTypesPtr(allocator, compInfo, param.type, replaceGenerics);
        const newParam: blitzAst.Parameter = .{
            .name = try string.cloneString(allocator, param.name),
            .type = typePtr,
        };

        try parameters.append(newParam);
    }

    return try allocator.dupe(blitzAst.Parameter, parameters.items);
}

pub fn cloneAstNode(allocator: Allocator, compInfo: *CompInfo, node: blitzAst.AstNode, replaceGenerics: bool) !blitzAst.AstNode {
    switch (node) {
        .NoOp => return node,

        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtr(allocator, compInfo, index.index, replaceGenerics),
                .value = try cloneAstNodePtr(allocator, compInfo, index.value, replaceGenerics),
            },
        },
        .MathOp => |op| {
            const sides = .{
                .left = try cloneAstNodePtr(allocator, compInfo, op.left, replaceGenerics),
                .right = try cloneAstNodePtr(allocator, compInfo, op.right, replaceGenerics),
            };

            const opTypes = &[_]blitzAst.MathOps{ .Add, .Sub, .Mult, .Div };

            inline for (opTypes) |opType| {
                if (op.type == opType) {
                    return .{
                        .MathOp = .{
                            .type = opType,
                            .left = sides.left,
                            .right = sides.right,
                        },
                    };
                }
            }

            unreachable;
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = try string.cloneString(allocator, ref),
            };
        },
        .Seq => |seq| {
            var newSeq = ArrayList(*const blitzAst.AstNode).init(allocator);
            defer newSeq.deinit();

            for (seq.nodes) |seqNode| {
                const nodePtr = try cloneAstNodePtr(allocator, compInfo, seqNode, replaceGenerics);
                try newSeq.append(nodePtr);
            }

            const seqSlice = try allocator.dupe(*const blitzAst.AstNode, newSeq.items);

            return .{
                .Seq = .{
                    .nodes = seqSlice,
                },
            };
        },
        .Value => |val| {
            switch (val) {
                .String => |str| {
                    return .{
                        .Value = .{
                            .String = try string.cloneString(allocator, str),
                        },
                    };
                },
                .Bool => |b| {
                    return .{
                        .Value = .{ .Bool = b },
                    };
                },
                .Char => |c| {
                    return .{
                        .Value = .{ .Char = c },
                    };
                },
                .Number => |num| {
                    return .{
                        .Value = .{
                            .Number = .{
                                .type = num.type,
                                .value = try string.cloneString(allocator, num.value),
                            },
                        },
                    };
                },
                .StaticArray => |arr| {
                    return .{
                        .Value = .{
                            .StaticArray = try cloneNodeArr(allocator, compInfo, arr, replaceGenerics),
                        },
                    };
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtr(allocator, compInfo, dec.setNode, replaceGenerics);
            var clonedType: ?*const blitzAst.AstTypes = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypesPtr(allocator, compInfo, annotation, replaceGenerics);
            }

            return .{
                .VarDec = .{
                    .name = try string.cloneString(allocator, dec.name),
                    .isConst = dec.isConst,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                },
            };
        },
        .Type => |t| {
            return .{
                .Type = try cloneAstTypes(allocator, compInfo, t, replaceGenerics),
            };
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtr(allocator, compInfo, cast.node, replaceGenerics);
            const typePtr = try cloneAstTypesPtr(allocator, compInfo, cast.toType, replaceGenerics);

            return .{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| {
            return .{
                .Variable = try string.cloneString(allocator, v),
            };
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenerics(allocator, compInfo, dec.generics, replaceGenerics);
            var deriveType: ?*const blitzAst.AstTypes = null;
            const attributes = try cloneStructAttrDec(allocator, compInfo, dec.attributes, replaceGenerics);
            const totalMemberList = try cloneStructAttrDec(allocator, compInfo, dec.attributes, replaceGenerics);

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypesPtr(
                    allocator,
                    compInfo,
                    dType,
                    replaceGenerics,
                );
            }

            const structDec: blitzAst.StructDecNode = .{
                .name = try string.cloneString(allocator, dec.name),
                .generics = clonedGenerics,
                .deriveType = deriveType,
                .attributes = attributes,
                .totalMemberList = totalMemberList,
            };
            const structNode = try createMut(blitzAst.StructDecNode, allocator, structDec);

            return .{
                .StructDec = structNode,
            };
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtr(allocator, compInfo, statement.body, replaceGenerics);
            const conditionPtr = try cloneAstNodePtr(allocator, compInfo, statement.condition, replaceGenerics);

            return .{
                .IfStatement = .{
                    .body = bodyPtr,
                    .condition = conditionPtr,
                },
            };
        },
        .FuncDec => |dec| {
            return .{
                .FuncDec = try string.cloneString(allocator, dec),
            };
        },
        .FuncCall => |call| {
            const clonedFunc = try cloneAstNodePtr(
                allocator,
                compInfo,
                call.func,
                replaceGenerics,
            );
            const newParams = try cloneNodeArr(allocator, compInfo, call.params, replaceGenerics);

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| {
            return .{
                .ReturnNode = try cloneAstNodePtr(allocator, compInfo, ret, replaceGenerics),
            };
        },
        .StructInit => |init| {
            const generics = try cloneTypesArr(allocator, compInfo, init.generics, replaceGenerics);
            const name = try string.cloneString(allocator, init.name);
            const attributes = try cloneAttrDef(allocator, compInfo, init.attributes, replaceGenerics);

            return .{
                .StructInit = .{
                    .attributes = attributes,
                    .name = name,
                    .generics = generics,
                },
            };
        },
        .Bang => |bangNode| {
            return .{
                .Bang = try cloneAstNodePtr(allocator, compInfo, bangNode, replaceGenerics),
            };
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtr(allocator, compInfo, access.value, replaceGenerics);
            const prop = try string.cloneString(allocator, access.property);

            return .{
                .PropertyAccess = .{
                    .value = value,
                    .property = prop,
                },
            };
        },
        .StaticStructInstance => |inst| {
            return .{
                .StaticStructInstance = try string.cloneString(allocator, inst),
            };
        },
        .ErrorDec => |def| {
            var newVariants = try ArrayList([]u8).initCapacity(allocator, def.variants.len);
            defer newVariants.deinit();

            for (def.variants) |variant| {
                const str = try string.cloneString(allocator, variant);
                try newVariants.append(str);
            }

            return .{
                .ErrorDec = try create(blitzAst.ErrorDecNode, allocator, .{
                    .name = try string.cloneString(allocator, def.name),
                    .variants = try newVariants.toOwnedSlice(),
                }),
            };
        },
        .Error => |err| {
            return .{
                .Error = try string.cloneString(allocator, err),
            };
        },
    }
}

fn cloneStructAttrDec(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.StructAttribute, replaceGenerics: bool) ![]blitzAst.StructAttribute {
    var attributes = try ArrayList(blitzAst.StructAttribute).initCapacity(allocator, attrs.len);
    defer attributes.deinit();

    for (attrs) |attr| {
        const newAttr: blitzAst.StructAttribute = .{
            .static = attr.static,
            .attr = try cloneStructAttributeUnion(allocator, compInfo, attr.attr, replaceGenerics),
            .name = try string.cloneString(allocator, attr.name),
            .visibility = attr.visibility,
        };

        try attributes.append(newAttr);
    }

    return attributes.toOwnedSlice();
}

fn cloneAttrDef(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.AttributeDefinition, replaceGenerics: bool) ![]blitzAst.AttributeDefinition {
    var attributes = try ArrayList(blitzAst.AttributeDefinition).initCapacity(allocator, attrs.len);
    defer attributes.deinit();

    for (attrs) |attr| {
        const newAttr: blitzAst.AttributeDefinition = .{
            .name = try string.cloneString(allocator, attr.name),
            .value = try cloneAstNodePtr(allocator, compInfo, attr.value, replaceGenerics),
        };

        try attributes.append(newAttr);
    }

    return attributes.toOwnedSlice();
}

pub fn cloneStructAttributeUnion(allocator: Allocator, compInfo: *CompInfo, structAttrUnion: blitzAst.StructAttributeUnion, replaceGenerics: bool) !blitzAst.StructAttributeUnion {
    return switch (structAttrUnion) {
        .Function => |func| .{
            .Function = try cloneFuncDec(allocator, compInfo, func, replaceGenerics),
        },
        .Member => |member| .{
            .Member = try cloneAstTypesPtr(allocator, compInfo, member, replaceGenerics),
        },
    };
}

pub fn cloneStructAttributeUnionType(allocator: Allocator, compInfo: *CompInfo, structAttrUnion: blitzAst.StructAttributeUnion, replaceGenerics: bool) !blitzAst.AstTypes {
    return switch (structAttrUnion) {
        .Function => |func| .{
            .Function = func,
        },
        .Member => |member| try cloneAstTypes(allocator, compInfo, member.*, replaceGenerics),
    };
}

pub fn cloneTypesArr(allocator: Allocator, compInfo: *CompInfo, nodes: []*const blitzAst.AstTypes, replaceGenerics: bool) ![]*const blitzAst.AstTypes {
    var arr = try ArrayList(*const blitzAst.AstTypes).initCapacity(allocator, nodes.len);
    defer arr.deinit();

    for (nodes) |node| {
        const typePtr = try cloneAstTypesPtr(allocator, compInfo, node, replaceGenerics);
        try arr.append(typePtr);
    }

    return try arr.toOwnedSlice();
}

fn cloneNodeArr(allocator: Allocator, compInfo: *CompInfo, nodes: []*const blitzAst.AstNode, replaceGenerics: bool) ![]*const blitzAst.AstNode {
    var newNodes = ArrayList(*const blitzAst.AstNode).init(allocator);
    defer newNodes.deinit();

    for (nodes) |node| {
        const nodePtr = try cloneAstNodePtr(allocator, compInfo, node, replaceGenerics);
        try newNodes.append(nodePtr);
    }

    return try allocator.dupe(*const blitzAst.AstNode, newNodes.items);
}

pub fn cloneFuncDec(allocator: Allocator, compInfo: *CompInfo, dec: *blitzAst.FuncDecNode, replaceGenerics: bool) !*blitzAst.FuncDecNode {
    const bodyPtr = try cloneAstNodePtr(allocator, compInfo, dec.body, replaceGenerics);
    const name = try string.cloneString(allocator, dec.name);
    var generics: ?[]blitzAst.GenericType = null;
    const returnType = try cloneAstTypesPtr(allocator, compInfo, dec.returnType, replaceGenerics);
    const params = try cloneParameters(allocator, compInfo, dec.params, replaceGenerics);

    if (dec.generics) |decGenerics| {
        generics = try cloneGenerics(allocator, compInfo, decGenerics, replaceGenerics);
    }

    return try createMut(blitzAst.FuncDecNode, allocator, .{
        .body = bodyPtr,
        .bodyTokens = dec.bodyTokens,
        .name = name,
        .params = params,
        .generics = generics,
        .returnType = returnType,
    });
}

fn cloneAstNodePtr(allocator: Allocator, compInfo: *CompInfo, node: *const blitzAst.AstNode, replaceGenerics: bool) (Allocator.Error || CloneError)!*const blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, compInfo, node.*, replaceGenerics);
    return try create(blitzAst.AstNode, allocator, clonedNode);
}

pub fn cloneAstTypesPtr(allocator: Allocator, compInfo: *CompInfo, types: *const blitzAst.AstTypes, replaceGenerics: bool) !*const blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, compInfo, types.*, replaceGenerics);
    return try create(blitzAst.AstTypes, allocator, clonedType);
}

fn cloneGeneric(allocator: Allocator, compInfo: *CompInfo, generic: blitzAst.GenericType, replaceGenerics: bool) !blitzAst.GenericType {
    var restriction: ?*const blitzAst.AstTypes = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypesPtr(allocator, compInfo, rest, replaceGenerics);
    }

    return .{
        .name = try string.cloneString(allocator, generic.name),
        .restriction = restriction,
    };
}

pub fn cloneGenerics(allocator: Allocator, compInfo: *CompInfo, generics: []blitzAst.GenericType, replaceGenerics: bool) ![]blitzAst.GenericType {
    var clonedGenerics = ArrayList(blitzAst.GenericType).init(allocator);
    defer clonedGenerics.deinit();

    for (generics) |generic| {
        const newGeneric = try cloneGeneric(allocator, compInfo, generic, replaceGenerics);
        try clonedGenerics.append(newGeneric);
    }

    return try allocator.dupe(blitzAst.GenericType, clonedGenerics.items);
}
