const std = @import("std");
const blitz = @import("root").blitz;
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const Allocator = std.mem.Allocator;
const create = utils.create;
const ArrayList = std.ArrayList;

pub fn cloneAstTypes(allocator: Allocator, types: blitzAst.AstTypes) !blitzAst.AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number => return types,

        .DynamicArray => |arr| {
            const newPtr = try cloneAstTypesPtr(allocator, arr);
            return .{ .DynamicArray = newPtr };
        },
        .StaticArray => |arr| {
            const clonedType = try cloneAstTypesPtr(allocator, arr.type);
            const clonedNode = try cloneAstNodePtr(allocator, arr.size);

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
                .Nullable = try cloneAstTypesPtr(allocator, t),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneTypesArr(allocator, custom.generics);

            return .{
                .Custom = .{
                    .name = try string.cloneString(allocator, custom.name),
                    .generics = genericsSlice,
                },
            };
        },
        .Generic => |generic| {
            return .{
                .Generic = try string.cloneString(allocator, generic),
            };
        },
        .Function => |func| {
            var clonedGenerics: ?[]blitzAst.GenericType = null;

            if (func.generics) |generics| {
                clonedGenerics = try cloneGenerics(allocator, generics);
            }

            const name = try string.cloneString(allocator, func.name);
            const params = try cloneParameters(allocator, func.params);
            const returnType = try cloneAstTypesPtr(allocator, func.returnType);
            const bodyPtr = try cloneAstNodePtr(allocator, func.body);

            return .{
                .Function = try create(blitzAst.FuncDecNode, allocator, .{
                    .generics = clonedGenerics,
                    .name = name,
                    .params = params,
                    .returnType = returnType,
                    .body = bodyPtr,
                }),
            };
        },
    }
}

fn cloneParameters(allocator: Allocator, params: []blitzAst.Parameter) ![]blitzAst.Parameter {
    var parameters = ArrayList(blitzAst.Parameter).init(allocator);
    defer parameters.deinit();

    for (params) |param| {
        const typePtr = try cloneAstTypesPtr(allocator, param.type);
        const newParam = .{
            .name = try string.cloneString(allocator, param.name),
            .type = typePtr,
        };

        try parameters.append(newParam);
    }

    return try allocator.dupe(blitzAst.Parameter, parameters.items);
}

pub fn cloneAstNode(allocator: Allocator, node: blitzAst.AstNode) !blitzAst.AstNode {
    switch (node) {
        .NoOp => return node,

        .Add => |op| {
            return .{
                .Add = .{
                    .left = try cloneAstNodePtr(allocator, op.left),
                    .right = try cloneAstNodePtr(allocator, op.right),
                },
            };
        },
        .Sub => |op| {
            return .{
                .Sub = .{
                    .left = try cloneAstNodePtr(allocator, op.left),
                    .right = try cloneAstNodePtr(allocator, op.right),
                },
            };
        },
        .Mult => |op| {
            return .{
                .Mult = .{
                    .left = try cloneAstNodePtr(allocator, op.left),
                    .right = try cloneAstNodePtr(allocator, op.right),
                },
            };
        },
        .Div => |op| {
            return .{
                .Div = .{
                    .left = try cloneAstNodePtr(allocator, op.left),
                    .right = try cloneAstNodePtr(allocator, op.right),
                },
            };
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
                const nodePtr = try cloneAstNodePtr(allocator, seqNode);
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
                            .StaticArray = try cloneNodeArr(allocator, arr),
                        },
                    };
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtr(allocator, dec.setNode);
            var clonedType: ?*const blitzAst.AstTypes = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypesPtr(allocator, annotation);
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
            return .{ .Type = try cloneAstTypes(allocator, t) };
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtr(allocator, cast.node);
            const typePtr = try cloneAstTypesPtr(allocator, cast.toType);

            return .{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| {
            return .{
                .Variable = .{
                    .name = try string.cloneString(allocator, v.name),
                },
            };
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenerics(allocator, dec.generics);
            var deriveType: ?*const blitzAst.AstTypes = null;
            var attributes = ArrayList(blitzAst.StructAttribute).init(allocator);
            defer attributes.deinit();

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypesPtr(allocator, dType);
            }

            for (dec.attributes) |attr| {
                const newAttr = .{
                    .static = attr.static,
                    .attr = try cloneStructAttributeUnion(allocator, attr.attr),
                    .name = try string.cloneString(allocator, attr.name),
                    .visibility = attr.visibility,
                };

                try attributes.append(newAttr);
            }

            const attributesSlice = try allocator.dupe(blitzAst.StructAttribute, attributes.items);

            const structDec = .{
                .name = try string.cloneString(allocator, dec.name),
                .generics = clonedGenerics,
                .deriveType = deriveType,
                .attributes = attributesSlice,
            };
            const structNode = try create(blitzAst.StructDecNode, allocator, structDec);

            return .{
                .StructDec = structNode,
            };
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtr(allocator, statement.body);
            const conditionPtr = try cloneAstNodePtr(allocator, statement.condition);

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
            const clonedFunc = try cloneAstNodePtr(allocator, call.func);
            const newParams = try cloneNodeArr(allocator, call.params);

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| {
            return .{
                .ReturnNode = try cloneAstNodePtr(allocator, ret),
            };
        },
        .StructInit => |init| {
            const generics = try cloneTypesArr(allocator, init.generics);
            const name = try string.cloneString(allocator, init.name);
            var attributes = ArrayList(blitzAst.AttributeDefinition).init(allocator);
            defer attributes.deinit();

            for (init.attributes) |attr| {
                const newAttr = .{
                    .name = try string.cloneString(allocator, attr.name),
                    .value = try cloneAstNodePtr(allocator, attr.value),
                };

                try attributes.append(newAttr);
            }

            const attributesSlice = try allocator.dupe(blitzAst.AttributeDefinition, attributes.items);

            return .{
                .StructInit = .{
                    .attributes = attributesSlice,
                    .name = name,
                    .generics = generics,
                },
            };
        },
        .Bang => |bangNode| {
            return .{
                .Bang = try cloneAstNodePtr(allocator, bangNode),
            };
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtr(allocator, access.value);
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
    }
}

fn cloneStructAttributeUnion(allocator: Allocator, structAttrUnion: blitzAst.StructAttributeUnion) !blitzAst.StructAttributeUnion {
    switch (structAttrUnion) {
        .Function => |func| {
            const dec = try cloneFuncDec(allocator, func);
            return .{ .Function = dec };
        },
        .Member => |member| {
            const memType = try cloneAstTypesPtr(allocator, member);
            return .{ .Member = memType };
        },
    }
}

fn cloneTypesArr(allocator: Allocator, nodes: []*const blitzAst.AstTypes) ![]*const blitzAst.AstTypes {
    var newTypes = ArrayList(*const blitzAst.AstTypes).init(allocator);
    defer newTypes.deinit();

    for (nodes) |node| {
        const typePtr = try cloneAstTypesPtr(allocator, node);
        try newTypes.append(typePtr);
    }

    return try allocator.dupe(*const blitzAst.AstTypes, newTypes.items);
}

fn cloneNodeArr(allocator: Allocator, nodes: []*const blitzAst.AstNode) ![]*const blitzAst.AstNode {
    var newNodes = ArrayList(*const blitzAst.AstNode).init(allocator);
    defer newNodes.deinit();

    for (nodes) |node| {
        const nodePtr = try cloneAstNodePtr(allocator, node);
        try newNodes.append(nodePtr);
    }

    return try allocator.dupe(*const blitzAst.AstNode, newNodes.items);
}

pub fn cloneFuncDec(allocator: Allocator, dec: *const blitzAst.FuncDecNode) !*const blitzAst.FuncDecNode {
    const bodyPtr = try cloneAstNodePtr(allocator, dec.body);
    const name = try string.cloneString(allocator, dec.name);
    var generics: ?[]blitzAst.GenericType = null;
    const returnType = try cloneAstTypesPtr(allocator, dec.returnType);
    const params = try cloneParameters(allocator, dec.params);

    if (dec.generics) |decGenerics| {
        generics = try cloneGenerics(allocator, decGenerics);
    }

    return try create(blitzAst.FuncDecNode, allocator, .{
        .body = bodyPtr,
        .name = name,
        .params = params,
        .generics = generics,
        .returnType = returnType,
    });
}

fn cloneAstNodePtr(allocator: Allocator, node: *const blitzAst.AstNode) Allocator.Error!*const blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, node.*);
    return try create(blitzAst.AstNode, allocator, clonedNode);
}

fn cloneAstTypesPtr(allocator: Allocator, types: *const blitzAst.AstTypes) Allocator.Error!*const blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, types.*);
    return try create(blitzAst.AstTypes, allocator, clonedType);
}

fn cloneGeneric(allocator: Allocator, generic: blitzAst.GenericType) !blitzAst.GenericType {
    var restriction: ?*const blitzAst.AstTypes = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypesPtr(allocator, rest);
    }

    return .{
        .name = try string.cloneString(allocator, generic.name),
        .restriction = restriction,
    };
}

fn cloneGenerics(allocator: Allocator, generics: []blitzAst.GenericType) ![]blitzAst.GenericType {
    var clonedGenerics = ArrayList(blitzAst.GenericType).init(allocator);
    defer clonedGenerics.deinit();

    for (generics) |generic| {
        const newGeneric = try cloneGeneric(allocator, generic);
        try clonedGenerics.append(newGeneric);
    }

    return try allocator.dupe(blitzAst.GenericType, clonedGenerics.items);
}
