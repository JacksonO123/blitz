const std = @import("std");
const astMod = @import("ast.zig");
const utils = @import("utils.zig");
const AstTypes = astMod.AstTypes;
const Allocator = std.mem.Allocator;
const create = utils.create;
const cloneString = utils.cloneString;
const AstNode = astMod.AstNode;
const ArrayList = std.ArrayList;
const GenericType = astMod.GenericType;
const Parameter = astMod.Parameter;
const FuncDecNode = astMod.FuncDecNode;
const AttributeDefinition = astMod.AttributeDefinition;
const StructAttribute = astMod.StructAttribute;
const StructDecNode = astMod.StructDecNode;
const StructAttributeUnion = astMod.StructAttributeUnion;

pub fn cloneAstTypes(allocator: Allocator, types: AstTypes) !AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number => return types,

        .DynamicArray => |arr| {
            const newPtr = try cloneAstTypesPtr(allocator, arr);
            return AstTypes{ .DynamicArray = newPtr };
        },
        .StaticArray => |arr| {
            const clonedType = try cloneAstTypesPtr(allocator, arr.type);
            const clonedNode = try cloneAstNodePtr(allocator, arr.size);

            return AstTypes{
                .StaticArray = .{
                    .type = clonedType,
                    .size = clonedNode,
                },
            };
        },
        .StaticStructInstance => |name| {
            return AstTypes{
                .StaticStructInstance = try cloneString(allocator, name),
            };
        },
        .Nullable => |t| {
            return AstTypes{
                .Nullable = try cloneAstTypesPtr(allocator, t),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneTypesArr(allocator, custom.generics);

            return AstTypes{
                .Custom = .{
                    .name = try cloneString(allocator, custom.name),
                    .generics = genericsSlice,
                },
            };
        },
        .Generic => |generic| {
            return AstTypes{
                .Generic = try cloneString(allocator, generic),
            };
        },
        .Function => |func| {
            var clonedGenerics: ?[]GenericType = null;

            if (func.generics) |generics| {
                clonedGenerics = try cloneGenerics(allocator, generics);
            }

            const name = try cloneString(allocator, func.name);
            const params = try cloneParameters(allocator, func.params);
            const returnType = try cloneAstTypesPtr(allocator, func.returnType);
            const bodyPtr = try cloneAstNodePtr(allocator, func.body);

            return AstTypes{
                .Function = .{
                    .generics = clonedGenerics,
                    .name = name,
                    .params = params,
                    .returnType = returnType,
                    .body = bodyPtr,
                },
            };
        },
    }
}

fn cloneParameters(allocator: Allocator, params: []Parameter) ![]Parameter {
    var parameters = ArrayList(Parameter).init(allocator);
    defer parameters.deinit();

    for (params) |param| {
        const typePtr = try cloneAstTypesPtr(allocator, param.type);
        const newParam = Parameter{
            .name = try cloneString(allocator, param.name),
            .type = typePtr,
        };

        try parameters.append(newParam);
    }

    return try allocator.dupe(Parameter, parameters.items);
}

pub fn cloneAstNode(allocator: Allocator, node: AstNode) !AstNode {
    switch (node) {
        .NoOp => return node,

        .Seq => |seq| {
            var newSeq = ArrayList(*const AstNode).init(allocator);
            defer newSeq.deinit();

            for (seq.nodes) |seqNode| {
                const nodePtr = try cloneAstNodePtr(allocator, seqNode);
                try newSeq.append(nodePtr);
            }

            const seqSlice = try allocator.dupe(*const AstNode, newSeq.items);

            return AstNode{
                .Seq = .{
                    .nodes = seqSlice,
                },
            };
        },
        .Value => |val| {
            switch (val) {
                .String => |str| {
                    return AstNode{
                        .Value = .{
                            .String = try cloneString(allocator, str),
                        },
                    };
                },
                .Bool => |b| {
                    return AstNode{
                        .Value = .{ .Bool = b },
                    };
                },
                .Char => |c| {
                    return AstNode{
                        .Value = .{ .Char = c },
                    };
                },
                .Number => |num| {
                    return AstNode{
                        .Value = .{
                            .Number = .{
                                .type = num.type,
                                .value = try cloneString(allocator, num.value),
                            },
                        },
                    };
                },
                .StaticArray => |arr| {
                    return AstNode{
                        .Value = .{
                            .StaticArray = try cloneNodeArr(allocator, arr),
                        },
                    };
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtr(allocator, dec.setNode);
            var clonedType: ?*const AstTypes = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypesPtr(allocator, annotation);
            }

            return AstNode{
                .VarDec = .{
                    .name = try cloneString(allocator, dec.name),
                    .isConst = dec.isConst,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                },
            };
        },
        .Type => |t| {
            return AstNode{ .Type = try cloneAstTypes(allocator, t) };
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtr(allocator, cast.node);
            const typePtr = try cloneAstTypesPtr(allocator, cast.toType);

            return AstNode{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| {
            return AstNode{
                .Variable = .{
                    .name = try cloneString(allocator, v.name),
                },
            };
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenerics(allocator, dec.generics);
            var deriveType: ?*const AstTypes = null;
            var attributes = ArrayList(StructAttribute).init(allocator);
            defer attributes.deinit();

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypesPtr(allocator, dType);
            }

            for (dec.attributes) |attr| {
                const newAttr = StructAttribute{
                    .static = attr.static,
                    .attr = try cloneStructAttributeUnion(allocator, attr.attr),
                };

                try attributes.append(newAttr);
            }

            const attributesSlice = try allocator.dupe(StructAttribute, attributes.items);

            const structDec = StructDecNode{
                .name = try cloneString(allocator, dec.name),
                .generics = clonedGenerics,
                .deriveType = deriveType,
                .attributes = attributesSlice,
            };
            const structNode = try create(StructDecNode, allocator, structDec);

            return AstNode{
                .StructDec = structNode,
            };
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtr(allocator, statement.body);
            const conditionPtr = try cloneAstNodePtr(allocator, statement.condition);

            return AstNode{
                .IfStatement = .{
                    .body = bodyPtr,
                    .condition = conditionPtr,
                },
            };
        },
        .FuncDec => |dec| {
            return AstNode{
                .FuncDec = try cloneFuncDec(allocator, dec),
            };
        },
        .FuncCall => |call| {
            const clonedFunc = try cloneFuncDec(allocator, call.func.*);
            const funcPtr = try create(FuncDecNode, allocator, clonedFunc);

            const newParams = try cloneNodeArr(allocator, call.params);

            return AstNode{
                .FuncCall = .{
                    .func = funcPtr,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| {
            return AstNode{
                .ReturnNode = try cloneAstNodePtr(allocator, ret),
            };
        },
        .StructInit => |init| {
            const generics = try cloneTypesArr(allocator, init.generics);
            const name = try cloneString(allocator, init.name);
            var attributes = ArrayList(AttributeDefinition).init(allocator);
            defer attributes.deinit();

            for (init.attributes) |attr| {
                const newAttr = AttributeDefinition{
                    .name = try cloneString(allocator, attr.name),
                    .value = try cloneAstNodePtr(allocator, attr.value),
                };

                try attributes.append(newAttr);
            }

            const attributesSlice = try allocator.dupe(AttributeDefinition, attributes.items);

            return AstNode{
                .StructInit = .{
                    .attributes = attributesSlice,
                    .name = name,
                    .generics = generics,
                },
            };
        },
        .Bang => |bangNode| {
            return AstNode{
                .Bang = try cloneAstNodePtr(allocator, bangNode),
            };
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtr(allocator, access.value);
            const prop = try cloneString(allocator, access.property);

            return AstNode{
                .PropertyAccess = .{
                    .value = value,
                    .property = prop,
                },
            };
        },
        .StaticStructInstance => |inst| {
            return AstNode{
                .StaticStructInstance = try cloneString(allocator, inst),
            };
        },
    }
}

fn cloneStructAttributeUnion(allocator: Allocator, structAttrUnion: StructAttributeUnion) !StructAttributeUnion {
    switch (structAttrUnion) {
        .Function => |func| {
            const dec = try cloneFuncDec(allocator, func.func);
            const name = try cloneString(allocator, func.name);
            const visibility = func.visibility;
            return StructAttributeUnion{
                .Function = .{
                    .func = dec,
                    .name = name,
                    .visibility = visibility,
                },
            };
        },
        .Member => |member| {
            const memType = try cloneAstTypesPtr(allocator, member.type);
            const name = try cloneString(allocator, member.name);
            const visibility = member.visibility;
            return StructAttributeUnion{
                .Member = .{
                    .type = memType,
                    .name = name,
                    .visibility = visibility,
                },
            };
        },
    }
}

fn cloneTypesArr(allocator: Allocator, nodes: []*const AstTypes) ![]*const AstTypes {
    var newTypes = ArrayList(*const AstTypes).init(allocator);
    defer newTypes.deinit();

    for (nodes) |node| {
        const typePtr = try cloneAstTypesPtr(allocator, node);
        try newTypes.append(typePtr);
    }

    return try allocator.dupe(*const AstTypes, newTypes.items);
}

fn cloneNodeArr(allocator: Allocator, nodes: []*const AstNode) ![]*const AstNode {
    var newNodes = ArrayList(*const AstNode).init(allocator);
    defer newNodes.deinit();

    for (nodes) |node| {
        const nodePtr = try cloneAstNodePtr(allocator, node);
        try newNodes.append(nodePtr);
    }

    return try allocator.dupe(*const AstNode, newNodes.items);
}

pub fn cloneFuncDec(allocator: Allocator, dec: FuncDecNode) !FuncDecNode {
    const bodyPtr = try cloneAstNodePtr(allocator, dec.body);
    const name = try cloneString(allocator, dec.name);
    var generics: ?[]GenericType = null;
    const returnType = try cloneAstTypesPtr(allocator, dec.returnType);
    const params = try cloneParameters(allocator, dec.params);

    if (dec.generics) |decGenerics| {
        generics = try cloneGenerics(allocator, decGenerics);
    }

    return .{
        .body = bodyPtr,
        .name = name,
        .params = params,
        .generics = generics,
        .returnType = returnType,
    };
}

fn cloneAstNodePtr(allocator: Allocator, node: *const AstNode) Allocator.Error!*const AstNode {
    const clonedNode = try cloneAstNode(allocator, node.*);
    return try create(AstNode, allocator, clonedNode);
}

fn cloneAstTypesPtr(allocator: Allocator, types: *const AstTypes) Allocator.Error!*const AstTypes {
    const clonedType = try cloneAstTypes(allocator, types.*);
    return try create(AstTypes, allocator, clonedType);
}

fn cloneGeneric(allocator: Allocator, generic: GenericType) !GenericType {
    var restriction: ?*const AstTypes = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypesPtr(allocator, rest);
    }

    return GenericType{
        .name = try cloneString(allocator, generic.name),
        .restriction = restriction,
    };
}

fn cloneGenerics(allocator: Allocator, generics: []GenericType) ![]GenericType {
    var clonedGenerics = ArrayList(GenericType).init(allocator);
    defer clonedGenerics.deinit();

    for (generics) |generic| {
        const newGeneric = try cloneGeneric(allocator, generic);
        try clonedGenerics.append(newGeneric);
    }

    return try allocator.dupe(GenericType, clonedGenerics.items);
}
