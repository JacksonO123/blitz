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
        .String, .Bool, .Char, .Void, .Number, .Null, .RawNumber, .Any => return types,

        .DynamicArray => |arr| {
            const newPtr = try cloneAstTypeInfo(allocator, compInfo, arr, replaceGenerics);
            return .{ .DynamicArray = newPtr };
        },
        .StaticArray, .GeneralArray => |arr| {
            const clonedType = try cloneAstTypeInfo(allocator, compInfo, arr.type, replaceGenerics);
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
                .Nullable = try cloneAstTypeInfo(allocator, compInfo, t, replaceGenerics),
            };
        },
        .Custom => |custom| {
            // TODO - deep clone these
            const genericsSlice = try cloneCustomGenerics(allocator, compInfo, custom.generics, replaceGenerics);

            return .{
                .Custom = .{
                    .name = try string.cloneString(allocator, custom.name),
                    .generics = genericsSlice,
                },
            };
        },
        .Generic => |generic| {
            if (replaceGenerics) {
                const genType = try compInfo.getGeneric(generic);
                // TODO - bring this back and figure something out for info
                // if (genType) |gType| return cloneAstTypeInfo(allocator, compInfo, gType, replaceGenerics);
                if (genType) |gType| return cloneAstTypes(allocator, compInfo, gType.astType.*, replaceGenerics);
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
            const returnType = try cloneAstTypeInfo(allocator, compInfo, func.returnType, replaceGenerics);
            const bodyPtr = try cloneAstNodePtr(allocator, compInfo, func.body, replaceGenerics);

            var capturedValues: ?*utils.CaptureScope = null;
            if (func.capturedValues) |values| {
                capturedValues = try cloneCaptureScope(allocator, values);
            }

            var capturedTypes: ?*utils.TypeScope = null;
            if (func.capturedTypes) |captured| {
                capturedTypes = try cloneGenericScope(allocator, captured);
            }

            return .{
                .Function = try createMut(blitzAst.FuncDecNode, allocator, .{
                    .generics = clonedGenerics,
                    .name = name,
                    .params = params,
                    .returnType = returnType,
                    .body = bodyPtr,
                    .bodyTokens = func.bodyTokens,
                    .capturedValues = capturedValues,
                    .capturedTypes = capturedTypes,
                    .builtin = func.builtin,
                }),
            };
        },
        .Error => |err| {
            var payload: ?blitzAst.AstTypeInfo = null;

            if (err.payload) |errPayload| {
                payload = try cloneAstTypeInfo(allocator, compInfo, errPayload, replaceGenerics);
            }

            return .{
                .Error = .{
                    .name = try string.cloneString(allocator, err.name),
                    .payload = payload,
                },
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

pub fn cloneCustomGenerics(
    allocator: Allocator,
    compInfo: *CompInfo,
    generics: []blitzAst.AstTypeInfo,
    replaceGenerics: bool,
) ![]blitzAst.AstTypeInfo {
    const genericsSlice = try allocator.alloc(blitzAst.AstTypeInfo, generics.len);

    for (generics, 0..) |gen, index| {
        genericsSlice[index] = try cloneAstTypeInfo(allocator, compInfo, gen, replaceGenerics);
    }

    return genericsSlice;
}

pub fn cloneAstTypeInfo(allocator: Allocator, compInfo: *CompInfo, info: blitzAst.AstTypeInfo, replaceGenerics: bool) !blitzAst.AstTypeInfo {
    return .{
        .astType = try cloneAstTypesPtr(allocator, compInfo, info.astType, replaceGenerics),
        .isConst = info.isConst,
    };
}

fn cloneCaptureScope(allocator: Allocator, scope: *const utils.CaptureScope) !*utils.CaptureScope {
    const newScope = try utils.initMutPtrT(utils.CaptureScope, allocator);
    var it = scope.iterator();
    while (it.next()) |item| {
        try newScope.put(item.key_ptr.*, item.value_ptr.*);
    }
    return newScope;
}

fn cloneGenericScope(allocator: Allocator, scope: *const utils.TypeScope) !*utils.TypeScope {
    const newScope = try utils.initMutPtrT(utils.TypeScope, allocator);
    var it = scope.iterator();
    while (it.next()) |item| {
        try newScope.put(item.key_ptr.*, item.value_ptr.*);
    }
    return newScope;
}

fn cloneParameters(allocator: Allocator, compInfo: *CompInfo, params: []blitzAst.Parameter, replaceGenerics: bool) ![]blitzAst.Parameter {
    const parameters = try allocator.alloc(blitzAst.Parameter, params.len);

    for (params, 0..) |param, index| {
        const typePtr = try cloneAstTypeInfo(allocator, compInfo, param.type, replaceGenerics);
        const newParam: blitzAst.Parameter = .{
            .name = try string.cloneString(allocator, param.name),
            .type = typePtr,
        };

        parameters[index] = newParam;
    }

    return parameters;
}

pub fn cloneAstNode(allocator: Allocator, compInfo: *CompInfo, node: blitzAst.AstNode, replaceGenerics: bool) !blitzAst.AstNode {
    switch (node) {
        .NoOp, .StructPlaceholder => return node,
        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtr(allocator, compInfo, index.index, replaceGenerics),
                .value = try cloneAstNodePtr(allocator, compInfo, index.value, replaceGenerics),
            },
        },
        .OpExpr => |op| {
            const sides = .{
                .left = try cloneAstNodePtrMut(allocator, compInfo, op.left, replaceGenerics),
                .right = try cloneAstNodePtrMut(allocator, compInfo, op.right, replaceGenerics),
            };

            const opType = op.type;

            return .{
                .OpExpr = .{
                    .type = opType,
                    .left = sides.left,
                    .right = sides.right,
                },
            };
        },
        .IncOne => |val| {
            return .{
                .IncOne = try cloneAstNodePtr(allocator, compInfo, val, replaceGenerics),
            };
        },
        .DecOne => |val| {
            return .{
                .DecOne = try cloneAstNodePtr(allocator, compInfo, val, replaceGenerics),
            };
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = try string.cloneString(allocator, ref),
            };
        },
        .Seq => |seq| {
            var newSeq = try allocator.alloc(*const blitzAst.AstNode, seq.nodes.len);

            for (seq.nodes, 0..) |seqNode, index| {
                const nodePtr = try cloneAstNodePtr(allocator, compInfo, seqNode, replaceGenerics);
                newSeq[index] = nodePtr;
            }

            return .{
                .Seq = .{
                    .nodes = newSeq,
                },
            };
        },
        .Value => |val| {
            switch (val) {
                .Bool, .Char, .Number, .Null => return node,
                .String => |str| return .{
                    .Value = .{
                        .String = try string.cloneString(allocator, str),
                    },
                },
                .RawNumber => |num| return .{
                    .Value = .{
                        .RawNumber = try string.cloneString(allocator, num),
                    },
                },
                .GeneralArray => |arr| return .{
                    .Value = .{
                        .GeneralArray = try cloneNodeArr(allocator, compInfo, arr, replaceGenerics),
                    },
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtr(allocator, compInfo, dec.setNode, replaceGenerics);
            var clonedType: ?blitzAst.AstTypeInfo = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypeInfo(allocator, compInfo, annotation, replaceGenerics);
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
        .ValueSet => |set| return .{
            .ValueSet = .{
                .value = try cloneAstNodePtr(allocator, compInfo, set.value, replaceGenerics),
                .setNode = try cloneAstNodePtrMut(allocator, compInfo, set.setNode, replaceGenerics),
            },
        },
        .VarEqOp => |op| return .{
            .VarEqOp = .{
                .variable = try string.cloneString(allocator, op.variable),
                .value = try cloneAstNodePtrMut(allocator, compInfo, op.value, replaceGenerics),
                .opType = op.opType,
            },
        },
        .Type => |t| return .{
            .Type = try cloneAstTypes(allocator, compInfo, t, replaceGenerics),
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtr(allocator, compInfo, cast.node, replaceGenerics);
            const typePtr = try cloneAstTypeInfo(allocator, compInfo, cast.toType, replaceGenerics);

            return .{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| return .{
            .Variable = try string.cloneString(allocator, v),
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenerics(allocator, compInfo, dec.generics, replaceGenerics);
            var deriveType: ?blitzAst.AstTypeInfo = null;
            const attributes = try cloneStructAttrDec(allocator, compInfo, dec.attributes, replaceGenerics);
            const totalMemberList = try cloneStructAttrDec(allocator, compInfo, dec.attributes, replaceGenerics);

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypeInfo(
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

            var newFallback: ?*const blitzAst.IfFallback = null;
            if (statement.fallback) |fallback| {
                newFallback = try cloneIfFallback(allocator, compInfo, fallback, replaceGenerics);
            }

            return .{
                .IfStatement = .{
                    .body = bodyPtr,
                    .condition = conditionPtr,
                    .fallback = newFallback,
                },
            };
        },
        .ForLoop => |loop| {
            var newInitNode: ?*const blitzAst.AstNode = null;

            if (loop.initNode) |init| {
                newInitNode = try cloneAstNodePtr(allocator, compInfo, init, replaceGenerics);
            }

            return .{
                .ForLoop = .{
                    .initNode = newInitNode,
                    .condition = try cloneAstNodePtr(allocator, compInfo, loop.condition, replaceGenerics),
                    .incNode = try cloneAstNodePtr(allocator, compInfo, loop.incNode, replaceGenerics),
                    .body = try cloneAstNodePtr(allocator, compInfo, loop.body, replaceGenerics),
                },
            };
        },
        .WhileLoop => |loop| {
            return .{
                .WhileLoop = .{
                    .condition = try cloneAstNodePtr(allocator, compInfo, loop.condition, replaceGenerics),
                    .body = try cloneAstNodePtr(allocator, compInfo, loop.body, replaceGenerics),
                },
            };
        },
        .FuncDec => |dec| return .{
            .FuncDec = try string.cloneString(allocator, dec),
        },
        .FuncCall => |call| {
            const clonedFunc = try cloneAstNodePtr(
                allocator,
                compInfo,
                call.func,
                replaceGenerics,
            );
            const newParams = try cloneNodeArrMut(allocator, compInfo, call.params, replaceGenerics);

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| return .{
            .ReturnNode = try cloneAstNodePtr(allocator, compInfo, ret, replaceGenerics),
        },
        .StructInit => |init| {
            const generics = try allocator.dupe(blitzAst.AstTypeInfo, init.generics);
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
        .Bang => |bangNode| return .{
            .Bang = try cloneAstNodePtr(allocator, compInfo, bangNode, replaceGenerics),
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtrMut(allocator, compInfo, access.value, replaceGenerics);
            const prop = try string.cloneString(allocator, access.property);

            return .{
                .PropertyAccess = .{
                    .value = value,
                    .property = prop,
                },
            };
        },
        .StaticStructInstance => |inst| return .{
            .StaticStructInstance = try string.cloneString(allocator, inst),
        },
        .ErrorDec => |def| {
            var newVariants: ?[][]u8 = null;

            if (def.variants) |variants| {
                newVariants = try string.cloneStringArray(allocator, variants);
            }

            return .{
                .ErrorDec = try create(blitzAst.ErrorDecNode, allocator, .{
                    .name = try string.cloneString(allocator, def.name),
                    .variants = newVariants,
                }),
            };
        },
        .Error => |err| return .{
            .Error = try string.cloneString(allocator, err),
        },
        .Group => |group| return .{
            .Group = try cloneAstNodePtrMut(allocator, compInfo, group, replaceGenerics),
        },
        .Scope => |scope| return .{
            .Scope = try cloneAstNodePtr(allocator, compInfo, scope, replaceGenerics),
        },
    }
}

fn cloneIfFallback(allocator: Allocator, compInfo: *CompInfo, fallback: *const blitzAst.IfFallback, replaceGenerics: bool) !*const blitzAst.IfFallback {
    var newCondition: ?*const blitzAst.AstNode = null;
    if (fallback.condition) |condition| {
        newCondition = try cloneAstNodePtr(allocator, compInfo, condition, replaceGenerics);
    }

    var newFallback: ?*const blitzAst.IfFallback = null;
    if (fallback.fallback) |innerFallback| {
        newFallback = try cloneIfFallback(allocator, compInfo, innerFallback, replaceGenerics);
    }

    return try create(blitzAst.IfFallback, allocator, .{
        .condition = newCondition,
        .body = try cloneAstNodePtr(allocator, compInfo, fallback.body, replaceGenerics),
        .fallback = newFallback,
    });
}

fn cloneStructAttrDec(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.StructAttribute, replaceGenerics: bool) ![]blitzAst.StructAttribute {
    var attributes = try allocator.alloc(blitzAst.StructAttribute, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.StructAttribute = .{
            .static = attr.static,
            .attr = try cloneStructAttributeUnion(allocator, compInfo, attr.attr, replaceGenerics),
            .name = try string.cloneString(allocator, attr.name),
            .visibility = attr.visibility,
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

fn cloneAttrDef(allocator: Allocator, compInfo: *CompInfo, attrs: []blitzAst.AttributeDefinition, replaceGenerics: bool) ![]blitzAst.AttributeDefinition {
    var attributes = try allocator.alloc(blitzAst.AttributeDefinition, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.AttributeDefinition = .{
            .name = try string.cloneString(allocator, attr.name),
            .value = try cloneAstNodePtr(allocator, compInfo, attr.value, replaceGenerics),
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn cloneStructAttributeUnion(allocator: Allocator, compInfo: *CompInfo, structAttrUnion: blitzAst.StructAttributeUnion, replaceGenerics: bool) !blitzAst.StructAttributeUnion {
    return switch (structAttrUnion) {
        .Function => |func| .{
            .Function = try cloneFuncDec(allocator, compInfo, func, replaceGenerics),
        },
        .Member => |member| .{
            .Member = try cloneAstTypeInfo(allocator, compInfo, member, replaceGenerics),
        },
    };
}

pub fn cloneStructAttributeUnionType(allocator: Allocator, compInfo: *CompInfo, structAttrUnion: blitzAst.StructAttributeUnion, replaceGenerics: bool) !blitzAst.AstTypeInfo {
    return switch (structAttrUnion) {
        .Function => |func| try utils.astTypesToInfo(allocator, .{ .Function = func }, true),
        .Member => |member| try cloneAstTypeInfo(allocator, compInfo, member, replaceGenerics),
    };
}

fn cloneNodeArr(allocator: Allocator, compInfo: *CompInfo, nodes: []*const blitzAst.AstNode, replaceGenerics: bool) ![]*const blitzAst.AstNode {
    var newNodes = try allocator.alloc(*const blitzAst.AstNode, nodes.len);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtr(allocator, compInfo, node, replaceGenerics);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

fn cloneNodeArrMut(allocator: Allocator, compInfo: *CompInfo, nodes: []*blitzAst.AstNode, replaceGenerics: bool) ![]*blitzAst.AstNode {
    var newNodes = try allocator.alloc(*blitzAst.AstNode, nodes.len);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtrMut(allocator, compInfo, node, replaceGenerics);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

pub fn cloneFuncDec(allocator: Allocator, compInfo: *CompInfo, dec: *blitzAst.FuncDecNode, replaceGenerics: bool) !*blitzAst.FuncDecNode {
    const bodyPtr = try cloneAstNodePtr(allocator, compInfo, dec.body, replaceGenerics);
    const name = try string.cloneString(allocator, dec.name);
    var generics: ?[]blitzAst.GenericType = null;
    const returnType = try cloneAstTypeInfo(allocator, compInfo, dec.returnType, replaceGenerics);
    const params = try cloneParameters(allocator, compInfo, dec.params, replaceGenerics);

    if (dec.generics) |decGenerics| {
        generics = try cloneGenerics(allocator, compInfo, decGenerics, replaceGenerics);
    }

    var capturedValues: ?*utils.CaptureScope = null;
    if (dec.capturedValues) |values| {
        capturedValues = try cloneCaptureScope(allocator, values);
    }

    var capturedTypes: ?*utils.TypeScope = null;
    if (dec.capturedTypes) |captured| {
        capturedTypes = try cloneGenericScope(allocator, captured);
    }

    return try createMut(blitzAst.FuncDecNode, allocator, .{
        .body = bodyPtr,
        .bodyTokens = dec.bodyTokens,
        .name = name,
        .params = params,
        .generics = generics,
        .returnType = returnType,
        .capturedValues = capturedValues,
        .capturedTypes = capturedTypes,
        .builtin = dec.builtin,
    });
}

fn cloneAstNodePtr(allocator: Allocator, compInfo: *CompInfo, node: *const blitzAst.AstNode, replaceGenerics: bool) (Allocator.Error || CloneError)!*const blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, compInfo, node.*, replaceGenerics);
    return try create(blitzAst.AstNode, allocator, clonedNode);
}

fn cloneAstNodePtrMut(allocator: Allocator, compInfo: *CompInfo, node: *const blitzAst.AstNode, replaceGenerics: bool) (Allocator.Error || CloneError)!*blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, compInfo, node.*, replaceGenerics);
    return try createMut(blitzAst.AstNode, allocator, clonedNode);
}

pub fn cloneAstTypesPtr(allocator: Allocator, compInfo: *CompInfo, types: *const blitzAst.AstTypes, replaceGenerics: bool) !*const blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, compInfo, types.*, replaceGenerics);
    return try create(blitzAst.AstTypes, allocator, clonedType);
}

fn cloneGeneric(allocator: Allocator, compInfo: *CompInfo, generic: blitzAst.GenericType, replaceGenerics: bool) !blitzAst.GenericType {
    var restriction: ?blitzAst.AstTypeInfo = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypeInfo(allocator, compInfo, rest, replaceGenerics);
    }

    return .{
        .name = try string.cloneString(allocator, generic.name),
        .restriction = restriction,
    };
}

pub fn cloneGenerics(allocator: Allocator, compInfo: *CompInfo, generics: []blitzAst.GenericType, replaceGenerics: bool) ![]blitzAst.GenericType {
    var clonedGenerics = try allocator.alloc(blitzAst.GenericType, generics.len);

    for (generics, 0..) |generic, index| {
        const newGeneric = try cloneGeneric(allocator, compInfo, generic, replaceGenerics);
        clonedGenerics[index] = newGeneric;
    }

    return clonedGenerics;
}
