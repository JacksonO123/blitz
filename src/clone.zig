const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const blitzCompInfo = blitz.compInfo;
const debug = blitz.debug;
const Allocator = std.mem.Allocator;
const create = utils.create;
const createMut = utils.createMut;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;

pub const CloneError = error{
    GenericNotFound,
    BadGenericClone,
};

pub fn cloneAstTypes(
    allocator: Allocator,
    context: *Context,
    types: blitzAst.AstTypes,
    replaceGenerics: bool,
) (Allocator.Error || CloneError)!blitzAst.AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number, .Null, .RawNumber, .Any => return types,

        .VarInfo => |info| {
            return .{
                .VarInfo = try cloneAstTypeInfo(allocator, context, info, replaceGenerics),
            };
        },
        .ArraySlice => |arr| {
            const typeClone = try cloneAstTypeInfo(allocator, context, arr.type, replaceGenerics);
            var sizeClone: ?*blitzAst.AstNode = null;
            if (arr.size) |size| {
                sizeClone = try cloneAstNodePtrMut(allocator, context, size, replaceGenerics);
            }

            return .{
                .ArraySlice = .{
                    .type = typeClone,
                    .size = sizeClone,
                },
            };
        },
        .StaticStructInstance => |name| {
            return .{
                .StaticStructInstance = name,
            };
        },
        .Pointer => |ptr| {
            return .{
                .Pointer = try cloneAstTypeInfo(allocator, context, ptr, replaceGenerics),
            };
        },
        .Nullable => |t| {
            return .{
                .Nullable = try cloneAstTypeInfo(allocator, context, t, replaceGenerics),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneCustomGenerics(
                allocator,
                context,
                custom.generics,
                replaceGenerics,
            );

            return .{
                .Custom = .{
                    .name = custom.name,
                    .generics = genericsSlice,
                    .allowPrivateReads = custom.allowPrivateReads,
                },
            };
        },
        .Function => |func| {
            return .{
                .Function = try cloneFuncDec(allocator, context, func, replaceGenerics),
            };
        },
        .Error => |err| {
            var payload: ?blitzAst.AstTypeInfo = null;

            if (err.payload) |errPayload| {
                payload = try cloneAstTypeInfo(allocator, context, errPayload, replaceGenerics);
            }

            return .{
                .Error = .{
                    .name = err.name,
                    .payload = payload,
                },
            };
        },
        .ErrorVariant => |err| {
            return .{
                .ErrorVariant = .{
                    .from = if (err.from) |from| from else null,
                    .variant = err.variant,
                },
            };
        },
        .Generic => {
            return CloneError.BadGenericClone;
        },
    }
}

fn cloneAstTypeInfos(
    allocator: Allocator,
    context: *Context,
    infos: []blitzAst.AstTypeInfo,
    replaceGenerics: bool,
) ![]blitzAst.AstTypeInfo {
    const newSlice = try allocator.alloc(blitzAst.AstTypeInfo, infos.len);
    for (infos, 0..) |info, index| {
        const clonedInfo = try cloneAstTypeInfo(allocator, context, info, replaceGenerics);
        newSlice[index] = clonedInfo;
    }
    return newSlice;
}

pub fn cloneCustomGenerics(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.AstTypeInfo,
    replaceGenerics: bool,
) ![]blitzAst.AstTypeInfo {
    const genericsSlice = try allocator.alloc(blitzAst.AstTypeInfo, generics.len);

    for (generics, 0..) |gen, index| {
        genericsSlice[index] = try cloneAstTypeInfo(allocator, context, gen, replaceGenerics);
    }

    return genericsSlice;
}

pub fn cloneAstTypeInfo(
    allocator: Allocator,
    context: *Context,
    info: blitzAst.AstTypeInfo,
    replaceGenerics: bool,
) (CloneError || Allocator.Error)!blitzAst.AstTypeInfo {
    if (info.astType.* == .Generic) {
        const generic = info.astType.*.Generic;
        if (replaceGenerics) {
            const genType = try context.compInfo.getGeneric(generic);
            if (genType) |gType| {
                return cloneAstTypeInfo(allocator, context, gType, replaceGenerics);
            }

            return CloneError.GenericNotFound;
        }

        return .{
            .astType = try createMut(blitzAst.AstTypes, allocator, .{
                .Generic = generic,
            }),
            .isConst = info.isConst,
        };
    }

    return .{
        .astType = try cloneAstTypesPtrMut(allocator, context, info.astType, replaceGenerics),
        .isConst = info.isConst,
    };
}

fn cloneCaptureScope(
    allocator: Allocator,
    scope: *const blitzCompInfo.CaptureScope,
) !*blitzCompInfo.CaptureScope {
    const newScope = try utils.initMutPtrT(blitzCompInfo.CaptureScope, allocator);
    var it = scope.iterator();
    while (it.next()) |item| {
        try newScope.put(item.key_ptr.*, item.value_ptr.*);
    }
    return newScope;
}

fn cloneGenericScope(
    allocator: Allocator,
    scope: *const blitzCompInfo.TypeScope,
) !*blitzCompInfo.TypeScope {
    const newScope = try utils.initMutPtrT(blitzCompInfo.TypeScope, allocator);
    var it = scope.iterator();
    while (it.next()) |item| {
        try newScope.put(item.key_ptr.*, item.value_ptr.*);
    }
    return newScope;
}

fn cloneParameters(
    allocator: Allocator,
    context: *Context,
    params: []blitzAst.Parameter,
    replaceGenerics: bool,
) ![]blitzAst.Parameter {
    const parameters = try allocator.alloc(blitzAst.Parameter, params.len);

    for (params, 0..) |param, index| {
        const typePtr = try cloneAstTypeInfo(allocator, context, param.type, replaceGenerics);
        const newParam: blitzAst.Parameter = .{
            .name = param.name,
            .type = typePtr,
            .isConst = param.isConst,
        };

        parameters[index] = newParam;
    }

    return parameters;
}

pub fn cloneAstNode(
    allocator: Allocator,
    context: *Context,
    node: blitzAst.AstNode,
    replaceGenerics: bool,
) !blitzAst.AstNode {
    switch (node) {
        .NoOp, .StructPlaceholder, .Break, .Continue => return node,
        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtrMut(allocator, context, index.index, replaceGenerics),
                .value = try cloneAstNodePtrMut(allocator, context, index.value, replaceGenerics),
            },
        },
        .OpExpr => |op| {
            const opType = op.type;

            return .{
                .OpExpr = .{
                    .type = opType,
                    .left = try cloneAstNodePtrMut(allocator, context, op.left, replaceGenerics),
                    .right = try cloneAstNodePtrMut(allocator, context, op.right, replaceGenerics),
                    .depth = op.depth,
                },
            };
        },
        .IncOne => |val| {
            return .{
                .IncOne = try cloneAstNodePtrMut(allocator, context, val, replaceGenerics),
            };
        },
        .DecOne => |val| {
            return .{
                .DecOne = try cloneAstNodePtrMut(allocator, context, val, replaceGenerics),
            };
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = ref,
            };
        },
        .Seq => |seq| {
            var newSeq = try allocator.alloc(*blitzAst.AstNode, seq.nodes.len);

            for (seq.nodes, 0..) |seqNode, index| {
                const nodePtr = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    seqNode,
                    replaceGenerics,
                );
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
                        .String = str,
                    },
                },
                .RawNumber => |num| return .{
                    .Value = .{
                        .RawNumber = .{
                            .digits = num.digits,
                            .numType = num.numType,
                        },
                    },
                },
                .ArraySlice => |arr| return .{
                    .Value = .{
                        .ArraySlice = try cloneNodeArrMut(
                            allocator,
                            context,
                            arr,
                            replaceGenerics,
                        ),
                    },
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtrMut(
                allocator,
                context,
                dec.setNode,
                replaceGenerics,
            );
            var clonedType: ?blitzAst.AstTypeInfo = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypeInfo(allocator, context, annotation, replaceGenerics);
            }

            return .{
                .VarDec = .{
                    .name = dec.name,
                    .isConst = dec.isConst,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                    .setType = dec.setType,
                },
            };
        },
        .ValueSet => |set| return .{
            .ValueSet = .{
                .value = try cloneAstNodePtrMut(allocator, context, set.value, replaceGenerics),
                .setNode = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    set.setNode,
                    replaceGenerics,
                ),
            },
        },
        .VarEqOp => |op| return .{
            .VarEqOp = .{
                .variable = op.variable,
                .value = try cloneAstNodePtrMut(allocator, context, op.value, replaceGenerics),
                .opType = op.opType,
            },
        },
        .Type => |t| return .{
            .Type = try cloneAstTypes(allocator, context, t, replaceGenerics),
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtrMut(allocator, context, cast.node, replaceGenerics);
            const typePtr = try cloneAstTypeInfo(allocator, context, cast.toType, replaceGenerics);

            return .{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| return .{
            .Variable = v,
        },
        .Pointer => |ptr| return .{
            .Pointer = .{
                .node = try cloneAstNodePtrMut(allocator, context, ptr.node, replaceGenerics),
                .isConst = ptr.isConst,
            },
        },
        .Dereference => |deref| return .{
            .Dereference = try cloneAstNodePtrMut(allocator, context, deref, replaceGenerics),
        },
        .HeapAlloc => |alloc| {
            const allocTypeClone = if (alloc.allocType) |allocType|
                try cloneAstTypeInfo(allocator, context, allocType, replaceGenerics)
            else
                null;

            return .{
                .HeapAlloc = .{
                    .node = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        alloc.node,
                        replaceGenerics,
                    ),
                    .allocType = allocTypeClone,
                },
            };
        },
        .HeapFree => |toFree| return .{
            .HeapFree = try cloneAstNodePtrMut(
                allocator,
                context,
                toFree,
                replaceGenerics,
            ),
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenerics(
                allocator,
                context,
                dec.generics,
                replaceGenerics,
            );
            var deriveType: ?blitzAst.AstTypeInfo = null;
            const attributes = try cloneStructAttrDec(
                allocator,
                context,
                dec.attributes,
                replaceGenerics,
            );
            const totalMemberList = try cloneStructAttrDec(
                allocator,
                context,
                dec.attributes,
                replaceGenerics,
            );

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    dType,
                    replaceGenerics,
                );
            }

            const list = try dec.toScanTypes.clone(allocator);
            for (list.items) |*item| {
                item.* = try cloneGenToInfoRels(allocator, context, item.*, replaceGenerics);
            }
            const listPtr = try createMut(blitzAst.ToScanTypesList, allocator, list);

            const structDec: blitzAst.StructDecNode = .{
                .name = dec.name,
                .generics = clonedGenerics,
                .deriveType = deriveType,
                .attributes = attributes,
                .totalMemberList = totalMemberList,
                .toScanTypes = listPtr,
            };
            const structNode = try createMut(blitzAst.StructDecNode, allocator, structDec);

            return .{
                .StructDec = structNode,
            };
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.body,
                replaceGenerics,
            );
            const conditionPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.condition,
                replaceGenerics,
            );

            var newFallback: ?*const blitzAst.IfFallback = null;
            if (statement.fallback) |fallback| {
                newFallback = try cloneIfFallback(allocator, context, fallback, replaceGenerics);
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
            var newInitNode: ?*blitzAst.AstNode = null;

            if (loop.initNode) |init| {
                newInitNode = try cloneAstNodePtrMut(allocator, context, init, replaceGenerics);
            }

            return .{
                .ForLoop = .{
                    .initNode = newInitNode,
                    .condition = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.condition,
                        replaceGenerics,
                    ),
                    .incNode = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.incNode,
                        replaceGenerics,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, replaceGenerics),
                },
            };
        },
        .WhileLoop => |loop| {
            return .{
                .WhileLoop = .{
                    .condition = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.condition,
                        replaceGenerics,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, replaceGenerics),
                },
            };
        },
        .FuncDec => |dec| return .{
            .FuncDec = dec,
        },
        .FuncCall => |call| {
            const clonedFunc = try cloneAstNodePtrMut(
                allocator,
                context,
                call.func,
                replaceGenerics,
            );
            const newParams = try cloneNodeArrMut(
                allocator,
                context,
                call.params,
                replaceGenerics,
            );

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| return .{
            .ReturnNode = try cloneAstNodePtrMut(allocator, context, ret, replaceGenerics),
        },
        .StructInit => |init| {
            const generics = try allocator.dupe(blitzAst.AstTypeInfo, init.generics);
            const name = init.name;
            const attributes = try cloneAttrDef(
                allocator,
                context,
                init.attributes,
                replaceGenerics,
            );

            return .{
                .StructInit = .{
                    .attributes = attributes,
                    .name = name,
                    .generics = generics,
                },
            };
        },
        .Bang => |bangNode| return .{
            .Bang = try cloneAstNodePtrMut(allocator, context, bangNode, replaceGenerics),
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtrMut(
                allocator,
                context,
                access.value,
                replaceGenerics,
            );
            const prop = access.property;

            return .{
                .PropertyAccess = .{
                    .value = value,
                    .property = prop,
                },
            };
        },
        .StaticStructInstance => |inst| return .{
            .StaticStructInstance = inst,
        },
        .ErrorDec => |def| {
            const newVariants = try allocator.alloc([]const u8, def.variants.len);
            @memcpy(newVariants, def.variants);

            return .{
                .ErrorDec = try create(blitzAst.ErrorDecNode, allocator, .{
                    .name = def.name,
                    .variants = newVariants,
                }),
            };
        },
        .Error => |err| return .{
            .Error = err,
        },
        .InferErrorVariant => |err| return .{
            .InferErrorVariant = err,
        },
        .Group => |group| return .{
            .Group = try cloneAstNodePtrMut(allocator, context, group, replaceGenerics),
        },
        .Scope => |scope| return .{
            .Scope = try cloneAstNodePtrMut(allocator, context, scope, replaceGenerics),
        },
        .ArrayInit => |init| return .{
            .ArrayInit = .{
                .size = init.size,
                .initType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    init.initType,
                    replaceGenerics,
                ),
                .initNode = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    init.initNode,
                    replaceGenerics,
                ),
                .indexIdent = init.indexIdent,
                .ptrIdent = init.ptrIdent,
            },
        },
    }
}

fn cloneIfFallback(
    allocator: Allocator,
    context: *Context,
    fallback: *const blitzAst.IfFallback,
    replaceGenerics: bool,
) !*const blitzAst.IfFallback {
    var newCondition: ?*blitzAst.AstNode = null;
    if (fallback.condition) |condition| {
        newCondition = try cloneAstNodePtrMut(allocator, context, condition, replaceGenerics);
    }

    var newFallback: ?*const blitzAst.IfFallback = null;
    if (fallback.fallback) |innerFallback| {
        newFallback = try cloneIfFallback(allocator, context, innerFallback, replaceGenerics);
    }

    return create(blitzAst.IfFallback, allocator, .{
        .condition = newCondition,
        .body = try cloneAstNodePtrMut(allocator, context, fallback.body, replaceGenerics),
        .fallback = newFallback,
    });
}

fn cloneStructAttrDec(
    allocator: Allocator,
    context: *Context,
    attrs: []blitzAst.StructAttribute,
    replaceGenerics: bool,
) ![]blitzAst.StructAttribute {
    var attributes = try allocator.alloc(blitzAst.StructAttribute, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.StructAttribute = .{
            .static = attr.static,
            .attr = try cloneStructAttributeUnion(allocator, context, attr.attr, replaceGenerics),
            .name = attr.name,
            .visibility = attr.visibility,
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

fn cloneAttrDef(
    allocator: Allocator,
    context: *Context,
    attrs: []blitzAst.AttributeDefinition,
    replaceGenerics: bool,
) ![]blitzAst.AttributeDefinition {
    var attributes = try allocator.alloc(blitzAst.AttributeDefinition, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.AttributeDefinition = .{
            .name = attr.name,
            .value = try cloneAstNodePtrMut(allocator, context, attr.value, replaceGenerics),
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn cloneStructAttributeUnion(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: blitzAst.StructAttributeUnion,
    replaceGenerics: bool,
) !blitzAst.StructAttributeUnion {
    return switch (structAttrUnion) {
        .Function => |func| .{
            .Function = try cloneFuncDec(allocator, context, func, replaceGenerics),
        },
        .Member => |member| .{
            .Member = try cloneAstTypeInfo(allocator, context, member, replaceGenerics),
        },
    };
}

pub fn cloneStructAttributeUnionType(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: blitzAst.StructAttributeUnion,
    replaceGenerics: bool,
) !blitzAst.AstTypeInfo {
    return switch (structAttrUnion) {
        .Function => |func| try utils.astTypesToInfo(allocator, .{ .Function = func }, true),
        .Member => |member| try cloneAstTypeInfo(allocator, context, member, replaceGenerics),
    };
}

fn cloneNodeArr(
    allocator: Allocator,
    context: *Context,
    nodes: []*const blitzAst.AstNode,
    replaceGenerics: bool,
) ![]*const blitzAst.AstNode {
    return cloneNodeArrMut(allocator, context, nodes, replaceGenerics);
}

fn cloneNodeArrMut(
    allocator: Allocator,
    context: *Context,
    nodes: []*blitzAst.AstNode,
    replaceGenerics: bool,
) ![]*blitzAst.AstNode {
    var newNodes = try allocator.alloc(*blitzAst.AstNode, nodes.len);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtrMut(allocator, context, node, replaceGenerics);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

pub fn cloneFuncDec(
    allocator: Allocator,
    context: *Context,
    dec: *blitzAst.FuncDecNode,
    replaceGenerics: bool,
) !*blitzAst.FuncDecNode {
    const bodyPtr = try cloneAstNodePtrMut(allocator, context, dec.body, replaceGenerics);
    var generics: ?[]blitzAst.GenericType = null;
    const returnType = try cloneAstTypeInfo(allocator, context, dec.returnType, replaceGenerics);
    const params = try cloneParameters(allocator, context, dec.params, replaceGenerics);

    if (dec.generics) |decGenerics| {
        generics = try cloneGenerics(allocator, context, decGenerics, replaceGenerics);
    }

    var capturedValues: ?*blitzCompInfo.CaptureScope = null;
    if (dec.capturedValues) |values| {
        capturedValues = try cloneCaptureScope(allocator, values);
    }

    var capturedTypes: ?*blitzCompInfo.TypeScope = null;
    if (dec.capturedTypes) |captured| {
        capturedTypes = try cloneGenericScope(allocator, captured);
    }

    var capturedFuncs: ?*blitzCompInfo.StringListScope = null;
    if (dec.capturedFuncs) |captured| {
        const capturedFuncNames = captured.items;
        const tempList = try ArrayList([]const u8).initCapacity(allocator, captured.items.len);
        capturedFuncs = try createMut(ArrayList([]const u8), allocator, tempList);
        capturedFuncs.?.appendSliceAssumeCapacity(capturedFuncNames);
    }

    const list = try dec.toScanTypes.clone(allocator);
    for (list.items) |*item| {
        item.* = try cloneGenToInfoRels(allocator, context, item.*, replaceGenerics);
    }
    const listPtr = try createMut(blitzAst.ToScanTypesList, allocator, list);

    return createMut(blitzAst.FuncDecNode, allocator, .{
        .body = bodyPtr,
        .bodyTokens = dec.bodyTokens,
        .name = dec.name,
        .params = params,
        .generics = generics,
        .returnType = returnType,
        .capturedValues = capturedValues,
        .capturedTypes = capturedTypes,
        .capturedFuncs = capturedFuncs,
        .toScanTypes = listPtr,
        .funcType = dec.funcType,
        .visited = false,
        .globallyDefined = dec.globallyDefined,
    });
}

pub fn cloneGenToInfoRels(
    allocator: Allocator,
    context: *Context,
    rels: []blitzAst.StrToTypeInfoRel,
    replaceGenerics: bool,
) ![]blitzAst.StrToTypeInfoRel {
    const newSlice = try allocator.alloc(blitzAst.StrToTypeInfoRel, rels.len);
    for (rels, 0..) |item, index| {
        newSlice[index] = .{
            .str = item.str,
            .info = try cloneAstTypeInfo(allocator, context, item.info, replaceGenerics),
        };
    }
    return newSlice;
}

fn cloneAstNodePtr(
    allocator: Allocator,
    context: *Context,
    node: *const blitzAst.AstNode,
    replaceGenerics: bool,
) (Allocator.Error || CloneError)!*const blitzAst.AstNode {
    return cloneAstNodePtrMut(allocator, context, node, replaceGenerics);
}

fn cloneAstNodePtrMut(
    allocator: Allocator,
    context: *Context,
    node: *const blitzAst.AstNode,
    replaceGenerics: bool,
) (Allocator.Error || CloneError)!*blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, context, node.*, replaceGenerics);
    return createMut(blitzAst.AstNode, allocator, clonedNode);
}

pub fn cloneAstTypesPtr(
    allocator: Allocator,
    context: *Context,
    types: *const blitzAst.AstTypes,
    replaceGenerics: bool,
) !*const blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, context, types.*, replaceGenerics);
    return create(blitzAst.AstTypes, allocator, clonedType);
}

pub fn cloneAstTypesPtrMut(
    allocator: Allocator,
    context: *Context,
    types: *blitzAst.AstTypes,
    replaceGenerics: bool,
) !*blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, context, types.*, replaceGenerics);
    return createMut(blitzAst.AstTypes, allocator, clonedType);
}

fn cloneGeneric(
    allocator: Allocator,
    context: *Context,
    generic: blitzAst.GenericType,
    replaceGenerics: bool,
) !blitzAst.GenericType {
    var restriction: ?blitzAst.AstTypeInfo = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypeInfo(allocator, context, rest, replaceGenerics);
    }

    return .{
        .name = generic.name,
        .restriction = restriction,
    };
}

pub fn cloneGenerics(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.GenericType,
    replaceGenerics: bool,
) ![]blitzAst.GenericType {
    var clonedGenerics = try allocator.alloc(blitzAst.GenericType, generics.len);

    for (generics, 0..) |generic, index| {
        const newGeneric = try cloneGeneric(allocator, context, generic, replaceGenerics);
        clonedGenerics[index] = newGeneric;
    }

    return clonedGenerics;
}

pub fn cloneGenRels(
    allocator: Allocator,
    context: *Context,
    rels: []blitzAst.StrToTypeInfoRel,
    replaceGenerics: bool,
) ![]blitzAst.StrToTypeInfoRel {
    const res = try allocator.alloc(blitzAst.StrToTypeInfoRel, rels.len);

    for (rels, 0..) |rel, index| {
        res[index] = .{
            .gen = rel.gen,
            .info = try cloneAstTypeInfo(allocator, context, rel.info, replaceGenerics),
        };
    }

    return res;
}

//
// clone with context
//

pub fn cloneAstTypeInfoPool(
    allocator: Allocator,
    context: *Context,
    info: blitzAst.AstTypeInfo,
    withGenDef: bool,
) !blitzAst.AstTypeInfo {
    if (info.astType.* == .Generic) {
        const generic = info.astType.*.Generic;
        if (withGenDef) {
            const genType = try context.compInfo.getGeneric(generic);
            if (genType) |gType| {
                return cloneAstTypeInfoPool(allocator, context, gType, withGenDef);
            }

            return CloneError.GenericNotFound;
        }

        return .{
            .astType = try context.pools.types.new(.{
                .Generic = generic,
            }),
            .isConst = info.isConst,
        };
    }

    return .{
        .astType = try cloneAstTypesPtrMutPool(allocator, context, info.astType, withGenDef),
        .isConst = info.isConst,
    };
}

fn cloneAstTypesPtrMutPool(
    allocator: Allocator,
    context: *Context,
    astType: *blitzAst.AstTypes,
    withGenDef: bool,
) !*blitzAst.AstTypes {
    const clonedType = try cloneAstTypesPool(allocator, context, astType.*, withGenDef);
    return try context.pools.types.new(clonedType);
}

pub fn cloneAstTypesPool(
    allocator: Allocator,
    context: *Context,
    types: blitzAst.AstTypes,
    withGenDef: bool,
) (Allocator.Error || CloneError)!blitzAst.AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number, .Null, .RawNumber, .Any => return types,

        .VarInfo => |info| {
            return .{
                .VarInfo = try cloneAstTypeInfoPool(context, info, withGenDef),
            };
        },
        .ArraySlice => |arr| {
            const typeClone = try cloneAstTypeInfoPool(context, arr.type, withGenDef);
            var sizeClone: ?*blitzAst.AstNode = null;
            if (arr.size) |size| {
                sizeClone = try cloneAstNodePtrMutPool(context, size, withGenDef);
            }

            return .{
                .ArraySlice = .{
                    .type = typeClone,
                    .size = sizeClone,
                },
            };
        },
        .StaticStructInstance => |name| {
            return .{
                .StaticStructInstance = name,
            };
        },
        .Pointer => |ptr| {
            return .{
                .Pointer = try cloneAstTypeInfoPool(allocator, context, ptr, withGenDef),
            };
        },
        .Nullable => |t| {
            return .{
                .Nullable = try cloneAstTypeInfoPool(allocator, context, t, withGenDef),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneCustomGenericsPool(
                allocator,
                context,
                custom.generics,
                withGenDef,
            );

            return .{
                .Custom = .{
                    .name = custom.name,
                    .generics = genericsSlice,
                    .allowPrivateReads = custom.allowPrivateReads,
                },
            };
        },
        .Error => |err| {
            var payload: ?blitzAst.AstTypeInfo = null;

            if (err.payload) |errPayload| {
                payload = try cloneAstTypeInfoPool(allocator, context, errPayload, withGenDef);
            }

            return .{
                .Error = .{
                    .name = err.name,
                    .payload = payload,
                },
            };
        },
        .ErrorVariant => |err| {
            return .{
                .ErrorVariant = .{
                    .from = if (err.from) |from| from else null,
                    .variant = err.variant,
                },
            };
        },
        .Generic => return CloneError.BadGenericClone,
        .Function => @panic("No cloning functions"),
    }
}

fn cloneAstNodePtrMutPool(
    allocator: Allocator,
    context: *Context,
    node: *const blitzAst.AstNode,
    withGenDef: bool,
) (Allocator.Error || CloneError)!*blitzAst.AstNode {
    const clonedNode = try cloneAstNodePool(allocator, context, node.*, withGenDef);
    return try context.pools.nodes.new(clonedNode);
}

pub fn cloneAstNodePool(
    allocator: Allocator,
    context: *Context,
    node: blitzAst.AstNode,
    withGenDef: bool,
) !blitzAst.AstNode {
    switch (node) {
        .NoOp, .StructPlaceholder, .Break, .Continue => return node,
        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtrMutPool(allocator, context, index.index, withGenDef),
                .value = try cloneAstNodePtrMutPool(allocator, context, index.value, withGenDef),
            },
        },
        .OpExpr => |op| {
            const opType = op.type;

            return .{
                .OpExpr = .{
                    .type = opType,
                    .left = try cloneAstNodePtrMutPool(allocator, context, op.left, withGenDef),
                    .right = try cloneAstNodePtrMutPool(allocator, context, op.right, withGenDef),
                    .depth = op.depth,
                },
            };
        },
        .IncOne => |val| {
            return .{
                .IncOne = try cloneAstNodePtrMutPool(allocator, context, val, withGenDef),
            };
        },
        .DecOne => |val| {
            return .{
                .DecOne = try cloneAstNodePtrMutPool(allocator, context, val, withGenDef),
            };
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = ref,
            };
        },
        .Seq => |seq| {
            var newSeq = try allocator.alloc(*blitzAst.AstNode, seq.nodes.len);
            try context.deferCleanup.appendNodeSlice(newSeq);

            for (seq.nodes, 0..) |seqNode, index| {
                newSeq[index] = try cloneAstNodePtrMutPool(
                    allocator,
                    context,
                    seqNode,
                    withGenDef,
                );
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
                        .String = str,
                    },
                },
                .RawNumber => |num| return .{
                    .Value = .{
                        .RawNumber = .{
                            .digits = num.digits,
                            .numType = num.numType,
                        },
                    },
                },
                .ArraySlice => |arr| return .{
                    .Value = .{
                        .ArraySlice = try cloneNodeArrMutPool(
                            allocator,
                            context,
                            arr,
                            withGenDef,
                        ),
                    },
                },
            }
        },
        .VarDec => |dec| {
            const nodePtr = try cloneAstNodePtrMutPool(
                allocator,
                context,
                dec.setNode,
                withGenDef,
            );
            var clonedType: ?blitzAst.AstTypeInfo = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypeInfoPool(allocator, context, annotation, withGenDef);
            }

            return .{
                .VarDec = .{
                    .name = dec.name,
                    .isConst = dec.isConst,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                    .setType = dec.setType,
                },
            };
        },
        .ValueSet => |set| return .{
            .ValueSet = .{
                .value = try cloneAstNodePtrMutPool(allocator, context, set.value, withGenDef),
                .setNode = try cloneAstNodePtrMutPool(
                    allocator,
                    context,
                    set.setNode,
                    withGenDef,
                ),
            },
        },
        .VarEqOp => |op| return .{
            .VarEqOp = .{
                .variable = op.variable,
                .value = try cloneAstNodePtrMutPool(allocator, context, op.value, withGenDef),
                .opType = op.opType,
            },
        },
        .Type => |t| return .{
            .Type = try cloneAstTypesPool(allocator, context, t, withGenDef),
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtrMutPool(allocator, context, cast.node, withGenDef);
            const typePtr = try cloneAstTypeInfoPool(allocator, context, cast.toType, withGenDef);

            return .{
                .Cast = .{
                    .node = nodePtr,
                    .toType = typePtr,
                },
            };
        },
        .Variable => |v| return .{
            .Variable = v,
        },
        .Pointer => |ptr| return .{
            .Pointer = .{
                .node = try cloneAstNodePtrMutPool(allocator, context, ptr.node, withGenDef),
                .isConst = ptr.isConst,
            },
        },
        .Dereference => |deref| return .{
            .Dereference = try cloneAstNodePtrMutPool(allocator, context, deref, withGenDef),
        },
        .HeapAlloc => |alloc| {
            const allocTypeClone = if (alloc.allocType) |allocType|
                try cloneAstTypeInfoPool(allocator, context, allocType, withGenDef)
            else
                null;

            return .{
                .HeapAlloc = .{
                    .node = try cloneAstNodePtrMutPool(
                        allocator,
                        context,
                        alloc.node,
                        withGenDef,
                    ),
                    .allocType = allocTypeClone,
                },
            };
        },
        .HeapFree => |toFree| return .{
            .HeapFree = try cloneAstNodePtrMutPool(
                allocator,
                context,
                toFree,
                withGenDef,
            ),
        },
        .StructDec => |dec| {
            const clonedGenerics = try cloneGenericsPool(
                allocator,
                context,
                dec.generics,
                withGenDef,
            );
            var deriveType: ?blitzAst.AstTypeInfo = null;
            const attributes = try cloneStructAttrDecPool(
                allocator,
                context,
                dec.attributes,
                withGenDef,
            );
            const totalMemberList = try cloneStructAttrDec(
                allocator,
                context,
                dec.attributes,
                withGenDef,
            );

            if (dec.deriveType) |dType| {
                deriveType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    dType,
                    withGenDef,
                );
            }

            const list = try dec.toScanTypes.clone(allocator);
            for (list.items) |*item| {
                item.* = try cloneGenToInfoRels(allocator, context, item.*, withGenDef);
            }
            const listPtr = try createMut(blitzAst.ToScanTypesList, allocator, list);

            const structDec: blitzAst.StructDecNode = .{
                .name = dec.name,
                .generics = clonedGenerics,
                .deriveType = deriveType,
                .attributes = attributes,
                .totalMemberList = totalMemberList,
                .toScanTypes = listPtr,
            };
            const structNode = try createMut(blitzAst.StructDecNode, allocator, structDec);

            return .{
                .StructDec = structNode,
            };
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.body,
                withGenDef,
            );
            const conditionPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.condition,
                withGenDef,
            );

            var newFallback: ?*const blitzAst.IfFallback = null;
            if (statement.fallback) |fallback| {
                newFallback = try cloneIfFallback(allocator, context, fallback, withGenDef);
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
            var newInitNode: ?*blitzAst.AstNode = null;

            if (loop.initNode) |init| {
                newInitNode = try cloneAstNodePtrMut(allocator, context, init, withGenDef);
            }

            return .{
                .ForLoop = .{
                    .initNode = newInitNode,
                    .condition = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.condition,
                        withGenDef,
                    ),
                    .incNode = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.incNode,
                        withGenDef,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, withGenDef),
                },
            };
        },
        .WhileLoop => |loop| {
            return .{
                .WhileLoop = .{
                    .condition = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.condition,
                        withGenDef,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, withGenDef),
                },
            };
        },
        .FuncDec => |dec| return .{
            .FuncDec = dec,
        },
        .FuncCall => |call| {
            const clonedFunc = try cloneAstNodePtrMut(
                allocator,
                context,
                call.func,
                withGenDef,
            );
            const newParams = try cloneNodeArrMut(
                allocator,
                context,
                call.params,
                withGenDef,
            );

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| return .{
            .ReturnNode = try cloneAstNodePtrMut(allocator, context, ret, withGenDef),
        },
        .StructInit => |init| {
            const generics = try allocator.dupe(blitzAst.AstTypeInfo, init.generics);
            const name = init.name;
            const attributes = try cloneAttrDef(
                allocator,
                context,
                init.attributes,
                withGenDef,
            );

            return .{
                .StructInit = .{
                    .attributes = attributes,
                    .name = name,
                    .generics = generics,
                },
            };
        },
        .Bang => |bangNode| return .{
            .Bang = try cloneAstNodePtrMut(allocator, context, bangNode, withGenDef),
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtrMut(
                allocator,
                context,
                access.value,
                withGenDef,
            );
            const prop = access.property;

            return .{
                .PropertyAccess = .{
                    .value = value,
                    .property = prop,
                },
            };
        },
        .StaticStructInstance => |inst| return .{
            .StaticStructInstance = inst,
        },
        .ErrorDec => |def| {
            const newVariants = try allocator.alloc([]const u8, def.variants.len);
            @memcpy(newVariants, def.variants);

            return .{
                .ErrorDec = try create(blitzAst.ErrorDecNode, allocator, .{
                    .name = def.name,
                    .variants = newVariants,
                }),
            };
        },
        .Error => |err| return .{
            .Error = err,
        },
        .InferErrorVariant => |err| return .{
            .InferErrorVariant = err,
        },
        .Group => |group| return .{
            .Group = try cloneAstNodePtrMut(allocator, context, group, withGenDef),
        },
        .Scope => |scope| return .{
            .Scope = try cloneAstNodePtrMut(allocator, context, scope, withGenDef),
        },
        .ArrayInit => |init| return .{
            .ArrayInit = .{
                .size = init.size,
                .initType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    init.initType,
                    withGenDef,
                ),
                .initNode = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    init.initNode,
                    withGenDef,
                ),
                .indexIdent = init.indexIdent,
                .ptrIdent = init.ptrIdent,
            },
        },
    }
}

pub fn cloneCustomGenericsPool(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.AstTypeInfo,
    withGenDef: bool,
) ![]blitzAst.AstTypeInfo {
    const genericsSlice = try allocator.alloc(blitzAst.AstTypeInfo, generics.len);
    try context.deferCleanup.appendTypeInfoSlice(genericsSlice);

    for (generics, 0..) |gen, index| {
        genericsSlice[index] = try cloneAstTypeInfoPool(allocator, context, gen, withGenDef);
    }

    return genericsSlice;
}

fn cloneNodeArrMutPool(
    allocator: Allocator,
    context: *Context,
    nodes: []*blitzAst.AstNode,
    withGenDef: bool,
) ![]*blitzAst.AstNode {
    var newNodes = try allocator.alloc(*blitzAst.AstNode, nodes.len);
    try context.deferCleanup.appendNodeSlice(newNodes);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtrMutPool(allocator, context, node, withGenDef);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

pub fn cloneGenericsPool(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.GenericType,
    withGenDef: bool,
) ![]blitzAst.GenericType {
    var clonedGenerics = try allocator.alloc(blitzAst.GenericType, generics.len);
    try context.deferCleanup.appendGenericTypeSlice(clonedGenerics);

    for (generics, 0..) |generic, index| {
        const newGeneric = try cloneGenericPool(allocator, context, generic, withGenDef);
        clonedGenerics[index] = newGeneric;
    }

    return clonedGenerics;
}

fn cloneGenericPool(
    allocator: Allocator,
    context: *Context,
    generic: blitzAst.GenericType,
    withGenDef: bool,
) !blitzAst.GenericType {
    var restriction: ?blitzAst.AstTypeInfo = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypeInfoPool(allocator, context, rest, withGenDef);
    }

    return .{
        .name = generic.name,
        .restriction = restriction,
    };
}

fn cloneStructAttrDecPool(
    allocator: Allocator,
    context: *Context,
    attrs: []blitzAst.StructAttribute,
    replaceGenerics: bool,
) ![]blitzAst.StructAttribute {
    var attributes = try allocator.alloc(blitzAst.StructAttribute, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.StructAttribute = .{
            .static = attr.static,
            .attr = try cloneStructAttributeUnion(allocator, context, attr.attr, replaceGenerics),
            .name = attr.name,
            .visibility = attr.visibility,
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn cloneStructAttributeUnionPool(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: blitzAst.StructAttributeUnion,
    replaceGenerics: bool,
) !blitzAst.StructAttributeUnion {
    return switch (structAttrUnion) {
        .Member => |member| .{
            .Member = try cloneAstTypeInfoPool(allocator, context, member, replaceGenerics),
        },
        .Function => structAttrUnion,
    };
}
