const std = @import("std");
const blitz = @import("blitz.zig");
const blitzAst = blitz.ast;
const utils = blitz.utils;
const string = blitz.string;
const blitzCompInfo = blitz.compInfo;
const debug = blitz.debug;
const scanner = blitz.scanner;
const free = blitz.free;
const Allocator = std.mem.Allocator;
const create = utils.create;
const createMut = utils.createMut;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Context = blitz.context.Context;

pub const CloneError = error{
    GenericNotFound,
    BadGenericClone,
    CannotCloneFunction,
    CannotCloneStructDec,
    CannotCloneErrorDec,
};

pub fn cloneAstTypeInfo(
    allocator: Allocator,
    context: *Context,
    info: blitzAst.AstTypeInfo,
    withGenDef: bool,
) (Allocator.Error || CloneError)!blitzAst.AstTypeInfo {
    if (info.astType.* == .Generic) {
        const generic = info.astType.*.Generic;
        if (withGenDef) {
            const genType = try context.compInfo.getGeneric(generic);
            if (genType) |gType| {
                const clonedType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    gType.info,
                    withGenDef,
                );
                return clonedType;
            }

            return CloneError.GenericNotFound;
        }

        return .{
            .astType = try context.pools.types.new(.{
                .Generic = generic,
            }),
            .mutState = info.mutState,
        };
    }

    return .{
        .astType = try cloneAstTypesPtrMut(allocator, context, info.astType, withGenDef),
        .mutState = info.mutState,
    };
}

pub fn cloneAstTypesPtrMut(
    allocator: Allocator,
    context: *Context,
    astType: *blitzAst.AstTypes,
    withGenDef: bool,
) !*blitzAst.AstTypes {
    const clonedType = try cloneAstTypes(allocator, context, astType.*, withGenDef);
    return try context.pools.types.new(clonedType);
}

pub fn cloneAstTypes(
    allocator: Allocator,
    context: *Context,
    types: blitzAst.AstTypes,
    withGenDef: bool,
) (Allocator.Error || CloneError)!blitzAst.AstTypes {
    switch (types) {
        .String, .Bool, .Char, .Void, .Number, .Null, .RawNumber, .Any => return types,

        .VarInfo => |info| {
            return .{
                .VarInfo = (try cloneAstTypeInfo(
                    allocator,
                    context,
                    info.info,
                    withGenDef,
                )).toAllocInfo(.Allocated),
            };
        },
        .ArraySlice => |arr| {
            const typeClone = (try cloneAstTypeInfo(
                allocator,
                context,
                arr.type.info,
                withGenDef,
            )).toAllocInfo(.Allocated);
            var sizeClone: ?*blitzAst.AstNode = null;
            if (arr.size) |size| {
                sizeClone = try cloneAstNodePtrMut(allocator, context, size, withGenDef);
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
                .Pointer = (try cloneAstTypeInfo(
                    allocator,
                    context,
                    ptr.info,
                    withGenDef,
                )).toAllocInfo(.Allocated),
            };
        },
        .Nullable => |t| {
            return .{
                .Nullable = try cloneAstTypeInfo(allocator, context, t, withGenDef),
            };
        },
        .Custom => |custom| {
            const genericsSlice = try cloneCustomGenerics(
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
                payload = try cloneAstTypeInfo(allocator, context, errPayload, withGenDef);
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
        .Function => return CloneError.CannotCloneFunction,
    }
}

fn cloneAstNodePtrMut(
    allocator: Allocator,
    context: *Context,
    node: *const blitzAst.AstNode,
    withGenDef: bool,
) (Allocator.Error || CloneError)!*blitzAst.AstNode {
    const clonedNode = try cloneAstNode(allocator, context, node.*, withGenDef);
    return try context.pools.nodes.new(clonedNode);
}

pub fn cloneAstNode(
    allocator: Allocator,
    context: *Context,
    node: blitzAst.AstNode,
    withGenDef: bool,
) !blitzAst.AstNode {
    switch (node) {
        .NoOp, .StructPlaceholder, .Break, .Continue => return node,
        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtrMut(allocator, context, index.index, withGenDef),
                .value = try cloneAstNodePtrMut(allocator, context, index.value, withGenDef),
            },
        },
        .OpExpr => |op| {
            const opType = op.type;

            return .{
                .OpExpr = .{
                    .type = opType,
                    .left = try cloneAstNodePtrMut(allocator, context, op.left, withGenDef),
                    .right = try cloneAstNodePtrMut(allocator, context, op.right, withGenDef),
                    .depth = op.depth,
                },
            };
        },
        .IncOne => |val| {
            return .{
                .IncOne = try cloneAstNodePtrMut(allocator, context, val, withGenDef),
            };
        },
        .DecOne => |val| {
            return .{
                .DecOne = try cloneAstNodePtrMut(allocator, context, val, withGenDef),
            };
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = ref,
            };
        },
        .Seq => |seq| {
            var newSeq = try allocator.alloc(*blitzAst.AstNode, seq.len);
            try context.deferCleanup.slices.nodeSlices.append(newSeq);

            for (seq, 0..) |seqNode, index| {
                newSeq[index] = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    seqNode,
                    withGenDef,
                );
            }

            return .{
                .Seq = newSeq,
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
                            withGenDef,
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
                withGenDef,
            );
            var clonedType: ?blitzAst.AstTypeInfo = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypeInfo(allocator, context, annotation, withGenDef);
            }

            return .{
                .VarDec = .{
                    .name = dec.name,
                    .mutState = dec.mutState,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                },
            };
        },
        .ValueSet => |set| return .{
            .ValueSet = .{
                .value = try cloneAstNodePtrMut(allocator, context, set.value, withGenDef),
                .setNode = try cloneAstNodePtrMut(
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
                .value = try cloneAstNodePtrMut(allocator, context, op.value, withGenDef),
                .opType = op.opType,
            },
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtrMut(allocator, context, cast.node, withGenDef);
            const typePtr = try cloneAstTypeInfo(allocator, context, cast.toType, withGenDef);

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
                .node = try cloneAstNodePtrMut(allocator, context, ptr.node, withGenDef),
                .mutState = ptr.mutState,
            },
        },
        .Dereference => |deref| return .{
            .Dereference = try cloneAstNodePtrMut(allocator, context, deref, withGenDef),
        },
        .HeapAlloc => |alloc| {
            return .{
                .HeapAlloc = .{
                    .node = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        alloc.node,
                        withGenDef,
                    ),
                },
            };
        },
        .HeapFree => |toFree| return .{
            .HeapFree = try cloneAstNodePtrMut(
                allocator,
                context,
                toFree,
                withGenDef,
            ),
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

            var newFallback: ?blitzAst.FallbackInfo = null;
            if (statement.fallback) |fallback| {
                newFallback = .{
                    .node = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        fallback.node,
                        withGenDef,
                    ),
                    .hasCondition = fallback.hasCondition,
                };
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
            const generics = try allocator.alloc(blitzAst.AstTypeInfo, init.generics.len);
            for (init.generics, generics) |generic, *to| {
                to.* = try cloneAstTypeInfo(allocator, context, generic, withGenDef);
            }
            try context.deferCleanup.slices.typeInfoSlices.append(generics);

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
        .StructDec => return CloneError.CannotCloneStructDec,
        .ErrorDec => return CloneError.CannotCloneErrorDec,
    }
}

pub fn cloneCustomGenerics(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.AstTypeInfo,
    withGenDef: bool,
) ![]blitzAst.AstTypeInfo {
    const genericsSlice = try allocator.alloc(blitzAst.AstTypeInfo, generics.len);
    try context.deferCleanup.slices.typeInfoSlices.append(genericsSlice);

    for (generics, 0..) |gen, index| {
        genericsSlice[index] = try cloneAstTypeInfo(allocator, context, gen, withGenDef);
    }

    return genericsSlice;
}

fn cloneNodeArrMut(
    allocator: Allocator,
    context: *Context,
    nodes: []*blitzAst.AstNode,
    withGenDef: bool,
) ![]*blitzAst.AstNode {
    var newNodes = try allocator.alloc(*blitzAst.AstNode, nodes.len);
    try context.deferCleanup.slices.nodeSlices.append(newNodes);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtrMut(allocator, context, node, withGenDef);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

pub fn cloneGenerics(
    allocator: Allocator,
    context: *Context,
    generics: []blitzAst.GenericType,
    withGenDef: bool,
) ![]blitzAst.GenericType {
    var clonedGenerics = try allocator.alloc(blitzAst.GenericType, generics.len);
    try context.deferCleanup.slices.genericTypeSlices.append(clonedGenerics);

    for (generics, 0..) |generic, index| {
        const newGeneric = try cloneGeneric(allocator, context, generic, withGenDef);
        clonedGenerics[index] = newGeneric;
    }

    return clonedGenerics;
}

fn cloneGeneric(
    allocator: Allocator,
    context: *Context,
    generic: blitzAst.GenericType,
    withGenDef: bool,
) !blitzAst.GenericType {
    var restriction: ?blitzAst.AstTypeInfo = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypeInfo(allocator, context, rest, withGenDef);
    }

    return .{
        .name = generic.name,
        .restriction = restriction,
    };
}

fn cloneStructAttrDec(
    allocator: Allocator,
    context: *Context,
    attrs: []blitzAst.StructAttribute,
    withGenDef: bool,
) ![]blitzAst.StructAttribute {
    var attributes = try allocator.alloc(blitzAst.StructAttribute, attrs.len);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.StructAttribute = .{
            .static = attr.static,
            .attr = try cloneStructAttributeUnion(allocator, context, attr.attr, withGenDef),
            .name = attr.name,
            .visibility = attr.visibility,
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn cloneStructAttributeUnion(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: blitzAst.StructAttributeUnion,
    withGenDef: bool,
) !blitzAst.StructAttributeUnion {
    return switch (structAttrUnion) {
        .Member => |member| .{
            .Member = try cloneAstTypeInfo(allocator, context, member, withGenDef),
        },
        .Function => structAttrUnion,
    };
}

pub fn cloneStructAttributeUnionType(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: blitzAst.StructAttributeUnion,
    withGenDef: bool,
) !blitzAst.AstTypeInfo {
    return switch (structAttrUnion) {
        .Function => |func| (try context.pools.types.new(.{
            .Function = func,
        })).toTypeInfo(.Const),
        .Member => |member| try cloneAstTypeInfo(allocator, context, member, withGenDef),
    };
}

fn cloneAttrDef(
    allocator: Allocator,
    context: *Context,
    attrs: []blitzAst.AttributeDefinition,
    withGenDef: bool,
) ![]blitzAst.AttributeDefinition {
    var attributes = try allocator.alloc(blitzAst.AttributeDefinition, attrs.len);
    try context.deferCleanup.slices.attrDefSlices.append(attributes);

    for (attrs, 0..) |attr, index| {
        const newAttr: blitzAst.AttributeDefinition = .{
            .name = attr.name,
            .value = try cloneAstNodePtrMut(allocator, context, attr.value, withGenDef),
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn replaceGenericsOnTypeInfo(
    allocator: Allocator,
    context: *Context,
    info: scanner.TypeAndAllocInfo,
    withGenDef: bool,
) !scanner.TypeAndAllocInfo {
    if (!withGenDef) return info;

    return .{
        .info = try cloneAstTypeInfo(allocator, context, info.info, withGenDef),
        .allocState = .Allocated,
    };
}

pub fn replaceGenericsOnTypeInfoAndRelease(
    allocator: Allocator,
    context: *Context,
    info: scanner.TypeAndAllocInfo,
    withGenDef: bool,
) !scanner.TypeAndAllocInfo {
    if (!withGenDef) return info;

    const res = scanner.TypeAndAllocInfo{
        .info = try cloneAstTypeInfo(allocator, context, info.info, withGenDef),
        .allocState = .Allocated,
    };

    if (info.allocState == .Allocated) {
        free.recursiveReleaseType(allocator, context, info.info.astType);
    }

    return res;
}
