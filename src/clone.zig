const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const print = blitz.print;
const scanner = blitz.scanner;
const pools = blitz.allocPools;
const Context = blitz.context.Context;
const errors = blitz.errors;

pub const CloneConfig = struct {
    withGenDef: bool,
    setAttrSizes: bool = false,
};

pub fn cloneAstTypeInfo(
    allocator: Allocator,
    context: *Context,
    info: ast.AstTypeInfo,
    cloneConfig: CloneConfig,
) (Allocator.Error || errors.CloneError)!ast.AstTypeInfo {
    if (info.astType.* == .Generic) {
        const generic = info.astType.Generic;
        if (cloneConfig.withGenDef) {
            const genType = try context.compInfo.getGeneric(allocator, context, generic);
            if (genType) |gType| {
                const clonedType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    gType.info,
                    cloneConfig,
                );
                return clonedType;
            }

            return errors.CloneError.GenericNotFound;
        }

        return .{
            .astType = try context.pools.newType(context, .{
                .Generic = generic,
            }),
            .mutState = info.mutState,
        };
    }

    return .{
        .astType = try cloneAstTypesPtrMut(allocator, context, info.astType, cloneConfig),
        .mutState = info.mutState,
    };
}

pub fn cloneAstTypesPtrMut(
    allocator: Allocator,
    context: *Context,
    astType: *ast.AstTypes,
    cloneConfig: CloneConfig,
) !*ast.AstTypes {
    const clonedType = try cloneAstTypes(allocator, context, astType.*, cloneConfig);
    return try context.pools.newType(context, clonedType);
}

pub fn cloneAstTypes(
    allocator: Allocator,
    context: *Context,
    types: ast.AstTypes,
    cloneConfig: CloneConfig,
) (Allocator.Error || errors.CloneError)!ast.AstTypes {
    return switch (types) {
        .Bool,
        .Void,
        .Number,
        .Null,
        .Any,
        .Undef,
        .Enum,
        => types,

        .VarInfo => |info| {
            const varInfo = try cloneAstTypeInfo(
                allocator,
                context,
                info.info,
                cloneConfig,
            );
            return .{
                .VarInfo = varInfo.toAllocInfo(.Allocated),
            };
        },
        .ArrayDec => |arr| {
            const typeClone = (try cloneAstTypeInfo(
                allocator,
                context,
                arr.type.info,
                cloneConfig,
            )).toAllocInfo(.Allocated);

            return .{
                .ArrayDec = .{
                    .type = typeClone,
                    .size = arr.size,
                },
            };
        },
        .StaticStructInstance => |nameIdentId| .{ .StaticStructInstance = nameIdentId },
        .Pointer => |ptr| .{
            .Pointer = (try cloneAstTypeInfo(
                allocator,
                context,
                ptr.info,
                cloneConfig,
            )).toAllocInfo(.Allocated),
        },
        .Nullable => |t| .{
            .Nullable = try cloneAstTypeInfo(allocator, context, t, cloneConfig),
        },
        .Custom => |custom| {
            const genericsSlice = try cloneCustomGenerics(
                allocator,
                context,
                custom.generics,
                cloneConfig,
            );

            const attrSizes, const clonedNestedInstances = if (custom.attrSizes.len == 0 and cloneConfig.setAttrSizes)
                try attrSizesFromCustom(allocator, context, custom, cloneConfig)
            else
                .{
                    custom.attrSizes, try cloneNestedInstances(
                        allocator,
                        context,
                        custom.nestedInstances,
                        cloneConfig,
                    ),
                };

            return .{
                .Custom = .{
                    .nameIdentId = custom.nameIdentId,
                    .generics = genericsSlice,
                    .allowPrivateReads = custom.allowPrivateReads,
                    .attrSizes = attrSizes,
                    .nestedInstances = clonedNestedInstances,
                },
            };
        },
        .CustomInstance => |id| {
            return .{
                .CustomInstance = id,
            };
        },
        .Error => |err| {
            var payload: ?ast.AstTypeInfo = null;

            if (err.payload) |errPayload| {
                payload = try cloneAstTypeInfo(allocator, context, errPayload, cloneConfig);
            }

            return .{
                .Error = .{
                    .nameIdentId = err.nameIdentId,
                    .payload = payload,
                },
            };
        },
        .EnumVariant => |enumVariant| .{
            .EnumVariant = .{
                .fromIdentId = enumVariant.fromIdentId,
                .variantIdentId = enumVariant.variantIdentId,
            },
        },
        .ErrorVariant => |err| .{
            .ErrorVariant = .{
                .fromIdentId = err.fromIdentId,
                .variantIdentId = err.variantIdentId,
            },
        },
        .Generic => return errors.CloneError.BadGenericClone,
        .Function, .StructMethod => return errors.CloneError.CannotCloneFunction,
    };
}

pub fn attrSizesFromCustom(
    allocator: Allocator,
    context: *Context,
    customType: ast.CustomType,
    cloneConfig: CloneConfig,
) (Allocator.Error || errors.CloneError)!struct { []ast.IdentSizeRelation, []ast.InstanceRelation } {
    var attrSizes: std.ArrayList(ast.IdentSizeRelation) = .empty;
    var nestedInstances: std.ArrayList(ast.InstanceRelation) = .empty;
    const dec = context.compInfo.getStructDec(customType.nameIdentId).?;

    for (dec.totalMemberList) |item| {
        const size = try item.attr.Member.astType.getSize(allocator, context);
        const alignment = try item.attr.Member.astType.getAlignment(allocator, context);

        const nestedInstanceOrNull = try scanner.nonPrimitiveTypeToInstance(
            allocator,
            context,
            item.attr.Member.toAllocInfo(.Recycled),
            cloneConfig,
        );
        if (nestedInstanceOrNull) |nestedInstance| {
            try nestedInstances.append(allocator, .{
                .identId = item.nameIdentId,
                .instanceAstType = nestedInstance,
            });
        }

        try attrSizes.append(allocator, .{
            .identId = item.nameIdentId,
            .size = size,
            .alignment = alignment,
        });
    }

    return .{ attrSizes.items, nestedInstances.items };
}

pub fn cloneAstNodePtrMut(
    allocator: Allocator,
    context: *Context,
    node: *const ast.AstNode,
    cloneConfig: CloneConfig,
) (Allocator.Error || errors.CloneError)!*ast.AstNode {
    const clonedNode = try cloneAstNode(allocator, context, node.*, cloneConfig);
    return try context.pools.newNode(context, clonedNode);
}

pub fn cloneAstNodeUnion(
    allocator: Allocator,
    context: *Context,
    node: ast.AstNodeUnion,
    cloneConfig: CloneConfig,
) !ast.AstNodeUnion {
    switch (node) {
        .NoOp, .StructPlaceholder, .Break, .Continue, .UndefValue, .Enum => return node,
        .IndexValue => |index| return .{
            .IndexValue = .{
                .index = try cloneAstNodePtrMut(allocator, context, index.index, cloneConfig),
                .target = try cloneAstNodePtrMut(allocator, context, index.target, cloneConfig),
            },
        },
        .OpExpr => |op| {
            const opType = op.type;

            return .{
                .OpExpr = .{
                    .type = opType,
                    .left = try cloneAstNodePtrMut(allocator, context, op.left, cloneConfig),
                    .right = try cloneAstNodePtrMut(allocator, context, op.right, cloneConfig),
                    .depth = op.depth,
                },
            };
        },
        .IncOne => |val| {
            return .{
                .IncOne = try cloneAstNodePtrMut(allocator, context, val, cloneConfig),
            };
        },
        .DecOne => |val| {
            return .{
                .DecOne = try cloneAstNodePtrMut(allocator, context, val, cloneConfig),
            };
        },
        .FuncReference => |ref| {
            return .{
                .FuncReference = ref,
            };
        },
        .Seq => |seq| {
            var newSeq = try allocator.alloc(*ast.AstNode, seq.len);
            try context.deferCleanup.nodeSlices.append(allocator, newSeq);

            for (seq, 0..) |seqNode, index| {
                newSeq[index] = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    seqNode,
                    cloneConfig,
                );
            }

            return .{
                .Seq = newSeq,
            };
        },
        .Value => |val| {
            switch (val) {
                .Bool, .Number, .Null => return node,
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
                .ArrayDec => |arr| return .{
                    .Value = .{
                        .ArrayDec = try cloneNodeArrMut(
                            allocator,
                            context,
                            arr,
                            cloneConfig,
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
                cloneConfig,
            );
            var clonedType: ?ast.AstTypeInfo = null;

            if (dec.annotation) |annotation| {
                clonedType = try cloneAstTypeInfo(allocator, context, annotation, cloneConfig);
            }

            return .{
                .VarDec = .{
                    .nameIdentId = dec.nameIdentId,
                    .mutState = dec.mutState,
                    .setNode = nodePtr,
                    .annotation = clonedType,
                },
            };
        },
        .ValueSet => |set| return .{
            .ValueSet = .{
                .value = try cloneAstNodePtrMut(allocator, context, set.value, cloneConfig),
                .setNode = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    set.setNode,
                    cloneConfig,
                ),
            },
        },
        .VarEqOp => |op| return .{
            .VarEqOp = .{
                .variable = op.variable,
                .value = try cloneAstNodePtrMut(allocator, context, op.value, cloneConfig),
                .opType = op.opType,
            },
        },
        .Cast => |cast| {
            const nodePtr = try cloneAstNodePtrMut(allocator, context, cast.node, cloneConfig);
            const typePtr = try cloneAstTypeInfo(allocator, context, cast.toType, cloneConfig);

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
                .node = try cloneAstNodePtrMut(allocator, context, ptr.node, cloneConfig),
                .mutState = ptr.mutState,
            },
        },
        .Dereference => |deref| return .{
            .Dereference = try cloneAstNodePtrMut(allocator, context, deref, cloneConfig),
        },
        .HeapAlloc => |alloc| {
            return .{
                .HeapAlloc = .{
                    .node = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        alloc.node,
                        cloneConfig,
                    ),
                },
            };
        },
        .HeapFree => |toFree| return .{
            .HeapFree = try cloneAstNodePtrMut(
                allocator,
                context,
                toFree,
                cloneConfig,
            ),
        },
        .IfStatement => |statement| {
            const bodyPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.body,
                cloneConfig,
            );
            const conditionPtr = try cloneAstNodePtrMut(
                allocator,
                context,
                statement.condition,
                cloneConfig,
            );

            var newFallback: ?ast.FallbackInfo = null;
            if (statement.fallback) |fallback| {
                newFallback = .{
                    .node = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        fallback.node,
                        cloneConfig,
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
            var newInitNode: ?*ast.AstNode = null;

            if (loop.initNode) |init| {
                newInitNode = try cloneAstNodePtrMut(allocator, context, init, cloneConfig);
            }

            return .{
                .ForLoop = .{
                    .initNode = newInitNode,
                    .condition = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.condition,
                        cloneConfig,
                    ),
                    .incNode = try cloneAstNodePtrMut(
                        allocator,
                        context,
                        loop.incNode,
                        cloneConfig,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, cloneConfig),
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
                        cloneConfig,
                    ),
                    .body = try cloneAstNodePtrMut(allocator, context, loop.body, cloneConfig),
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
                cloneConfig,
            );
            const newParams = try cloneNodeArrMut(
                allocator,
                context,
                call.params,
                cloneConfig,
            );

            return .{
                .FuncCall = .{
                    .func = clonedFunc,
                    .params = newParams,
                },
            };
        },
        .ReturnNode => |ret| return .{
            .ReturnNode = try cloneAstNodePtrMut(allocator, context, ret, cloneConfig),
        },
        .StructInit => |init| {
            const generics = try allocator.alloc(ast.AstTypeInfo, init.generics.len);
            for (init.generics, generics) |generic, *to| {
                to.* = try cloneAstTypeInfo(allocator, context, generic, cloneConfig);
            }
            try context.deferCleanup.typeInfoSlices.append(allocator, generics);

            const name = init.nameIdentId;
            const attributes = try cloneAttrDef(
                allocator,
                context,
                init.attributes,
                cloneConfig,
            );

            const clonedAttrSizes = try allocator.dupe(ast.IdentSizeRelation, init.attrSizes);

            return .{
                .StructInit = .{
                    .attributes = attributes,
                    .nameIdentId = name,
                    .generics = generics,
                    .attrSizes = clonedAttrSizes,
                },
            };
        },
        .Bang => |bangNode| return .{
            .Bang = try cloneAstNodePtrMut(allocator, context, bangNode, cloneConfig),
        },
        .PropertyAccess => |access| {
            const value = try cloneAstNodePtrMut(
                allocator,
                context,
                access.value,
                cloneConfig,
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
        .InferEnumVariant => |err| return .{
            .InferEnumVariant = err,
        },
        .Group => |group| return .{
            .Group = try cloneAstNodePtrMut(allocator, context, group, cloneConfig),
        },
        .Scope => |scope| return .{
            .Scope = try cloneAstNodePtrMut(allocator, context, scope, cloneConfig),
        },
        .ArrayInit => |init| return .{
            .ArrayInit = .{
                .size = init.size,
                .initType = try cloneAstTypeInfo(
                    allocator,
                    context,
                    init.initType,
                    cloneConfig,
                ),
                .initNode = try cloneAstNodePtrMut(
                    allocator,
                    context,
                    init.initNode,
                    cloneConfig,
                ),
                .indexIdentId = init.indexIdentId,
                .ptrIdentId = init.ptrIdentId,
            },
        },
        .StructDec => return errors.CloneError.CannotCloneStructDec,
        .ErrorDec => return errors.CloneError.CannotCloneErrorDec,
        .EnumDec => return errors.CloneError.CannotCloneEnumDec,
    }
}

pub fn cloneAstNode(
    allocator: Allocator,
    context: *Context,
    node: ast.AstNode,
    cloneConfig: CloneConfig,
) !ast.AstNode {
    const clonedUnion = try cloneAstNodeUnion(allocator, context, node.variant, cloneConfig);
    return .{
        .variant = clonedUnion,
        .typeInfo = node.typeInfo,
    };
}

pub fn cloneCustomGenerics(
    allocator: Allocator,
    context: *Context,
    generics: []ast.AstTypeInfo,
    cloneConfig: CloneConfig,
) ![]ast.AstTypeInfo {
    const genericsSlice = try allocator.alloc(ast.AstTypeInfo, generics.len);
    try context.deferCleanup.typeInfoSlices.append(allocator, genericsSlice);

    for (generics, 0..) |gen, index| {
        genericsSlice[index] = try cloneAstTypeInfo(allocator, context, gen, cloneConfig);
    }

    return genericsSlice;
}

fn cloneNodeArrMut(
    allocator: Allocator,
    context: *Context,
    nodes: []*ast.AstNode,
    cloneConfig: CloneConfig,
) ![]*ast.AstNode {
    var newNodes = try allocator.alloc(*ast.AstNode, nodes.len);
    try context.deferCleanup.nodeSlices.append(allocator, newNodes);

    for (nodes, 0..) |node, index| {
        const nodePtr = try cloneAstNodePtrMut(allocator, context, node, cloneConfig);
        newNodes[index] = nodePtr;
    }

    return newNodes;
}

fn cloneGeneric(
    allocator: Allocator,
    context: *Context,
    generic: ast.GenericType,
    cloneConfig: CloneConfig,
) !ast.GenericType {
    var restriction: ?ast.AstTypeInfo = null;

    if (generic.restriction) |rest| {
        restriction = try cloneAstTypeInfo(allocator, context, rest, cloneConfig);
    }

    return .{
        .nameIdentId = generic.nameIdentId,
        .restriction = restriction,
    };
}

pub fn cloneStructAttributeUnionType(
    allocator: Allocator,
    context: *Context,
    structAttrUnion: ast.StructAttributeUnion,
    cloneConfig: CloneConfig,
) !ast.AstTypeInfo {
    return switch (structAttrUnion) {
        .Function => |func| {
            const res = try context.pools.newType(context, .{
                .Function = func,
            });
            return res.toTypeInfo(.Const);
        },
        .Member => |member| try cloneAstTypeInfo(allocator, context, member, cloneConfig),
    };
}

fn cloneAttrDef(
    allocator: Allocator,
    context: *Context,
    attrs: []ast.AttributeDefinition,
    cloneConfig: CloneConfig,
) ![]ast.AttributeDefinition {
    var attributes = try allocator.alloc(ast.AttributeDefinition, attrs.len);
    try context.deferCleanup.attrDefSlices.append(allocator, attributes);

    for (attrs, 0..) |attr, index| {
        const newAttr = ast.AttributeDefinition{
            .nameIdentId = attr.nameIdentId,
            .value = try cloneAstNodePtrMut(allocator, context, attr.value, cloneConfig),
        };

        attributes[index] = newAttr;
    }

    return attributes;
}

pub fn replaceGenericsOnTypeInfo(
    allocator: Allocator,
    context: *Context,
    info: scanner.TypeAndAllocInfo,
    cloneConfig: CloneConfig,
) errors.CloneError!scanner.TypeAndAllocInfo {
    if (!cloneConfig.withGenDef) return info;

    return .{
        .info = try cloneAstTypeInfo(allocator, context, info.info, cloneConfig),
        .allocState = .Allocated,
    };
}

pub fn replaceGenericsOnTypeInfoAndRelease(
    allocator: Allocator,
    context: *Context,
    info: scanner.TypeAndAllocInfo,
    cloneConfig: CloneConfig,
) !scanner.TypeAndAllocInfo {
    if (!cloneConfig.withGenDef) return info;

    const res = scanner.TypeAndAllocInfo{
        .info = try cloneAstTypeInfo(allocator, context, info.info, cloneConfig),
        .allocState = .Allocated,
    };

    if (info.allocState == .Allocated) {
        pools.recursiveReleaseType(context, info.info.astType);
    }

    return res;
}

fn cloneNestedInstances(
    allocator: Allocator,
    context: *Context,
    nestedInstances: []ast.InstanceRelation,
    cloneConfig: CloneConfig,
) ![]ast.InstanceRelation {
    var relations = try allocator.alloc(ast.InstanceRelation, nestedInstances.len);

    for (nestedInstances, 0..) |instance, index| {
        const clonedType = try cloneAstTypeInfo(
            allocator,
            context,
            instance.instanceAstType.info,
            cloneConfig,
        );
        relations[index] = .{
            .identId = instance.identId,
            .instanceAstType = clonedType.toAllocInfo(.Recycled),
        };
    }

    return relations;
}
