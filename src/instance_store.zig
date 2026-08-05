const std = @import("std");

const blitz = @import("blitz.zig");
const ast = blitz.ast;
const utils = blitz.utils;

pub const InstanceStore = struct {
    const Self = @This();

    instances: utils.ArenaArrayList(ast.CustomType),

    pub fn init() Self {
        return .{ .instances = utils.ArenaArrayList(ast.CustomType).init() };
    }

    pub fn appendInstanceGetRefType(self: *Self, instance: ast.CustomType) !ast.AstTypes {
        const index = self.instances.list.items.len;
        try self.instances.append(instance);

        const resType = ast.AstTypes{
            .CustomInstance = index,
        };

        return resType;
    }

    pub fn getInstanceById(self: *Self, id: usize) ?*const ast.CustomType {
        if (id < self.instances.list.items.len) {
            return &self.instances.list.items[id];
        }

        return null;
    }
};
