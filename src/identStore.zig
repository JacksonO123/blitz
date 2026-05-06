const std = @import("std");
const Allocator = std.mem.Allocator;

pub const IdentId = u32;

pub const IdentStoreError = error{
    IdentNotFound,
};

const KnownIdentIds = struct {
    main: IdentId = 0,
    self: IdentId = 1,
    len: IdentId = 2,
    ptr: IdentId = 3,
};

pub const KNOWN_IDENT_IDS: KnownIdentIds = .{};

pub const IdentStore = struct {
    const Self = @This();

    allocator: Allocator,
    identToIdMap: std.StringHashMap(IdentId),
    idents: std.ArrayList([]const u8),
    currentId: IdentId = 0,

    pub inline fn init(allocator: Allocator) !Self {
        var identToIdMap = std.StringHashMap(IdentId).init(allocator);
        var idents: std.ArrayList([]const u8) = .empty;
        var currentId: IdentId = 0;

        const typeInfo = @typeInfo(@TypeOf(KNOWN_IDENT_IDS));
        inline for (typeInfo.@"struct".fields) |field| {
            try identToIdMap.put(field.name, @field(KNOWN_IDENT_IDS, field.name));
            try idents.append(allocator, field.name);
            currentId += 1;
        }

        return .{
            .allocator = allocator,
            .identToIdMap = identToIdMap,
            .idents = idents,
            .currentId = currentId,
        };
    }

    pub fn getIdentId(self: *Self, ident: []const u8) !IdentId {
        if (self.identToIdMap.get(ident)) |id| {
            return id;
        }

        const id = self.currentId;
        try self.identToIdMap.put(ident, id);
        try self.idents.append(self.allocator, ident);

        self.currentId += 1;
        return id;
    }

    pub fn getIdentFromId(self: Self, id: IdentId) ?[]const u8 {
        if (id >= self.idents.items.len) return null;
        return self.idents.items[id];
    }
};
