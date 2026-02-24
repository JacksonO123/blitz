const std = @import("std");
const blitz = @import("../blitz.zig");
const codegen = blitz.codegen;
const Context = blitz.context.Context;
const Allocator = std.mem.Allocator;

pub fn begin(allocator: Allocator, context: *Context) !void {
    // temporary and preserved registers split
    // remaining register space equally
    // (256 - 8) / 2 = 124
    const paramLimits: codegen.RegisterRange = .{
        .start = 0,
        .end = 8,
    };
    context.genInfo.registerLimits.params = paramLimits;
    context.genInfo.registerLimits.temporary = .{
        .start = 8,
        .end = 8 + 124,
    };
    context.genInfo.registerLimits.preserved = .{
        .start = 8 + 124,
        .end = 8 + 124 + 124,
    };

    try context.genInfo.activeRegisters.ensureTotalCapacityPrecise(
        allocator,
        context.genInfo.registerLimits.preserved.end,
    );
    context.genInfo.activeRegisters.items.len = context.genInfo.registerLimits.preserved.end;
    @memset(context.genInfo.activeRegisters.items, .{});

    inline for (paramLimits.start..paramLimits.end) |index| {
        context.genInfo.activeRegisters.items[index].active = true;
    }

    allocateRegisters(context);
}

fn allocateRegisters(context: *Context) void {
    for (context.genInfo.registers.items, 0..) |info, index| {
        std.debug.print(":: r{d}  {?d}\n", .{ index, info.lastUsedInstrIndex });
    }
}
