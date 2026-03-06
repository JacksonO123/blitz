const std = @import("std");
const Allocator = std.mem.Allocator;

const blitz = @import("../blitz.zig");
const Context = blitz.context.Context;
const codegen = blitz.codegen;

pub fn initMetadataNoop(_: Allocator, _: *Context) Allocator.Error!void {}

pub fn allocateRegistersNoop(
    _: Allocator,
    _: *Context,
    _: []codegen.Instr,
    _: usize,
    _: *u64,
) Allocator.Error!void {}
