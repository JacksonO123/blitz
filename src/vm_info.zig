const std = @import("std");

const blitz = @import("blitz.zig");
const codegen = blitz.codegen;
const version = @import("version.zig").VERSION;

// semantic representation for locations on stack
// in interpreter runtime, for other cases just use u64
pub const RegisterType = u64;
pub const REGISTER_SIZE = @sizeOf(RegisterType);
pub const POINTER_SIZE = REGISTER_SIZE;
pub const RuntimeRegister = u8;
pub const NUM_REGISTERS = std.math.maxInt(RuntimeRegister);

pub const StartStackType = u32;
pub const START_STACK_TYPE_SIZE = @sizeOf(StartStackType);
pub const VM_INFO_BYTECODE_LEN = @sizeOf(@TypeOf(version)) + START_STACK_TYPE_SIZE;
pub const TempRegister = u32;
pub const LabelType = u32;

// temporary and preserved registers split
// remaining register space equally
// (256 - 8) / 2 = 124
pub const bytecodeRegLimits: codegen.BackendRegLimits = .{
    .params = .{
        .start = 0,
        .end = 8,
    },
    .temporary = .{
        .start = 8,
        .end = 8 + 124,
    },
    .preserved = .{
        .start = 8 + 124,
        .end = 8 + 124 + 124,
    },
};

// behavior
pub const ARRAY_INIT_UNROLL_LIMIT = 1024;
