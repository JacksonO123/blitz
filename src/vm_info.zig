const std = @import("std");

const blitz = @import("blitz.zig");
const codegen = blitz.codegen;
const utils = blitz.utils;
const version = @import("version.zig").VERSION;

// semantic representation for locations on stack
// in interpreter runtime, for other cases just use u64
pub const RegisterType = u64;
pub const REGISTER_SIZE = @sizeOf(RegisterType);
pub const POINTER_SIZE = REGISTER_SIZE;
pub const RuntimeRegister = u8;
pub const NUM_REGISTERS = std.math.maxInt(RuntimeRegister);

// 4B (stack start size) + 4B (instr start ptr) + 1B (version)
pub const BYTECODE_HEADER_LEN = 9;
pub const INSTR_START_PTR_LOCATION = 0;
pub const STACK_START_LOCATION = 4;
pub const VERSION_LOCATION = 8;
pub const PADDED_BYTECODE_HEADER_LEN = BYTECODE_HEADER_LEN + utils.calculatePadding(
    BYTECODE_HEADER_LEN,
    POINTER_SIZE,
);
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

pub const ADD_SP_BASE_OFFSET = 0;
pub const PUSH_REG_BASE_OFFSET = 1;
pub const PUSH_LR_BASE_OFFSET = 2;

// behavior
pub const ARRAY_INIT_UNROLL_LIMIT = 1024;
