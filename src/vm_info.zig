const std = @import("std");

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

// TODO - move
pub const NUM_PARAM_REGISTERS = 8;
pub const NUM_TEMP_REGISTERS = 124;
pub const NUM_PRESERVE_REGISTERS = 124;

// TODO - move
pub const PARAM_REGISTER_START = 0;
pub const TEMP_REGISTER_START = 8;
pub const PRESERVE_REGISTER_START = 132;

// behavior
pub const ARRAY_INIT_UNROLL_LIMIT = 1024;
