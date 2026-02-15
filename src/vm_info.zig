const std = @import("std");
// semantic representation for locations on stack
// in interpreter runtime, for other cases just use u64
pub const RegisterType = u64;
pub const REGISTER_SIZE = @sizeOf(RegisterType);
pub const POINTER_SIZE = REGISTER_SIZE;
pub const RuntimeRegister = u8;
pub const NUM_REGISTERS = std.math.maxInt(RuntimeRegister);

pub const StartStackType = u32;
pub const START_STACK_SIZE = @sizeOf(StartStackType);
pub const VM_INFO_BYTECODE_LEN = 1 + START_STACK_SIZE;
pub const TempRegister = u32;
pub const LabelType = u32;

pub const NUM_PARAM_REGISTERS = 8;
pub const NUM_TEMP_REGISTERS = 64;
pub const NUM_PRESERVE_REGISTERS = NUM_REGISTERS - NUM_TEMP_REGISTERS - NUM_PARAM_REGISTERS;

pub const PARAM_REGISTER_START = 0;
pub const TEMP_REGISTER_START = NUM_PARAM_REGISTERS;
pub const PRESERVE_REGISTER_START = NUM_PARAM_REGISTERS + NUM_TEMP_REGISTERS;

// one greater than last available
pub const PARAM_REGISTER_END = NUM_PARAM_REGISTERS;
pub const TEMP_REGISTER_END = NUM_PARAM_REGISTERS + NUM_TEMP_REGISTERS;
pub const PRESERVE_REGISTER_END = NUM_PARAM_REGISTERS + NUM_TEMP_REGISTERS + NUM_PRESERVE_REGISTERS;

// behavior
pub const ARRAY_INIT_UNROLL_LIMIT = 1024;
