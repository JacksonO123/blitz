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

pub const NUM_STRUCT_CONTENT_REGISTERS = 16; // arbitrary

// behavior
pub const ARRAY_INIT_UNROLL_LIMIT = 1024;
