// semantic representation for locations on stack
// in interpreter runtime, for other cases just use u64
pub const PointerType = u64;
pub const RegisterType = u64;
pub const RegisterNumber = u8;
pub const NUM_REGISTERS = 256;
pub const REGISTER_SIZE = 8; // bytes
pub const StartStackType = u32;
pub const startStackSize = @sizeOf(StartStackType);
pub const VM_INFO_BYTECODE_LEN = 1 + startStackSize;
