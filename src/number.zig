const blitz = @import("blitz.zig");
const ast = blitz.ast;

pub fn numberLength(num: usize) usize {
    if (num == 0) return 1;
    const float: f64 = @floatFromInt(num);
    return @as(usize, @intFromFloat(@floor(@log10(@abs(float))) + 1));
}

pub fn numberIsFloat(num: ast.AstNumberVariants) bool {
    return switch (num) {
        .F32, .F64, .F128 => true,
        else => false,
    };
}

pub fn numberIsUInt(num: ast.AstNumberVariants) bool {
    return switch (num) {
        .U8, .U16, .U32, .U64, .U128 => true,
        else => false,
    };
}

pub fn numberIsInt(num: ast.AstNumberVariants) bool {
    return switch (num) {
        .I8, .I16, .I32, .I64, .I128 => true,
        else => false,
    };
}
