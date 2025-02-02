const blitz = @import("root").blitz;
const ast = blitz.ast;

pub fn numberLength(num: usize) usize {
    if (num == 0) return 1;
    const float: f64 = @floatFromInt(num);
    return @as(usize, @intFromFloat(@floor(@log10(@abs(float))) + 1));
}

pub fn numberIsFloat(num: ast.AstNumberVariants) bool {
    return switch (num) {
        .F8, .F16, .F32, .F64, .F128 => true,
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

pub fn sameType(num1: ast.AstNumberVariants, num2: ast.AstNumberVariants) bool {
    if (numberIsFloat(num1) != numberIsFloat(num2)) return false;
    if (numberIsInt(num1) != numberIsInt(num2)) return false;
    if (numberIsUInt(num1) != numberIsUInt(num2)) return false;
    return true;
}

/// expects both nums to be same layout (int, float, uint, usize)
/// does not check for errors
pub fn largestNumType(num1: ast.AstNumberVariants, num2: ast.AstNumberVariants) ast.AstNumberVariants {
    return switch (num1) {
        .F8 => return num2,
        .F16 => return switch (num2) {
            .F8 => return num1,
            else => return num2,
        },
        .F32 => return switch (num2) {
            .F8, .F16 => return num1,
            else => return num2,
        },
        .F64 => return switch (num2) {
            .F8, .F16, .F32 => return num1,
            else => return num2,
        },
        .F128 => return num1,

        .I8 => return num2,
        .I16 => return switch (num2) {
            .I8 => return num1,
            else => return num2,
        },
        .I32 => return switch (num2) {
            .I8, .I16 => return num1,
            else => return num2,
        },
        .I64 => return switch (num2) {
            .I8, .I16, .I32 => return num1,
            else => return num2,
        },
        .I128 => return num1,

        .U8 => return num2,
        .U16 => return switch (num2) {
            .U8 => return num1,
            else => return num2,
        },
        .U32 => return switch (num2) {
            .U8, .U16 => return num1,
            else => return num2,
        },
        .U64 => return switch (num2) {
            .U8, .U16, .U32 => return num1,
            else => return num2,
        },
        .U128 => return num1,

        .USize => return num1,
    };
}
