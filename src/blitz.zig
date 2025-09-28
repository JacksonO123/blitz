pub const ast = @import("ast.zig");
pub const scanner = @import("scan.zig");
pub const tokenizer = @import("tokenizer.zig");
pub const clone = @import("clone.zig");
pub const number = @import("number.zig");
pub const string = @import("string.zig");
pub const utils = @import("utils.zig");
pub const free = @import("free.zig");
pub const logger = @import("logger.zig");
pub const builtins = @import("builtins.zig");
pub const codegen = @import("codegen.zig");
pub const version = @import("version.zig");
pub const compInfo = @import("comp_info.zig");
pub const debug = @import("debug.zig");
pub const vmInfo = @import("vm_info.zig");

pub const Context = struct {
    const Self = @This();

    logger: *logger.Logger,
    tokens: *tokenizer.TokenUtil,
    compInfo: *compInfo.CompInfo,
    scanInfo: *scanner.ScanInfo,
    genInfo: *codegen.GenInfo,
    code: []u8,

    pub fn getTokString(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start..tok.end];
    }

    pub fn getTokStringDropQuotes(self: Self, tok: tokenizer.Token) []const u8 {
        return self.code[tok.start + 1 .. tok.end - 1];
    }
};
