const builtin = @import("builtin");
const std = @import("std");
const diffBytecode = @import("diff_bytecode.zig");
const utils = @import("utils.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buf);
    defer stdout.end() catch {};
    const writer = &stdout.interface;

    const recordDirectory = try std.fs.cwd().openDir(diffBytecode.RECORDS_DIR, .{});
    const featureDirectory = try std.fs.cwd().openDir(diffBytecode.FEATURE_DIR, .{
        .iterate = true,
    });

    var it = featureDirectory.iterate();
    while (try it.next()) |item| {
        if (item.kind != .file) continue;

        const refFileName = try diffBytecode.getRefName(allocator, item.name, writer);

        const sourceFileName = try std.fmt.allocPrint(allocator, "{s}/{s}", .{
            diffBytecode.FEATURE_DIR,
            item.name,
        });

        const fileExists = try doesFileExist(refFileName, recordDirectory);
        try diffBytecode.diffBytecode(
            allocator,
            diffBytecode.RECORDS_DIR,
            sourceFileName,
            !fileExists,
            false,
        );
    }
}

fn doesFileExist(name: []const u8, dir: std.fs.Dir) !bool {
    const file = dir.openFile(name, .{}) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    file.close();
    return true;
}
