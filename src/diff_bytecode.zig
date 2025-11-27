const builtin = @import("builtin");
const std = @import("std");
const utils = @import("utils.zig");
const compiler = @import("compiler.zig");

const RECORDS_DIR = "bytecode-records";

const terminalColors = .{
    .reset = "\x1b[0m",
    .red = "\x1b[30;41m",
    .green = "\x1b[30;42m",
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    // TODO - extend later
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        return error.NoInputFile;
    }

    var saveNew = false;
    if (args.len > 2) {
        for (args[2..]) |arg| {
            if (std.mem.eql(u8, arg, "--save")) {
                saveNew = true;
            }
        }
    }

    var buffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    defer stdout.end() catch {};
    const printWriter = &stdout.interface;

    const path = args[1];
    try printWriter.print("opening [{s}]\n", .{path});
    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    const lastSlash = if (lastIndexOf(path, '/')) |index| index + 1 else 0;
    const periodIndex = std.ascii.indexOfIgnoreCase(path, ".") orelse {
        try printWriter.writeAll("Expected input file to be .blitz");
        return;
    };

    const outName = try std.fmt.allocPrint(allocator, "{s}.bzc.txt", .{path[lastSlash..periodIndex]});
    defer allocator.free(outName);

    try printWriter.print("Saving [{s}]\n", .{outName});

    var recordsDir = try std.fs.cwd().openDir(RECORDS_DIR, .{});
    defer recordsDir.close();

    if (saveNew) {
        const outFile = try recordsDir.createFile(outName, .{});
        defer outFile.close();

        var fileBuffer: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
        var fileBufferedWriter = outFile.writer(&fileBuffer);
        defer fileBufferedWriter.end() catch {};
        const fileWriter = &fileBufferedWriter.interface;

        try compiler.compile(allocator, code, printWriter, fileWriter, .None, .PlainText);
        return;
    }

    var listBuf: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var list: std.ArrayList(u8) = .empty;
    defer list.deinit(allocator);
    var listWriter = list.writer(allocator).adaptToNewApi(&listBuf);
    const listWriterInterface = &listWriter.new_interface;
    try compiler.compile(allocator, code, printWriter, listWriterInterface, .None, .PlainText);

    const outFile = recordsDir.openFile(outName, .{}) catch |err| {
        try printWriter.print("{s} :: {s}/{s}\n", .{ @errorName(err), RECORDS_DIR, outName });
        return;
    };
    defer outFile.close();
    const origContents = try outFile.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(origContents);

    try listWriterInterface.flush();

    var missmatchStart: ?usize = null;
    for (list.items, 0..) |char, index| {
        if (index < origContents.len) {
            const missmatch = char != origContents[index] and char != '\n';

            if (!missmatch) {
                if (missmatchStart) |start| {
                    try printWriter.writeAll(terminalColors.green);
                    for (origContents[start..index]) |origChar| {
                        if (origChar == '\n') {
                            try printWriter.writeAll(terminalColors.reset);
                        }
                        try printWriter.writeByte(origChar);
                        if (origChar == '\n') {
                            try printWriter.writeAll(terminalColors.green);
                        }
                    }
                    try printWriter.writeAll(terminalColors.reset);
                    missmatchStart = null;
                }
            } else if (missmatchStart == null) {
                missmatchStart = index;
            }

            if (missmatch) {
                try printWriter.writeAll(terminalColors.red);
            }
            try printWriter.writeByte(char);
            if (missmatch) {
                try printWriter.writeAll(terminalColors.reset);
            }
            continue;
        }

        if (char != '\n') {
            try printWriter.writeAll(terminalColors.red);
        }
        try printWriter.writeByte(char);
        if (char != '\n') {
            try printWriter.writeAll(terminalColors.reset);
        }
    }
}

fn lastIndexOf(text: []u8, char: u8) ?usize {
    var i = text.len - 1;
    while (i > 0) : (i -= 1) {
        if (text[i] == char) return i;
    }
    if (text[0] == char) return 0;
    return null;
}
