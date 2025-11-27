const builtin = @import("builtin");
const std = @import("std");
const utils = @import("utils.zig");
const compiler = @import("compiler.zig");
const objdump = @import("bzc_objdump.zig");

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

    const saveNew = strArrContains(args, "--save");
    const fromObjDump = strArrContains(args, "--from-objdump");

    if (saveNew and fromObjDump) {
        return error.CannotSaveFromObjdump;
    }

    var printBuf: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&printBuf);
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

    var recordsDir = try std.fs.cwd().openDir(RECORDS_DIR, .{});
    defer recordsDir.close();

    var buf: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;

    if (saveNew) {
        try printWriter.print("Saving [{s}]\n", .{outName});

        const outFile = try recordsDir.createFile(outName, .{});
        defer outFile.close();

        var fileBufferedWriter = outFile.writer(&buf);
        defer fileBufferedWriter.end() catch {};
        const fileWriter = &fileBufferedWriter.interface;

        try compiler.compile(allocator, code, printWriter, fileWriter, .None, .PlainText);
        return;
    }

    try printWriter.writeByte('\n');

    const newContents = if (fromObjDump) a: {
        var binaryList: std.ArrayList(u8) = .empty;
        defer binaryList.deinit(allocator);
        var binaryWriter = binaryList.writer(allocator).adaptToNewApi(&buf);
        const binaryWriterInterface = &binaryWriter.new_interface;
        try compiler.compile(allocator, code, printWriter, binaryWriterInterface, .None, .Binary);
        try binaryWriterInterface.flush();

        var textList: std.ArrayList(u8) = .empty;
        var textWriter = textList.writer(allocator).adaptToNewApi(&buf);
        const textWriterInterface = &textWriter.new_interface;
        try objdump.printBytecode(binaryList.items, textWriterInterface);
        try textWriterInterface.flush();

        break :a try textList.toOwnedSlice(allocator);
    } else a: {
        var textList: std.ArrayList(u8) = .empty;
        defer textList.deinit(allocator);
        var textWriter = textList.writer(allocator).adaptToNewApi(&buf);
        const textWriterInterface = &textWriter.new_interface;
        try compiler.compile(allocator, code, printWriter, textWriterInterface, .None, .PlainText);
        try textWriterInterface.flush();

        break :a try textList.toOwnedSlice(allocator);
    };
    defer allocator.free(newContents);

    const outFile = recordsDir.openFile(outName, .{}) catch |err| {
        try printWriter.print("{s} :: {s}/{s}\n", .{ @errorName(err), RECORDS_DIR, outName });
        return;
    };
    defer outFile.close();
    const origContents = try outFile.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(origContents);

    var missmatchStart: ?usize = null;
    for (newContents, 0..) |char, index| {
        if (index < origContents.len) {
            const missmatch = char != origContents[index] and char != '\n';

            if (!missmatch) a: {
                const start = missmatchStart orelse break :a;
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

fn strArrContains(arr: [][:0]u8, value: []const u8) bool {
    for (arr) |item| {
        if (std.mem.eql(u8, item, value)) return true;
    }
    return false;
}
