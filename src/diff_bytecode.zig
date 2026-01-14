const builtin = @import("builtin");
const std = @import("std");
const utils = @import("utils.zig");
const compiler = @import("compiler.zig");
const objdump = @import("bzc_objdump.zig");
const Writer = std.Io.Writer;
const Allocator = std.mem.Allocator;

pub const DIFF_DIR = "bytecode-diffs";
pub const RECORDS_DIR = DIFF_DIR ++ "/records";
pub const FEATURE_DIR = DIFF_DIR ++ "/features";

const TERMINAL_COLORS = .{
    .reset = "\x1b[0m",
    .red = "\x1b[30;41m",
    .green = "\x1b[30;42m",
};

pub fn main() !void {
    const dbg = builtin.mode == .Debug;
    var gp = std.heap.GeneralPurposeAllocator(.{ .safety = dbg }){};
    defer _ = gp.deinit();
    const allocator = gp.allocator();

    var buf: [1024]u8 = undefined;
    var stdio = std.fs.File.stdout().writer(&buf);
    const writer = &stdio.interface;
    defer writer.flush() catch {};

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        try writer.writeAll("No input file");
        return error.NoInputFile;
    }

    const flagStructure = .{
        .{"--help"},
        .{"--save"},
        .{"--from-objdump"},
        .{ "--record-path", "Path to record directory" },
    };

    const flagMap = std.StaticStringMap(usize).initComptime(formatFlagStructure(flagStructure));

    const hasHelp = strArrContains(args, "--help", &flagMap) != null;
    if (hasHelp) {
        if (args.len > 2) {
            return error.TooManyArguments;
        }

        try printStructure(flagStructure, writer);
        return;
    }

    const saveNew = strArrContains(args, "--save", &flagMap) != null;
    const fromObjDump = strArrContains(args, "--from-objdump", &flagMap) != null;
    const recordPath = if (strArrContains(args, "--record-path", &flagMap)) |index|
        args[index + 1]
    else
        RECORDS_DIR;

    if (saveNew and fromObjDump) {
        return error.CannotSaveFromObjdump;
    }

    try diffBytecode(allocator, recordPath, args[1], saveNew, fromObjDump);
}

pub fn diffBytecode(
    allocator: Allocator,
    recordPath: []const u8,
    path: []const u8,
    saveNew: bool,
    fromObjDump: bool,
) !void {
    var printBuf: [utils.BUFFERED_WRITER_SIZE]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&printBuf);
    defer stdout.end() catch {};
    const printWriter = &stdout.interface;

    try printWriter.print("opening [{s}]\n", .{path});
    const code = try utils.readRelativeFile(allocator, path);
    defer allocator.free(code);

    const outName = try getRefName(allocator, path, printWriter);
    defer allocator.free(outName);

    var recordsDir = try std.fs.cwd().openDir(recordPath, .{});
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
        var binaryAllocating = Writer.Allocating.init(allocator);
        defer binaryAllocating.deinit();
        const binaryWriter = &binaryAllocating.writer;
        try compiler.compile(allocator, code, printWriter, binaryWriter, .None, .Binary);
        try binaryWriter.flush();

        var textAllocating = Writer.Allocating.init(allocator);
        defer textAllocating.deinit();
        const textWriter = &textAllocating.writer;
        const ownedBinarySlice = try binaryAllocating.toOwnedSlice();
        defer allocator.free(ownedBinarySlice);
        try objdump.printBytecode(ownedBinarySlice, textWriter);
        try textWriter.flush();

        break :a try textAllocating.toOwnedSlice();
    } else a: {
        var textAllocating = Writer.Allocating.init(allocator);
        defer textAllocating.deinit();
        const textWriter = &textAllocating.writer;
        try compiler.compile(allocator, code, printWriter, textWriter, .None, .PlainText);
        try textWriter.flush();

        break :a try textAllocating.toOwnedSlice();
    };
    defer allocator.free(newContents);

    const outFile = recordsDir.openFile(outName, .{}) catch |err| {
        try printWriter.print("{s} :: {s}/{s}\n", .{ @errorName(err), recordPath, outName });
        return;
    };
    defer outFile.close();
    const origContents = try outFile.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(origContents);

    try writeDiff(newContents, origContents, printWriter);
    try printWriter.writeByte('\n');
}

pub fn getRefName(allocator: Allocator, path: []const u8, printWriter: *Writer) ![]const u8 {
    const lastSlash = if (lastIndexOf(path, '/')) |index| index + 1 else 0;
    const periodIndex = std.ascii.indexOfIgnoreCase(path, ".") orelse {
        try printWriter.writeAll("Expected input file to be .blitz");
        return error.NoBlitzFile;
    };
    const res = try std.fmt.allocPrint(allocator, "{s}.bzc.txt", .{path[lastSlash..periodIndex]});
    return res;
}

fn writeDiff(newContents: []u8, origContents: []u8, printWriter: *Writer) !void {
    var missmatchStart: ?usize = null;
    for (newContents, 0..) |char, index| {
        if (index == origContents.len) {
            try writeDiffOverflow(newContents[index..], printWriter);
            break;
        }

        const missmatch = char != origContents[index];

        if (!missmatch) a: {
            const start = missmatchStart orelse break :a;
            try writeDiffActual(origContents, start, index, printWriter);
            missmatchStart = null;
        } else if (missmatchStart == null) {
            missmatchStart = index;
        }

        if (missmatch) {
            try printWriter.writeAll(TERMINAL_COLORS.red);
            if (char == '\n') {
                try printWriter.writeAll("↵");
                try printWriter.writeAll(TERMINAL_COLORS.reset);
                try printWriter.writeByte('\n');
                continue;
            }

            try printWriter.writeByte(char);
            try printWriter.writeAll(TERMINAL_COLORS.reset);
            continue;
        }

        try printWriter.writeByte(char);

        continue;
    }
}

fn writeDiffOverflow(overflowContents: []u8, printWriter: *Writer) !void {
    try printWriter.writeAll(TERMINAL_COLORS.red);

    for (overflowContents) |char| {
        if (char == '\n') {
            try printWriter.writeAll("↵");
            try printWriter.writeAll(TERMINAL_COLORS.reset);
            try printWriter.writeByte('\n');
            try printWriter.writeAll(TERMINAL_COLORS.red);
            continue;
        }

        try printWriter.writeByte(char);
    }
    try printWriter.writeAll(TERMINAL_COLORS.reset);
}

fn writeDiffActual(origContents: []u8, start: usize, end: usize, printWriter: *Writer) !void {
    try printWriter.writeAll(TERMINAL_COLORS.green);
    for (origContents[start..end]) |origChar| {
        if (origChar == '\n') {
            try printWriter.writeAll(TERMINAL_COLORS.reset);
            try printWriter.writeByte('\n');
            try printWriter.writeAll(TERMINAL_COLORS.green);
            continue;
        }

        try printWriter.writeByte(origChar);
    }
    try printWriter.writeAll(TERMINAL_COLORS.reset);
}

fn lastIndexOf(text: []const u8, char: u8) ?usize {
    var i = text.len - 1;
    while (i > 0) : (i -= 1) {
        if (text[i] == char) return i;
    }
    if (text[0] == char) return 0;
    return null;
}

fn strArrContains(
    arr: [][:0]u8,
    value: []const u8,
    flagMap: *const std.StaticStringMap(usize),
) ?usize {
    var i: usize = 0;
    while (i < arr.len) : (i += 1) {
        if (std.mem.eql(u8, arr[i], value)) return i;
        if (flagMap.get(arr[i])) |width| i += width;
    }
    return null;
}

const FlagInfo = struct { []const u8, usize };

fn formatFlagStructure(comptime structure: anytype) [structure.len]FlagInfo {
    var thing: [structure.len]FlagInfo = undefined;

    inline for (structure, 0..) |tuple, index| {
        thing[index] = .{ tuple[0], tuple.len - 1 };
    }

    return thing;
}

fn printStructure(comptime structure: anytype, writer: *Writer) !void {
    const detailPad = 20;
    inline for (structure) |item| {
        try writer.writeAll(item[0]);

        if (item.len > 1) {
            try writer.writeAll((" " ** (detailPad - item[0].len)) ++ "[");
            inline for (item, 0..) |detail, index| {
                if (index == 0) continue;

                try writer.writeAll(detail);
                if (index < item.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeByte(']');
        }

        try writer.writeByte('\n');
    }
}
