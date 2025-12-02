const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const compilerName = if (optimize == .Debug) "blitzc-debug" else "blitzc";
    const objdumpName = if (optimize == .Debug) "bzc-objdump-debug" else "bzc-objdump";
    const interpreterName = if (optimize == .Debug) "blitz-debug" else "blitz";
    const diffName = if (optimize == .Debug) "blitz-diff-debug" else "blitz-diff";

    const compilerExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = compilerName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/compiler.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const objdumpExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = objdumpName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bzc_objdump.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const interpreterExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = interpreterName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const diffExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = diffName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/diff_bytecode.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(compilerExe);
    b.installArtifact(objdumpExe);
    b.installArtifact(interpreterExe);
    b.installArtifact(diffExe);
}
