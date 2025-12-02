const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const forProd = b.option(bool, "prod", "Production build") != null;
    const mode: std.builtin.OptimizeMode = if (forProd) .ReleaseFast else .Debug;

    const compilerName = if (mode == .Debug) "blitzc-debug" else "blitzc";
    const objdumpName = if (mode == .Debug) "bzc-objdump-debug" else "bzc-objdump";
    const interpreterName = if (mode == .Debug) "blitz-debug" else "blitz";
    const diffName = if (mode == .Debug) "blitz-diff-debug" else "blitz-diff";

    const compilerExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = compilerName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/compiler.zig"),
            .target = target,
            .optimize = mode,
        }),
    });

    const objdumpExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = objdumpName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bzc_objdump.zig"),
            .target = target,
            .optimize = mode,
        }),
    });

    const interpreterExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = interpreterName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter.zig"),
            .target = target,
            .optimize = mode,
        }),
    });

    const diffExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = diffName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/diff_bytecode.zig"),
            .target = target,
            .optimize = mode,
        }),
    });

    b.installArtifact(compilerExe);
    b.installArtifact(objdumpExe);
    b.installArtifact(interpreterExe);
    b.installArtifact(diffExe);
}
