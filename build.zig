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
    const diffName = "blitz-diff";

    const blitzModule = b.addModule("blitz", .{
        .root_source_file = b.path("src/blitz.zig"),
        .target = target,
    });

    const compilerExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = compilerName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bin/compiler.zig"),
            .target = target,
            .optimize = mode,
        }),
    });
    compilerExe.root_module.addImport("blitz", blitzModule);

    const objdumpExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = objdumpName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bin/bzc_objdump.zig"),
            .target = target,
            .optimize = mode,
        }),
    });
    objdumpExe.root_module.addImport("blitz", blitzModule);

    const interpreterExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = interpreterName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bin/interpreter.zig"),
            .target = target,
            .optimize = mode,
        }),
    });
    interpreterExe.root_module.addImport("blitz", blitzModule);

    const diffExe = b.addExecutable(.{
        .use_llvm = true, // for debugger variables
        .name = diffName,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bin/diff_bytecode.zig"),
            .target = target,
            .optimize = .Debug,
        }),
    });
    diffExe.root_module.addImport("blitz", blitzModule);

    b.installArtifact(compilerExe);
    b.installArtifact(objdumpExe);
    b.installArtifact(interpreterExe);
    b.installArtifact(diffExe);

    const unitTests = b.addTest(.{
        .root_module = blitzModule,
    });

    const runUnitTests = b.addRunArtifact(unitTests);

    const testStep = b.step("test", "Run unit tests");
    testStep.dependOn(&runUnitTests.step);

    const runCompilerExe = b.addRunArtifact(compilerExe);
    const runCompilerStep = b.step("run-compiler", "Run the compiler");
    runCompilerStep.dependOn(&runCompilerExe.step);

    const runInterpreterExe = b.addRunArtifact(interpreterExe);
    const runInterpreterStep = b.step("run-interpreter", "Run the interpreter");
    runInterpreterStep.dependOn(&runInterpreterExe.step);

    if (b.args) |args| {
        runCompilerExe.addArgs(args);
        runInterpreterExe.addArgs(args);
    }
}
