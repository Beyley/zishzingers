const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zishzingers",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    // In non-debug modes we use c_allocator
    if (optimize != .Debug)
        exe.linkLibC();

    if (b.option(bool, "use_llvm", "Uses LLVM for compilation")) |use_llvm|
        exe.use_llvm = use_llvm;
    if (b.option(bool, "use_lld", "Uses LLD for compilation")) |use_lld|
        exe.use_lld = use_lld;

    exe.root_module.addImport("clap", b.dependency("clap", .{}).module("clap"));

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
