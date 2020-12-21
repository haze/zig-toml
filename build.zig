const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("zig-toml", "src/toml.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("src/tests.zig");
    var debug_tests = b.addTest("src/toml.zig");
    main_tests.setBuildMode(mode);
    debug_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
    test_step.dependOn(&debug_tests.step);
}
