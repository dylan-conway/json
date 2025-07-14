const std = @import("std");
const foo = @import("foo");
const json = @import("./json.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const root_pkg_json_contents = std.fs.cwd().readFileAlloc(allocator, "package.json", std.math.maxInt(u64)) catch |err| {
        std.log.err("failed to read package.json: {s}", .{@errorName(err)});
        std.process.exit(1);
    };
    defer allocator.free(root_pkg_json_contents);

    var json_parser: json.Parser(u8) = .init(allocator, root_pkg_json_contents);
    defer json_parser.deinit();

    const root_pkg_json = try json_parser.parse().unwrap();
    defer root_pkg_json.deinit();

    const _num: f64 = 0;

    std.debug.print("{}, {}, {}, {}\n", .{
        std.math.isPositiveZero(_num),
        std.math.isNegativeZero(_num),
        std.math.isPositiveZero(-_num),
        std.math.isNegativeZero(-_num),
    });

    var test_arr: std.ArrayList(u8) = .init(allocator);
    defer test_arr.deinit();

    try test_arr.ensureTotalCapacityPrecise(1);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
