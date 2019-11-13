const std = @import("std");
const toml = @import("toml.zig");
const t = std.testing;

fn kib(comptime mul: comptime_int) comptime_int {
    return 1024 * mul;
}

fn expectArray(comptime expected: var, actual: []toml.Value) void {
    t.expectEqual(expected.len, actual.len);
    const expectedArrChildType = @typeInfo(@typeOf(expected)).Array.child;
    const fieldName = switch (expectedArrChildType) {
        i64 => "Integer",
        f64 => "Float",
        bool => "Boolean",
        []const u8 => "String",
        else => @compileError("Type " ++ @typeName(expectedArrChildType) ++ " not allowed."),
    };
    for (actual) |ac, aci| {
        const exp = expected[aci];
        const act = @field(ac, fieldName);
        if (expectedArrChildType == []const u8) {
            t.expect(std.mem.eql(u8, exp, act));
        } else t.expect(exp == act);
    }
}

test "Example TOML File" {
    // var buf: [kib(18)]u8 = undefined;
    var buf: [kib(32)]u8 = undefined;
    const allocator = &std.heap.FixedBufferAllocator.init(&buf).allocator;
    const table = try toml.Table.fromString(allocator, @embedFile("tests/example.toml"));
    defer table.deinit();
    t.expectEqual(@as(usize, 5), table.count());

    const database = table.get("database").?.SubTable;
    expectArray([_]i64{ 8001, 8001, 8002 }, database.get("ports").?.Array);
    t.expectEqual(@as(i64, 5000), database.get("connection_max").?.Integer);
    t.expectEqualSlices(u8, "192.168.1.1", database.get("server").?.String);
    t.expectEqual(true, database.get("enabled").?.Boolean);

    const clients = table.get("clients").?.SubTable;
    expectArray([_][]const u8{ "alpha", "omega" }, clients.get("hosts").?.Array);
    const clientsData = clients.get("data").?.Array;
    expectArray([_][]const u8{ "gamma", "delta" }, clientsData[0].Array);
    expectArray([_]i64{ 1, 2 }, clientsData[1].Array);
    const servers = table.get("servers").?.SubTable;
    t.expectEqual(@as(usize, 2), servers.count());
    const alphaServer = servers.get("alpha").?.SubTable;
    t.expectEqualSlices(u8, "10.0.0.1", alphaServer.get("ip").?.String);
    t.expectEqualSlices(u8, "eqdc10", alphaServer.get("dc").?.String);
    const betaServer = servers.get("beta").?.SubTable;
    t.expectEqualSlices(u8, "10.0.0.2", betaServer.get("ip").?.String);
    t.expectEqualSlices(u8, "eqdc10", betaServer.get("dc").?.String);
    const owner = table.get("owner").?.SubTable;
    t.expectEqualSlices(u8, "Haze Booth", owner.get("name").?.String);
    t.expectEqualSlices(u8, "Zig Toml Example", table.get("title").?.String);
}
