const std = @import("std");
const toml = @import("toml.zig");
const mem = std.mem;
const t = std.testing;

const warn = std.debug.warn;
const FA = std.debug.failing_allocator;
const GA = std.debug.global_allocator;

fn printTokens(tokens: []toml.Token) void {
    for (tokens) |token| {
        switch (token) {
            .TableDefinition => |tableName| warn("[{}]\n", tableName),
            .KeyValue => |kv| warn("KV{{{} = {}}}\n", kv.key, kv.value),
        }
    }
}

fn printTokensFor(source: []const u8) !void {
    warn("\n");
    var parser = toml.Parser.init(GA, source);
    const tokens = try parser.lex();
    warn("Got {} tokens:\n", tokens.len);
    printTokens(tokens);
}

fn quoted(comptime source: []const u8) []const u8 {
    return "\"" ++ source ++ "\"";
}

fn singleQuoted(comptime source: []const u8) []const u8 {
    return "'" ++ source ++ "'";
}

fn tokensFor(allocator: *std.mem.Allocator, source: []const u8) ![]toml.Token {
    return toml.Parser.init(allocator, source).lex();
}

test "Comment" {
    const file =
        \\# This is a full-line comment
        \\key = "value"  # This is a comment at the end of a line
        \\another = "# This is not a comment"
    ;
    const tokens = try tokensFor(GA, file);
    t.expect(tokens[0].KeyValue.eqlBare("key", quoted("value")));
    t.expect(tokens[1].KeyValue.eqlBare("another", quoted("# This is not a comment")));
}

test "Key/Value Pair" {
    const file =
        \\key = "value"
    ;
    const invalidFile =
        \\key = # INVALID
    ;
    const invalidFile2 =
        \\first = "Haze" last = "Booth" # INVALID
    ;
    const fileTokens = try tokensFor(GA, file);
    t.expect(fileTokens[0].KeyValue.eqlBare("key", quoted("value")));
    t.expectError(error.BadValue, tokensFor(GA, invalidFile));
    t.expectError(error.BadValue, tokensFor(GA, invalidFile2));
}

test "Keys" {
    // bare keys
    const bareKeysFile =
        \\key = "value"
        \\bare_key = "value"
        \\bare-key = "value"
        \\1234 = "value"
    ;
    const bareKeysTokens = try tokensFor(GA, bareKeysFile);
    t.expect(bareKeysTokens[0].KeyValue.eqlBare("key", quoted("value")));
    t.expect(bareKeysTokens[1].KeyValue.eqlBare("bare_key", quoted("value")));
    t.expect(bareKeysTokens[2].KeyValue.eqlBare("bare-key", quoted("value")));
    t.expect(bareKeysTokens[3].KeyValue.eqlBare("1234", quoted("value")));

    // quoted keys
    const quotedKeysFile =
        \\"127.0.0.1" = "value"
        \\"character encoding" = "value"
        \\"ʎǝʞ" = "value"
        \\'key2' = "value"
        \\'quoted "value"' = "value"
    ;
    const quotedKeysTokens = try tokensFor(GA, quotedKeysFile);
    t.expect(quotedKeysTokens[0].KeyValue.eqlBare(quoted("127.0.0.1"), quoted("value")));
    t.expect(quotedKeysTokens[1].KeyValue.eqlBare(quoted("character encoding"), quoted("value")));
    t.expect(quotedKeysTokens[2].KeyValue.eqlBare(quoted("ʎǝʞ"), quoted("value")));
    t.expect(quotedKeysTokens[3].KeyValue.eqlBare(singleQuoted("key2"), quoted("value")));
    t.expect(quotedKeysTokens[4].KeyValue.eqlBare(singleQuoted("quoted \"value\""), quoted("value")));

    // invalid bare keys
    const invalidBareKeys =
        \\= "no key name"  # INVALID
    ;
    t.expectError(error.UnexpectedCharacter, tokensFor(GA, invalidBareKeys));

    const validBareKeys =
        \\"" = "blank"     # VALID but discouraged
        \\'' = 'blank'     # VALID but discouraged
    ;
    const validBareKeyTokens = try tokensFor(GA, validBareKeys);
    t.expect(validBareKeyTokens[0].KeyValue.eqlBare(quoted(""), quoted("blank")));
    t.expect(validBareKeyTokens[1].KeyValue.eqlBare(singleQuoted(""), singleQuoted("blank")));

    // dotted keys

    const dottedKeys =
        \\name = "Orange"
        \\physical.color = "orange"
        \\physical.shape = "round"
        \\site."google.com" = true
    ;
    const dottedKeyTokens = try tokensFor(GA, dottedKeys);
    t.expect(dottedKeyTokens[0].KeyValue.eqlBare("name", quoted("Orange")));
    t.expect(dottedKeyTokens[1].KeyValue.eqlBare("physical.color", quoted("orange")));
    t.expect(dottedKeyTokens[2].KeyValue.eqlBare("physical.shape", quoted("round")));
    t.expect(dottedKeyTokens[3].KeyValue.eqlBare("site.\"google.com\"", "true"));
}

fn expectString(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: []const u8) !bool {
    const table = try toml.Parser.parse(allocator, source);
    switch (table.get(key).?) {
        .String => |str| return mem.eql(u8, str, expected),
        else => return false,
    }
}

test "String" {
    t.expect(try expectString(GA, "foo = 'im a string!' #comment", "foo", singleQuoted("im a string!")));
}

fn expectInteger(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: i64) bool {
    const table = toml.Parser.parse(allocator, source) catch |err| {
        std.debug.warn("expected: {}, got: {}\n", expected, err);
        return false;
    };
    switch (table.get(key).?) {
        .Integer => |i| return i == expected,
        else => return false,
    }
}

test "Integers" {
    t.expect(expectInteger(GA, "foo=+99 #comment", "foo", 99));
    t.expect(expectInteger(GA, "foo=42 #comment", "foo", 42));
    t.expect(expectInteger(GA, "foo=0 #comment", "foo", 0));
    t.expect(expectInteger(GA, "foo=-17 #comment", "foo", -17));
    t.expect(expectInteger(GA, "foo=1_000 #comment", "foo", 1000));
    t.expect(expectInteger(GA, "foo=5_349_221 #comment", "foo", 5349221));
    t.expect(expectInteger(GA, "foo=1_2_3_4_5 #comment", "foo", 12345));
    t.expect(expectInteger(GA, "foo=0xDEADBEEF #comment", "foo", 3735928559));
    t.expect(expectInteger(GA, "foo=0xdeadbeef #comment", "foo", 3735928559));
    t.expect(expectInteger(GA, "foo=0xdead_beef #comment", "foo", 3735928559));
    t.expect(expectInteger(GA, "foo=0o01234567 #comment", "foo", 342391));
    t.expect(expectInteger(GA, "foo=0o755 #comment", "foo", 493));
    t.expect(expectInteger(GA, "foo=0b11010110 #comment", "foo", 214));
}

fn expectFloat(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: f64) bool {
    const table = toml.Parser.parse(allocator, source) catch |err| {
        std.debug.warn("expected: {}, got: {}\n", expected, err);
        return false;
    };
    switch (table.get(key).?) {
        .Float => |f| return f == expected,
        else => return false,
    }
}

fn expectFloatApprox(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: f64, epsilon: f64) bool {
    const table = toml.Parser.parse(allocator, source) catch |err| {
        std.debug.warn("expected: {}, got: {}\n", expected, err);
        return false;
    };
    switch (table.get(key).?) {
        .Float => |f| return std.math.approxEq(f64, f, expected, epsilon),
        else => return false,
    }
}

test "Floats" {
    t.expect(expectFloat(GA, "foo=+1.0 #comment", "foo", 1.0));
    t.expect(expectFloat(GA, "foo=3.1415 #comment", "foo", 3.1415));
    t.expect(expectFloat(GA, "foo=-0.01 #comment", "foo", -0.01));
    // t.expect(expectFloatApprox(GA, "foo=-5e+22 #comment", "foo", -5e+22, 0.0000000000000000000001));
    t.expect(expectFloat(GA, "foo=1e06 #comment", "foo", 1e06));
    t.expect(expectFloat(GA, "foo=-2E-2 #comment", "foo", -2e-2));
    t.expect(expectFloat(GA, "foo=6.626e-34 #comment", "foo", 6.626e-34));
}

fn expectBoolean(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: bool) bool {
    const table = toml.Parser.parse(allocator, source) catch |err| {
        std.debug.warn("expected: {}, got: {}\n", expected, err);
        return false;
    };
    switch (table.get(key).?) {
        .Boolean => |b| return b == expected,
        else => return false,
    }
}

test "Boolean" {
    t.expect(expectBoolean(GA, "foo=true #comment", "foo", true));
    t.expect(expectBoolean(GA, "foo=false #comment", "foo", false));
}

fn expectArray(allocator: *mem.Allocator, source: []const u8, key: []const u8, expected: var) bool {
    const table = toml.Parser.parse(allocator, source) catch |err| {
        std.debug.warn("expected: {}, got: {}\n", expected, err);
        return false;
    };
    const ty = @typeOf(expected);
    if (@typeInfo(ty) != .Array) @compileError("expectArray requires an Array type");
    const concreteType = @typeInfo(ty).Array.child;
    switch (table.get(key).?) {
        .Array => |arr| {
            if (concreteType == i64) { // array of ints
                for (arr) |v, i| {
                    if (v.Integer != expected[i]) return false;
                }
            } else if (concreteType == bool) {
                for (arr) |v, i| {
                    if (v.Boolean != expected[i]) return false;
                }
            } else if (concreteType == []const u8) {
                for (arr) |v, i| {
                    if (!mem.eql(u8, v.String, expected[i])) return false;
                }
            } else {
                @compileError("expectArray does not support type " ++ @typeName(concreteType) ++ " yet");
            }
            return true;
        },
        else => return false,
    }
}

test "Arrays" {
    t.expect(expectArray(GA, "foo=[1, 2, 3] #comment", "foo", [_]i64{ 1, 2, 3 }));
    t.expect(expectArray(GA, "foo=[true, false, true, true] #comment", "foo", [_]bool{ true, false, true, true }));
    t.expect(expectArray(GA, "foo=['bruh moment', \"haze booth\"] #comment", "foo", [_][]const u8{ singleQuoted("bruh moment"), quoted("haze booth") }));
}

test "Inline Table" {
    const root = try toml.Parser.parse(GA, "name = { first = \"Sir, Haze\", last = \"Booth\", age = 18 }");
    const inlineTable = root.get("name").?.SubTable;
    t.expect(mem.eql(u8, inlineTable.get("first").?.String, quoted("Sir, Haze")));
    t.expect(mem.eql(u8, inlineTable.get("last").?.String, quoted("Booth")));
    t.expectEqual(inlineTable.get("age").?.Integer, 18);
}

test "Array of Tables" {
    const file =
        \\[[products]]
        \\name = "Hammer"
        \\sku = 738594937
        \\  
        \\[[products]]
        \\
        \\[[products]]
        \\name = "Nail"
        \\sku = 284758393
        \\color = "gray"
    ;
    const root = try toml.Parser.parse(GA, file);
    const products = root.get("products").?.Array;
    t.expectEqual(products.len, 3);
    const productTableA = products[0].SubTable;
    const productTableB = products[1].SubTable;
    const productTableC = products[2].SubTable;
    t.expectEqual(productTableA.count(), 2);
    t.expectEqual(productTableB.count(), 0);
    t.expectEqual(productTableC.count(), 3);

    t.expect(mem.eql(u8, productTableA.get("name").?.String, quoted("Hammer")));
    t.expectEqual(productTableA.get("sku").?.Integer, 738594937);

    t.expect(mem.eql(u8, productTableC.get("name").?.String, quoted("Nail")));
    t.expectEqual(productTableC.get("sku").?.Integer, 284758393);
    t.expect(mem.eql(u8, productTableC.get("color").?.String, quoted("gray")));
}
