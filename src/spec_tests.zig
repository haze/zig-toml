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
