const std = @import("std");
const toml = @import("toml.zig");
const t = std.testing;

const warn = std.debug.warn;
const FA = std.debug.failing_allocator;
const GA = std.debug.global_allocator;

fn printTokens(tokens: []toml.Token) void {
    for (tokens) |token| {
        switch (token) {
            .TableDefinition => |tableName| warn("[{}]\n", tableName),
            .KeyValue => |kv| warn("KV{{{} equals {}}}\n", kv.key, kv.value),
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
}
