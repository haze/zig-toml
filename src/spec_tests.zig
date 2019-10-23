const std = @import("std");
const toml = @import("toml.zig");
const t = std.testing;

const warn = std.debug.warn;
const FA = std.debug.failing_allocator;
const GA = std.debug.global_allocator;

fn printTokens(tokens: []Token) void {
    for (tokens) |token| {
        switch (token) {
            .TableDefinition => |tableName| warn("[{}]\n", tableName),
            .KeyValue => |kv| warn("KV{{{}={}}}\n", kv.key, kv.value),
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
    warn("[0]={}\n", tokens[0].KeyValue);
    warn("[1]={}\n", tokens[1].KeyValue);
    t.expect(tokens[0].KeyValue.eqlBare("key", quoted("value")));
    t.expect(tokens[1].KeyValue.eqlBare("another", quoted("# This is not a comment")));
}
