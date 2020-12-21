const toml = @import("toml.zig");
const std = @import("std");
const StreamingParser = toml.StreamingParser;
const debug_comptime = toml.debug_comptime;

fn expectLexemes(comptime input: []const u8, lexeme_arr: anytype) void {
    var parser = StreamingParser.init();
    var lexeme_offset: usize = 0;
    inline for (input) |char|
        if (parser.feed(char) catch unreachable) |lexeme| {
            if (debug_comptime)
                @compileLog(lexeme.kind, "offset=", lexeme.offset, " length=", lexeme.length);
            if (std.meta.eql(lexeme, lexeme_arr[lexeme_offset]))
                lexeme_offset += 1;
        };
    if (parser.feed(null) catch unreachable) |lexeme| {
        if (debug_comptime)
            @compileLog(lexeme.kind, "offset=", lexeme.offset, " length=", lexeme.length);
        if (std.meta.eql(lexeme, lexeme_arr[lexeme_offset]))
            lexeme_offset += 1;
    }
    if (lexeme_offset != lexeme_arr.len) {
        const err = std.fmt.comptimePrint("Expected {} Lexemes, but only found {}", .{ lexeme_arr.len, lexeme_offset });
        @compileError(err);
    }
}

fn runtimeExpectLexemes(input: []const u8, lexemes: []const StreamingParser.Lexeme) void {
    var lex_offset: usize = 0;
    var parser = StreamingParser.init();
    for (input) |char|
        if (parser.feed(char) catch unreachable) |lex| {
            std.debug.print("\n\nactual: {}\nexp: {}?\n\n", .{ lex, lexemes[lex_offset] });
            if (std.meta.eql(lex, lexemes[lex_offset])) {
                std.debug.print("slice = '{}'\n", .{lex.slice(input)});
                lex_offset += 1;
            }
        };
    if (parser.feed(null) catch unreachable) |lex| {
        std.debug.print("\n\nactual: {}\nexp: {}?\n\n", .{ lex, lexemes[lex_offset] });
        if (std.meta.eql(lex, lexemes[lex_offset])) {
            std.debug.print("slice = '{}'\n", .{lex.slice(input)});
            lex_offset += 1;
        }
    }
    testing.expectEqual(lexemes.len, lex_offset);
}

test "parser: keys & basic key value pair" {
    // // non quoted keys
    // comptime expectLexemes("key=1", .{
    //     StreamingParser.Lexeme{
    //         .offset = 0,
    //         .kind = .KeyWithTransition,
    //         .length = 3,
    //     },
    //     StreamingParser.Lexeme{
    //         .offset = 4,
    //         .kind = .IntValue,
    //         .length = 1,
    //     },
    // });
    // // quoted keys
    // comptime expectLexemes("\"foo bar\"=1", .{
    //     StreamingParser.Lexeme{
    //         .offset = 0,
    //         .kind = .Key,
    //         .length = 9,
    //     },
    //     StreamingParser.Lexeme{
    //         .offset = 8,
    //         .kind = .KeyValueTransition,
    //         .length = 1,
    //     },
    //     StreamingParser.Lexeme{
    //         .offset = 9,
    //         .kind = .IntValue,
    //         .length = 1,
    //     },
    // });
}
