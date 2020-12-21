const std = @import("std");
const testing = std.testing;

const debug_comptime = false;

const QuoteKind = enum {
    Single,
    Double,

    fn fromChar(char: u8) ?QuoteKind {
        if (char == '"')
            return .Double;
        if (char == '\'')
            return .Single;
        return null;
    }
};

fn StatefulRead(pattern: []const u8) type {
    return struct {
        const repr = pattern;
        offset: usize = 1,

        fn isFinished(self: @This()) bool {
            return self.offset == repr.len;
        }

        fn matches(self: @This(), char: u8) bool {
            return char == repr[self.offset];
        }
    };
}

/// Three levels of parsing
/// StreamingParser: reads input one byte at a time, uses internal state machien to determine whether or not input
/// is partial
///
/// TokenStream: Wraps a `StreamingParser` and is initialized with a slice. This Stream can also be grown so that you
/// may feed additional slices of input and get only tokens back out
///
/// Table: Top level construct to parse entire TOML byte-streams; cannot be grown and represents a finalized TOML document
const StreamingParser = struct {
    /// Different kinds of states we could be reading values for
    // TODO(haze): Dates
    const ValueKind = union(enum) {
        const BooleanKind = enum {
            True, False
        };

        String: QuoteKind,
        Boolean: struct {
            const true_repr = "true";
            const false_repr = "false";

            offset: usize = 0,
            kind: BooleanKind,

            fn isFinished(self: @This()) bool {
                return switch (self.kind) {
                    .True => self.offset == true_repr.len,
                    .False => self.offset == false_repr.len,
                };
            }

            fn matches(self: @This(), char: u8) bool {
                return char == switch (self.kind) {
                    .True => true_repr[self.offset],
                    .False => false_repr[self.offset],
                };
            }
        },
    };

    const State = union(enum) {
        ExpectingKey,
        ReadingKey: ?QuoteKind,
        ExpectingValue,
        ReadingValue: ValueKind,

        /// When we see a '0', '+' or '-', we are unsure if it's still a float, integer, or non-negative integer
        ReadingNumberValue,
        ReadingFloatFromDecimal,
        ReadingFloatFromExponent,
        // what initiated the float reading? this is to make sure we don't parse multiple decimals or exponents
        // this also may represent whether or not we found both
        // TODO(haze): better comment
        ReadingFloatValue: struct {
            seen_decimal: bool = false,
            seen_exponent: bool = false,
            seen_sign: bool = false,
        },

        ReadingInf: StatefulRead("inf"),
        ReadingNan: StatefulRead("nan"),
        ReadingPrefixedIntegerValue,

        ExpectingKeyValueTransition,
        ReadingComment,
    };

    const Lexeme = struct {
        const Kind = enum {
            Key,
            KeyWithTransition,
            KeyValueTransition,
            IntValue,
            StringValue,
            BooleanValue,
        };
        /// The offset within the original stream that this lexeme was found at
        offset: usize,
        /// The kind of lexeme
        kind: Kind,
        /// The length of the lexeme
        length: usize,

        fn slice(self: Lexeme, input: []const u8) []const u8 {
            return input[self.offset .. self.offset + self.length];
        }
    };

    const ByteCursor = struct {
        start_offset: usize,
        bytes_per_current_lexeme: usize,

        fn reset(self: *ByteCursor) void {
            self.start_offset = 0;
            self.bytes_per_current_lexeme = 0;
        }
    };

    offset: usize,
    cursor: ByteCursor,
    state: State,

    fn init() StreamingParser {
        var parser: StreamingParser = undefined;
        parser.reset();
        return parser;
    }

    fn reset(self: *StreamingParser) void {
        self.state = .ExpectingKey;
        self.offset = 0;
        self.cursor.reset();
    }

    const space_debug = "(space)";
    const null_debug = "(null)";

    fn incrementBytesPerLexeme(self: *StreamingParser) void {
        if (!isComptime())
            std.debug.print("inc byte per lexeme\n", .{});
        self.cursor.bytes_per_current_lexeme += 1;
    }

    fn updateLexemeOffset(self: *StreamingParser) void {
        if (!isComptime())
            std.debug.print("updating cursor.start_offset from {} to {}\n", .{ self.cursor.start_offset, self.offset - 1 });
        self.cursor.start_offset = self.offset - 1;
    }

    fn makeLexeme(self: *StreamingParser, kind: Lexeme.Kind) Lexeme {
        const lex = Lexeme{
            .kind = kind,
            .offset = self.cursor.start_offset,
            .length = self.cursor.bytes_per_current_lexeme,
        };
        self.cursor.bytes_per_current_lexeme = 1;
        return lex;
    }

    fn feed(self: *StreamingParser, maybe_char: ?u8) !?Lexeme {
        var dbg_buf: [space_debug.len]u8 = undefined;

        const dbg_slice = blk: {
            if (maybe_char != null and std.ascii.isSpace(maybe_char.?)) {
                std.mem.copy(u8, &dbg_buf, space_debug);
                break :blk dbg_buf[0..space_debug.len];
            } else if (maybe_char) |char| {
                break :blk std.fmt.bufPrint(&dbg_buf, "'{c}'", .{char}) catch unreachable;
            } else {
                std.mem.copy(u8, &dbg_buf, null_debug);
                break :blk dbg_buf[0..null_debug.len];
            }
        };

        if (!isComptime())
            std.debug.print("{}, reading: {}, offset={}, {}\n", .{
                self.state,
                dbg_slice,
                self.offset,
                self.cursor,
            });
        self.offset += 1;

        // TODO(haze): special logic for EOF
        const char = maybe_char orelse {
            switch (self.state) {
                // we can't finish a float with EOF
                .ReadingFloatFromDecimal => return error.UnfinishedFloatValue,
                .ReadingNumberValue => return self.makeLexeme(.IntValue),
                else => return null,
            }
        };

        const maybe_quote = QuoteKind.fromChar(char);
        const is_quote = maybe_quote != null;
        const is_space = std.ascii.isSpace(char);
        const is_sign = char == '+' or char == '-';

        switch (self.state) {
            .ReadingComment => {
                if (char == '\n') {
                    self.state = .ExpectingKey;
                    return null;
                }
            },
            .ExpectingKey => {
                if (is_space) {
                    return null;
                } else if (char == '#') {
                    self.state = .ReadingComment;
                    return null;
                } else {
                    self.updateLexemeOffset();
                    self.state = .{ .ReadingKey = maybe_quote };
                    if (is_quote)
                        self.incrementBytesPerLexeme();
                    self.incrementBytesPerLexeme();
                    return null;
                }
            },
            .ReadingKey => |maybe_key_quote| {
                if (is_space and maybe_key_quote == null) {
                    self.state = .ExpectingKeyValueTransition;
                    return self.makeLexeme(.Key);
                }
                if (maybe_quote != null and
                    maybe_key_quote != null and
                    maybe_quote.? == maybe_key_quote.?)
                {
                    self.state = .ExpectingKeyValueTransition;
                    const lex = self.makeLexeme(.Key);
                    self.updateLexemeOffset();
                    return lex;
                }
                if (char == '=') {
                    self.state = .ExpectingValue;
                    const lex = self.makeLexeme(.KeyWithTransition);
                    self.cursor.start_offset = self.offset;
                    return lex;
                }
                self.incrementBytesPerLexeme();
            },
            .ExpectingKeyValueTransition => {
                if (is_space)
                    return null;
                if (char == '=') {
                    self.state = .ExpectingValue;
                    const lex = self.makeLexeme(.KeyValueTransition);
                    self.updateLexemeOffset();
                    return lex;
                }
            },
            .ExpectingValue => {
                if (is_space)
                    return null;
                // float numbers may not start with a .
                if (char == '.') {
                    return error.InvalidFloatValue;
                }
                if (maybe_quote != null) {
                    self.state = .{ .ReadingValue = .{ .String = maybe_quote.? } };
                    return null;
                }
                if (char == 'i') {
                    self.state = .{ .ReadingInf = .{} };
                    return null;
                } else if (char == 'n') {
                    self.state = .{ .ReadingNan = .{} };
                    return null;
                }
                if (char == '0' or char == '+' or char == '-' or std.ascii.isDigit(char)) {
                    self.state = .ReadingNumberValue;
                    return null;
                }
                if (char == 't' or char == 'f') {
                    const is_true = char == 't';
                    self.state = .{
                        .ReadingValue = .{
                            .Boolean = .{
                                .offset = 1,
                                .kind = if (is_true) .True else .False,
                            },
                        },
                    };
                }
            },
            .ReadingInf => |*inf_state| {
                if (is_space) {
                    if (inf_state.isFinished()) {
                        self.state = .ExpectingKey;
                        return null;
                    } else return error.InvalidFloatValueInf;
                }
                if (!inf_state.matches(char))
                    return error.InvalidFloatValueInf;
                inf_state.offset += 1;
                return null;
            },
            .ReadingNan => |*nan_state| {
                if (is_space) {
                    if (nan_state.isFinished()) {
                        self.state = .ExpectingKey;
                        return null;
                    } else return error.InvalidFloatValueNan;
                }
                if (!nan_state.matches(char))
                    return error.InvalidFloatValueNan;
                nan_state.offset += 1;
                return null;
            },
            .ReadingNumberValue => {
                if (char == 'i') {
                    self.state = .{ .ReadingInf = .{} };
                    return null;
                } else if (char == 'n') {
                    self.state = .{ .ReadingNan = .{} };
                    return null;
                }
                if (char == 'b' or char == 'x' or char == 'o') {
                    self.state = .ReadingPrefixedIntegerValue;
                    return null;
                }
                if (char == '.') {
                    self.state = .ReadingFloatFromDecimal;
                    return null;
                } else if (char == 'e' or char == 'E') {
                    self.state = .ReadingFloatFromExponent;
                    return null;
                }
                if (is_space) {
                    self.state = .ExpectingKey;
                    return null;
                }
                if (!std.ascii.isDigit(char) and char != '_')
                    return error.InvalidIntegerValue;
            },
            // this is after we read a 'e|E' from ReadingNumber
            // the next character MUST be a digit
            .ReadingFloatFromExponent => {
                if (!std.ascii.isDigit(char) and !is_sign)
                    return error.InvalidFloatValue;
                self.state = .{ .ReadingFloatValue = .{ .seen_exponent = true, .seen_sign = is_sign } };
                return null;
            },
            .ReadingFloatFromDecimal => {
                // can't end a float value with space or non digit
                if (is_space or !std.ascii.isDigit(char))
                    return error.InvalidFloatValue;
                self.state = .{ .ReadingFloatValue = .{ .seen_decimal = true } };
                return null;
            },
            .ReadingFloatValue => |*float_state| {
                const is_e = char == 'e' or char == 'E';
                // TODO(haze): Clean up logic here
                // can't encounter another exponent or decimal value if we already found both
                if ((float_state.seen_decimal and float_state.seen_exponent) and (is_e or char == '.'))
                    return error.InvalidFloatValue;
                // can't start with a decimal and find another
                if (char == '.' and float_state.seen_decimal)
                    return error.InvalidFloatValueDecimal;
                // can't start from exponent and find another
                if (is_e and float_state.seen_exponent) {
                    return error.InvalidFloatValueExponent;
                } else if (is_e) {
                    float_state.seen_exponent = true;
                    return null;
                }
                if (is_e and char == '.') {
                    float_state.seen_decimal = true;
                    return null;
                }
                if (float_state.seen_sign and is_sign)
                    return error.InvalidFloatValueSign;
                if (is_sign) {
                    float_state.seen_sign = true;
                    return null;
                }
                if (!std.ascii.isDigit(char))
                    return error.InvalidFloatValue;
                if (is_space) {
                    self.state = .ExpectingKey;
                    return null;
                }
            },
            .ReadingPrefixedIntegerValue => {
                if (is_space) {
                    self.state = .ExpectingKey;
                }
            },
            .ReadingValue => |*value_kind| {
                switch (value_kind.*) {
                    .String => |string_quote_kind| {
                        if (maybe_quote != null and string_quote_kind == maybe_quote.?) {
                            self.state = .ExpectingKey;
                            return self.makeLexeme(.StringValue);
                        }
                        return null;
                    },
                    .Boolean => |*boolean_kind| {
                        if (is_space) {
                            if (boolean_kind.isFinished()) {
                                self.state = .ExpectingKey;
                                return self.makeLexeme(.BooleanValue);
                            } else return error.InvalidBooleanValue;
                        }
                        if (!boolean_kind.matches(char))
                            return error.InvalidBooleanValue;
                        boolean_kind.offset += 1;
                        return null;
                    },
                }
            },
        }
        return null;
    }
};
const TokenStream = struct {};
const Table = struct {};

// https://github.com/ziglang/zig/issues/868
fn isComptime() bool {
    var t: bool = true;
    const x = if (t) @as(u7, 0) else @as(u8, 0);
    return @TypeOf(x) == u7;
}

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
    // testing.expectEqual(lexeme_arr.len, lexeme_offset);
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

// test "parser: boolean" {
//     var parser = StreamingParser.init();

//     const true_input = "key=fal";
//     const false_input = "key=tr";

//     for (true_input[0 .. true_input.len - 1]) |char|
//         _ = try parser.feed(char);
//     testing.expectError(error.InvalidBooleanValue, parser.feed(true_input[true_input.len]));
//     parser.reset();

//     for (false_input[0 .. false_input.len - 1]) |char|
//         _ = try parser.feed(char);
//     testing.expectError(error.InvalidBooleanValue, parser.feed(false_input[false_input.len]));
// }

// test "parser: floats" {
//     expectCleanParse("f=+1.0");
//     expectCleanParse("f=3.1415");
//     expectCleanParse("f=-0.01");
//     expectCleanParse("f=5e+22");
//     expectCleanParse("f=1e06");
//     expectCleanParse("f=-2E-2");
//     expectCleanParse("f=6.626e-34");

//     expectParseError(error.InvalidFloatValue, "float = .7");
//     expectParseError(error.UnfinishedFloatValue, "float = 7.");
//     expectParseError(error.InvalidFloatValue, "float = 3.e+20");

//     expectCleanParse("f=+inf");
//     expectCleanParse("f=-inf");
//     expectCleanParse("f=inf");

//     expectCleanParse("f=+nan");
//     expectCleanParse("f=-nan");
//     expectCleanParse("f=nan");
// }

// fn expectCleanParse(input: []const u8) void {
//     std.debug.print("\n=== Valid Input: '{}' ===\n", .{input});
//     var parser = StreamingParser.init();
//     for (input) |char|
//         _ = parser.feed(char) catch unreachable;
//     _ = parser.feed(null) catch unreachable;
// }

// fn expectParseError(expected_error: anyerror, input: []const u8) void {
//     std.debug.print("\n=== Invalid Input: '{}' ({}) ===\n", .{ input, @errorName(expected_error) });
//     var parser = StreamingParser.init();
//     for (input) |char|
//         _ = parser.feed(char) catch |actual_error| {
//             if (actual_error != expected_error) {
//                 std.debug.panic("expected error.{}, found error.{}", .{
//                     @errorName(expected_error),
//                     @errorName(actual_error),
//                 });
//             } else return;
//         };
//     testing.expectError(expected_error, parser.feed(null));
// }
