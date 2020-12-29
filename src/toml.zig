// TODO(haze): some better error names man lol
const std = @import("std");
const mem = std.mem;
const testing = std.testing;

pub const debug_comptime = false;

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
pub const StreamingParser = struct {
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

        fn lexemeKind(self: ValueKind) Lexeme.Kind {
            return switch (self) {
                .String => .StringValue,
                .Boolean => .BooleanValue,
            };
        }
    };

    const State = union(enum) {
        const NestedState = enum {
            within_inline_table,
            within_array,
            top_level,

            fn isSeparatorChar(self: NestedState, char: u8) bool {
                return switch (self) {
                    .within_inline_table, .within_array => char == ',',
                    else => false,
                };
            }

            fn isEndChar(self: NestedState, char: u8) bool {
                return switch (self) {
                    .within_inline_table => char == '}',
                    .within_array => char == ']',
                    else => false,
                };
            }

            /// Calling `endLexeme` with `.top_level` is safety checked UB
            fn endLexeme(self: NestedState, optional_value_kind: ?TokenStream.ValueKind) Lexeme.Kind {
                return switch (self) {
                    .within_inline_table => .{ .InlineTableEnd = optional_value_kind },
                    .within_array => .{ .ArrayEnd = optional_value_kind },
                    else => unreachable,
                };
            }

            /// Calling `separatorLexeme` with `.top_level` is safety checked UB
            fn separatorLexeme(self: NestedState, optional_value_kind: ?TokenStream.ValueKind) Lexeme.Kind {
                return switch (self) {
                    .within_inline_table => .{ .InlineTableSeparator = optional_value_kind },
                    .within_array => .{ .ArraySeparator = optional_value_kind },
                    else => unreachable,
                };
            }

            fn acceptsSeparator(self: NestedState) bool {
                return switch (self) {
                    .top_level => false,
                    else => true,
                };
            }
        };
        /// bool represents whether or not said item is within an inline table
        ExpectingKey: bool,
        ReadingKey: struct {
            quotes: ?QuoteKind,
            within_inline_table: bool,
        },
        ExpectingValue: NestedState,
        ReadingValue: struct {
            kind: ValueKind,
            nested_state: NestedState,
        },

        /// When we see a '0', '+' or '-', we are unsure if it's still a float, integer, or non-negative integer
        ReadingNumberValue: NestedState,
        ReadingFloatFromDecimal: NestedState,
        ReadingFloatFromExponent: NestedState,
        // what initiated the float reading? this is to make sure we don't parse multiple decimals or exponents
        // this also may represent whether or not we found both
        // TODO(haze): better comment
        ReadingFloatValue: struct {
            seen_decimal: bool = false,
            seen_exponent: bool = false,
            seen_sign: bool = false,
            nested_state: NestedState,
        },

        ReadingInf: struct {
            progress: StatefulRead("inf") = .{},
            nested_state: NestedState,
        },
        ReadingNan: struct {
            progress: StatefulRead("nan") = .{},
            nested_state: NestedState,
        },
        ReadingPrefixedIntegerValue: NestedState,

        ReadingTableNamePart,
        ReadingTableNameTopLevel,

        /// bool represents whether or not said item is within an inline table
        ExpectingKeyValueTransition: bool,
        ReadingComment: NestedState,
    };

    pub const Lexeme = struct {
        pub const Kind = union(enum) {
            Key,
            KeyWithTransition,
            KeyValueTransition,
            IntValue,
            StringValue,
            BooleanValue,
            FloatValue,
            TableNamePart,
            TableNameBegin,
            TableNameEnd,

            InlineTableBegin,
            InlineTableEnd: ?TokenStream.ValueKind,
            InlineTableSeparator: ?TokenStream.ValueKind,

            ArrayBegin,
            ArrayEnd: ?TokenStream.ValueKind,
            ArraySeparator: ?TokenStream.ValueKind,

            fn toValueKind(self: Kind) TokenStream.ValueKind {
                return switch (self) {
                    .IntValue => .Int,
                    .StringValue => .String,
                    .BooleanValue => .Boolean,
                    .FloatValue => .Float,
                    .InlineTableEnd, .InlineTableSeparator => |maybe_value_kind| maybe_value_kind.?,
                    else => unreachable,
                };
            }
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
            self.bytes_per_current_lexeme = 1;
        }
    };

    offset: usize,
    cursor: ByteCursor,
    array_depth: usize,
    state: State,

    pub fn init() StreamingParser {
        var parser: StreamingParser = undefined;
        parser.reset();
        return parser;
    }

    fn reset(self: *StreamingParser) void {
        // reset to .top_level
        self.state = .{ .ExpectingKey = false };
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

    pub fn feed(self: *StreamingParser, maybe_char: ?u8) !?Lexeme {
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
            std.debug.print("reading: {}, {}, offset={}, {}\n", .{
                dbg_slice,
                self.state,
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
                .ReadingValue => |value_state| return self.makeLexeme(value_state.kind.lexemeKind()),
                else => return null,
            }
        };

        const maybe_quote = QuoteKind.fromChar(char);
        const is_quote = maybe_quote != null;
        const is_space = std.ascii.isSpace(char);
        const is_sign = char == '+' or char == '-';

        switch (self.state) {
            .ReadingComment => |comment_state| {
                if (char == '\n') {
                    if (comment_state == .within_array) {
                        self.state = .{ .ExpectingValue = comment_state };
                    } else {
                        self.state = .{ .ExpectingKey = comment_state == .within_inline_table };
                    }
                    return null;
                }
            },
            .ReadingTableNameTopLevel => {
                if (is_space)
                    return null;
                if (char == ']') {
                    self.state = .{ .ExpectingKey = false };
                    return null;
                }
                self.updateLexemeOffset();
                self.state = .ReadingTableNamePart;
                return null;
            },
            .ReadingTableNamePart => {
                if (char == '.') {
                    self.state = .ReadingTableNameTopLevel;
                    return self.makeLexeme(.TableNamePart);
                } else if (char == ']') {
                    self.state = .{ .ExpectingKey = false };
                    return self.makeLexeme(.TableNameEnd);
                }
                self.incrementBytesPerLexeme();
            },
            .ExpectingKey => |within_inline_table| {
                if (within_inline_table and char == ',') {
                    self.updateLexemeOffset();
                    return self.makeLexeme(.{ .InlineTableSeparator = null });
                }
                if (is_space) {
                    return null;
                } else if (char == '#') {
                    self.state = .{ .ReadingComment = if (within_inline_table) .within_inline_table else .top_level };
                    return null;
                } else if (char == '}') {
                    self.updateLexemeOffset();
                    return self.makeLexeme(.{ .InlineTableEnd = null });
                } else if (char == '[') {
                    if (within_inline_table)
                        return error.TableRedefinitionInInlineTable;
                    self.updateLexemeOffset();
                    self.state = .ReadingTableNameTopLevel;
                    return self.makeLexeme(.TableNameBegin);
                } else {
                    self.updateLexemeOffset();
                    self.state = .{ .ReadingKey = .{ .quotes = maybe_quote, .within_inline_table = within_inline_table } };
                    if (is_quote)
                        self.incrementBytesPerLexeme();
                    return null;
                }
            },
            .ReadingKey => |reading_key_state| {
                if (is_space and reading_key_state.quotes == null) {
                    self.state = .{ .ExpectingKeyValueTransition = reading_key_state.within_inline_table };
                    return self.makeLexeme(.Key);
                }
                if (maybe_quote != null and
                    reading_key_state.quotes != null and
                    maybe_quote.? == reading_key_state.quotes.?)
                {
                    self.state = .{ .ExpectingKeyValueTransition = reading_key_state.within_inline_table };
                    const lex = self.makeLexeme(.Key);
                    self.updateLexemeOffset();
                    return lex;
                }
                if (char == '=') {
                    self.state = .{ .ExpectingValue = if (reading_key_state.within_inline_table) .within_inline_table else .top_level };
                    const lex = self.makeLexeme(.KeyWithTransition);
                    self.updateLexemeOffset();
                    return lex;
                }
                self.incrementBytesPerLexeme();
            },
            .ExpectingKeyValueTransition => |within_inline_table| {
                if (is_space)
                    return null;
                if (char == '=') {
                    self.state = .{ .ExpectingValue = if (within_inline_table) .within_inline_table else .top_level };
                    const lex = self.makeLexeme(.KeyValueTransition);
                    self.updateLexemeOffset();
                    return lex;
                }
            },
            .ExpectingValue => |expecting_value_state| {
                if (is_space)
                    return null;
                // float numbers may not start with a .
                if (char == '.') {
                    return error.InvalidFloatValue;
                }
                if (char == '{') {
                    self.updateLexemeOffset();
                    self.state = .{ .ExpectingKey = true };
                    return self.makeLexeme(.InlineTableBegin);
                }
                if (char == '[') {
                    self.array_depth += 1;
                    self.updateLexemeOffset();
                    self.state = .{ .ExpectingValue = .within_array };
                    return self.makeLexeme(.ArrayBegin);
                }
                if (maybe_quote) |quotes| {
                    self.state = .{
                        .ReadingValue = .{
                            .nested_state = expecting_value_state,
                            .kind = .{ .String = quotes },
                        },
                    };
                    self.updateLexemeOffset();
                    return null;
                }
                if (char == 'i') {
                    self.updateLexemeOffset();
                    self.state = .{
                        .ReadingInf = .{
                            .nested_state = expecting_value_state,
                        },
                    };
                    return null;
                } else if (char == 'n') {
                    self.updateLexemeOffset();
                    self.state = .{
                        .ReadingNan = .{
                            .nested_state = expecting_value_state,
                        },
                    };
                    return null;
                }
                if (char == '0' or char == '+' or char == '-' or std.ascii.isDigit(char)) {
                    self.state = .{ .ReadingNumberValue = expecting_value_state };
                    self.updateLexemeOffset();
                    return null;
                }
                if (char == 't' or char == 'f') {
                    const is_true = char == 't';
                    self.updateLexemeOffset();
                    self.state = .{
                        .ReadingValue = .{
                            .nested_state = expecting_value_state,
                            .kind = .{
                                .Boolean = .{
                                    .offset = 1,
                                    .kind = if (is_true) .True else .False,
                                },
                            },
                        },
                    };
                }
            },
            .ReadingInf => |*inf_state| {
                if (is_space) {
                    if (inf_state.progress.isFinished()) {
                        self.state = .{ .ExpectingKey = inf_state.nested_state == .within_inline_table };
                        return null;
                    } else return error.InvalidFloatValueInf;
                }
                if (inf_state.nested_state == .within_inline_table and inf_state.nested_state.isEndChar(char)) {
                    return self.makeLexeme(inf_state.nested_state.endLexeme(.Float));
                }
                if (inf_state.nested_state.acceptsSeparator() and inf_state.nested_state.isSeparatorChar(char)) {
                    return self.makeLexeme(inf_state.nested_state.separatorLexeme(.Float));
                }
                if (!inf_state.progress.matches(char))
                    return error.InvalidFloatValueInf;
                inf_state.progress.offset += 1;
                self.incrementBytesPerLexeme();
                return null;
            },
            .ReadingNan => |*nan_state| {
                if (is_space) {
                    if (nan_state.progress.isFinished()) {
                        self.state = .{ .ExpectingKey = nan_state.nested_state == .within_inline_table };
                        return null;
                    } else return error.InvalidFloatValueNan;
                }
                if (nan_state.nested_state == .within_inline_table and nan_state.nested_state.isEndChar(char)) {
                    return self.makeLexeme(nan_state.nested_state.endLexeme(.Float));
                }
                if (nan_state.nested_state.acceptsSeparator() and nan_state.nested_state.isSeparatorChar(char)) {
                    return self.makeLexeme(nan_state.nested_state.separatorLexeme(.Float));
                }
                if (!nan_state.progress.matches(char))
                    return error.InvalidFloatValueNan;
                nan_state.progress.offset += 1;
                self.incrementBytesPerLexeme();
                return null;
            },
            .ReadingNumberValue => |number_value_nested_state| {
                if (char == 'i') {
                    // this is so we account for the +/-
                    self.incrementBytesPerLexeme();
                    self.state = .{
                        .ReadingInf = .{
                            .nested_state = number_value_nested_state,
                        },
                    };
                    return null;
                } else if (char == 'n') {
                    // this is so we account for the +/-
                    self.incrementBytesPerLexeme();
                    self.state = .{
                        .ReadingNan = .{
                            .nested_state = number_value_nested_state,
                        },
                    };
                    return null;
                }
                if (char == 'b' or char == 'x' or char == 'o') {
                    self.state = .{ .ReadingPrefixedIntegerValue = number_value_nested_state };
                    return null;
                }
                if (char == '.') {
                    self.state = .{ .ReadingFloatFromDecimal = number_value_nested_state };
                    return null;
                } else if (char == 'e' or char == 'E') {
                    self.state = .{ .ReadingFloatFromExponent = number_value_nested_state };
                    return null;
                }
                if (is_space) {
                    self.state = .{ .ExpectingKey = number_value_nested_state == .within_inline_table };
                    self.updateLexemeOffset();
                    return self.makeLexeme(.IntValue);
                }
                if (number_value_nested_state.isEndChar(char)) {
                    self.state = .{ .ExpectingValue = number_value_nested_state };
                    switch (number_value_nested_state) {
                        .within_array => {
                            if (self.array_depth - 1 == 0) {
                                self.state = .{ .ExpectingKey = false };
                            } else {
                                self.state = .{ .ExpectingValue = number_value_nested_state };
                            }
                        },
                        .within_inline_table => self.state = .{ .ExpectingKey = false },
                        else => {},
                    }
                    return self.makeLexeme(number_value_nested_state.endLexeme(.Int));
                } else if (number_value_nested_state.isSeparatorChar(char)) {
                    switch (number_value_nested_state) {
                        .within_array => self.state = .{ .ExpectingValue = number_value_nested_state },
                        .within_inline_table => self.state = .{ .ExpectingKey = true },
                        else => {},
                    }
                    return self.makeLexeme(number_value_nested_state.separatorLexeme(.Int));
                }
                if (!std.ascii.isDigit(char) and char != '_')
                    return error.InvalidIntegerValue;
                self.incrementBytesPerLexeme();
            },
            // this is after we read a 'e|E' from ReadingNumber
            // the next character MUST be a digit
            .ReadingFloatFromExponent => |float_from_exp_nested_state| {
                if (!std.ascii.isDigit(char) and !is_sign)
                    return error.InvalidFloatValue;
                self.state = .{
                    .ReadingFloatValue = .{
                        .seen_exponent = true,
                        .seen_sign = is_sign,
                        .nested_state = float_from_exp_nested_state,
                    },
                };
                return null;
            },
            .ReadingFloatFromDecimal => |float_from_dec_nested_state| {
                // can't end a float value with space or non digit
                if (is_space or !std.ascii.isDigit(char))
                    return error.InvalidFloatValue;
                self.state = .{ .ReadingFloatValue = .{ .seen_decimal = true, .nested_state = float_from_dec_nested_state } };
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
                    self.state = .{ .ExpectingKey = float_state.nested_state == .within_inline_table };
                    return null;
                }
            },
            // TODO(haze): vvvv
            .ReadingPrefixedIntegerValue => |int_value_nested_state| {
                if (is_space) {
                    self.state = .{ .ExpectingKey = int_value_nested_state == .within_inline_table };
                }
            },
            .ReadingValue => |*value_state| {
                switch (value_state.kind) {
                    .String => |string_quote_kind| {
                        if (maybe_quote != null and string_quote_kind == maybe_quote.?) {
                            self.state = .{ .ExpectingKey = value_state.nested_state == .within_inline_table };
                            self.incrementBytesPerLexeme();
                            return self.makeLexeme(.StringValue);
                        }
                        self.incrementBytesPerLexeme();
                        return null;
                    },
                    .Boolean => |*boolean_kind| {
                        if (is_space) {
                            if (boolean_kind.isFinished()) {
                                switch (value_state.nested_state) {
                                    .within_array => self.state = .{ .ExpectingValue = value_state.nested_state == .within_inline_table },
                                    else => self.state = .{ .ExpectingKey = value_state.nested_state == .within_inline_table },
                                }
                                return self.makeLexeme(.BooleanValue);
                            } else return error.InvalidBooleanValue;
                        }
                        if (value_state.nested_state.acceptsSeparator() and value_state.nested_state.isSeparatorChar(char)) {
                            if (boolean_kind.isFinished()) {
                                self.state = .{ .ExpectingKey = value_state.nested_state == .within_inline_table };
                                return self.makeLexeme(value_state.nested_state.separatorLexeme(.Boolean));
                            } else return error.InvalidBooleanValue;
                        }
                        if (!boolean_kind.matches(char))
                            return error.InvalidBooleanValue;
                        boolean_kind.offset += 1;
                        self.incrementBytesPerLexeme();
                        return null;
                    },
                }
            },
        }
        return null;
    }
};

const TokenStream = struct {
    const ArrayState = struct {
        stack: std.ArrayList(std.ArrayList(Value)),
        current: *std.ArrayList(Value),
        /// cannot be `.Array`
        kind: ?ValueKind = null,
    };

    parser: StreamingParser = StreamingParser.init(),
    offset: usize = 0,
    input: []const u8,

    /// current key for the being read key/value pair
    current_key: ?StreamingParser.Lexeme = null,
    /// key for the inline table we are reading
    inline_table_key: ?StreamingParser.Lexeme = null,

    seen_transition: bool = false,
    current_table_path: ?std.ArrayList([]const u8) = null,
    arena: std.heap.ArenaAllocator,

    array_state: ?ArrayState = null,

    pub fn init(input: []const u8, allocator: *mem.Allocator) TokenStream {
        return TokenStream{
            .input = input,
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    const TokenKind = enum {
        KeyValuePair,
    };

    const ValueKind = union(enum) {
        Int,
        Float,
        Boolean,
        String,
        Array,
    };

    const Value = union(enum) {
        text: []const u8,
        array: std.ArrayList(Value),
    };

    const Token = union(enum) {
        KeyValuePair: struct {
            const ArrayInfo = struct {
                depth: usize,
                kind: ValueKind,
            };
            key: []const u8,
            value: Value,
            array_information: ?ArrayInfo = null,
            kind: ValueKind,
        },
        /// Table path
        TableDefinition: [][]const u8,
        /// Key is the name of the table
        InlineTableBegin: []const u8,
        InlineTableEnd,

        fn deinit(self: *Token, allocator: *mem.Allocator) void {
            switch (self.*) {
                .TableDefinition => |parts| allocator.free(parts),
                else => {},
            }
        }
    };

    fn processValue(self: *TokenStream, tokens: *std.ArrayList(Token), lex: StreamingParser.Lexeme) !void {
        if (!self.seen_transition)
            return error.ValueWithoutTransition;
        if (self.current_key) |key| {
            try tokens.append(.{
                .KeyValuePair = .{
                    .key = key.slice(self.input),
                    .value = .{ .text = lex.slice(self.input) },
                    .kind = lex.kind.toValueKind(),
                },
            });
            self.current_key = null;
            self.seen_transition = true;
            std.debug.print("set current_key to null\n", .{});
        } else
            return error.ReadingValueWithoutKey;
    }

    fn processLexeme(
        self: *TokenStream,
        tokens: *std.ArrayList(Token),
    ) !void {
        const parser_input = if (self.offset >= self.input.len)
            null
        else
            self.input[self.offset];
        std.debug.print("parser_input={}\n", .{parser_input});
        if (try self.parser.feed(parser_input)) |lex| {
            std.debug.print("lex={}, slice='{}'\n", .{ lex, lex.slice(self.input) });
            switch (lex.kind) {
                .ArraySeparator => {
                    if (self.array_state == null)
                        return error.MissingArray;
                    if (self.array_state) |arr_state| {
                        try arr_state.current.append(.{ .text = lex.slice(self.input) });
                    } else return error.NoArrayToAddItemTo;
                },
                .ArrayBegin => {
                    if (self.array_state) |*arr_state| {
                        std.debug.print("We are nesting one more layer (depth={})\n", .{arr_state.stack.items.len});
                    } else {
                        std.debug.print("Creating new array stack...\n", .{});
                        var stack = std.ArrayList(std.ArrayList(Value)).init(&self.arena.allocator);
                        try stack.append(std.ArrayList(Value).init(&self.arena.allocator));
                        self.array_state = .{
                            .stack = stack,
                            .current = &stack.items[0],
                        };
                    }
                },
                .ArrayEnd => |item| {
                    std.debug.print("END_ITEM={}\n", .{item});
                    if (self.current_key == null)
                        return error.ArrayWithNoKey;
                    if (self.array_state) |arr_state| {
                        try arr_state.current.append(.{ .text = lex.slice(self.input) });
                        try tokens.append(.{
                            .KeyValuePair = .{
                                .key = self.current_key.?.slice(self.input),
                                // by this point, current should point towards the initial array
                                .value = .{ .array = arr_state.current.* },
                                .kind = .Array,
                            },
                        });
                        self.current_key = null;
                        self.seen_transition = true;
                    } else return error.ArrayEndWithNoArray;
                },
                // this happens when we see a ','
                .InlineTableSeparator => |maybe_value_kind| {
                    if (maybe_value_kind != null)
                        try self.processValue(tokens, lex);
                    self.current_key = null;
                    self.seen_transition = false;
                },
                .InlineTableBegin => {
                    if (self.inline_table_key != null)
                        return error.NestedInlineTables;
                    if (self.current_key == null)
                        return error.InlineTableWithNoKey;
                    // set the current_key to the inline table key
                    try tokens.append(.{ .InlineTableBegin = self.current_key.?.slice(self.input) });

                    self.inline_table_key = self.current_key;
                    self.current_key = null;
                },
                .InlineTableEnd => |maybe_value_kind| {
                    if (maybe_value_kind != null)
                        try self.processValue(tokens, lex);
                    if (self.inline_table_key == null)
                        return error.EndingInlineTableFromTopLevel;
                    try tokens.append(.InlineTableEnd);
                    self.inline_table_key = null;
                    self.current_key = null;
                },
                .TableNameBegin => {
                    if (self.current_key != null)
                        return error.DefiningTableWhileReadingKey;
                    self.current_table_path = std.ArrayList([]const u8).init(tokens.allocator);
                },
                .TableNameEnd => {
                    if (self.current_table_path) |*stack| {
                        try stack.append(lex.slice(self.input));
                        try tokens.append(.{ .TableDefinition = stack.toOwnedSlice() });
                        self.current_table_path = null;
                    } else
                        return error.EndingTableWithoutStack;
                },
                .TableNamePart => {
                    if (self.current_table_path) |*stack|
                        try stack.append(lex.slice(self.input));
                },
                .Key => {
                    if (self.current_key != null)
                        return error.AlreadyReadingKey;
                    self.current_key = lex;
                    std.debug.print("set current_key to {}\n", .{lex});
                },
                .KeyWithTransition => {
                    if (self.current_key != null)
                        return error.AlreadyReadingKey;
                    self.current_key = lex;
                    self.seen_transition = true;
                    std.debug.print("set current_key to {}\n", .{lex});
                },
                .KeyValueTransition => {
                    if (self.current_key == null)
                        return error.TransitionWithoutKey;
                    self.seen_transition = true;
                },
                .IntValue, .StringValue, .FloatValue, .BooleanValue => try self.processValue(tokens, lex),
            }
        }
    }

    /// Caller is responsible for freeing the returned memory with the same `allocator` provided.
    fn process(self: *TokenStream) ![]Token {
        var tokens = std.ArrayList(Token).init(&self.arena.allocator);
        errdefer tokens.deinit();
        while (self.offset != self.input.len) {
            try self.processLexeme(&tokens);
            self.offset += 1;
        }
        try self.processLexeme(&tokens);
        return tokens.toOwnedSlice();
    }

    fn deinit(self: *TokenStream) void {
        self.arena.deinit();
        self.* = undefined;
    }
};

const Table = struct {
    const ValueKind = enum {
        Integer,
        String,
        Boolean,
        Float,
        Array,
    };

    const Value = union(ValueKind) {
        Integer: i64,
        String: []const u8,
        Boolean: bool,
        float: f64,
        Array: []Value,
    };

    namespace: std.StringHashMap(Value),
};

test "TokenStream" {
    var input =
        \\key=[true, false]
    ;

    var stream = TokenStream.init(input, std.testing.allocator);
    defer stream.deinit();

    for (try stream.process()) |token|
        switch (token) {
            .TableDefinition => |parts| {
                for (parts) |part, idx| {
                    if (idx == 0) {
                        std.debug.print("Table Definition: [{}, ", .{part});
                    } else {
                        std.debug.print("{}, ", .{part});
                    }
                    if (idx + 1 == parts.len) {
                        std.debug.print("]\n", .{});
                    }
                }
            },
            else => switch (token) {
                .KeyValuePair => |kvp| {
                    switch (kvp.value) {
                        .array => |items| {
                            std.debug.print("[ARRAY] key={}, \nval=[\n", .{kvp.key});
                            for (items.items) |item|
                                std.debug.print("\t{},\n", .{item});
                            std.debug.print("]\n", .{});
                        },
                        else => std.debug.print("token={}\n", .{token}),
                    }
                },
                else => std.debug.print("token={}\n", .{token}),
            },
        };
}

// fn printArrayValue(kvp: TokenStream.Token) void {
//     var current = kvp.KeyValuePair.value;
//     var ending_parens = 1;
//     std.debug.print("[", .{});
//     while (current == .array) {
//             ending_parens += 1;
//     }
//     var count: usize = 0;
//     while (count < ending_parens): {ending_parens += 1} {
//         std.debug.print("]", .{});
//     }
//     std.debug.print("\n", .{});
// }

// https://github.com/ziglang/zig/issues/868
fn isComptime() bool {
    var t: bool = true;
    const x = if (t) @as(u7, 0) else @as(u8, 0);
    return @TypeOf(x) == u7;
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
