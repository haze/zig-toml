const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const ascii = std.ascii;

// Removoe later
const warn = std.debug.warn;

pub const StrKV = struct {
    const Self = @This();

    key: []const u8,
    value: []const u8,

    fn eqlBare(self: Self, key: []const u8, value: []const u8) bool {
        return mem.eql(u8, self.key, key) and mem.eql(u8, self.value, value);
    }

    fn eql(self: Self, other: Self) bool {
        return self.eqlBare(other.key, other.value);
    }
};

pub const Token = union(enum) {
    TableDefinition: []const u8,
    KeyValue: StrKV,
};

const State = enum {
    Root,
    Key,
    Value,
    TableDefinition,
};

pub const Parser = struct {
    const Self = @This();
    const QuoteType = enum {
        Double,
        Single,
    };

    state: State,
    allocator: *mem.Allocator,
    source: []const u8,
    inStringQuoteType: ?QuoteType,

    index: usize,
    lagIndex: usize,
    processed: usize,

    key: ?[]const u8,

    fn withinString(self: Self) bool {
        return self.inStringQuoteType != null;
    }

    pub fn init(allocator: *mem.Allocator, source: []const u8) Parser {
        return Parser{
            .state = .Root,
            .allocator = allocator,
            .source = source,
            .index = 0,
            .lagIndex = 0,
            .inStringQuoteType = null,
            .processed = 0,
            .key = null,
        };
    }

    fn setState(self: *Self, state: State) void {
        warn("{} => {}\n", self.state, state);
        self.state = state;
    }

    // processQuote makes sure that whenever we encounter a quote
    // the internal parser state is updated to represent that.
    // it will not update if we encounter a quote of another type,
    // or if the quote is escaped.
    fn processQuote(self: *Self, char: u8) void {
        if (char == '"') {
            if (self.inStringQuoteType) |qt| {
                if (qt == .Double) {
                    self.inStringQuoteType = null;
                }
            } else {
                self.inStringQuoteType = QuoteType.Double;
            }
        } else if (char == '\'') {
            if (self.inStringQuoteType) |qt| {
                if (qt == .Single) {
                    self.inStringQuoteType = null;
                }
            } else {
                self.inStringQuoteType = QuoteType.Single;
            }
        }
    }

    // skipToNewline will jump over everything from where it was
    // until the next newline. if none exists, the parser sets itself
    // to the end of the source
    fn skipToNewline(self: *Self) void {
        if (mem.indexOfScalar(u8, self.source[self.index..], '\n')) |newLineIndex| {
            warn("nl comment jump\n");
            self.index += newLineIndex + 1;
        } else {
            warn("eof comment jump\n");
            self.index = self.source.len;
        }
        self.lagIndex = self.index;
    }

    // process increments the index of the parser and transforms the
    // state of the parser if a transition criteria is found.
    // if no transition criteria is found, process will return false to
    // signal to stop processing.
    fn process(self: *Self, tokens: *std.ArrayList(Token)) !bool {
        const char = self.source[self.index];

        const isNewline = char == '\n';
        const isComment = char == '#';

        const eof = self.index == self.source.len - 1;
        const isEscaped = b: {
            if (self.index > 1) {
                break :b self.source[self.index - 1] == '\\';
            } else break :b false;
        };
        if (!isEscaped) self.processQuote(char);
        if (isNewline) {
            warn("on <newline> ({})\n", self.index);
        } else warn("on {c} ({})\n", char, self.index);
        switch (self.state) {
            // we are at the default state,
            .Root => {
                if (char == '[') {
                    self.setState(.TableDefinition);
                } else if (ascii.isSpace(char)) {
                    // cont
                } else if (isComment) {
                    // we hit a comment, skip to next line
                    self.skipToNewline();
                } else if (isValidKeyChar(char)) {
                    self.setState(.Key);
                } else return error.UnexpectedCharacter;
            },
            .Key => {
                if (char == '=') {
                    self.key = self.source[self.lagIndex..self.index];
                    self.lagIndex = self.index;
                    self.setState(.Value);
                } else if (!isValidKeyChar(char)) {
                    warn("bad char: [{c}]\n", char);
                    return error.UnexpectedCharacter;
                }
            },
            .Value => {
                if (isNewline or (!self.withinString() and isComment) or eof) {
                    if (self.key) |k| {
                        const end = b: {
                            if (isNewline or isComment) {
                                break :b self.index;
                            } else break :b self.index + 1;
                        };
                        const token = Token{
                            .KeyValue = StrKV{
                                .key = stripWhitespace(k),
                                .value = stripWhitespace(self.source[self.lagIndex + 1 .. end]),
                            },
                        };
                        try tokens.append(token);
                        self.key = null;
                        self.lagIndex = b: {
                            if (isNewline or isComment) {
                                break :b self.index + 1;
                            }
                            break :b self.index;
                        };
                        // if we hit a comment, we have to push the index
                        // to the next newline
                        if (isComment) {
                            // warn("comment jump window:\n{{{}}}\n", self.source[self.index..]);
                            if (mem.indexOfScalar(u8, self.source[self.index..], '\n')) |newLineIndex| {
                                self.index += newLineIndex;
                                self.lagIndex = self.index + 1;
                            }
                        }
                        self.setState(.Root);
                    } else unreachable;
                }
                // } else if (!self.withinString() and isComment) {
                //     self.skipToNewline();
                // }
            },
            .TableDefinition => {
                if (char == ']') {
                    const tableName = self.source[self.lagIndex + 1 .. self.index];
                    try tokens.append(Token{ .TableDefinition = tableName });
                    self.lagIndex = self.index;
                    self.setState(.Root);
                }
            },
        }
        self.index += 1;
        return self.index < self.source.len;
    }

    pub fn lex(self: *Self) ![]Token {
        var tokens = std.ArrayList(Token).init(self.allocator);
        while (try self.process(&tokens)) {}
        return tokens.toOwnedSlice();
    }
};

fn stripWhitespace(source: []const u8) []const u8 {
    return mem.trim(u8, source, " ");
}
// solely for parsing
fn isValidKeyChar(char: u8) bool {
    return ascii.isAlpha(char) or char == '_' or char == '-' or char == '"' or char == '\'' or char == ' ';
}
