const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const ascii = std.ascii;

// Removoe later
const warn = std.debug.warn;
const dbg = false;

// caller is responsible for freeing memory
fn trimUnderscores(allocator: *mem.Allocator, src: []const u8) ![]const u8 {
    const underscoreCount: usize = b: {
        var n: usize = 0;
        for (src) |c| {
            if (c != '_') n += 1;
        }
        break :b n;
    };
    const new = try allocator.alloc(u8, underscoreCount);
    var n: usize = 0;
    for (src) |c| {
        if (c != '_') {
            new[n] = c;
            n += 1;
        }
    }
    return new;
}

const QuoteTracker = struct {
    const Self = @This();
    const QuoteType = enum {
        Single,
        Double,
        // TODO(hazebooth)): multiline
        // Multiline
    };

    currentlyWithin: ?QuoteType,

    // TOOD(hazebooth): turn into union
    const Event = union(enum) {
        Enter: QuoteType,
        Exit: QuoteType,
        None,
    };

    fn process(self: *Self, source: []const u8, index: usize) Event {
        const escaped = if (index > 1) source[index - 1] == '\\' else false;
        // TODO(hazebooth): evaluate naiveness
        const c = source[index];
        if (!escaped) {
            if (c == '"' and !self.inString()) { // start double quote
                self.currentlyWithin = QuoteType.Double;
                return Event{ .Enter = self.currentlyWithin.? };
            } else if (c == '"' and self.isInDoubleQuoteString()) { // end double quote
                self.currentlyWithin = null;
                return Event{ .Exit = QuoteType.Double };
            } else if (c == '\'' and !self.inString()) { // start single quote
                self.currentlyWithin = QuoteType.Single;
                return Event{ .Enter = self.currentlyWithin.? };
            } else if (c == '\'' and self.isInSingleQuoteString()) { // end single quote}
                self.currentlyWithin = null;
                return Event{ .Exit = QuoteType.Single };
            }
        }
        return Event.None;
    }

    fn inString(self: Self) bool {
        return self.currentlyWithin != null;
    }

    fn isInStringOfQuoteType(self: Self, ty: QuoteType) bool {
        return self.inString() and self.currentlyWithin.? == ty;
    }

    fn isInSingleQuoteString(self: Self) bool {
        return self.isInStringOfQuoteType(QuoteType.Single);
    }

    fn isInDoubleQuoteString(self: Self) bool {
        return self.isInStringOfQuoteType(QuoteType.Double);
    }

    fn new() Self {
        return Self{
            .currentlyWithin = null,
        };
    }
};

pub const StrKV = struct {
    const Self = @This();

    key: []const u8,
    value: []const u8,
    kind: ValueType,

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
    QuoteKey,
    Value,
    TableDefinition,
};

pub const ValueType = enum {
    Array,
    String,
    Integer,
    Float,
    Boolean,
    SubTable,
};

pub const Value = union(ValueType) {
    Array: []Value,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    SubTable: Table,

    const Error = error{BadValue} || mem.Allocator.Error;

    // TODO(hazebooth): unicode replacements, etc
    fn fromString(str: []const u8) Value {
        return Value{ .String = str };
    }

    // TODO(hazebooth): this will only get here if true or false
    // remove assumption
    fn fromBoolean(str: []const u8) Value {
        return Value{ .Boolean = ascii.eqlIgnoreCase(str, "true") };
    }

    fn fromFloat(allocator: *mem.Allocator, str: []const u8) !Value {
        if (dbg) warn("fromFloat({})\n", str);
        const withoutUnderscores = try trimUnderscores(allocator, str);
        errdefer allocator.free(withoutUnderscores);
        return Value{ .Float = try std.fmt.parseFloat(f64, withoutUnderscores) };
    }

    fn fromInteger(allocator: *mem.Allocator, str: []const u8) !Value {
        if (dbg) warn("fromInteger({})\n", str);
        const withoutUnderscores = try trimUnderscores(allocator, str);
        errdefer allocator.free(withoutUnderscores);
        switch (IntegerFormat.of(withoutUnderscores)) {
            .Hex => |v| return Value{
                .Integer = try std.fmt.parseInt(i64, v, 16),
            },
            .Binary => |v| return Value{
                .Integer = try std.fmt.parseInt(i64, v, 2),
            },
            .Octal => |v| return Value{
                .Integer = try std.fmt.parseInt(i64, v, 8),
            },
            .Decimal => return Value{
                .Integer = try std.fmt.parseInt(i64, withoutUnderscores, 10),
            },
        }
    }

    fn fromArray(allocator: *mem.Allocator, str: []const u8) Error!Value {
        var values = std.ArrayList(Value).init(allocator);
        var n: usize = 1;
        var lag: usize = 0;
        while (n < str.len - 1) {
            const c = str[n];
            if (dbg) warn("c={{{c}}}, lc={{{c}}}|{{{}}}, n={{{}}} x={{{}}}\n", c, str[lag], lag, n, n == (str.len - 2));
            const onLast = n == (str.len - 2);
            if (c == ',' or onLast) {
                // const start = if (onLast) lag else lag + 1;
                const end = if (onLast) n + 1 else n;
                const value = stripWhitespace(str[lag + 1 .. end]);
                const guess = isValidValue(value) orelse return error.BadValue;
                if (dbg) warn("value={{{}}}, guess={{{}}}\n", value, guess);
                try values.append(try Value.from(allocator, guess, value));
                lag = n;
            }
            n += 1;
        }
        return Value{ .Array = values.toOwnedSlice() };
    }

    pub fn from(allocator: *mem.Allocator, kind: ValueType, str: []const u8) Error!Value {
        if (dbg) warn("kind: {}, str: {}\n", kind, str);
        switch (kind) {
            .Float => return Value.fromFloat(allocator, str),
            .Array => return Value.fromArray(allocator, str),
            .Integer => return Value.fromInteger(allocator, str),
            .String => return Value.fromString(str),
            .Boolean => return Value.fromBoolean(str),
            else => return Value.fromString("TODO"),
        }
    }
};

pub const Table = struct {
    const ValueMap = std.StringHashMap(Value);
    const Self = @This();

    space: ValueMap,
    name: ?[]const u8,
    allocator: *mem.Allocator,

    fn newRoot(allocator: *mem.Allocator) Table {
        return Table{
            .space = ValueMap.init(allocator),
            .name = null,
            .allocator = allocator,
        };
    }

    fn newNamed(allocator: *mem.Allocator, name: ?[]const u8) Table {
        return Table{
            .space = ValueMap.init(allocator),
            .name = name,
            .allocator = allocator,
        };
    }

    pub fn get(self: Self, key: []const u8) ?Value {
        return self.space.getValue(key);
    }

    fn put(self: *Self, key: []const u8, value: Value) mem.Allocator.Error!void {
        if (dbg) warn("{}: put({}, {})\n", self.displayName(), key, value);
        if (mem.indexOfScalar(u8, key, '.')) |sepIdx| {
            const rest = key[sepIdx + 1 ..];
            var table = b: {
                const tableName = key[0..sepIdx];
                if (self.space.remove(tableName)) |existingTable| {
                    switch (existingTable.value) {
                        .SubTable => |t| break :b t,
                        else => {},
                        // else => return error.KeyIsNotATable,
                    }
                }
                break :b Table.newNamed(self.allocator, tableName);
            };
            try table.put(rest, value);
            return self.space.putNoClobber(key, Value{ .SubTable = table });
        } else {
            if (dbg) warn("{}: putFinal({}, {})\n", self.displayName(), key, value);
            return self.space.putNoClobber(key, value);
        }
    }

    fn displayName(self: Self) []const u8 {
        return if (self.isRoot()) "root" else self.name.?;
    }

    fn print(self: Self) void {
        warn("[{}] ({} items)\n", self.displayName(), self.space.count());
        var it = self.space.iterator();
        while (it.next()) |kv| {
            switch (kv.value) {
                .SubTable => |t| t.print(),
                else => |v| warn("{} = {}\n", kv.key, v),
            }
        }
    }

    fn deinit(self: Self) void {
        self.space.deinit();
    }

    fn isRoot(self: Self) bool {
        return self.name == null;
    }
};

pub const Parser = struct {
    const Self = @This();

    state: State,
    allocator: *mem.Allocator,
    source: []const u8,

    index: usize,
    lagIndex: usize,
    processed: usize,

    quoteTracker: QuoteTracker,

    key: ?[]const u8,

    pub fn init(allocator: *mem.Allocator, source: []const u8) Parser {
        return Parser{
            .state = .Root,
            .allocator = allocator,
            .source = source,
            .index = 0,
            .lagIndex = 0,
            .quoteTracker = QuoteTracker.new(),
            .processed = 0,
            .key = null,
        };
    }

    fn setState(self: *Self, state: State) void {
        if (dbg) warn("{} => {}\n", self.state, state);
        self.state = state;
    }

    // skipToNewline will jump over everything from where it was
    // until the next newline. if none exists, the parser sets itself
    // to the end of the source
    fn skipToNewline(self: *Self) void {
        if (mem.indexOfScalar(u8, self.source[self.index..], '\n')) |newLineIndex| {
            // warn("nl comment jump\n");
            self.index += newLineIndex + 1;
        } else {
            // warn("eof comment jump\n");
            self.index = self.source.len;
        }
        self.lagIndex = self.index;
    }

    fn processRoot(self: *Self, char: u8, isComment: bool) !void {
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
        // const isEscaped = if (self.index > 1) self.source[self.index - 1] == '\\' else false;
        // if (!isEscaped) self.processQuote(char);
        const quoteEvent = self.quoteTracker.process(self.source, self.index);
        if (dbg) {
            if (isNewline) {
                warn("on <newline> ({})\n", self.index);
            } else warn("on {c} ({})\n", char, self.index);
        }
        switch (self.state) {
            // we are at the default state,
            .Root => {
                // whitespace can exist
                switch (quoteEvent) {
                    .Enter => self.setState(.QuoteKey),
                    else => try self.processRoot(char, isComment),
                }
            },
            .QuoteKey => switch (quoteEvent) {
                .Exit => self.setState(.Key),
                .Enter => unreachable,
                .None => {},
            },

            .Key => {
                if (char == '=') {
                    self.key = self.source[self.lagIndex..self.index];
                    self.lagIndex = self.index;
                    self.setState(.Value);
                } else if (!isValidKeyChar(char)) {
                    // warn("bad char: [{c}]\n", char);
                    return error.UnexpectedCharacter;
                }
            },
            .Value => {
                if (isNewline or (!self.quoteTracker.inString() and isComment) or eof) {
                    if (self.key) |k| {
                        const end = b: {
                            if (isNewline or isComment) {
                                break :b self.index;
                            } else break :b self.index + 1;
                        };
                        const value = self.source[self.lagIndex + 1 .. end];
                        const strippedKey = stripWhitespace(k);
                        if (strippedKey.len == 0) return error.BadKey;
                        const strippedValue = stripWhitespace(value);
                        const valueGuess = isValidValue(strippedValue) orelse return error.BadValue;
                        const token = Token{
                            .KeyValue = StrKV{
                                .key = strippedKey,
                                .value = strippedValue,
                                .kind = valueGuess,
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

    // caller is responsible for freeing the resulting table
    pub fn parse(allocator: *mem.Allocator, source: []const u8) !Table {
        var rootTable = Table.newRoot(allocator);
        var parser = Parser.init(allocator, source);
        const tokens = try parser.lex();
        var current = &rootTable;
        for (tokens) |token| {
            if (dbg) warn("consuming token {}\n", token);
            switch (token) {
                .KeyValue => |kv| try current.put(kv.key, try Value.from(allocator, kv.kind, kv.value)),
                .TableDefinition => |tableName| {
                    var newTable = Table.newNamed(allocator, tableName);
                    try rootTable.put(tableName, Value{ .SubTable = newTable });
                    current = &newTable;
                },
            }
        }
        return rootTable;
    }
};

fn stripWhitespace(source: []const u8) []const u8 {
    return mem.trim(u8, source, " ");
}

// solely for parsing
fn isValidKeyChar(char: u8) bool {
    const isPunct = char == '.';
    const isDash = char == '_' or char == '-';
    const isQuote = char == '"' or char == '\'';
    return ascii.isSpace(char) or ascii.isAlpha(char) or ascii.isDigit(char) or isDash or isQuote or isPunct;
}

fn isValidValueInteger(value: []const u8) bool {
    if (dbg) warn("iVVI: {}\n", value);
    // trim leading zeroes
    const trimmed = if (!mem.allEqual(u8, value, '0')) mem.trimLeft(u8, value, "0") else "0";
    // check every char to make sure its in desired set
    var okay = true;
    for (b: {
        const first = trimmed[0];
        if (first == '+' or first == '-') {
            break :b trimmed[1..];
        } else break :b trimmed;
    }) |c| {
        if (dbg) warn("(int) okay={}, c={c}\n", okay, c);
        const isValidChar = switch (IntegerFormat.of(value)) {
            .Hex => ascii.isXDigit(c),
            .Binary => c == '0' or c == '1',
            .Octal => c <= '7' and c >= '0',
            .Decimal => ascii.isDigit(c),
        };
        // TODO(hazebooth): could be more strict
        okay = okay and (isValidChar or c == '_' or eqlIgnoreCaseChar(c, 'x') or eqlIgnoreCaseChar(c, 'o') or eqlIgnoreCaseChar(c, 'b'));
    }
    return okay;
}

fn isValidValueFloat(value: []const u8) bool {
    if (dbg) warn("iVVF: {}\n", value);
    // fast path, check if there is a '.' or 'e'
    if (mem.indexOf(u8, value, ".e") != null) {
        return false;
    }
    // trim leading zeroes
    const trimmed = if (!mem.allEqual(u8, value, '0')) mem.trimLeft(u8, value, "0") else "0";
    // check every char to make sure its in desired set
    var okay = true;
    for (b: {
        const first = trimmed[0];
        if (first == '+' or first == '-') {
            break :b trimmed[1..];
        } else break :b trimmed;
    }) |c| {
        if (dbg) warn("(float) okay={}, c={c}\n", okay, c);
        okay = okay and (ascii.isXDigit(c) or c == '_' or c == '.' or c == '+' or c == '-');
    }
    return okay;
}

fn eqlIgnoreCaseChar(char: u8, eql: u8) bool {
    return ascii.eqlIgnoreCase([_]u8{char}, [_]u8{eql});
}

fn isValidValueString(value: []const u8) bool {
    // if the first char isnt a quote
    if (value[0] != '"' and value[0] != '\'') return false;
    if (value[0] == '"' and value[value.len - 1] != '"') return false;
    if (value[0] == '\'' and value[value.len - 1] != '\'') return false;
    // fast path over, iteratre and maintain stack to make sure

    var level: usize = 0;
    var n: usize = 0;
    var t = QuoteTracker.new();
    var enteredString = false;
    while (n < value.len) : (n += 1) {
        const ev = t.process(value, n);
        switch (ev) {
            .Enter => {
                if (enteredString) unreachable; // cant enter string while entered
                enteredString = true;
            },
            .Exit => {
                if (enteredString) {
                    // we should exit the string at the end of the value
                    // TODO(hazebooth): support multiline
                    return n == (value.len - 1);
                } else unreachable;
            },
            .None => {}, // do nothing
        }
    }
    return false;
}

fn isValidValueBoolean(value: []const u8) bool {
    return ascii.eqlIgnoreCase(value, "true") or ascii.eqlIgnoreCase(value, "false");
}

fn isValidValueArray(value: []const u8) bool {
    return value[0] == '[' and value[value.len - 1] == ']';
}

// returns the best guess for said value for later parsing
fn isValidValue(value: []const u8) ?ValueType {
    const empty = value.len == 0;
    if (!empty) {
        if (isValidValueArray(value)) {
            return .Array;
        } else if (isValidValueString(value)) {
            return .String;
        } else if (isValidValueBoolean(value)) {
            return .Boolean;
        } else if (isValidValueInteger(value)) {
            return .Integer;
        } else if (isValidValueFloat(value)) {
            return .Float;
        }
    }
    return null;
}

const IntegerFormat = union(enum) {
    Hex: []const u8,
    Binary: []const u8,
    Octal: []const u8,
    Decimal, // doesn't need further clipping

    fn of(value: []const u8) IntegerFormat {
        if (value.len > 2) {
            const start = value[0..2];
            const rest = value[2..];
            if (ascii.eqlIgnoreCase(start, "0x")) {
                return IntegerFormat{ .Hex = rest };
            } else if (ascii.eqlIgnoreCase(start, "0b")) {
                return IntegerFormat{ .Binary = rest };
            } else if (ascii.eqlIgnoreCase(start, "0o")) {
                return IntegerFormat{ .Octal = rest };
            }
        }
        return .Decimal;
    }
};
