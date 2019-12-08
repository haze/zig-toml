const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const ascii = std.ascii;

// Remove later
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
        MultilineSingle,
        MultilineDouble,

        fn isMultiline(self: QuoteType) bool {
            return self == .MultilineDouble or self == .MultilineSingle;
        }
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
        const c = source[index];
        if (!escaped) {
            if (index + 3 <= source.len) {
                const three = source[index .. index + 3];
                const isDoubleMultiQuote = mem.eql(u8, three, "\"\"\"");
                const isSingleMultiQuote = mem.eql(u8, three, "'''");
                if (isDoubleMultiQuote and !self.inString()) {
                    self.currentlyWithin = QuoteType.MultilineDouble;
                    return Event{ .Enter = self.currentlyWithin.? };
                } else if (isDoubleMultiQuote and self.isInMultiDoubleQuoteString()) {
                    self.currentlyWithin = null;
                    return Event{ .Exit = QuoteType.MultilineDouble };
                } else if (isSingleMultiQuote and !self.inString()) {
                    self.currentlyWithin = QuoteType.MultilineSingle;
                    return Event{ .Enter = self.currentlyWithin.? };
                } else if (isSingleMultiQuote and self.isInMultiSingleQuoteString()) {
                    self.currentlyWithin = null;
                    return Event{ .Exit = QuoteType.MultilineSingle };
                }
            }
            const isDoubleQuote = c == '"';
            const isSingleQuote = c == '\'';
            if (isDoubleQuote and !self.inString()) { // start double quote
                self.currentlyWithin = QuoteType.Double;
                return Event{ .Enter = self.currentlyWithin.? };
            } else if (isDoubleQuote and self.isInDoubleQuoteString()) { // end double quote
                self.currentlyWithin = null;
                return Event{ .Exit = QuoteType.Double };
            } else if (isSingleQuote and !self.inString()) { // start single quote
                self.currentlyWithin = QuoteType.Single;
                return Event{ .Enter = self.currentlyWithin.? };
            } else if (isSingleQuote and self.isInSingleQuoteString()) { // end single quote}
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

    fn isInMultiDoubleQuoteString(self: Self) bool {
        return self.isInStringOfQuoteType(QuoteType.MultilineDouble);
    }

    fn isInMultiSingleQuoteString(self: Self) bool {
        return self.isInStringOfQuoteType(QuoteType.MultilineSingle);
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

pub const StrKVA = struct {
    const Self = @This();

    key: []const u8,
    values: [][]const u8,
    kind: ValueType,
};

pub const Token = union(enum) {
    // normal definitions
    TableDefinition: []const u8,
    KeyValue: StrKV,
    KeyValueArray: StrKVA,

    // inline tables
    InlineTableDefinition: []const u8,
    StopInlineTable,

    // table array
    TableArrayDefinition: []const u8,
};

pub const ValueType = enum {
    Array,
    String,
    Integer,
    Float,
    Boolean,
    SubTable,
};

fn addCodepointToBuffer(codepointStr: []const u8, buf: *std.Buffer) !void {
    const codepoint = std.fmt.parseInt(u32, codepointStr, 16) catch return error.BadValue;
    if (dbg) warn("codepoint = {{{}}}\n", codepoint);
    // grow the buffer by the codepoint size
    const codepointSize = std.unicode.utf8CodepointSequenceLength(codepoint) catch return error.BadValue;
    const oldBufLen = buf.len();
    const newSize = oldBufLen + codepointSize;
    try buf.resize(newSize);
    _ = std.unicode.utf8Encode(codepoint, buf.toSlice()[oldBufLen..]) catch return error.BadValue;
}

pub const Value = union(ValueType) {
    Array: []Value,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    SubTable: Table,

    const Error = error{BadValue} || mem.Allocator.Error;

    fn string(self: Value, allocator: *mem.Allocator) mem.Allocator.Error![]const u8 {
        switch (self) {
            .SubTable => |t| return t.displayName(),
            .Array => |vals| {
                var list = std.ArrayList([]const u8).init(allocator);
                for (vals) |val| {
                    try list.append(try val.string(allocator));
                }
                defer list.deinit();
                const joined = try mem.join(allocator, ", ", list.toSlice());
                return try mem.concat(allocator, u8, [_][]const u8{ "[", joined, "]" });
            },
            .String => |s| return s,
            .Integer => |i| return try std.fmt.allocPrint(allocator, "{}", i),
            .Float => |i| return try std.fmt.allocPrint(allocator, "{}", i),
            .Boolean => |b| return if (b) "true" else "false",
        }
    }

    fn fromString(allocator: *mem.Allocator, str: []const u8) Error!Value {
        // can be at most str size
        if (mem.indexOfScalar(u8, str, '\\') == null) { // if we dont find any escaped char
            return Value{ .String = trimQuotes(u8, str) };
        }
        // var new = try allocator.alloc(u8, str.len);
        //  TODO(hazebooth): find a way to use a buffer while still being able to edit
        // existing parts
        var buf = try std.Buffer.initSize(allocator, 0);
        var srcPtr: usize = 0;
        while (srcPtr < str.len) {
            const c = str[srcPtr];
            const maybeLookAhead = b: {
                if (str.len > srcPtr + 1) {
                    break :b str[srcPtr + 1];
                } else break :b null;
            };
            if (maybeLookAhead) |lookAhead| {
                if (c == '\\' and (lookAhead != '\'' and lookAhead != '"')) { // dont increment
                    switch (lookAhead) {
                        'b' => try buf.appendByte('\u{0008}'),
                        'f' => try buf.appendByte('\u{000C}'),
                        't' => try buf.appendByte('\t'),
                        'n' => try buf.appendByte('\n'),
                        'r' => try buf.appendByte('\r'),
                        'U' => {
                            // +2 = \U
                            const codepointStr = str[srcPtr + 2 .. srcPtr + 10];
                            try addCodepointToBuffer(codepointStr, &buf);
                            srcPtr += 8;
                        },
                        'u' => {
                            const codepointStr = str[srcPtr + 2 .. srcPtr + 6];
                            try addCodepointToBuffer(codepointStr, &buf);
                            srcPtr += 4;
                        },
                        else => return error.BadValue,
                    }
                    // did successful replacement, increment src
                    srcPtr += 2;
                    continue;
                } else {
                    try buf.appendByte(c);
                }
            } else {
                try buf.appendByte(c);
            }
            srcPtr += 1;
        }
        if (dbg) warn("final={{{}}}\n", buf.toSliceConst());
        return Value{ .String = trimQuotes(u8, buf.toOwnedSlice()) };
    }

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
        // if (dbg) warn("from(): kind: {}, str: {}\n", kind, str);
        switch (kind) {
            .Float => return Value.fromFloat(allocator, str),
            .Array => return Value.fromArray(allocator, str),
            .Integer => return Value.fromInteger(allocator, str),
            .String => return Value.fromString(allocator, str),
            .Boolean => return Value.fromBoolean(str),
            else => return error.BadValue,
        }
    }
};

pub const Table = struct {
    const ValueMap = std.StringHashMap(Value);
    const Self = @This();

    space: ValueMap,
    name: ?[]const u8,
    allocator: *mem.Allocator,

    pub fn fromString(allocator: *mem.Allocator, source: []const u8) !Table {
        return Parser.parse(allocator, source);
    }

    pub fn newRoot(allocator: *mem.Allocator) Table {
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

    pub fn count(self: Self) usize {
        return self.space.count();
    }

    fn put(self: *Self, key: []const u8, value: Value) mem.Allocator.Error!void {
        if (mem.indexOfScalar(u8, key, '.')) |sepIdx| {
            if (dbg) {
                if (value == .SubTable) {
                    warn("{}: put({}, {})\n", self.displayName(), key, value.SubTable.name);
                } else warn("{}: put({}, {})\n", self.displayName(), key, value);
            }
            const rest = key[sepIdx + 1 ..];
            const tableName = key[0..sepIdx];
            var t = b: {
                if (self.space.remove(tableName)) |existingTable| {
                    switch (existingTable.value) {
                        .SubTable => |t| break :b t,
                        else => {},
                        // else => return error.KeyIsNotATable,
                    }
                }
                break :b Table.newNamed(self.allocator, tableName);
            };
            try t.put(rest, value);
            return self.space.putNoClobber(tableName, Value{ .SubTable = t });
        } else {
            if (dbg) {
                if (value == .SubTable) {
                    warn("{}: putFinal({}, {})\n", self.displayName(), key, value.SubTable.name);
                } else warn("{}: putFinal({}, {})\n", self.displayName(), key, value);
            }
            return self.space.putNoClobber(key, value);
        }
    }

    fn displayName(self: Self) []const u8 {
        return if (self.isRoot()) "root" else self.name.?;
    }

    fn printLevel(self: Self, level: usize) void {
        if (level != 0) {
            var n: usize = 0;
            while (n < level) : (n += 1) {
                warn("  ");
            }
            warn("┃ ");
        }
        warn("[{}] ({} items)\n", self.displayName(), self.space.count());
        var it = self.space.iterator();
        while (it.next()) |kv| {
            switch (kv.value) {
                .SubTable => |t| t.printLevel(level + 1),
                else => {},
            }
        }
        it.reset();
        while (it.next()) |kv| {
            var n: usize = 0;
            while (n < level) : (n += 1) {
                warn("  ");
            }
            switch (kv.value) {
                .SubTable => {},
                else => |v| warn("┣ {} = {}\n", kv.key, v.string(self.allocator)),
            }
        }
    }

    fn print(self: Self) void {
        self.printLevel(0);
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

    const State = enum {
        Root,
        Key,
        QuoteKey,
        Value,
        ValueArray,
        TableDefinition,
        InlineTable,
        TableArrayDefinition,
    };

    state: State,
    allocator: *mem.Allocator,
    source: []const u8,

    index: usize,
    lagIndex: usize,
    processed: usize,
    valueArrayDepth: usize,

    arrayValues: std.ArrayList([]const u8),
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
            .valueArrayDepth = 0,
            .arrayValues = std.ArrayList([]const u8).init(allocator),
            .processed = 0,
            .key = null,
        };
    }

    pub fn deinit(self: Self) void {
        self.arrayValues.deinit();
    }

    fn setState(self: *Self, state: State) void {
        if (dbg) warn("{} => {}\n", self.state, state);
        self.state = state;
    }

    // skipToNewline will jump over everything from where it was
    // until the next newline. if none exists, the parser sets itself
    // to the end of the source

    /// skipToNewline will jump to the closest newLine and return true. if there is none, it will
    /// set the parser state to finished and return false.
    fn skipToNewline(self: *Self) bool {
        if (mem.indexOfScalar(u8, self.source[self.index..], '\n')) |newLineIndex| {
            if (dbg) warn("nl comment jump\n");
            self.index += newLineIndex;
            self.lagIndex = self.index;
            return true; // keep parsing
        } else {
            if (dbg) warn("eof comment jump (finishing)\n");
            self.finish();
            return false; // stop parsing
        }
    }

    /// prints parser state
    fn print(self: Self, window: usize) void {
        const leftIndexWindow = win: {
            const from = f: {
                if (self.index >= window) {
                    break :f self.index - window;
                } else break :f 0;
            };
            const to = self.index;
            break :win self.source[from..to];
        };
        const rightIndexWindow = win: {
            const to = f: {
                if ((self.index + window) > self.source.len) {
                    break :f self.source.len;
                } else break :f self.index + window;
            };
            const from = self.index + 1;
            break :win self.source[from..to];
        };
        const rawChar = self.source[self.index];
        const char: []const u8 = if (rawChar == '\n') "\\n" else [_]u8{rawChar};

        const leftLagIndexWindow = win: {
            const from = f: {
                if (self.lagIndex >= window) {
                    break :f self.lagIndex - window;
                } else break :f 0;
            };
            const to = self.lagIndex;
            break :win self.source[from..to];
        };
        const rightLagIndexWindow = win: {
            const to = f: {
                if ((self.lagIndex + window) > self.source.len) {
                    break :f self.source.len;
                } else break :f self.lagIndex + window;
            };
            const from = self.lagIndex + 1;
            break :win self.source[from..to];
        };
        const rawLagChar = self.source[self.lagIndex];
        const lagChar: []const u8 = if (rawLagChar == '\n') "\\n" else [_]u8{rawLagChar};
        warn("index: {}, {{{}|{}|{}}} | lagIndex: {}, {{{}|{}|{}}} | arrValDepth: {}\n", self.index, leftIndexWindow, char, rightIndexWindow, self.lagIndex, leftLagIndexWindow, lagChar, rightLagIndexWindow, self.valueArrayDepth);
    }

    /// finish sets the parsers internal state to done (which is index, lagIndex == source.len + 1)
    fn finish(self: *Self) void {
        self.index = self.source.len + 1;
        self.lagIndex = self.index;
    }

    fn isFinished(self: Self) bool {
        // warn("{}\n", self.index == self.source.len + 1);
        // warn("{}\n", self.lagIndex == self.source.len + 1);
        return (self.index == self.source.len + 1) and (self.lagIndex == self.source.len + 1);
    }

    fn addKeyValueToken(self: Self, tokens: *std.ArrayList(Token), key: []const u8, value: []const u8) !void {
        const strippedKey = stripWhitespace(key);
        if (strippedKey.len == 0) return error.BadKey;
        const strippedValue = stripWhitespace(value);
        if (dbg) warn("addKV({}, {})\n", strippedKey, strippedValue);
        const valueGuess = isValidValue(strippedValue) orelse return error.BadValue;
        const token = Token{
            .KeyValue = StrKV{
                .key = strippedKey,
                .value = strippedValue,
                .kind = valueGuess,
            },
        };
        if (dbg) warn("Added token: {{{}}}\n", token);
        try tokens.append(token);
    }

    /// process increments the current character index and parses it based on
    /// its current state. the bool return value represents wether or not the
    /// parser should continue
    fn process(self: *Self, tokens: *std.ArrayList(Token)) !bool {
        if (self.isFinished()) return false;
        const quoteEvent = self.quoteTracker.process(self.source, self.index);
        switch (quoteEvent) {
            .Exit => |ty| {
                if (ty.isMultiline()) { // we just left a triple string, so jump +3
                    self.index += 2;
                }
            },
            .Enter => |ty| {
                if (ty.isMultiline()) { // we just entered a triple string, so jump +3
                    self.index += 2;
                }
            },
            else => {},
        }
        const char = self.source[self.index];
        const before = b: {
            if (self.index > 1) {
                break :b self.source[self.index - 1];
            } else break :b null;
        };

        const isNewline = char == '\n';
        const isComment = char == '#';

        const eof = self.index == self.source.len - 1;
        if (dbg) {
            self.print(5);
            if (quoteEvent != .None and self.valueArrayDepth == 0) warn("quoteEvent: {}\n", quoteEvent);
        }
        switch (self.state) {
            // we are at the default state,
            .Root => {
                // whitespace can exist
                switch (quoteEvent) {
                    .Enter => self.setState(.QuoteKey),
                    else => {
                        if (char == '[') {
                            self.lagIndex = self.index;
                            self.setState(.TableDefinition);
                        } else if (ascii.isSpace(char)) {
                            // whitespace is okay, do nothing
                        } else if (isComment) {
                            // we hit a comment, skip to next line
                            return self.skipToNewline();
                        } else if (isValidKeyChar(char)) {
                            self.setState(.Key);
                        } else return error.UnexpectedCharacter;
                    },
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
            .ValueArray => {
                const notInString = !self.quoteTracker.inString();
                if (notInString) {
                    if (char == '#') {
                        const oldLagIndex = self.lagIndex;
                        defer self.lagIndex = oldLagIndex;
                        return self.skipToNewline();
                    } else if (char == ',' and self.valueArrayDepth == 1) { // new value
                        const value = self.source[self.lagIndex..self.index];
                        const strippedValue = stripWhitespace(value);
                        try self.arrayValues.append(strippedValue);
                        self.lagIndex = self.index + 1;
                    } else if (char == '[') {
                        self.valueArrayDepth += 1;
                    } else if (char == ']') {
                        self.valueArrayDepth -= 1;
                        if (self.valueArrayDepth == 0) {
                            // there is still one more value to add
                            try self.arrayValues.append(stripWhitespace(self.source[self.lagIndex..self.index]));
                            const lastValue = self.source[self.lagIndex..self.index];
                            const strippedKey = stripWhitespace(self.key.?);
                            if (strippedKey.len == 0) return error.BadKey;
                            if (self.arrayValues.count() == 0) {
                                try tokens.append(Token{
                                    .KeyValue = StrKV{
                                        .key = strippedKey,
                                        .value = "[]",
                                        .kind = .Array,
                                    },
                                });
                            } else {
                                const values = self.arrayValues.toOwnedSlice();
                                const arrValueGuess = isValidValue(values[0]) orelse return error.BadArrayValue;
                                try tokens.append(Token{
                                    .KeyValueArray = StrKVA{
                                        .key = strippedKey,
                                        .values = values,
                                        .kind = arrValueGuess,
                                    },
                                });
                            }
                            self.key = null;
                            self.lagIndex = self.index + 1;
                            self.setState(.Root);
                        }
                    }
                }
            },
            .Value => {
                const notInString = !self.quoteTracker.inString();
                const readyToCaptureValue = isNewline or isComment;
                if (char == '[') {
                    self.setState(.ValueArray);
                    self.valueArrayDepth += 1;
                    self.lagIndex = self.index + 1;
                } else if ((notInString and readyToCaptureValue) or eof) {
                    const end = b: {
                        if (isNewline or isComment) {
                            break :b self.index;
                        } else break :b self.index + 1;
                    };
                    const value = self.source[self.lagIndex + 1 .. end];
                    try self.addKeyValueToken(tokens, self.key.?, value);
                    self.key = null;
                    self.lagIndex = b: {
                        if (isNewline or isComment) {
                            break :b self.index + 1;
                        }
                        break :b self.index;
                    };
                    self.setState(.Root);
                    if (isComment) return self.skipToNewline();
                } else if (char == '{') {
                    const strippedKey = stripWhitespace(self.key.?);
                    try tokens.append(Token{ .InlineTableDefinition = strippedKey });
                    self.setState(.InlineTable);
                }
            },
            .TableDefinition => {
                if (char == '[') {
                    self.setState(.TableArrayDefinition);
                } else if (char == ']') {
                    const tableName = stripWhitespace(self.source[self.lagIndex + 1 .. self.index]);
                    if (dbg) warn("adding table: {{{}}}\n", tableName);
                    try tokens.append(Token{ .TableDefinition = tableName });
                    self.lagIndex = self.index + 1;
                    self.setState(.Root);
                }
            },
            .TableArrayDefinition => {
                if (before) |charBefore| {
                    if (char == ']' and charBefore == ']') {
                        const start = if (self.lagIndex == 0) 0 else self.lagIndex + 1;
                        const tableName = stripWhitespace(self.source[start .. self.index + 1]);
                        if (dbg) warn("adding array table: {{{}}}\n", tableName);
                        try tokens.append(Token{ .TableArrayDefinition = tableName[2 .. tableName.len - 2] });
                        self.lagIndex = if (self.lagIndex == 0) self.index + 2 else self.index + 1;
                        self.setState(.Root);
                    }
                }
            },
            .InlineTable => {
                const isTableEnd = char == '}';
                const isSeperator = char == ',';
                if (!self.quoteTracker.inString() and (isTableEnd or isSeperator or isNewline or eof)) {
                    const raw = self.source[self.lagIndex + 1 .. self.index];
                    const rawNoWhitespace = stripWhitespace(raw);
                    const trimmed = mem.trimLeft(u8, rawNoWhitespace, "{");
                    const keyValue = stripWhitespace(trimmed);
                    if (mem.indexOfScalar(u8, keyValue, '=')) |eqIdx| {
                        const strippedKey = stripWhitespace(keyValue[0..eqIdx]);
                        const strippedValue = stripWhitespace(keyValue[eqIdx + 1 ..]);
                        const valueGuess = isValidValue(strippedValue) orelse return error.BadValue;
                        const token = Token{
                            .KeyValue = StrKV{
                                .key = strippedKey,
                                .value = strippedValue,
                                .kind = valueGuess,
                            },
                        };
                        try tokens.append(token);
                    }
                    self.lagIndex = self.index;
                    if (isNewline or eof) {
                        try tokens.append(.StopInlineTable);
                        self.setState(.Root);
                    }
                }
            },
        }
        self.index += 1;
        return self.shouldContinue();
    }

    fn shouldContinue(self: Self) bool {
        return self.index < self.source.len and !self.isFinished();
    }

    pub fn lex(self: *Self) ![]Token {
        var tokens = std.ArrayList(Token).init(self.allocator);
        while (try self.process(&tokens)) {}
        return tokens.toOwnedSlice();
    }

    const ArrayTableContext = struct {
        name: []const u8,
        tables: std.ArrayList(Value),
        currentTable: Table,
        allocator: *mem.Allocator,

        fn init(allocator: *mem.Allocator, rootTable: *Table, name: []const u8) ArrayTableContext {
            const slice = b: {
                if (rootTable.space.remove(name)) |t| {
                    break :b t.value.Array;
                }
                break :b [_]Value{};
            };
            return ArrayTableContext{
                .name = name,
                .tables = std.ArrayList(Value).fromOwnedSlice(allocator, slice),
                .currentTable = Table.newNamed(allocator, name),
                .allocator = allocator,
            };
        }

        fn another(self: *ArrayTableContext) !void {
            try self.tables.append(Value{ .SubTable = self.currentTable });
            self.currentTable = Table.newNamed(self.allocator, self.name);
        }

        // dumps the entire arraylist of tables as an owned slice
        fn toValue(self: *ArrayTableContext) Value {
            return Value{ .Array = self.tables.toOwnedSlice() };
        }
    };

    pub fn parse(allocator: *mem.Allocator, source: []const u8) !Table {
        var root = Table.newRoot(allocator);
        var parser = Parser.init(allocator, source);
        defer parser.deinit();
        const tokens = try parser.lex();
        var consumingInlineTable: ?Table = null;
        var tableArrayCtx: ?ArrayTableContext = null;
        var tableContext: ?[]const u8 = null;
        for (tokens) |token| {
            if (dbg) warn("consuming token {}\n", token);
            if (tableArrayCtx) |*ctx| {
                switch (token) {
                    .KeyValue => |kv| try ctx.currentTable.put(kv.key, try Value.from(allocator, kv.kind, kv.value)),
                    .TableDefinition => |tableName| { // normal table def, go back to normal
                        try root.put(ctx.name, ctx.toValue());
                        tableArrayCtx = null;
                        tableContext = tableName;
                    },
                    .TableArrayDefinition => |tableName| { // start a new object
                        try ctx.another();
                    },
                    else => unreachable,
                }
            } else if (consumingInlineTable) |*inlineTable| {
                switch (token) {
                    .KeyValue => |kv| try inlineTable.put(kv.key, try Value.from(allocator, kv.kind, kv.value)),
                    .StopInlineTable => {
                        try root.put(inlineTable.name.?, Value{ .SubTable = inlineTable.* });
                        consumingInlineTable = null;
                    },
                    else => unreachable,
                }
            } else switch (token) {
                .KeyValue => |kv| {
                    const keyName = key: {
                        if (tableContext) |contextName| {
                            break :key try mem.concat(allocator, u8, [_][]const u8{ contextName, ".", kv.key });
                        } else break :key kv.key;
                    };
                    try root.put(keyName, try Value.from(allocator, kv.kind, kv.value));
                },
                .KeyValueArray => |kva| {
                    var arr = std.ArrayList(Value).init(allocator);
                    for (kva.values) |value| {
                        try arr.append(try Value.from(allocator, kva.kind, value));
                    }
                    const values = arr.toOwnedSlice();
                    const keyName = key: {
                        if (tableContext) |contextName| {
                            break :key try mem.concat(allocator, u8, [_][]const u8{ contextName, ".", kva.key });
                        } else break :key kva.key;
                    };
                    try root.put(keyName, Value{ .Array = values });
                },
                .InlineTableDefinition => |tableName| {
                    var newTable = Table.newNamed(allocator, tableName);
                    consumingInlineTable = newTable;
                },
                .TableDefinition => |tableName| tableContext = tableName,
                .StopInlineTable => unreachable,
                .TableArrayDefinition => |tableName| {
                    tableArrayCtx = ArrayTableContext.init(allocator, &root, tableName);
                },
            }
        }
        if (tableArrayCtx) |*ctx| {
            try ctx.another();
            try root.put(ctx.name, ctx.toValue());
            tableArrayCtx = null;
        }
        return root;
    }
};

fn stripWhitespace(source: []const u8) []const u8 {
    return mem.trim(u8, source, " \r\t\n");
}

// solely for parsing
fn isValidKeyChar(char: u8) bool {
    const isPunct = char == '.';
    const isDash = char == '_' or char == '-';
    const isQuote = char == '"' or char == '\'';
    return ascii.isSpace(char) or ascii.isAlpha(char) or ascii.isDigit(char) or isDash or isQuote or isPunct;
}

fn trimQuotes(comptime T: type, input: var) @typeOf(input) {
    return mem.trim(T, input, "\"'");
}

fn isValidValueInteger(value: []const u8) bool {
    if (dbg) warn("iVVI: {}\n", value);
    // trim leading zeroes
    const trimmed = if (!mem.allEqual(u8, value, '0')) mem.trimLeft(u8, value, "0") else "0";
    // check every char to make sure its in desired set
    const format = IntegerFormat.of(value);
    const clean = switch (format) {
        .Hex, .Binary, .Octal => |clean| clean,
        .Decimal => value,
    };
    for (b: {
        const first = clean[0];
        if (first == '+' or first == '-') {
            break :b clean[1..];
        } else break :b clean;
    }) |c| {
        if (dbg) warn("(int) c={c}\n", c);
        const isValidChar = switch (IntegerFormat.of(value)) {
            .Hex => ascii.isXDigit(c),
            .Binary => c == '0' or c == '1',
            .Octal => c <= '7' and c >= '0',
            .Decimal => ascii.isDigit(c),
        };
        if (!isValidChar and c != '_') return false;
    }
    return true;
}

fn isValidValueFloat(value: []const u8) bool {
    if (dbg) warn("iVVF: {}\n", value);
    // fast path, check if there is a '.' or 'e'
    if (mem.indexOfAny(u8, value, ".eE") == null) {
        return false;
    }
    // trim leading zeroes
    const trimmed = if (!mem.allEqual(u8, value, '0')) mem.trimLeft(u8, value, "0") else "0";
    // check every char to make sure its in desired set
    for (b: {
        const first = trimmed[0];
        if (first == '+' or first == '-') {
            break :b trimmed[1..];
        } else break :b trimmed;
    }) |c| {
        if (dbg) warn("(int) c={c}\n", c);
        const isPunct = c == '_' or c == '.';
        const isSign = c == '+' or c == '-';
        if (!ascii.isDigit(c) and !isPunct and !isSign and !eqlIgnoreCaseChar(c, 'e')) return false;
    }
    return true;
}

fn eqlIgnoreCaseChar(char: u8, eql: u8) bool {
    return ascii.eqlIgnoreCase([_]u8{char}, [_]u8{eql});
}

fn isValidValueString(value: []const u8) bool {
    // if the first char isnt a quote
    if (value[0] != '"' and value[0] != '\'') return false;

    // make sure quotes match
    if (value[0] == '"' and value[value.len - 1] != '"') return false;
    if (value[0] == '\'' and value[value.len - 1] != '\'') return false;

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
            .Exit => |ty| {
                if (enteredString) {
                    // we should exit the string at the end of the value
                    if (ty.isMultiline()) {
                        return n == (value.len - 3);
                    }
                    return n == (value.len - 1);
                } else unreachable;
            },
            .None => {}, // do nothing
        }
    }
    return false;
}

fn isValidValueBoolean(value: []const u8) bool {
    if (value.len > 5 or value.len < 4) return false;
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
