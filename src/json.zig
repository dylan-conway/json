const std = @import("std");

const OOM = std.mem.Allocator.Error;

pub const Expr = union(enum) {
    null: Null,
    boolean: Boolean,
    number: Number,
    string: String,
    array: Array,
    object: Object,

    pub fn eql(l: *const Expr, r: *const Expr) bool {
        return switch (l.*) {
            .null => r.* == .null,
            .boolean => r.* == .boolean and l.boolean.value == r.boolean.value,
            .number => r.* == .number and l.number.value == r.number.value,
            .string => {
                // TODO
                return true;
            },
            .array => {
                if (r.* != .array or l.array.items.len != r.array.items.len) {
                    return false;
                }

                // TODO: !!!
                // for (l.array.items.items, r.array.items.items) |l_item, r_item| {
                //     if (!l_item.eql(&r_item)) {
                //         return false;
                //     }
                // }

                return true;
            },
            .object => {
                if (r.* != .object or l.object.props.len != r.object.props.len) {
                    return false;
                }

                // TODO: !!!
                // for (l.object.props.items, r.object.props.items) |l_prop, r_prop| {
                //     // TODO
                //     // if (!l_prop.key.eql(&r_prop.key)) {
                //     //     return false;
                //     // }

                //     if (!l_prop.value.eql(&r_prop.value)) {
                //         return false;
                //     }
                // }

                return true;
            },
        };
    }

    pub fn deinit(this: *const Expr) void {
        switch (this.*) {
            .array => {
                // for (this.array.items.items) |item| {
                //     item.deinit();
                // }
                // this.array.items.deinit();
            },
            .object => {
                // for (this.object.props.items) |prop| {
                //     prop.value.deinit();
                // }
                // this.object.props.deinit();
            },
            else => {},
        }
    }

    const Null = struct {
        pos: usize,
    };

    const Boolean = struct {
        pos: usize,
        value: bool,
    };

    const Number = struct {
        pos: usize,
        value: f64,
    };

    pub const String = struct {
        pos: usize,
        chars: Slice(u8),
    };

    const Array = struct {
        pos: usize,
        items: Slice(Expr),
    };

    pub const Object = struct {
        pos: usize,
        props: Slice(Prop),

        pub const Prop = struct {
            key: Expr,
            value: Expr,
        };
    };
};

const Token = union(enum) {
    eof,
    true: True,
    false: False,
    null: Null,
    number: Number,
    string: String,
    opening_bracket: OpeningBracket,
    closing_bracket: ClosingBracket,
    opening_brace: OpeningBrace,
    closing_brace: ClosingBrace,
    comma: Comma,
    colon: Colon,

    const Eof = void;
    const True = struct {
        pos: usize,

        pub fn expr(this: *const @This()) Expr {
            return .{ .boolean = .{ .pos = this.pos, .value = true } };
        }
    };
    const False = struct {
        pos: usize,

        pub fn expr(this: *const @This()) Expr {
            return .{ .boolean = .{ .pos = this.pos, .value = false } };
        }
    };
    const Null = struct {
        pos: usize,

        pub fn expr(this: *const @This()) Expr {
            return .{ .null = .{ .pos = this.pos } };
        }
    };
    pub const Number = struct {
        pos: usize,
        value: f64,

        pub fn expr(this: *const @This()) Expr {
            return .{ .number = .{ .pos = this.pos, .value = this.value } };
        }
    };
    pub const String = struct {
        pos: usize,
        chars: Slice(u8),

        pub fn expr(this: *const String) Expr {
            return .{ .string = .{ .pos = this.pos, .chars = this.chars } };
        }
    };
    const OpeningBracket = struct {
        pos: usize,
    };
    const ClosingBracket = struct {
        pos: usize,
    };
    const OpeningBrace = struct {
        pos: usize,
    };
    const ClosingBrace = struct {
        pos: usize,
    };
    const Comma = struct {
        pos: usize,
    };
    const Colon = struct {
        pos: usize,
    };
};

fn Slice(comptime T: type) type {
    return struct {
        off: usize,
        len: usize,

        pub const empty: @This() = .{ .off = 0, .len = 0 };

        pub fn slice(this: *const @This(), buf: []T) []T {
            return buf[this.off..][0..this.len];
        }
    };
}

fn ReverseStack(comptime T: type) type {
    return struct {
        items: []align(1) T,
        offset: usize,

        pub fn init(bytes: []u8) @This() {
            const items: []align(1) T = @ptrCast(@alignCast(bytes[@mod(bytes.len, @sizeOf(T))..]));
            return .{
                .items = items,
                .offset = items.len,
            };
        }

        pub fn slice(this: *const @This()) []T {
            return this.items[this.offset..];
        }

        pub fn push(this: *@This(), item: T) void {
            const offset = this.offset - 1;
            this.items[offset] = item;
            this.offset = offset;
        }

        pub fn pop(this: *@This()) T {
            const item = this.items[this.offset];
            this.offset += 1;
            return item;
        }
    };
}

const Buf = struct {
    bytes: std.ArrayListAlignedUnmanaged(u8, .@"1") = .empty,

    pub fn slice(this: *Buf, comptime T: type) []T {
        return @ptrCast(this.bytes.items);
    }

    pub fn appendChar(this: *Buf, char: u8) void {
        this.bytes.appendAssumeCapacity(char);
    }

    pub fn appendExpr(this: *Buf, expr: Expr) void {
        this.bytes.appendSliceAssumeCapacity(std.mem.asBytes(&expr));
    }

    pub fn appendString(this: *Buf, string: Expr.String) void {
        this.bytes.appendSliceAssumeCapacity(std.mem.asBytes(&string));
    }
};

pub fn parse(comptime encoding: Encoding, allocator: std.mem.Allocator, source: []const encoding.unit()) Parser(encoding).ParseResult {
    var parser: Parser(encoding) = .init(source);

    {
        // count
        parser.next(.count) catch |err| {
            return .fail(err, &parser, allocator);
        };

        _ = parser.countExpr() catch |err| {
            return .fail(err, &parser, allocator);
        };

        parser.next(.count) catch |err| {
            return .fail(err, &parser, allocator);
        };

        if (parser.token != .eof) {
            return .fail(error.UnexpectedCharacter, &parser, allocator);
        }

        // return .success(fake_expr, &parser);
    }

    parser.allocate(allocator) catch |err| {
        return .fail(err, &parser, allocator);
    };

    return .success(parser.appendExpr(), &parser, allocator);

    // append

    // const State = struct {
    //     //
    // };

    // var stack: std.ArrayList(State) = .init(allocator);
    // defer stack.deinit();

    // parser.next(.append) catch {
    //     unreachable;
    // };

    // const result = parser.countExpr(.append) catch {
    //     unreachable;
    // };

    // parser.next(.append) catch {
    //     unreachable;
    // };

    // return .success(result, &parser);
}

const Method = enum {
    count,
    append,
};

const Encoding = enum {
    utf8,
    utf16,
    utf32,

    pub fn unit(comptime encoding: Encoding) type {
        return switch (encoding) {
            .utf8 => u8,
            .utf16 => u16,
            .utf32 => u32,
        };
    }
};

pub fn Parser(comptime encoding: Encoding) type {
    return struct {
        source: []const encoding.unit(),

        pos: usize,
        token: Token,

        string_total: usize = 0,
        array_total: usize = 0,
        object_total: usize = 0,

        buf: Buf = .{},

        pub fn init(source: []const encoding.unit()) @This() {
            return .{
                .source = source,
                .pos = 0,
                .token = .eof,
            };
        }

        fn allocate(this: *@This(), allocator: std.mem.Allocator) OOM!void {
            const bytes = try allocator.alloc(
                u8,
                (this.array_total * @sizeOf(Expr)) + (this.object_total * 2 * @sizeOf(Expr)) + this.string_total,
            );

            // reset fields
            this.* = init(this.source);

            this.buf = .{ .bytes = .initBuffer(bytes) };
        }

        const ParseResult = struct {
            res: union(enum) {
                root: Expr,
                err: union(enum) {
                    oom,
                    syntax_error: Token,
                    unexpected_character: struct {
                        pos: usize,
                    },
                },
            },

            source: []const u8,

            allocator: std.mem.Allocator,
            buf: Buf,

            pub fn deinit(this: *const @This()) void {
                @constCast(this).buf.bytes.deinit(this.allocator);
            }

            pub fn success(expr: Expr, parser: *const Parser(encoding), allocator: std.mem.Allocator) @This() {
                return .{
                    .res = .{
                        .root = expr,
                    },

                    .source = parser.source,
                    .allocator = allocator,
                    .buf = parser.buf,
                };
            }

            pub fn fail(err: ParseError, parser: *const Parser(encoding), allocator: std.mem.Allocator) @This() {
                return .{
                    .res = .{
                        .err = switch (err) {
                            error.OutOfMemory => .oom,
                            error.SyntaxError => .{ .syntax_error = parser.token },
                            error.UnexpectedCharacter => .{ .unexpected_character = .{ .pos = parser.pos -| 1 } },
                        },
                    },
                    .source = parser.source,
                    .allocator = allocator,
                    .buf = parser.buf,
                };
            }
        };

        const ParseError = OOM || error{ SyntaxError, UnexpectedCharacter };

        fn appendExpr(this: *@This()) Expr {
            var stack: ReverseStack(Expr) = .init(this.buf.bytes.allocatedSlice());

            var root: Expr = switch (this.next(.append)) {
                .comma, .colon, .eof, .closing_brace, .closing_bracket => unreachable,

                inline .true, .false, .null, .number, .string => |t| {
                    return t.expr();
                },

                .opening_brace => |opening_brace| .{ .object = .{ .pos = opening_brace.pos, .props = .empty } },
                .opening_bracket => |opening_bracket| .{ .array = .{ .pos = opening_bracket.pos, .items = .empty } },
            };

            next: switch (this.next(.append)) {
                .eof => {
                    std.debug.assert(stack.offset == stack.items.len);
                    return root;
                },

                .comma, .colon => {
                    // json was already validated, we can skip to the next token
                    // without checking for correctness.
                    continue :next this.next(.append);
                },

                inline .true, .false, .null, .number, .string => |token| {
                    stack.push(token.expr());
                    continue :next this.next(.append);
                },

                .opening_brace => |opening_brace| {
                    const obj: Expr = .{ .object = .{ .pos = opening_brace.pos, .props = .empty } };
                    stack.push(obj);
                    continue :next this.next(.append);
                },

                .opening_bracket => |opening_bracket| {
                    const arr: Expr = .{ .array = .{ .pos = opening_bracket.pos, .items = .empty } };
                    stack.push(arr);
                    continue :next this.next(.append);
                },

                .closing_brace => |closing_brace| {
                    _ = closing_brace;

                    // flush the items from the stack.

                    const end = stack.offset;
                    var offset = end;
                    while (offset < stack.items.len and (stack.items[offset] != .object or stack.items[offset].object.props.off != 0 or offset == stack.items.len - 1)) {
                        offset += 1;
                    }

                    stack.offset = offset;

                    var obj = if (offset < stack.items.len) &stack.items[offset] else &root;
                    offset -|= 1;

                    obj.object.props.off = this.buf.bytes.items.len;
                    while (offset > end) {
                        this.buf.appendExpr(stack.items[offset]);
                        offset -= 1;
                    }
                    obj.object.props.len = this.buf.bytes.items.len - obj.object.props.off;

                    continue :next this.next(.append);
                },

                .closing_bracket => |closing_bracket| {
                    _ = closing_bracket;

                    // flush the keys and values from the stack.

                    const end = stack.offset;
                    var offset = end;
                    while (offset < stack.items.len and (stack.items[offset] != .array or stack.items[offset].array.items.off != 0 or offset == stack.items.len - 1)) {
                        offset += 1;
                    }

                    stack.offset = offset;

                    var arr = if (offset < stack.items.len) &stack.items[offset] else &root;
                    offset -|= 1;

                    arr.array.items.off = this.buf.bytes.items.len;
                    while (offset > end) {
                        this.buf.appendExpr(stack.items[offset]);
                        offset -= 1;
                    }
                    arr.array.items.len = this.buf.bytes.items.len - arr.array.items.off;

                    continue :next this.next(.append);
                },
            }
        }

        fn countExpr(this: *@This()) ParseError!Expr {
            switch (this.token) {
                .eof => {
                    return error.SyntaxError;
                },
                .comma => {
                    return error.SyntaxError;
                },
                .colon => {
                    return error.SyntaxError;
                },
                .closing_brace => {
                    return error.SyntaxError;
                },
                .closing_bracket => {
                    return error.SyntaxError;
                },
                .true => |@"true"| {
                    return .{ .boolean = .{ .pos = @"true".pos, .value = true } };
                },
                .false => |@"false"| {
                    return .{ .boolean = .{ .pos = @"false".pos, .value = false } };
                },
                .null => |@"null"| {
                    return .{ .null = .{ .pos = @"null".pos } };
                },
                .number => |number| {
                    return .{ .number = .{ .pos = number.pos, .value = number.value } };
                },
                .string => |string| {
                    return .{ .string = .{ .pos = string.pos, .chars = string.chars } };
                },
                .opening_brace => |opening_brace| {
                    try this.next(.count);

                    var first = true;

                    while (this.token != .closing_brace) {
                        if (first) {
                            first = false;
                        } else if (this.token != .comma) {
                            return error.SyntaxError;
                        } else {
                            try this.next(.count);
                        }

                        const key = try this.countExpr();
                        if (key != .string) {
                            return error.SyntaxError;
                        }

                        try this.next(.count);

                        if (this.token != .colon) {
                            return error.SyntaxError;
                        }

                        try this.next(.count);

                        const value = try this.countExpr();
                        _ = value;

                        this.object_total += 1;

                        try this.next(.count);
                    }

                    return .{ .object = .{ .pos = opening_brace.pos, .props = .empty } };
                },
                .opening_bracket => |opening_bracket| {
                    try this.next(.count);

                    var first = true;

                    while (this.token != .closing_bracket) {
                        if (first) {
                            first = false;
                        } else if (this.token != .comma) {
                            return error.SyntaxError;
                        } else {
                            try this.next(.count);
                        }

                        const item = try this.countExpr();
                        _ = item;

                        this.array_total += 1;

                        try this.next(.count);
                    }

                    return .{ .array = .{ .pos = opening_bracket.pos, .items = .empty } };
                },
            }
        }

        fn inc(this: *@This()) encoding.unit() {
            if (this.pos >= this.source.len) {
                this.pos += 1;
                return 0;
            }
            const char = this.source[this.pos];
            this.pos += 1;
            return char;
        }

        fn NextResult(comptime method: Method) type {
            return switch (method) {
                .count => ParseError!void,
                .append => Token,
            };
        }

        fn next(this: *@This(), comptime method: Method) NextResult(method) {
            next_char: switch (this.inc()) {
                else => switch (comptime method) {
                    .count => return error.UnexpectedCharacter,
                    .append => unreachable,
                },
                0 => {
                    if (comptime method == .append) {
                        return .eof;
                    }

                    if (this.pos <= this.source.len) {
                        return error.UnexpectedCharacter;
                    }

                    // 0 is also used for eof
                    this.token = .eof;
                    return;
                },
                ' ', '\n', '\r', '\t' => {
                    continue :next_char this.inc();
                },
                ',' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .comma = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                ':' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .colon = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                '[' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .opening_bracket = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                ']' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .closing_bracket = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                '{' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .opening_brace = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                '}' => {
                    const pos = this.pos -| 1;
                    const token: Token = .{ .closing_brace = .{ .pos = pos } };
                    if (comptime method == .append) {
                        return token;
                    }
                    this.token = token;
                    return;
                },
                'n' => {
                    if (comptime method == .append) {
                        return this.parseKeyword(.null) catch unreachable;
                    }
                    this.token = try this.parseKeyword(.null);
                    return;
                },
                'f' => {
                    if (comptime method == .append) {
                        return this.parseKeyword(.false) catch unreachable;
                    }
                    this.token = try this.parseKeyword(.false);
                    return;
                },
                't' => {
                    if (comptime method == .append) {
                        return this.parseKeyword(.true) catch unreachable;
                    }
                    this.token = try this.parseKeyword(.true);
                    return;
                },
                '"' => {
                    const start = this.pos -| 1;

                    const off = this.buf.bytes.items.len;

                    while (switch (this.inc()) {
                        '"' => null,
                        0 => switch (comptime method) {
                            .count => return error.UnexpectedCharacter,
                            .append => unreachable,
                        },
                        else => |unit| unit,
                    }) |unit| {
                        switch (unit) {
                            '\\' => {
                                switch (this.inc()) {
                                    '"' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('"'),
                                    },
                                    '\\' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\\'),
                                    },
                                    '/' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('/'),
                                    },
                                    'b' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\x08'),
                                    },
                                    'f' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\x0c'),
                                    },
                                    'n' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\n'),
                                    },
                                    'r' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\r'),
                                    },
                                    't' => switch (comptime method) {
                                        .count => this.string_total += 1,
                                        .append => this.buf.appendChar('\t'),
                                    },
                                    'u' => {
                                        switch (this.inc()) {
                                            0 => switch (comptime method) {
                                                .count => return error.UnexpectedCharacter,
                                                .append => unreachable,
                                            },
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    if (comptime method == .append) {
                                                        unreachable;
                                                    }
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => switch (comptime method) {
                                                .count => return error.UnexpectedCharacter,
                                                .append => unreachable,
                                            },
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    if (comptime method == .append) {
                                                        unreachable;
                                                    }
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => switch (comptime method) {
                                                .count => return error.UnexpectedCharacter,
                                                .append => unreachable,
                                            },
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    if (comptime method == .append) {
                                                        unreachable;
                                                    }
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => switch (comptime method) {
                                                .count => return error.UnexpectedCharacter,
                                                .append => unreachable,
                                            },
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    if (comptime method == .append) {
                                                        unreachable;
                                                    }
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }

                                        // TODO: decode and count/append
                                    },
                                    else => switch (comptime method) {
                                        .count => return error.UnexpectedCharacter,
                                        .append => unreachable,
                                    },
                                }
                            },
                            0...0x1f => switch (comptime method) {
                                .count => return error.UnexpectedCharacter,
                                .append => unreachable,
                            },
                            else => switch (comptime method) {
                                .count => this.string_total += 1,
                                .append => this.buf.appendChar(unit),
                            },
                        }
                    }

                    return switch (comptime method) {
                        .count => this.token = .{ .string = .{ .pos = start, .chars = .{ .off = 0, .len = 0 } } },
                        .append => .{
                            .string = .{
                                .pos = start,
                                .chars = .{ .off = off, .len = this.buf.bytes.items.len - off },
                            },
                        },
                    };
                },
                '-' => {
                    const start = this.pos -| 1;

                    const first_digit = switch (this.inc()) {
                        '0'...'9' => |digit| digit,
                        else => switch (comptime method) {
                            .count => return error.UnexpectedCharacter,
                            .append => unreachable,
                        },
                    };

                    if (comptime method == .append) {
                        return .{ .number = .{ .pos = start, .value = -(this.parseNumber(start + 1, first_digit) catch unreachable) } };
                    }

                    const num = try this.parseNumber(start + 1, first_digit);

                    this.token = .{ .number = .{ .pos = start, .value = -num } };
                    return;
                },
                '0'...'9' => |first_digit| {
                    const start = this.pos -| 1;

                    if (comptime method == .append) {
                        return .{ .number = .{ .pos = start, .value = this.parseNumber(start, first_digit) catch unreachable } };
                    }

                    const num = try this.parseNumber(start, first_digit);

                    this.token = .{ .number = .{ .pos = start, .value = num } };
                    return;
                },
            }
        }

        fn parseNumber(this: *@This(), start: usize, first_digit: u8) ParseError!f64 {
            std.debug.assert(std.ascii.isDigit(first_digit));

            var curr = first_digit;

            switch (curr) {
                '0' => {
                    curr = this.inc();
                },
                else => {
                    while (switch (curr) {
                        '0'...'9' => true,
                        else => false,
                    }) {
                        curr = this.inc();
                    }
                },
            }

            switch (curr) {
                '.' => {
                    curr = this.inc();
                    switch (curr) {
                        '0'...'9' => {
                            curr = this.inc();
                        },
                        else => return error.UnexpectedCharacter,
                    }

                    while (switch (curr) {
                        '0'...'9' => true,
                        else => false,
                    }) {
                        curr = this.inc();
                    }
                },
                else => {},
            }

            switch (curr) {
                'e', 'E' => {
                    curr = this.inc();
                    switch (curr) {
                        '+', '-' => {
                            curr = this.inc();
                            while (switch (curr) {
                                '0'...'9' => true,
                                else => false,
                            }) {
                                curr = this.inc();
                            }
                        },
                        else => {
                            while (switch (curr) {
                                '0'...'9' => true,
                                else => false,
                            }) {
                                curr = this.inc();
                            }
                        },
                    }
                },
                else => {},
            }

            this.pos -= 1;

            return std.fmt.parseFloat(f64, this.source[start..this.pos]) catch {
                return error.UnexpectedCharacter;
            };
        }

        const Keyword = enum {
            null,
            true,
            false,

            pub fn remaining(comptime this: @This()) []const u8 {
                return switch (this) {
                    .null => "ull",
                    .true => "rue",
                    .false => "alse",
                };
            }
        };

        fn parseKeyword(this: *@This(), comptime keyword: Keyword) ParseError!Token {
            const start = this.pos -| 1;
            inline for (comptime keyword.remaining()) |c| {
                if (this.inc() != c) {
                    return error.UnexpectedCharacter;
                }
            }
            return @unionInit(Token, @tagName(keyword), .{ .pos = start });
        }
    };
}
