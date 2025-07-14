const std = @import("std");

const OOM = std.mem.Allocator.Error;

const Expr = union(enum) {
    null: Null,
    boolean: Boolean,
    number: Number,
    string: String,
    array: Array,
    object: Object,

    pub fn deinit(this: *const Expr) void {
        switch (this.*) {
            .array => {
                for (this.array.items.items) |item| {
                    item.deinit();
                }
                this.array.items.deinit();
            },
            .object => {
                for (this.object.props.items) |prop| {
                    prop.value.deinit();
                }
                this.object.props.deinit();
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

    const String = struct {
        pos: usize,
        chars: struct {
            off: usize,
            len: usize,
        },
    };

    const Array = struct {
        pos: usize,
        items: std.ArrayList(Expr),
    };

    const Object = struct {
        pos: usize,
        props: std.ArrayList(Prop),

        const Prop = struct {
            key: String,
            value: Expr,
        };
    };
};

pub fn Parser(comptime CharType: type) type {
    return struct {
        allocator: std.mem.Allocator,
        lexer: Lexer(CharType),

        err: ?struct {
            pos: usize,
        },

        pub fn init(allocator: std.mem.Allocator, source: []const CharType) @This() {
            return .{
                .allocator = allocator,
                .lexer = .{
                    .allocator = allocator,
                    .source = source,
                    .pos = 0,
                    .token = .eof,
                },
                .err = null,
            };
        }

        pub fn deinit(this: *const @This()) void {
            _ = this;
        }

        const ParseResult = union(enum) {
            expr: Expr,
            err: union(enum) {
                oom,
                invalid_syntax: Lexer(CharType).Token,
                unexpected_character: struct {
                    pos: usize,
                },
            },

            pub fn unwrap(this: *const @This()) ParseError!Expr {
                return switch (this.*) {
                    .expr => |expr| expr,
                    .err => |err| switch (err) {
                        .oom => error.OutOfMemory,
                        .invalid_syntax => error.SyntaxError,
                        .unexpected_character => error.UnexpectedCharacter,
                    },
                };
            }

            pub fn success(expr: Expr) @This() {
                return .{ .expr = expr };
            }
        };

        const ParseError = OOM || Lexer(CharType).LexError || error{SyntaxError};

        pub fn parse(this: *@This()) ParseResult {
            this.lexer.next() catch |err| switch (err) {
                error.OutOfMemory => {
                    return .{ .err = .oom };
                },
                error.UnexpectedCharacter => {
                    return .{ .err = .{ .unexpected_character = .{ .pos = this.lexer.pos -| 1 } } };
                },
            };

            const res = this.parseExpr(.count) catch |err| {
                switch (err) {
                    error.OutOfMemory => {
                        return .{ .err = .oom };
                    },
                    error.SyntaxError => {
                        return .{ .err = .{ .invalid_syntax = this.lexer.token } };
                    },
                    error.UnexpectedCharacter => {
                        return .{ .err = .{ .unexpected_character = .{ .pos = this.lexer.pos -| 1 } } };
                    },
                }
            };

            this.lexer.next() catch |err| switch (err) {
                error.OutOfMemory => {
                    res.deinit();
                    return .{ .err = .oom };
                },
                error.UnexpectedCharacter => {
                    res.deinit();
                    return .{ .err = .{ .unexpected_character = .{ .pos = this.lexer.pos -| 1 } } };
                },
            };

            if (this.lexer.token != .eof) {
                res.deinit();
                return .{ .err = .oom };
            }

            return .success(res);
        }

        fn parseExpr(this: *@This(), comptime step: enum { count, append }) ParseError!Expr {
            switch (this.lexer.token) {
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
                    return .{ .string = .{ .pos = string.pos, .chars = .{ .off = string.chars.off, .len = string.chars.len } } };
                },
                .opening_brace => |opening_brace| {
                    var props: std.ArrayList(Expr.Object.Prop) = .init(this.allocator);
                    errdefer {
                        for (props.items) |prop| {
                            prop.value.deinit();
                        }
                        props.deinit();
                    }

                    try this.lexer.next();

                    var first = true;

                    while (this.lexer.token != .closing_brace) {
                        if (first) {
                            first = false;
                        } else if (this.lexer.token != .comma) {
                            return error.SyntaxError;
                        } else {
                            try this.lexer.next();
                        }

                        const key = try this.parseExpr(step);
                        if (key != .string) {
                            return error.SyntaxError;
                        }

                        try this.lexer.next();

                        if (this.lexer.token != .colon) {
                            return error.SyntaxError;
                        }

                        try this.lexer.next();

                        const value = try this.parseExpr(step);

                        try props.append(.{ .key = key.string, .value = value });

                        try this.lexer.next();
                    }

                    return .{ .object = .{ .props = props, .pos = opening_brace.pos } };
                },
                .opening_bracket => |opening_bracket| {
                    var items: std.ArrayList(Expr) = .init(this.allocator);
                    errdefer {
                        for (items.items) |item| {
                            item.deinit();
                        }
                        items.deinit();
                    }

                    try this.lexer.next();

                    var first = true;

                    while (this.lexer.token != .closing_bracket) {
                        if (first) {
                            first = false;
                        } else if (this.lexer.token != .comma) {
                            return error.SyntaxError;
                        } else {
                            try this.lexer.next();
                        }

                        try items.append(try this.parseExpr(step));

                        try this.lexer.next();
                    }

                    return .{ .array = .{ .items = items, .pos = opening_bracket.pos } };
                },
            }
        }
    };
}

fn Lexer(comptime CharType: type) type {
    return struct {
        allocator: std.mem.Allocator,
        source: []const CharType,
        pos: usize,

        token: Token,

        const Token = union(enum) {
            eof,

            true: struct {
                pos: usize,
            },
            false: struct {
                pos: usize,
            },
            null: struct {
                pos: usize,
            },
            number: Number,
            string: String,
            opening_bracket: struct {
                pos: usize,
            },
            closing_bracket: struct {
                pos: usize,
            },
            opening_brace: struct {
                pos: usize,
            },
            closing_brace: struct {
                pos: usize,
            },

            comma: struct {
                pos: usize,
            },
            colon: struct {
                pos: usize,
            },

            pub const Number = struct {
                pos: usize,
                value: f64,
            };

            pub const String = struct {
                pos: usize,
                chars: struct {
                    off: usize,
                    len: usize,
                },
            };
        };

        fn inc(this: *@This()) CharType {
            if (this.pos >= this.source.len) {
                this.pos += 1;
                return 0;
            }
            const char = this.source[this.pos];
            this.pos += 1;
            return char;
        }

        pub const LexError = OOM || error{UnexpectedCharacter};

        fn next(this: *@This()) LexError!void {
            next_char: switch (this.inc()) {
                else => {
                    return error.UnexpectedCharacter;
                },
                0 => {
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
                    this.token = .{ .comma = .{ .pos = pos } };
                    return;
                },
                ':' => {
                    const pos = this.pos -| 1;
                    this.token = .{ .colon = .{ .pos = pos } };
                    return;
                },
                '[' => {
                    const pos = this.pos -| 1;
                    this.token = .{ .opening_bracket = .{ .pos = pos } };
                    return;
                },
                ']' => {
                    const pos = this.pos -| 1;
                    this.token = .{ .closing_bracket = .{ .pos = pos } };
                    return;
                },
                '{' => {
                    const pos = this.pos -| 1;
                    this.token = .{ .opening_brace = .{ .pos = pos } };
                    return;
                },
                '}' => {
                    const pos = this.pos -| 1;
                    this.token = .{ .closing_brace = .{ .pos = pos } };
                    return;
                },
                'n' => {
                    this.token = try this.lexKeyword(.null);
                    return;
                },
                'f' => {
                    this.token = try this.lexKeyword(.false);
                    return;
                },
                't' => {
                    this.token = try this.lexKeyword(.true);
                    return;
                },
                '"' => {
                    const start = this.pos -| 1;

                    const off = this.pos;

                    while (switch (this.inc()) {
                        '"' => null,
                        0 => return error.UnexpectedCharacter,
                        else => |unit| unit,
                    }) |unit| {
                        switch (unit) {
                            '\\' => {
                                switch (this.inc()) {
                                    '"',
                                    '\\',
                                    '/',
                                    'b',
                                    'f',
                                    'n',
                                    'r',
                                    't',
                                    => {},
                                    'u' => {
                                        switch (this.inc()) {
                                            0 => return error.UnexpectedCharacter,
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => return error.UnexpectedCharacter,
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => return error.UnexpectedCharacter,
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                        switch (this.inc()) {
                                            0 => return error.UnexpectedCharacter,
                                            else => |h| {
                                                if (!std.ascii.isHex(h)) {
                                                    return error.UnexpectedCharacter;
                                                }
                                            },
                                        }
                                    },
                                    else => {
                                        return error.UnexpectedCharacter;
                                    },
                                }
                            },
                            '\n' => {
                                return error.UnexpectedCharacter;
                            },
                            '\t' => {
                                return error.UnexpectedCharacter;
                            },
                            else => {},
                        }
                    }
                    const end = this.pos;

                    this.token = .{ .string = .{ .pos = start, .chars = .{ .off = off, .len = end - off } } };
                    return;
                },
                '-' => {
                    const start = this.pos -| 1;

                    const first_digit = switch (this.inc()) {
                        '0'...'9' => |digit| digit,
                        else => {
                            return error.UnexpectedCharacter;
                        },
                    };

                    const num = try this.lexNumber(start + 1, first_digit);

                    this.token = .{ .number = .{ .pos = start, .value = -num } };
                    return;
                },
                '0'...'9' => |first_digit| {
                    const start = this.pos -| 1;

                    const num = try this.lexNumber(start, first_digit);

                    this.token = .{ .number = .{ .pos = start, .value = num } };
                    return;
                },
            }
        }

        fn lexNumber(this: *@This(), start: usize, first_digit: u8) LexError!f64 {
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

        fn lexKeyword(this: *@This(), comptime keyword: Keyword) LexError!Token {
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

test "numbers" {
    const Test = struct {
        source: []const u8,
        result: union(enum) {
            err,
            num: f64,
        },
    };
    const tests: []const Test = &.{
        .{ .source = "0", .result = .{ .num = 0 } },
        .{ .source = "1", .result = .{ .num = 1 } },
        .{ .source = "10", .result = .{ .num = 10 } },
        .{ .source = "123", .result = .{ .num = 123 } },
        .{ .source = "1e2", .result = .{ .num = 1e2 } },
        .{ .source = "1234567890", .result = .{ .num = 1234567890 } },

        .{ .source = "-", .result = .err },

        .{ .source = "-0", .result = .{ .num = -0.0 } },
        .{ .source = "-1", .result = .{ .num = -1 } },
        .{ .source = "-10", .result = .{ .num = -10 } },
        .{ .source = "-123", .result = .{ .num = -123 } },
        .{ .source = "-1234567890", .result = .{ .num = -1234567890 } },

        .{ .source = "0.0", .result = .{ .num = 0.0 } },
        .{ .source = "0.5", .result = .{ .num = 0.5 } },
        .{ .source = "1.0", .result = .{ .num = 1.0 } },
        .{ .source = "1.5", .result = .{ .num = 1.5 } },
        .{ .source = "1.25", .result = .{ .num = 1.25 } },
        .{ .source = "3.14159", .result = .{ .num = 3.14159 } },
        .{ .source = "123.456", .result = .{ .num = 123.456 } },
        .{ .source = "0.123456789", .result = .{ .num = 0.123456789 } },

        .{ .source = "-0.0", .result = .{ .num = -0.0 } },
        .{ .source = "-0.5", .result = .{ .num = -0.5 } },
        .{ .source = "-1.0", .result = .{ .num = -1.0 } },
        .{ .source = "-1.5", .result = .{ .num = -1.5 } },
        .{ .source = "-3.14159", .result = .{ .num = -3.14159 } },
        .{ .source = "-123.456", .result = .{ .num = -123.456 } },
        .{ .source = "-0.0000", .result = .{ .num = -0.0 } },

        .{ .source = "0e0", .result = .{ .num = 0e0 } },
        .{ .source = "0E0", .result = .{ .num = 0E0 } },
        .{ .source = "1e0", .result = .{ .num = 1e0 } },
        .{ .source = "1E0", .result = .{ .num = 1E0 } },
        .{ .source = "1e1", .result = .{ .num = 1e1 } },
        .{ .source = "1e2", .result = .{ .num = 1e2 } },
        .{ .source = "1e10", .result = .{ .num = 1e10 } },
        .{ .source = "1E10", .result = .{ .num = 1E10 } },
        .{ .source = "1.23e4", .result = .{ .num = 1.23e4 } },
        .{ .source = "1.23E4", .result = .{ .num = 1.23E4 } },
        .{ .source = "5.6e7", .result = .{ .num = 5.6e7 } },

        .{ .source = "1e+0", .result = .{ .num = 1e+0 } },
        .{ .source = "1e+1", .result = .{ .num = 1e+1 } },
        .{ .source = "1e+10", .result = .{ .num = 1e+10 } },
        .{ .source = "1E+10", .result = .{ .num = 1E+10 } },
        .{ .source = "1.23e+4", .result = .{ .num = 1.23e+4 } },

        .{ .source = "-1e+0", .result = .{ .num = -1e+0 } },
        .{ .source = "-1e+1", .result = .{ .num = -1e+1 } },
        .{ .source = "-1e+10", .result = .{ .num = -1e+10 } },
        .{ .source = "-1E+10", .result = .{ .num = -1E+10 } },
        .{ .source = "-1.23e+4", .result = .{ .num = -1.23e+4 } },

        .{ .source = "1e-0", .result = .{ .num = 1e-0 } },
        .{ .source = "1e-1", .result = .{ .num = 1e-1 } },
        .{ .source = "1e-10", .result = .{ .num = 1e-10 } },
        .{ .source = "1E-10", .result = .{ .num = 1E-10 } },
        .{ .source = "1.23e-4", .result = .{ .num = 1.23e-4 } },
        .{ .source = "5.67e-8", .result = .{ .num = 5.67e-8 } },

        .{ .source = "-1e0", .result = .{ .num = -1e0 } },
        .{ .source = "-1e1", .result = .{ .num = -1e1 } },
        .{ .source = "-1e-1", .result = .{ .num = -1e-1 } },
        .{ .source = "-1e+1", .result = .{ .num = -1e+1 } },
        .{ .source = "-1.23e4", .result = .{ .num = -1.23e4 } },
        .{ .source = "-1.23e-4", .result = .{ .num = -1.23e-4 } },

        .{ .source = "0.0e0", .result = .{ .num = 0.0e0 } },
        .{ .source = "0.5e1", .result = .{ .num = 0.5e1 } },
        .{ .source = "0.5e+1", .result = .{ .num = 0.5e+1 } },
        .{ .source = "0.5e-1", .result = .{ .num = 0.5e-1 } },

        .{ .source = "-0.0e0", .result = .{ .num = -0.0e0 } },
        .{ .source = "-0.5e1", .result = .{ .num = -0.5e1 } },
        .{ .source = "-0.5e+1", .result = .{ .num = -0.5e+1 } },
        .{ .source = "-0.5e-1", .result = .{ .num = -0.5e-1 } },

        .{ .source = "01", .result = .err },
        .{ .source = "00", .result = .err },
        .{ .source = "0123", .result = .err },
        .{ .source = "-01", .result = .err },
        .{ .source = "-00", .result = .err },

        .{ .source = ".", .result = .err },
        .{ .source = ".1", .result = .err },
        .{ .source = "1.", .result = .err },
        .{ .source = "-", .result = .err },
        .{ .source = "-.", .result = .err },
        .{ .source = "-.1", .result = .err },

        .{ .source = "e", .result = .err },
        .{ .source = "e1", .result = .err },
        .{ .source = "E1", .result = .err },
        .{ .source = "1e", .result = .err },
        .{ .source = "1E", .result = .err },
        .{ .source = "1e+", .result = .err },
        .{ .source = "1e-", .result = .err },
        .{ .source = "1e1.2", .result = .err },

        .{ .source = "+1", .result = .err },
        .{ .source = "+0", .result = .err },

        .{ .source = "1 2", .result = .err },
        .{ .source = "1.2.3", .result = .err },
        .{ .source = "1e2e3", .result = .err },
        .{ .source = "1ee2", .result = .err },

        .{ .source = "NaN", .result = .err },
        .{ .source = "Infinity", .result = .err },
        .{ .source = "-Infinity", .result = .err },

        .{ .source = "0x10", .result = .err },
        .{ .source = "0b10", .result = .err },
        .{ .source = "0o10", .result = .err },

        .{ .source = "01", .result = .err },
        .{ .source = "010", .result = .err },
    };

    for (tests) |t| {
        var parser: Parser(u8) = .init(std.testing.allocator, t.source);
        defer parser.deinit();

        const result = parser.parse();
        defer {
            if (result == .expr) {
                result.expr.deinit();
            }
        }

        switch (result) {
            .err => {
                try std.testing.expect(t.result == .err);
            },
            .expr => |expr| {
                try std.testing.expect(t.result == .num);
                try std.testing.expect(expr == .number);
                try std.testing.expect(t.result.num == expr.number.value);
            },
        }
    }
}

fn expectParseTest(comptime test_name: []const u8, expect: enum { success, fail }) !void {
    const source = try std.fs.cwd().readFileAlloc(std.testing.allocator, "test/JSONTestSuite/test_parsing/" ++ test_name, std.math.maxInt(u64));
    defer std.testing.allocator.free(source);

    var parser: Parser(u8) = .init(std.testing.allocator, source);
    defer parser.deinit();

    const result = parser.parse();
    defer {
        if (result == .expr) {
            result.expr.deinit();
        }
    }

    switch (expect) {
        .success => {
            try std.testing.expect(result == .expr);
        },
        .fail => {
            try std.testing.expect(result == .err);
        },
    }
}

test "y_array_empty-string.json" {
    const test_name = "y_array_empty-string.json";
    try expectParseTest(test_name, .success);
}
test "n_number_minus_sign_with_trailing_garbage.json" {
    const test_name = "n_number_minus_sign_with_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_with_single_string.json" {
    const test_name = "n_object_with_single_string.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_unescaped_ctrl_char.json" {
    const test_name = "n_string_unescaped_ctrl_char.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_too_big_pos_int.json" {
    const test_name = "i_number_too_big_pos_int.json";
    try expectParseTest(test_name, .success);
}
test "y_string_null_escape.json" {
    const test_name = "y_string_null_escape.json";
    try expectParseTest(test_name, .success);
}
test "i_string_overlong_sequence_6_bytes.json" {
    const test_name = "i_string_overlong_sequence_6_bytes.json";
    try expectParseTest(test_name, .success);
}
test "y_array_null.json" {
    const test_name = "y_array_null.json";
    try expectParseTest(test_name, .success);
}
test "n_object_missing_value.json" {
    const test_name = "n_object_missing_value.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_incomplete_surrogates_escape_valid.json" {
    const test_name = "i_string_incomplete_surrogates_escape_valid.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_open_object_close_array.json" {
    const test_name = "n_structure_open_object_close_array.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_real_pos_overflow.json" {
    const test_name = "i_number_real_pos_overflow.json";
    try expectParseTest(test_name, .success);
}
test "i_string_lone_utf8_continuation_byte.json" {
    const test_name = "i_string_lone_utf8_continuation_byte.json";
    try expectParseTest(test_name, .success);
}
test "n_number_-2..json" {
    const test_name = "n_number_-2..json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_unclosed_array.json" {
    const test_name = "n_structure_unclosed_array.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_escape_x.json" {
    const test_name = "n_string_escape_x.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_UTF-16LE_with_BOM.json" {
    const test_name = "i_string_UTF-16LE_with_BOM.json";
    // TODO: support this
    try expectParseTest(test_name, .fail);
}
test "n_number_invalid-utf-8-in-bigger-int.json" {
    const test_name = "n_number_invalid-utf-8-in-bigger-int.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_invalid-utf-8-in-exponent.json" {
    const test_name = "n_number_invalid-utf-8-in-exponent.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_1_2_3_bytes_UTF-8_sequences.json" {
    const test_name = "y_string_1_2_3_bytes_UTF-8_sequences.json";
    try expectParseTest(test_name, .success);
}
test "n_single_space.json" {
    const test_name = "n_single_space.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_real_exponent.json" {
    const test_name = "y_number_real_exponent.json";
    try expectParseTest(test_name, .success);
}
test "i_number_real_neg_overflow.json" {
    const test_name = "i_number_real_neg_overflow.json";
    try expectParseTest(test_name, .success);
}
test "y_object.json" {
    const test_name = "y_object.json";
    try expectParseTest(test_name, .success);
}
test "i_string_iso_latin_1.json" {
    const test_name = "i_string_iso_latin_1.json";
    try expectParseTest(test_name, .success);
}
test "n_object_unquoted_key.json" {
    const test_name = "n_object_unquoted_key.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_a_invalid_utf8.json" {
    const test_name = "n_array_a_invalid_utf8.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_minus_zero.json" {
    const test_name = "y_number_minus_zero.json";
    try expectParseTest(test_name, .success);
}
test "n_object_several_trailing_commas.json" {
    const test_name = "n_object_several_trailing_commas.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_double_close_to_zero.json" {
    const test_name = "y_number_double_close_to_zero.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_single_eacute.json" {
    const test_name = "n_structure_single_eacute.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_object_comma.json" {
    const test_name = "n_structure_open_object_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_allowed_escapes.json" {
    const test_name = "y_string_allowed_escapes.json";
    try expectParseTest(test_name, .success);
}
test "n_array_just_minus.json" {
    const test_name = "n_array_just_minus.json";
    try expectParseTest(test_name, .fail);
}
test "y_array_empty.json" {
    const test_name = "y_array_empty.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_open_object_open_array.json" {
    const test_name = "n_structure_open_object_open_array.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_invalid_unicode_escape.json" {
    const test_name = "n_string_invalid_unicode_escape.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_9.e+.json" {
    const test_name = "n_number_9.e+.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_incomplete_escape.json" {
    const test_name = "n_string_incomplete_escape.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_string_empty.json" {
    const test_name = "y_structure_string_empty.json";
    try expectParseTest(test_name, .success);
}
test "y_object_empty.json" {
    const test_name = "y_object_empty.json";
    try expectParseTest(test_name, .success);
}
test "y_string_pi.json" {
    const test_name = "y_string_pi.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_unclosed_array_unfinished_false.json" {
    const test_name = "n_structure_unclosed_array_unfinished_false.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_escaped_noncharacter.json" {
    const test_name = "y_string_escaped_noncharacter.json";
    try expectParseTest(test_name, .success);
}
test "n_number_with_leading_zero.json" {
    const test_name = "n_number_with_leading_zero.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_int_with_exp.json" {
    const test_name = "y_number_int_with_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_number_1.0e+.json" {
    const test_name = "n_number_1.0e+.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_-NaN.json" {
    const test_name = "n_number_-NaN.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_unescaped_tab.json" {
    const test_name = "n_string_unescaped_tab.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_ascii-unicode-identifier.json" {
    const test_name = "n_structure_ascii-unicode-identifier.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_incomplete_surrogate.json" {
    const test_name = "n_string_incomplete_surrogate.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_items_separated_by_semicolon.json" {
    const test_name = "n_array_items_separated_by_semicolon.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_invalid_surrogate.json" {
    const test_name = "i_string_invalid_surrogate.json";
    try expectParseTest(test_name, .success);
}
test "n_number_infinity.json" {
    const test_name = "n_number_infinity.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_comma_instead_of_closing_brace.json" {
    const test_name = "n_structure_comma_instead_of_closing_brace.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_duplicated_key_and_value.json" {
    const test_name = "y_object_duplicated_key_and_value.json";
    try expectParseTest(test_name, .success);
}
test "n_string_with_trailing_garbage.json" {
    const test_name = "n_string_with_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0.1.2.json" {
    const test_name = "n_number_0.1.2.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_last_surrogates_1_and_2.json" {
    const test_name = "y_string_last_surrogates_1_and_2.json";
    try expectParseTest(test_name, .success);
}
test "y_string_accepted_surrogate_pair.json" {
    const test_name = "y_string_accepted_surrogate_pair.json";
    try expectParseTest(test_name, .success);
}
test "i_string_1st_valid_surrogate_2nd_invalid.json" {
    const test_name = "i_string_1st_valid_surrogate_2nd_invalid.json";
    try expectParseTest(test_name, .success);
}
test "y_structure_lonely_false.json" {
    const test_name = "y_structure_lonely_false.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_uescaped_LF_before_string.json" {
    const test_name = "n_structure_uescaped_LF_before_string.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_real_capital_e_pos_exp.json" {
    const test_name = "y_number_real_capital_e_pos_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_number_Inf.json" {
    const test_name = "n_number_Inf.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_incomplete.json" {
    const test_name = "n_array_incomplete.json";
    try expectParseTest(test_name, .fail);
}
test "i_object_key_lone_2nd_surrogate.json" {
    const test_name = "i_object_key_lone_2nd_surrogate.json";
    try expectParseTest(test_name, .success);
}
test "i_string_utf16BE_no_BOM.json" {
    const test_name = "i_string_utf16BE_no_BOM.json";
    // TODO: support this
    try expectParseTest(test_name, .fail);
}
test "y_string_unicode.json" {
    const test_name = "y_string_unicode.json";
    try expectParseTest(test_name, .success);
}
test "n_string_accentuated_char_no_quotes.json" {
    const test_name = "n_string_accentuated_char_no_quotes.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_true_in_array.json" {
    const test_name = "y_structure_true_in_array.json";
    try expectParseTest(test_name, .success);
}
test "n_number_.2e-3.json" {
    const test_name = "n_number_.2e-3.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_array_comma.json" {
    const test_name = "n_structure_open_array_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_unicode_U+1FFFE_nonchar.json" {
    const test_name = "y_string_unicode_U+1FFFE_nonchar.json";
    try expectParseTest(test_name, .success);
}
test "y_string_backslash_and_u_escaped_zero.json" {
    const test_name = "y_string_backslash_and_u_escaped_zero.json";
    try expectParseTest(test_name, .success);
}
test "y_string_three-byte-utf-8.json" {
    const test_name = "y_string_three-byte-utf-8.json";
    try expectParseTest(test_name, .success);
}
test "y_string_backslash_doublequotes.json" {
    const test_name = "y_string_backslash_doublequotes.json";
    try expectParseTest(test_name, .success);
}
test "n_array_missing_value.json" {
    const test_name = "n_array_missing_value.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_trailing_comma.json" {
    const test_name = "n_object_trailing_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_two_commas_in_a_row.json" {
    const test_name = "n_object_two_commas_in_a_row.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_no_data.json" {
    const test_name = "n_structure_no_data.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_single_star.json" {
    const test_name = "n_structure_single_star.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_uescaped_newline.json" {
    const test_name = "y_string_uescaped_newline.json";
    try expectParseTest(test_name, .success);
}
test "n_number_invalid-negative-real.json" {
    const test_name = "n_number_invalid-negative-real.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_lone-invalid-utf-8.json" {
    const test_name = "n_structure_lone-invalid-utf-8.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_incomplete_invalid_value.json" {
    const test_name = "n_array_incomplete_invalid_value.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0e.json" {
    const test_name = "n_number_0e.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_invalid-utf-8-in-int.json" {
    const test_name = "n_number_invalid-utf-8-in-int.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_invalid_utf8_after_escape.json" {
    const test_name = "n_string_invalid_utf8_after_escape.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_number_and_comma.json" {
    const test_name = "n_array_number_and_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_unclosed_object.json" {
    const test_name = "n_structure_unclosed_object.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_starting_with_dot.json" {
    const test_name = "n_number_starting_with_dot.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_after_space.json" {
    const test_name = "y_number_after_space.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_whitespace_formfeed.json" {
    const test_name = "n_structure_whitespace_formfeed.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_2.e3.json" {
    const test_name = "n_number_2.e3.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_negative_one.json" {
    const test_name = "y_number_negative_one.json";
    try expectParseTest(test_name, .success);
}
test "n_object_key_with_single_quotes.json" {
    const test_name = "n_object_key_with_single_quotes.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_U+FF11_fullwidth_digit_one.json" {
    const test_name = "n_number_U+FF11_fullwidth_digit_one.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_lonely_int.json" {
    const test_name = "y_structure_lonely_int.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_unclosed_array_partial_null.json" {
    const test_name = "n_structure_unclosed_array_partial_null.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_huge_exp.json" {
    const test_name = "i_number_huge_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_object_with_trailing_garbage.json" {
    const test_name = "n_structure_object_with_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_UTF8_surrogate_U+D800.json" {
    const test_name = "i_string_UTF8_surrogate_U+D800.json";
    try expectParseTest(test_name, .success);
}
test "y_structure_lonely_negative_real.json" {
    const test_name = "y_structure_lonely_negative_real.json";
    try expectParseTest(test_name, .success);
}
test "n_object_non_string_key.json" {
    const test_name = "n_object_non_string_key.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_1.0e-.json" {
    const test_name = "n_number_1.0e-.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_++.json" {
    const test_name = "n_number_++.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_unicode_escaped_double_quote.json" {
    const test_name = "y_string_unicode_escaped_double_quote.json";
    try expectParseTest(test_name, .success);
}
test "n_object_garbage_at_end.json" {
    const test_name = "n_object_garbage_at_end.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_real_capital_e.json" {
    const test_name = "y_number_real_capital_e.json";
    try expectParseTest(test_name, .success);
}
test "y_string_double_escape_a.json" {
    const test_name = "y_string_double_escape_a.json";
    try expectParseTest(test_name, .success);
}
test "n_number_-01.json" {
    const test_name = "n_number_-01.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_real_fraction_exponent.json" {
    const test_name = "y_number_real_fraction_exponent.json";
    try expectParseTest(test_name, .success);
}
test "n_array_number_and_several_commas.json" {
    const test_name = "n_array_number_and_several_commas.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_comments.json" {
    const test_name = "y_string_comments.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_UTF8_BOM_no_data.json" {
    const test_name = "n_structure_UTF8_BOM_no_data.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_single_doublequote.json" {
    const test_name = "n_string_single_doublequote.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_too_big_neg_int.json" {
    const test_name = "i_number_too_big_neg_int.json";
    try expectParseTest(test_name, .success);
}
test "y_structure_lonely_null.json" {
    const test_name = "y_structure_lonely_null.json";
    try expectParseTest(test_name, .success);
}
test "y_string_space.json" {
    const test_name = "y_string_space.json";
    try expectParseTest(test_name, .success);
}
test "n_object_lone_continuation_byte_in_key_and_trailing_comma.json" {
    const test_name = "n_object_lone_continuation_byte_in_key_and_trailing_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_uEscape.json" {
    const test_name = "y_string_uEscape.json";
    try expectParseTest(test_name, .success);
}
test "y_array_with_1_and_newline.json" {
    const test_name = "y_array_with_1_and_newline.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_end_array.json" {
    const test_name = "n_structure_end_array.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_unicode_CapitalU.json" {
    const test_name = "n_string_unicode_CapitalU.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_extra_comma.json" {
    const test_name = "n_array_extra_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_with_alpha_char.json" {
    const test_name = "n_number_with_alpha_char.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_backslash_00.json" {
    const test_name = "n_string_backslash_00.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_1st_surrogate_but_2nd_missing.json" {
    const test_name = "i_string_1st_surrogate_but_2nd_missing.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_whitespace_U+2060_word_joiner.json" {
    const test_name = "n_structure_whitespace_U+2060_word_joiner.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_capitalized_True.json" {
    const test_name = "n_structure_capitalized_True.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_star_inside.json" {
    const test_name = "n_array_star_inside.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_trailing_comment_slash_open.json" {
    const test_name = "n_object_trailing_comment_slash_open.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_1_true_without_comma.json" {
    const test_name = "n_array_1_true_without_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_0e+1.json" {
    const test_name = "y_number_0e+1.json";
    try expectParseTest(test_name, .success);
}
test "y_object_with_newlines.json" {
    const test_name = "y_object_with_newlines.json";
    try expectParseTest(test_name, .success);
}
test "n_number_with_alpha.json" {
    const test_name = "n_number_with_alpha.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_array_apostrophe.json" {
    const test_name = "n_structure_open_array_apostrophe.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_object_unclosed_no_value.json" {
    const test_name = "n_structure_object_unclosed_no_value.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0_capital_E.json" {
    const test_name = "n_number_0_capital_E.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_real_capital_e_neg_exp.json" {
    const test_name = "y_number_real_capital_e_neg_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_incomplete_true.json" {
    const test_name = "n_incomplete_true.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_double_extra_comma.json" {
    const test_name = "n_array_double_extra_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_lonely_string.json" {
    const test_name = "y_structure_lonely_string.json";
    try expectParseTest(test_name, .success);
}
test "n_number_+Inf.json" {
    const test_name = "n_number_+Inf.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_hex_2_digits.json" {
    const test_name = "n_number_hex_2_digits.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_close_unopened_array.json" {
    const test_name = "n_structure_close_unopened_array.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_comma_instead_of_colon.json" {
    const test_name = "n_object_comma_instead_of_colon.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_object_string_with_apostrophes.json" {
    const test_name = "n_structure_open_object_string_with_apostrophes.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_negative_int.json" {
    const test_name = "y_number_negative_int.json";
    try expectParseTest(test_name, .success);
}
test "i_string_inverted_surrogates_U+1D11E.json" {
    const test_name = "i_string_inverted_surrogates_U+1D11E.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_object_with_comment.json" {
    const test_name = "n_structure_object_with_comment.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_incomplete_surrogate_escape_invalid.json" {
    const test_name = "n_string_incomplete_surrogate_escape_invalid.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_utf16LE_no_BOM.json" {
    const test_name = "i_string_utf16LE_no_BOM.json";
    // TODO: support this
    try expectParseTest(test_name, .fail);
}
test "i_string_overlong_sequence_6_bytes_null.json" {
    const test_name = "i_string_overlong_sequence_6_bytes_null.json";
    try expectParseTest(test_name, .success);
}
test "n_object_trailing_comment_open.json" {
    const test_name = "n_object_trailing_comment_open.json";
    try expectParseTest(test_name, .fail);
}
test "y_array_ending_with_newline.json" {
    const test_name = "y_array_ending_with_newline.json";
    try expectParseTest(test_name, .success);
}
test "n_array_colon_instead_of_comma.json" {
    const test_name = "n_array_colon_instead_of_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_trailing_comment_slash_open_incomplete.json" {
    const test_name = "n_object_trailing_comment_slash_open_incomplete.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_simple_real.json" {
    const test_name = "y_number_simple_real.json";
    try expectParseTest(test_name, .success);
}
test "n_number_2.e+3.json" {
    const test_name = "n_number_2.e+3.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_unclosed_with_object_inside.json" {
    const test_name = "n_array_unclosed_with_object_inside.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_array_trailing_garbage.json" {
    const test_name = "n_structure_array_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_truncated-utf-8.json" {
    const test_name = "i_string_truncated-utf-8.json";
    try expectParseTest(test_name, .success);
}
test "y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json" {
    const test_name = "y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_double_array.json" {
    const test_name = "n_structure_double_array.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_u+2028_line_sep.json" {
    const test_name = "y_string_u+2028_line_sep.json";
    try expectParseTest(test_name, .success);
}
test "n_number_neg_int_starting_with_zero.json" {
    const test_name = "n_number_neg_int_starting_with_zero.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_newlines_unclosed.json" {
    const test_name = "n_array_newlines_unclosed.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_lonely_true.json" {
    const test_name = "y_structure_lonely_true.json";
    try expectParseTest(test_name, .success);
}
test "n_object_missing_colon.json" {
    const test_name = "n_object_missing_colon.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_two-byte-utf-8.json" {
    const test_name = "y_string_two-byte-utf-8.json";
    try expectParseTest(test_name, .success);
}
test "y_string_unicodeEscapedBackslash.json" {
    const test_name = "y_string_unicodeEscapedBackslash.json";
    try expectParseTest(test_name, .success);
}
test "i_string_overlong_sequence_2_bytes.json" {
    const test_name = "i_string_overlong_sequence_2_bytes.json";
    try expectParseTest(test_name, .success);
}
test "n_object_missing_semicolon.json" {
    const test_name = "n_object_missing_semicolon.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_simple.json" {
    const test_name = "y_object_simple.json";
    try expectParseTest(test_name, .success);
}
test "y_string_unicode_U+2064_invisible_plus.json" {
    const test_name = "y_string_unicode_U+2064_invisible_plus.json";
    try expectParseTest(test_name, .success);
}
test "n_number_minus_space_1.json" {
    const test_name = "n_number_minus_space_1.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_escaped_control_character.json" {
    const test_name = "y_string_escaped_control_character.json";
    try expectParseTest(test_name, .success);
}
test "y_string_simple_ascii.json" {
    const test_name = "y_string_simple_ascii.json";
    try expectParseTest(test_name, .success);
}
test "y_string_unicode_U+10FFFE_nonchar.json" {
    const test_name = "y_string_unicode_U+10FFFE_nonchar.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_open_object.json" {
    const test_name = "n_structure_open_object.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_utf8.json" {
    const test_name = "y_string_utf8.json";
    try expectParseTest(test_name, .success);
}
test "n_string_escaped_ctrl_char_tab.json" {
    const test_name = "n_string_escaped_ctrl_char_tab.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_double_huge_neg_exp.json" {
    const test_name = "i_number_double_huge_neg_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_number_invalid+-.json" {
    const test_name = "n_number_invalid+-.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_100000_opening_arrays.json" {
    const test_name = "n_structure_100000_opening_arrays.json";
    _ = test_name;
    // TODO: support stack overflow detection or switch
    // to iterating
    // try expectParseTest(test_name, .fail);
}
test "n_string_leading_uescaped_thinspace.json" {
    const test_name = "n_string_leading_uescaped_thinspace.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0.3e.json" {
    const test_name = "n_number_0.3e.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_lone-open-bracket.json" {
    const test_name = "n_structure_lone-open-bracket.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_unescaped_char_delete.json" {
    const test_name = "y_string_unescaped_char_delete.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_object_followed_by_closing_object.json" {
    const test_name = "n_structure_object_followed_by_closing_object.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0e+.json" {
    const test_name = "n_number_0e+.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_unicode-identifier.json" {
    const test_name = "n_structure_unicode-identifier.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_1eE2.json" {
    const test_name = "n_number_1eE2.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_extreme_numbers.json" {
    const test_name = "y_object_extreme_numbers.json";
    try expectParseTest(test_name, .success);
}
test "n_number_+1.json" {
    const test_name = "n_number_+1.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_long_strings.json" {
    const test_name = "y_object_long_strings.json";
    try expectParseTest(test_name, .success);
}
test "n_incomplete_null.json" {
    const test_name = "n_incomplete_null.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_negative_zero.json" {
    const test_name = "y_number_negative_zero.json";
    try expectParseTest(test_name, .success);
}
test "n_multidigit_number_then_00.json" {
    const test_name = "n_multidigit_number_then_00.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_1_surrogate_then_escape_u.json" {
    const test_name = "n_string_1_surrogate_then_escape_u.json";
    try expectParseTest(test_name, .fail);
}
test "y_array_with_leading_space.json" {
    const test_name = "y_array_with_leading_space.json";
    try expectParseTest(test_name, .success);
}
test "n_array_double_comma.json" {
    const test_name = "n_array_double_comma.json";
    try expectParseTest(test_name, .fail);
}
test "y_structure_trailing_newline.json" {
    const test_name = "y_structure_trailing_newline.json";
    try expectParseTest(test_name, .success);
}
test "n_array_invalid_utf8.json" {
    const test_name = "n_array_invalid_utf8.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_repeated_null_null.json" {
    const test_name = "n_object_repeated_null_null.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_bad_value.json" {
    const test_name = "n_object_bad_value.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_pos_double_huge_exp.json" {
    const test_name = "i_number_pos_double_huge_exp.json";
    try expectParseTest(test_name, .success);
}
test "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json" {
    const test_name = "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json";
    try expectParseTest(test_name, .success);
}
test "n_number_2.e-3.json" {
    const test_name = "n_number_2.e-3.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_string_unicode.json" {
    const test_name = "y_object_string_unicode.json";
    try expectParseTest(test_name, .success);
}
test "n_string_no_quotes_with_bad_escape.json" {
    const test_name = "n_string_no_quotes_with_bad_escape.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_neg_with_garbage_at_end.json" {
    const test_name = "n_number_neg_with_garbage_at_end.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_1_surrogate_then_escape_u1.json" {
    const test_name = "n_string_1_surrogate_then_escape_u1.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_spaces_vertical_tab_formfeed.json" {
    const test_name = "n_array_spaces_vertical_tab_formfeed.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_array_object.json" {
    const test_name = "n_structure_open_array_object.json";
    _ = test_name;
    // TODO: support stack overflow detection or switch
    // to iterating
    // try expectParseTest(test_name, .fail);
}
test "n_structure_open_object_open_string.json" {
    const test_name = "n_structure_open_object_open_string.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_non_string_key_but_huge_number_instead.json" {
    const test_name = "n_object_non_string_key_but_huge_number_instead.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_real_underflow.json" {
    const test_name = "i_number_real_underflow.json";
    try expectParseTest(test_name, .success);
}
test "n_object_single_quote.json" {
    const test_name = "n_object_single_quote.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0.3e+.json" {
    const test_name = "n_number_0.3e+.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_real_garbage_after_e.json" {
    const test_name = "n_number_real_garbage_after_e.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_escaped_backslash_bad.json" {
    const test_name = "n_string_escaped_backslash_bad.json";
    try expectParseTest(test_name, .fail);
}
test "y_number_0e1.json" {
    const test_name = "y_number_0e1.json";
    try expectParseTest(test_name, .success);
}
test "y_number_real_pos_exponent.json" {
    const test_name = "y_number_real_pos_exponent.json";
    try expectParseTest(test_name, .success);
}
test "y_string_double_escape_n.json" {
    const test_name = "y_string_double_escape_n.json";
    try expectParseTest(test_name, .success);
}
test "y_array_arraysWithSpaces.json" {
    const test_name = "y_array_arraysWithSpaces.json";
    try expectParseTest(test_name, .success);
}
test "y_array_heterogeneous.json" {
    const test_name = "y_array_heterogeneous.json";
    try expectParseTest(test_name, .success);
}
test "y_string_with_del_character.json" {
    const test_name = "y_string_with_del_character.json";
    try expectParseTest(test_name, .success);
}
test "n_object_with_trailing_garbage.json" {
    const test_name = "n_object_with_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "y_array_with_several_null.json" {
    const test_name = "y_array_with_several_null.json";
    try expectParseTest(test_name, .success);
}
test "y_number_simple_int.json" {
    const test_name = "y_number_simple_int.json";
    try expectParseTest(test_name, .success);
}
test "n_number_expression.json" {
    const test_name = "n_number_expression.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_array_string.json" {
    const test_name = "n_structure_open_array_string.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_missing_key.json" {
    const test_name = "n_object_missing_key.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_1_surrogate_then_escape_u1x.json" {
    const test_name = "n_string_1_surrogate_then_escape_u1x.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_trailing_#.json" {
    const test_name = "n_structure_trailing_#.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_array_with_extra_array_close.json" {
    const test_name = "n_structure_array_with_extra_array_close.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_empty_key.json" {
    const test_name = "y_object_empty_key.json";
    try expectParseTest(test_name, .success);
}
test "y_string_nonCharacterInUTF-8_U+FFFF.json" {
    const test_name = "y_string_nonCharacterInUTF-8_U+FFFF.json";
    try expectParseTest(test_name, .success);
}
test "i_number_very_big_negative_int.json" {
    const test_name = "i_number_very_big_negative_int.json";
    try expectParseTest(test_name, .success);
}
test "i_string_invalid_lonely_surrogate.json" {
    const test_name = "i_string_invalid_lonely_surrogate.json";
    try expectParseTest(test_name, .success);
}
test "y_object_escaped_null_in_key.json" {
    const test_name = "y_object_escaped_null_in_key.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_angle_bracket_..json" {
    const test_name = "n_structure_angle_bracket_..json";
    try expectParseTest(test_name, .fail);
}
test "y_string_accepted_surrogate_pairs.json" {
    const test_name = "y_string_accepted_surrogate_pairs.json";
    try expectParseTest(test_name, .success);
}
test "i_structure_500_nested_arrays.json" {
    const test_name = "i_structure_500_nested_arrays.json";
    try expectParseTest(test_name, .success);
}
test "y_number_real_neg_exp.json" {
    const test_name = "y_number_real_neg_exp.json";
    try expectParseTest(test_name, .success);
}
test "y_structure_whitespace_array.json" {
    const test_name = "y_structure_whitespace_array.json";
    try expectParseTest(test_name, .success);
}
test "y_string_in_array_with_leading_space.json" {
    const test_name = "y_string_in_array_with_leading_space.json";
    try expectParseTest(test_name, .success);
}
test "y_string_nonCharacterInUTF-8_U+10FFFF.json" {
    const test_name = "y_string_nonCharacterInUTF-8_U+10FFFF.json";
    try expectParseTest(test_name, .success);
}
test "n_object_bracket_key.json" {
    const test_name = "n_object_bracket_key.json";
    try expectParseTest(test_name, .fail);
}
test "i_number_neg_int_huge_exp.json" {
    const test_name = "i_number_neg_int_huge_exp.json";
    try expectParseTest(test_name, .success);
}
test "n_number_NaN.json" {
    const test_name = "n_number_NaN.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_single_string_no_double_quotes.json" {
    const test_name = "n_string_single_string_no_double_quotes.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_neg_real_without_int_part.json" {
    const test_name = "n_number_neg_real_without_int_part.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_unescaped_newline.json" {
    const test_name = "n_string_unescaped_newline.json";
    try expectParseTest(test_name, .fail);
}
test "y_array_with_trailing_space.json" {
    const test_name = "y_array_with_trailing_space.json";
    try expectParseTest(test_name, .success);
}
test "i_string_not_in_unicode_range.json" {
    const test_name = "i_string_not_in_unicode_range.json";
    try expectParseTest(test_name, .success);
}
test "n_string_single_quote.json" {
    const test_name = "n_string_single_quote.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_1_000.json" {
    const test_name = "n_number_1_000.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_duplicated_key.json" {
    const test_name = "y_object_duplicated_key.json";
    try expectParseTest(test_name, .success);
}
test "n_string_incomplete_escaped_character.json" {
    const test_name = "n_string_incomplete_escaped_character.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_no-colon.json" {
    const test_name = "n_object_no-colon.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_one-byte-utf-8.json" {
    const test_name = "y_string_one-byte-utf-8.json";
    try expectParseTest(test_name, .success);
}
test "y_string_unicode_2.json" {
    const test_name = "y_string_unicode_2.json";
    try expectParseTest(test_name, .success);
}
test "n_object_trailing_comment.json" {
    const test_name = "n_object_trailing_comment.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_unicode_U+FDD0_nonchar.json" {
    const test_name = "y_string_unicode_U+FDD0_nonchar.json";
    try expectParseTest(test_name, .success);
}
test "n_incomplete_false.json" {
    const test_name = "n_incomplete_false.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_null-byte-outside-string.json" {
    const test_name = "n_structure_null-byte-outside-string.json";
    try expectParseTest(test_name, .fail);
}
test "i_structure_UTF-8_BOM_empty_object.json" {
    const test_name = "i_structure_UTF-8_BOM_empty_object.json";
    // TODO: support this
    try expectParseTest(test_name, .fail);
}
test "y_string_nbsp_uescaped.json" {
    const test_name = "y_string_nbsp_uescaped.json";
    try expectParseTest(test_name, .success);
}
test "n_number_.-1.json" {
    const test_name = "n_number_.-1.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_number_with_trailing_garbage.json" {
    const test_name = "n_structure_number_with_trailing_garbage.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_real_without_fractional_part.json" {
    const test_name = "n_number_real_without_fractional_part.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_invalid_utf-8.json" {
    const test_name = "i_string_invalid_utf-8.json";
    try expectParseTest(test_name, .success);
}
test "i_string_incomplete_surrogate_pair.json" {
    const test_name = "i_string_incomplete_surrogate_pair.json";
    try expectParseTest(test_name, .success);
}
test "n_string_invalid_backslash_esc.json" {
    const test_name = "n_string_invalid_backslash_esc.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_minus_infinity.json" {
    const test_name = "n_number_minus_infinity.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_unclosed.json" {
    const test_name = "n_array_unclosed.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_incomplete_surrogate_and_escape_valid.json" {
    const test_name = "i_string_incomplete_surrogate_and_escape_valid.json";
    try expectParseTest(test_name, .success);
}
test "y_array_false.json" {
    const test_name = "y_array_false.json";
    try expectParseTest(test_name, .success);
}
test "n_number_real_with_invalid_utf8_after_e.json" {
    const test_name = "n_number_real_with_invalid_utf8_after_e.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_unicode_U+FFFE_nonchar.json" {
    const test_name = "y_string_unicode_U+FFFE_nonchar.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_open_open.json" {
    const test_name = "n_structure_open_open.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_1.0e.json" {
    const test_name = "n_number_1.0e.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_unclosed_trailing_comma.json" {
    const test_name = "n_array_unclosed_trailing_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_comma_and_number.json" {
    const test_name = "n_array_comma_and_number.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_1_surrogate_then_escape.json" {
    const test_name = "n_string_1_surrogate_then_escape.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_unclosed_array_unfinished_true.json" {
    const test_name = "n_structure_unclosed_array_unfinished_true.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_escaped_emoji.json" {
    const test_name = "n_string_escaped_emoji.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_comma_after_close.json" {
    const test_name = "n_array_comma_after_close.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_reservedCharacterInUTF-8_U+1BFFF.json" {
    const test_name = "y_string_reservedCharacterInUTF-8_U+1BFFF.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_U+2060_word_joined.json" {
    const test_name = "n_structure_U+2060_word_joined.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0.e1.json" {
    const test_name = "n_number_0.e1.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_unclosed_with_new_lines.json" {
    const test_name = "n_array_unclosed_with_new_lines.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_angle_bracket_null.json" {
    const test_name = "n_structure_angle_bracket_null.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_emoji.json" {
    const test_name = "n_object_emoji.json";
    try expectParseTest(test_name, .fail);
}
test "n_object_unterminated-value.json" {
    const test_name = "n_object_unterminated-value.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_array_with_unclosed_string.json" {
    const test_name = "n_structure_array_with_unclosed_string.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_u+2029_par_sep.json" {
    const test_name = "y_string_u+2029_par_sep.json";
    try expectParseTest(test_name, .success);
}
test "n_object_double_colon.json" {
    const test_name = "n_object_double_colon.json";
    try expectParseTest(test_name, .fail);
}
test "y_object_basic.json" {
    const test_name = "y_object_basic.json";
    try expectParseTest(test_name, .success);
}
test "i_string_lone_second_surrogate.json" {
    const test_name = "i_string_lone_second_surrogate.json";
    try expectParseTest(test_name, .success);
}
test "n_structure_open_array_open_string.json" {
    const test_name = "n_structure_open_array_open_string.json";
    try expectParseTest(test_name, .fail);
}
test "n_array_extra_close.json" {
    const test_name = "n_array_extra_close.json";
    try expectParseTest(test_name, .fail);
}
test "i_string_UTF-8_invalid_sequence.json" {
    const test_name = "i_string_UTF-8_invalid_sequence.json";
    try expectParseTest(test_name, .success);
}
test "n_array_inner_array_no_comma.json" {
    const test_name = "n_array_inner_array_no_comma.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_start_escape_unclosed.json" {
    const test_name = "n_string_start_escape_unclosed.json";
    try expectParseTest(test_name, .fail);
}
test "n_string_invalid-utf-8-in-escape.json" {
    const test_name = "n_string_invalid-utf-8-in-escape.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_open_array_open_object.json" {
    const test_name = "n_structure_open_array_open_object.json";
    try expectParseTest(test_name, .fail);
}
test "n_number_hex_1_digit.json" {
    const test_name = "n_number_hex_1_digit.json";
    try expectParseTest(test_name, .fail);
}
test "n_structure_incomplete_UTF8_BOM.json" {
    const test_name = "n_structure_incomplete_UTF8_BOM.json";
    try expectParseTest(test_name, .fail);
}
test "y_string_in_array.json" {
    const test_name = "y_string_in_array.json";
    try expectParseTest(test_name, .success);
}
test "n_number_-1.0..json" {
    const test_name = "n_number_-1.0..json";
    try expectParseTest(test_name, .fail);
}
test "n_number_0_capital_E+.json" {
    const test_name = "n_number_0_capital_E+.json";
    try expectParseTest(test_name, .fail);
}
test "y_number.json" {
    const test_name = "y_number.json";
    try expectParseTest(test_name, .success);
}
test "n_array_just_comma.json" {
    const test_name = "n_array_just_comma.json";
    try expectParseTest(test_name, .fail);
}

const expected_tests: []const []const u8 = &.{
    "y_array_empty-string.json",
    "n_number_minus_sign_with_trailing_garbage.json",
    "n_object_with_single_string.json",
    "n_string_unescaped_ctrl_char.json",
    "i_number_too_big_pos_int.json",
    "y_string_null_escape.json",
    "i_string_overlong_sequence_6_bytes.json",
    "y_array_null.json",
    "n_object_missing_value.json",
    "i_string_incomplete_surrogates_escape_valid.json",
    "n_structure_open_object_close_array.json",
    "i_number_real_pos_overflow.json",
    "i_string_lone_utf8_continuation_byte.json",
    "n_number_-2..json",
    "n_structure_unclosed_array.json",
    "n_string_escape_x.json",
    "i_string_UTF-16LE_with_BOM.json",
    "n_number_invalid-utf-8-in-bigger-int.json",
    "n_number_invalid-utf-8-in-exponent.json",
    "y_string_1_2_3_bytes_UTF-8_sequences.json",
    "n_single_space.json",
    "y_number_real_exponent.json",
    "i_number_real_neg_overflow.json",
    "y_object.json",
    "i_string_iso_latin_1.json",
    "n_object_unquoted_key.json",
    "n_array_a_invalid_utf8.json",
    "y_number_minus_zero.json",
    "n_object_several_trailing_commas.json",
    "y_number_double_close_to_zero.json",
    "n_structure_single_eacute.json",
    "n_structure_open_object_comma.json",
    "y_string_allowed_escapes.json",
    "n_array_just_minus.json",
    "y_array_empty.json",
    "n_structure_open_object_open_array.json",
    "n_string_invalid_unicode_escape.json",
    "n_number_9.e+.json",
    "n_string_incomplete_escape.json",
    "y_structure_string_empty.json",
    "y_object_empty.json",
    "y_string_pi.json",
    "n_structure_unclosed_array_unfinished_false.json",
    "y_string_escaped_noncharacter.json",
    "n_number_with_leading_zero.json",
    "y_number_int_with_exp.json",
    "n_number_1.0e+.json",
    "n_number_-NaN.json",
    "n_string_unescaped_tab.json",
    "n_structure_ascii-unicode-identifier.json",
    "n_string_incomplete_surrogate.json",
    "n_array_items_separated_by_semicolon.json",
    "i_string_invalid_surrogate.json",
    "n_number_infinity.json",
    "n_structure_comma_instead_of_closing_brace.json",
    "y_object_duplicated_key_and_value.json",
    "n_string_with_trailing_garbage.json",
    "n_number_0.1.2.json",
    "y_string_last_surrogates_1_and_2.json",
    "y_string_accepted_surrogate_pair.json",
    "i_string_1st_valid_surrogate_2nd_invalid.json",
    "y_structure_lonely_false.json",
    "n_structure_uescaped_LF_before_string.json",
    "y_number_real_capital_e_pos_exp.json",
    "n_number_Inf.json",
    "n_array_incomplete.json",
    "i_object_key_lone_2nd_surrogate.json",
    "i_string_utf16BE_no_BOM.json",
    "y_string_unicode.json",
    "n_string_accentuated_char_no_quotes.json",
    "y_structure_true_in_array.json",
    "n_number_.2e-3.json",
    "n_structure_open_array_comma.json",
    "y_string_unicode_U+1FFFE_nonchar.json",
    "y_string_backslash_and_u_escaped_zero.json",
    "y_string_three-byte-utf-8.json",
    "y_string_backslash_doublequotes.json",
    "n_array_missing_value.json",
    "n_object_trailing_comma.json",
    "n_object_two_commas_in_a_row.json",
    "n_structure_no_data.json",
    "n_structure_single_star.json",
    "y_string_uescaped_newline.json",
    "n_number_invalid-negative-real.json",
    "n_structure_lone-invalid-utf-8.json",
    "n_array_incomplete_invalid_value.json",
    "n_number_0e.json",
    "n_number_invalid-utf-8-in-int.json",
    "n_string_invalid_utf8_after_escape.json",
    "n_array_number_and_comma.json",
    "n_structure_unclosed_object.json",
    "n_number_starting_with_dot.json",
    "y_number_after_space.json",
    "n_structure_whitespace_formfeed.json",
    "n_number_2.e3.json",
    "y_number_negative_one.json",
    "n_object_key_with_single_quotes.json",
    "n_number_U+FF11_fullwidth_digit_one.json",
    "y_structure_lonely_int.json",
    "n_structure_unclosed_array_partial_null.json",
    "i_number_huge_exp.json",
    "n_structure_object_with_trailing_garbage.json",
    "i_string_UTF8_surrogate_U+D800.json",
    "y_structure_lonely_negative_real.json",
    "n_object_non_string_key.json",
    "n_number_1.0e-.json",
    "n_number_++.json",
    "y_string_unicode_escaped_double_quote.json",
    "n_object_garbage_at_end.json",
    "y_number_real_capital_e.json",
    "y_string_double_escape_a.json",
    "n_number_-01.json",
    "y_number_real_fraction_exponent.json",
    "n_array_number_and_several_commas.json",
    "y_string_comments.json",
    "n_structure_UTF8_BOM_no_data.json",
    "n_string_single_doublequote.json",
    "i_number_too_big_neg_int.json",
    "y_structure_lonely_null.json",
    "y_string_space.json",
    "n_object_lone_continuation_byte_in_key_and_trailing_comma.json",
    "y_string_uEscape.json",
    "y_array_with_1_and_newline.json",
    "n_structure_end_array.json",
    "n_string_unicode_CapitalU.json",
    "n_array_extra_comma.json",
    "n_number_with_alpha_char.json",
    "n_string_backslash_00.json",
    "i_string_1st_surrogate_but_2nd_missing.json",
    "n_structure_whitespace_U+2060_word_joiner.json",
    "n_structure_capitalized_True.json",
    "n_array_star_inside.json",
    "n_object_trailing_comment_slash_open.json",
    "n_array_1_true_without_comma.json",
    "y_number_0e+1.json",
    "y_object_with_newlines.json",
    "n_number_with_alpha.json",
    "n_structure_open_array_apostrophe.json",
    "n_structure_object_unclosed_no_value.json",
    "n_number_0_capital_E.json",
    "y_number_real_capital_e_neg_exp.json",
    "n_incomplete_true.json",
    "n_array_double_extra_comma.json",
    "y_structure_lonely_string.json",
    "n_number_+Inf.json",
    "n_number_hex_2_digits.json",
    "n_structure_close_unopened_array.json",
    "n_object_comma_instead_of_colon.json",
    "n_structure_open_object_string_with_apostrophes.json",
    "y_number_negative_int.json",
    "i_string_inverted_surrogates_U+1D11E.json",
    "n_structure_object_with_comment.json",
    "n_string_incomplete_surrogate_escape_invalid.json",
    "i_string_utf16LE_no_BOM.json",
    "i_string_overlong_sequence_6_bytes_null.json",
    "n_object_trailing_comment_open.json",
    "y_array_ending_with_newline.json",
    "n_array_colon_instead_of_comma.json",
    "n_object_trailing_comment_slash_open_incomplete.json",
    "y_number_simple_real.json",
    "n_number_2.e+3.json",
    "n_array_unclosed_with_object_inside.json",
    "n_structure_array_trailing_garbage.json",
    "i_string_truncated-utf-8.json",
    "y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json",
    "n_structure_double_array.json",
    "y_string_u+2028_line_sep.json",
    "n_number_neg_int_starting_with_zero.json",
    "n_array_newlines_unclosed.json",
    "y_structure_lonely_true.json",
    "n_object_missing_colon.json",
    "y_string_two-byte-utf-8.json",
    "y_string_unicodeEscapedBackslash.json",
    "i_string_overlong_sequence_2_bytes.json",
    "n_object_missing_semicolon.json",
    "y_object_simple.json",
    "y_string_unicode_U+2064_invisible_plus.json",
    "n_number_minus_space_1.json",
    "y_string_escaped_control_character.json",
    "y_string_simple_ascii.json",
    "y_string_unicode_U+10FFFE_nonchar.json",
    "n_structure_open_object.json",
    "y_string_utf8.json",
    "n_string_escaped_ctrl_char_tab.json",
    "i_number_double_huge_neg_exp.json",
    "n_number_invalid+-.json",
    "n_structure_100000_opening_arrays.json",
    "n_string_leading_uescaped_thinspace.json",
    "n_number_0.3e.json",
    "n_structure_lone-open-bracket.json",
    "y_string_unescaped_char_delete.json",
    "n_structure_object_followed_by_closing_object.json",
    "n_number_0e+.json",
    "n_structure_unicode-identifier.json",
    "n_number_1eE2.json",
    "y_object_extreme_numbers.json",
    "n_number_+1.json",
    "y_object_long_strings.json",
    "n_incomplete_null.json",
    "y_number_negative_zero.json",
    "n_multidigit_number_then_00.json",
    "n_string_1_surrogate_then_escape_u.json",
    "y_array_with_leading_space.json",
    "n_array_double_comma.json",
    "y_structure_trailing_newline.json",
    "n_array_invalid_utf8.json",
    "n_object_repeated_null_null.json",
    "n_object_bad_value.json",
    "i_number_pos_double_huge_exp.json",
    "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json",
    "n_number_2.e-3.json",
    "y_object_string_unicode.json",
    "n_string_no_quotes_with_bad_escape.json",
    "n_number_neg_with_garbage_at_end.json",
    "n_string_1_surrogate_then_escape_u1.json",
    "n_array_spaces_vertical_tab_formfeed.json",
    "n_structure_open_array_object.json",
    "n_structure_open_object_open_string.json",
    "n_object_non_string_key_but_huge_number_instead.json",
    "i_number_real_underflow.json",
    "n_object_single_quote.json",
    "n_number_0.3e+.json",
    "n_number_real_garbage_after_e.json",
    "n_string_escaped_backslash_bad.json",
    "y_number_0e1.json",
    "y_number_real_pos_exponent.json",
    "y_string_double_escape_n.json",
    "y_array_arraysWithSpaces.json",
    "y_array_heterogeneous.json",
    "y_string_with_del_character.json",
    "n_object_with_trailing_garbage.json",
    "y_array_with_several_null.json",
    "y_number_simple_int.json",
    "n_number_expression.json",
    "n_structure_open_array_string.json",
    "n_object_missing_key.json",
    "n_string_1_surrogate_then_escape_u1x.json",
    "n_structure_trailing_#.json",
    "n_structure_array_with_extra_array_close.json",
    "y_object_empty_key.json",
    "y_string_nonCharacterInUTF-8_U+FFFF.json",
    "i_number_very_big_negative_int.json",
    "i_string_invalid_lonely_surrogate.json",
    "y_object_escaped_null_in_key.json",
    "n_structure_angle_bracket_..json",
    "y_string_accepted_surrogate_pairs.json",
    "i_structure_500_nested_arrays.json",
    "y_number_real_neg_exp.json",
    "y_structure_whitespace_array.json",
    "y_string_in_array_with_leading_space.json",
    "y_string_nonCharacterInUTF-8_U+10FFFF.json",
    "n_object_bracket_key.json",
    "i_number_neg_int_huge_exp.json",
    "n_number_NaN.json",
    "n_string_single_string_no_double_quotes.json",
    "n_number_neg_real_without_int_part.json",
    "n_string_unescaped_newline.json",
    "y_array_with_trailing_space.json",
    "i_string_not_in_unicode_range.json",
    "n_string_single_quote.json",
    "n_number_1_000.json",
    "y_object_duplicated_key.json",
    "n_string_incomplete_escaped_character.json",
    "n_object_no-colon.json",
    "y_string_one-byte-utf-8.json",
    "y_string_unicode_2.json",
    "n_object_trailing_comment.json",
    "y_string_unicode_U+FDD0_nonchar.json",
    "n_incomplete_false.json",
    "n_structure_null-byte-outside-string.json",
    "i_structure_UTF-8_BOM_empty_object.json",
    "y_string_nbsp_uescaped.json",
    "n_number_.-1.json",
    "n_structure_number_with_trailing_garbage.json",
    "n_number_real_without_fractional_part.json",
    "i_string_invalid_utf-8.json",
    "i_string_incomplete_surrogate_pair.json",
    "n_string_invalid_backslash_esc.json",
    "n_number_minus_infinity.json",
    "n_array_unclosed.json",
    "i_string_incomplete_surrogate_and_escape_valid.json",
    "y_array_false.json",
    "n_number_real_with_invalid_utf8_after_e.json",
    "y_string_unicode_U+FFFE_nonchar.json",
    "n_structure_open_open.json",
    "n_number_1.0e.json",
    "n_array_unclosed_trailing_comma.json",
    "n_array_comma_and_number.json",
    "n_string_1_surrogate_then_escape.json",
    "n_structure_unclosed_array_unfinished_true.json",
    "n_string_escaped_emoji.json",
    "n_array_comma_after_close.json",
    "y_string_reservedCharacterInUTF-8_U+1BFFF.json",
    "n_structure_U+2060_word_joined.json",
    "n_number_0.e1.json",
    "n_array_unclosed_with_new_lines.json",
    "n_structure_angle_bracket_null.json",
    "n_object_emoji.json",
    "n_object_unterminated-value.json",
    "n_structure_array_with_unclosed_string.json",
    "y_string_u+2029_par_sep.json",
    "n_object_double_colon.json",
    "y_object_basic.json",
    "i_string_lone_second_surrogate.json",
    "n_structure_open_array_open_string.json",
    "n_array_extra_close.json",
    "i_string_UTF-8_invalid_sequence.json",
    "n_array_inner_array_no_comma.json",
    "n_string_start_escape_unclosed.json",
    "n_string_invalid-utf-8-in-escape.json",
    "n_structure_open_array_open_object.json",
    "n_number_hex_1_digit.json",
    "n_structure_incomplete_UTF8_BOM.json",
    "y_string_in_array.json",
    "n_number_-1.0..json",
    "n_number_0_capital_E+.json",
    "y_number.json",
    "n_array_just_comma.json",
};

test "all tests tested" {
    var test_dir = try std.fs.cwd().openDirZ("test/JSONTestSuite/test_parsing", .{ .iterate = true });
    defer test_dir.close();

    var map: std.StringArrayHashMap(void) = .init(std.testing.allocator);
    defer map.deinit();

    for (expected_tests) |test_name| {
        try map.put(test_name, {});
    }

    var iter = test_dir.iterateAssumeFirstIteration();
    while (try iter.next()) |entry| {
        try std.testing.expect(map.orderedRemove(entry.name));
    }

    try std.testing.expect(map.count() == 0);
}
