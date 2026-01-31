const std = @import("std");

// @TODO: Better error handling
pub const TokenType = enum {
    l_bracket,
    r_bracket,
    l_paren,
    r_paren,
    l_square_bracket,
    r_square_bracket,
    semicolon,
    comma,

    identifier,
    number,
    string,
    boolean,

    // operators
    plus,
    minus,
    slash,
    asterisk,
    equals,
    double_equals,
    less_than,
    more_than,
    less_than_equal,
    more_than_equal,
    different_than,

    // keywords
    kw_if,
    kw_else,
    kw_while,
    kw_fn,
    kw_struct,

    eof,
};

pub const Token = struct { type: TokenType, value: []const u8, line: usize };

pub const Lexer = struct {
    source: []const u8,
    token_start: usize = 0,
    current_pos: usize = 0,
    line: usize = 1,

    fn tokenize(self: *Lexer) !Token {
        self.skip_whitespaces();

        if (self.is_end()) {
            return .{
                .line = self.line,
                .type = .eof,
                .value = "",
            };
        }

        self.token_start = self.current_pos;
        const char = self.advance();

        if (std.ascii.isAlphabetic(char) or char == '_') return self.tokenize_identifier();
        if (std.ascii.isDigit(char)) return self.tokenize_number();
        const next = self.peek();
        if (next != 0 and !std.ascii.isAlphabetic(next) and !std.ascii.isDigit(next)) {
            const result = try self.tokenize_double_op();
            if (result != null) {
                return result.?;
            }
        }

        return switch (char) {
            '(' => self.create_token(.l_paren),
            ')' => self.create_token(.r_paren),
            '{' => self.create_token(.l_bracket),
            '}' => self.create_token(.r_bracket),
            '[' => self.create_token(.l_square_bracket),
            ']' => self.create_token(.r_square_bracket),
            '=' => self.create_token(.equals),
            '>' => self.create_token(.more_than),
            '<' => self.create_token(.less_than),
            '+' => self.create_token(.plus),
            '-' => self.create_token(.minus),
            '/' => self.create_token(.slash),
            '*' => self.create_token(.asterisk),
            ';' => self.create_token(.semicolon),
            ',' => self.create_token(.comma),
            '"' => self.tokenize_string(),
            else => error.UnexpectedCharacter,
        };
    }

    fn tokenize_number(self: *Lexer) Token {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.') {
            _ = self.advance();

            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.create_token(.number);
    }

    fn tokenize_string(self: *Lexer) !Token {
        while (self.peek() != '"' and !self.is_end()) {
            _ = self.advance();
        }

        if (self.is_end()) {
            return error.UnterminatedString;
        }

        // consume closing "
        _ = self.advance();
        return self.create_token(.string);
    }

    fn tokenize_identifier(self: *Lexer) Token {
        while (std.ascii.isAlphabetic(self.peek()) or self.peek() == '_') {
            _ = self.advance();
        }

        const text = self.source[self.token_start..self.current_pos];

        if (std.mem.eql(u8, "true", text) or std.mem.eql(u8, "false", text)) {
            return self.create_token(.boolean);
        }

        const token_type = self.get_keyword_type(text);

        return self.create_token(token_type orelse .identifier);
    }

    fn tokenize_double_op(self: *Lexer) !?Token {
        var text: [2]u8 = undefined;
        _ = try std.fmt.bufPrint(&text, "{c}{c}", .{ self.peek_prev(), self.peek() });

        var result: ?Token = null;
        if (std.mem.eql(u8, &text, "==")) result = self.create_token(.double_equals);
        if (std.mem.eql(u8, &text, ">=")) result = self.create_token(.more_than_equal);
        if (std.mem.eql(u8, &text, "<=")) result = self.create_token(.less_than_equal);
        if (std.mem.eql(u8, &text, "!=")) result = self.create_token(.different_than);

        if (result != null) {
            _ = self.advance();
        }

        return result; // token not found
    }

    fn skip_whitespaces(self: *Lexer) void {
        while (!self.is_end()) {
            const current = self.source[self.current_pos];
            switch (current) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                // line comment '#'
                '#' => {
                    while (self.peek() != '\n' and !self.is_end()) {
                        _ = self.advance();
                    }
                },
                else => return,
            }
        }
    }

    fn peek(self: *Lexer) u8 {
        return if (self.is_end()) 0 else self.source[self.current_pos];
    }

    fn peek_prev(self: *Lexer) u8 {
        return if (self.current_pos <= 0) 0 else self.source[self.current_pos - 1];
    }

    fn peek_next(self: *Lexer) ?u8 {
        return if (self.current_pos + 1 >= self.source.len) null else self.source[self.current_pos + 1];
    }

    fn get_keyword_type(_: *Lexer, text: []const u8) ?TokenType {
        const map = std.StaticStringMap(TokenType)
            .initComptime(.{ .{ "if", .kw_if }, .{ "fn", .kw_fn }, .{ "while", .kw_while }, .{ "struct", .kw_struct }, .{ "else", .kw_else } });

        return map.get(text);
    }

    fn is_end(self: *Lexer) bool {
        return self.current_pos >= self.source.len;
    }

    fn advance(self: *Lexer) u8 {
        const char = self.current_pos;
        self.current_pos += 1;

        return self.source[char];
    }

    fn create_token(self: *Lexer, tt: TokenType) Token {
        return .{
            .line = self.line,
            .value = self.source[self.token_start..self.current_pos],
            .type = tt,
        };
    }
};

test "Test properly formatted while loop" {
    var lx: Lexer = .{ .source = "while (value >= 5) {}" };

    const while_kw = try lx.tokenize();
    try std.testing.expect(while_kw.type == .kw_while);

    const lparen = try lx.tokenize();
    try std.testing.expect(lparen.type == .l_paren);

    const variable = try lx.tokenize();
    try std.testing.expect(variable.type == .identifier);

    const ge = try lx.tokenize();
    try std.testing.expect(ge.type == .more_than_equal);

    const num = try lx.tokenize();
    try std.testing.expect(num.type == .number);

    const rparen = try lx.tokenize();
    try std.testing.expect(rparen.type == .r_paren);

    const lbracket = try lx.tokenize();
    try std.testing.expect(lbracket.type == .l_bracket);

    const rbracket = try lx.tokenize();
    try std.testing.expect(rbracket.type == .r_bracket);

    const eof = try lx.tokenize();
    try std.testing.expect(eof.type == .eof);
}

test "Test multiline source" {
    var lx: Lexer = .{ .source = "arr = [5, 2];\ntruthy = true;" };

    const arr = try lx.tokenize();
    try std.testing.expect(arr.type == .identifier);

    const eq = try lx.tokenize();
    try std.testing.expect(eq.type == .equals);

    const lbr = try lx.tokenize();
    try std.testing.expect(lbr.type == .l_square_bracket);

    const first_num = try lx.tokenize();
    try std.testing.expect(first_num.type == .number);

    const sep = try lx.tokenize();
    try std.testing.expect(sep.type == .comma);

    const sec_num = try lx.tokenize();
    try std.testing.expect(sec_num.type == .number);

    const rbr = try lx.tokenize();
    try std.testing.expect(rbr.type == .r_square_bracket);

    const semi = try lx.tokenize();
    try std.testing.expect(semi.type == .semicolon);

    const truthy = try lx.tokenize();
    try std.testing.expect(truthy.type == .identifier);
    try std.testing.expect(truthy.line == 2);
}
