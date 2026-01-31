const std = @import("std");

const lx = @import("lexer.zig");

// zig fmt: off
const AstNodeType = enum {
    while_loop,
    if_statement,
    function_definition,
    binary_op,
    assignment,
    literal
};

pub const AstNode = union(AstNodeType) {
    while_loop: struct {
        contition: *AstNode,
        body: std.ArrayList(*AstNode)
    },
    if_statement: struct {
        condition: *AstNode,
        then_branch: std.ArrayList(*AstNode),
        else_branch: ?std.ArrayList(*AstNode)
    },
    function_definition: struct {
        name: []const u8,
        arg_count: usize = 0,
        body: std.ArrayList(*AstNode)
    },
    binary_op: struct {
        operator: lx.TokenType,
        left: *AstNode,
        right: *AstNode
    },
    assignment: struct {
        identifier: []const u8,
        value: *AstNode
    },
    literal: union(enum) {
        number: f32,
        boolean: bool,
        string: []const u8,
        identifier: []const u8
    }
};
// zig fmt: on

pub const Parser = struct {
    tokens: []const lx.Token,
    current: usize = 0,
    allocator: std.mem.Allocator,

    priority_map: std.AutoHashMap(u8, []const lx.TokenType),

    pub fn init(allocator: std.mem.Allocator, tokens: []const lx.Token) Parser {
        var priority_map = std.AutoHashMap(u8, []const lx.TokenType).init(allocator);
        priority_map.put(4, &.{.l_paren});
        priority_map.put(3, &.{ .asterisk, .slash });
        priority_map.put(2, &.{ .plus, .minus });
        priority_map.put(1, &.{ .more_than, .more_than_equal, .less_than, .less_than_equal, .double_equals, .different_than });

        return .{
            .priority_map = priority_map.move(),
            .tokens = tokens,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.priority_map.deinit();
    }

    /// Receiver is responsible for freeing the whole AST
    pub fn parse(self: *Parser) !std.ArrayList(*AstNode) {
        const statements = std.ArrayList(*AstNode){};

        while (!self.is_end()) {
            const statement = try self.process_next();
            statements.append(self.allocator, statement);
        }

        return statements;
    }

    fn process_next(self: *Parser) !*AstNode {
        const token = self.advance();

        const node = switch (token.type) {
            .kw_while => self.while_loop(),
            else => error.UnexpectedToken,
        };

        return node;
    }

    fn while_loop(self: *Parser) !*AstNode {
        _ = self.advance(); // consume while token
        _ = try self.expect_and_advance(.l_paren, "Expected opening parenthesis '('");
        const condition = try self.binary_operation();
        _ = try self.expect_and_advance(.r_paren, "Expected closing parenthesis ')'");
        _ = try self.expect_and_advance(.l_bracket, "Expected opening bracket '{'");

        var body = std.ArrayList(*AstNode);
        while (!self.is_end() and self.peek().type != .r_bracket) {
            const statement = try self.process_next();
            try body.append(self.allocator, statement);
        }

        _ = self.expect_and_advance(.r_bracket, "Expected closing bracket '}'");

        const node = self.allocator.create(AstNode);

        node.* = .{ .while_loop = .{
            .contition = condition,
            .body = body,
        } };

        return node;
    }

    fn binary_operation(self: *Parser) !*AstNode {
        std.debug.print("Got: {any}", self.peek());
        // @TODO: Check binary ops with proper operation ordering
        // binary_op = <left> <operator> <right>
        // left = expression e.g. (a + 6) * 3 -> need to parse parenthesis properly
        // operator = oneof TokenType (>, >=, ==, <, <=)
        // right = expression same a left
        // my god this seems like a pain...

        return self.binary_operation_inner(1);
    }

    fn binary_operation_inner(self: *Parser, current_priority: u8) !*AstNode {
        // @TODO add advances so we don't end up checking same token over and over...
        if (current_priority >= 4) {
            if (self.peek().type == .l_paren) {
                _ = self.advance();
                const node = try self.binary_operation_inner(1);
                _ = try self.expect_and_advance(.r_paren, "Expected closing parenthesis ')'");
                return node;
            }

            const node = self.create_literal();
            _ = self.advance();
            return node;
        }

        const left = self.binary_operation_inner(current_priority + 1);
        const current_priority_operators = self.priority_map.get(current_priority);

        var exists = false;
        for (current_priority_operators.?) |value| {
            if (self.peek().type == value) {
                exists = true;
                break;
            }
        }

        if (exists) {
            const operator = self.peek().type;
            _ = self.advance();
            const right = self.binary_operation_inner(current_priority + 1);

            const node = self.allocator.create(AstNode);
            node.* = .{ .binary_op = .{
                .operator = operator,
                .left = left,
                .right = right,
            } };

            return node;
        }

        return error.UnexpectedOperation;
    }

    fn create_literal(self: *Parser) !*AstNode {
        return switch (self.peek().type) {
            .boolean => self.create_bool(),
            .string => self.create_string(),
            .number => self.create_number(),
            .identifier => self.create_identifier(),
            else => error.UnexpectedLiteral,
        };
    }

    fn create_identifier(self: *Parser) !*AstNode {
        const name = self.peek().value;

        const node = self.allocator.create(AstNode);
        node.* = .{ .literal = .{ .identifier = name } };

        return node;
    }

    fn create_bool(self: *Parser) !*AstNode {
        const value = self.peek().value;
        var parsed: bool = undefined;

        if (std.mem.eql(u8, "true", value)) {
            parsed = true;
        } else if (std.mem.eql(u8, "false", value)) {
            parsed = false;
        } else {
            return error.UnexpectedBooleanValue;
        }

        const node = self.allocator.create(AstNode);
        node.* = .{ .literal = .{ .boolean = parsed } };

        return node;
    }

    fn create_number(self: *Parser) !*AstNode {
        const value = try std.fmt.parseFloat(f32, self.peek().value);

        const node = self.allocator.create(AstNode);
        node.* = .{ .literal = .{ .number = value } };

        return node;
    }

    fn create_string(self: *Parser) !*AstNode {
        const value = self.peek().value;
        const str = value[1 .. value.len - 1];

        const node = self.allocator.create(AstNode);
        node.* = .{ .literal = .{ .string = str } };

        return node;
    }

    /// check if next token is of token_type and advance, throw error with provided message if it's not
    fn expect_and_advance(self: *Parser, token_type: lx.TokenType, error_message: []const u8) !lx.Token {
        if (self.is_end() or self.peek() != token_type) {
            std.debug.print("Parse error on line {d}. Message: {s}", .{ self.peek().line, error_message });
            return error.ParseError;
        }
        return self.advance();
    }

    fn is_end(self: *Parser) bool {
        return self.peek().type == .eof;
    }

    fn advance(self: *Parser) lx.Token {
        const current_token = self.peek();
        self.current += 1;

        return current_token;
    }

    fn peek(self: *Parser) lx.Token {
        return self.tokens[self.current];
    }

    fn peek_previous(self: *Parser) ?lx.Token {
        return if (self.current <= 0) null else self.tokens[self.current - 1];
    }

    fn peek_next(self: *Parser) ?lx.Token {
        return if (self.current + 1 >= self.tokens.len) null else self.tokens[self.current + 1];
    }
};
