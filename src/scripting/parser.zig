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
        condition: *AstNode,
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

    pub fn init(allocator: std.mem.Allocator, tokens: []const lx.Token) !Parser {
        var priority_map = std.AutoHashMap(u8, []const lx.TokenType).init(allocator);
        try priority_map.put(4, &.{.l_paren});
        try priority_map.put(3, &.{ .asterisk, .slash });
        try priority_map.put(2, &.{ .plus, .minus });
        try priority_map.put(1, &.{ .more_than, .more_than_equal, .less_than, .less_than_equal, .double_equals, .different_than });

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
        var statements: std.ArrayList(*AstNode) = .empty;

        while (!self.is_end()) {
            const statement = try self.process_next();
            try statements.append(self.allocator, statement);
        }

        return statements;
    }

    fn process_next(self: *Parser) error{UnexpectedToken}!*AstNode {
        const token = self.advance();

        const node = switch (token.type) {
            .kw_while => self.while_loop() catch unreachable,
            .kw_if => self.if_statement() catch unreachable,
            .kw_fn => self.function_declaration() catch unreachable,
            else => return error.UnexpectedToken,
        };

        return node;
    }

    fn while_loop(self: *Parser) !*AstNode {
        _ = try self.expect_and_advance(.l_paren, "Expected opening parenthesis '('");
        const condition = try self.binary_operation();
        _ = try self.expect_and_advance(.r_paren, "Expected closing parenthesis ')'");
        _ = try self.expect_and_advance(.l_bracket, "Expected opening bracket '{'");

        var body: std.ArrayList(*AstNode) = .empty;
        while (!self.is_end() and self.peek().type != .r_bracket) {
            const statement = self.process_next() catch unreachable;
            try body.append(self.allocator, statement);
        }

        _ = try self.expect_and_advance(.r_bracket, "Expected closing bracket '}'");

        const node = self.allocator.create(AstNode) catch unreachable;

        node.* = .{ .while_loop = .{
            .condition = condition,
            .body = body,
        } };

        return node;
    }

    fn if_statement(self: *Parser) !*AstNode {
        _ = try self.expect_and_advance(.l_paren, "Expected opening parenthesis '('");
        const condition = try self.binary_operation();
        _ = try self.expect_and_advance(.r_paren, "Expected closing parenthesis ')'");
        _ = try self.expect_and_advance(.l_bracket, "Expected opening bracket '{'");

        var body: std.ArrayList(*AstNode) = .empty;
        while (!self.is_end() and self.peek().type != .r_bracket) {
            const statement = self.process_next() catch unreachable;
            try body.append(self.allocator, statement);
        }

        _ = try self.expect_and_advance(.r_bracket, "Expected closing bracket '}'");

        var else_branch: ?std.ArrayList(*AstNode) = null;
        if (self.peek_next() != null and self.peek_next().?.type == .kw_else) {
            _ = self.advance(); // consume kw_else
            _ = self.advance(); // go to next
            _ = try self.expect_and_advance(.l_bracket, "Expected opening bracket '{'");

            else_branch.? = .empty;
            while (!self.is_end() and self.peek().type != .r_bracket) {
                const statement = self.process_next() catch unreachable;
                try else_branch.?.append(self.allocator, statement);
            }

            _ = try self.expect_and_advance(.r_bracket, "Expected closing bracket '}'");
        }

        const node = self.allocator.create(AstNode) catch unreachable;

        node.* = .{ .if_statement = .{ .condition = condition, .then_branch = body, .else_branch = else_branch } };

        return node;
    }

    fn function_declaration(self: *Parser) !*AstNode {
        const name_token = try self.expect_and_advance(.identifier, "Expected function name");
        _ = try self.expect_and_advance(.l_paren, "Expected opening parenthesis '('");

        var arg_count = 0;
        while (!self.is_end() and self.peek().type != .r_paren) {
            if (arg_count != 0) {
                _ = try self.expect_and_advance(.comma, "Expected parameter separator ','");
            }
            _ = try self.expect_and_advance(.identifier, "Expected parameter name");
            arg_count += 1;
        }

        _ = self.advance();
        _ = try self.expect_and_advance(.l_bracket, "Expected opening bracket '{'");

        var body: std.ArrayList(*AstNode) = .empty;
        while (!self.is_end() and self.peek().type != .r_bracket) {
            const statement = self.process_next() catch unreachable;
            try body.append(self.allocator, statement);
        }
        _ = try self.expect_and_advance(.r_bracket, "Expected closing bracket '}'");

        if (body.items.len == 0) {
            std.debug.print("Expected non-empty function body");
            return error.EmptyFunctionBody;
        }

        const node = self.allocator.create(AstNode) catch unreachable;
        node.* = .{ .function_definition = .{ .name = name_token.value, .arg_count = arg_count, .body = body } };

        return node;
    }

    fn binary_operation(self: *Parser) !*AstNode {
        return self.binary_operation_inner(1);
    }

    fn binary_operation_inner(self: *Parser, current_priority: u8) !*AstNode {
        if (current_priority >= 4) {
            if (self.peek().type == .l_paren) {
                _ = self.advance();
                const node = try self.binary_operation_inner(1);
                _ = try self.expect_and_advance(.r_paren, "Expected closing parenthesis ')'");
                return node;
            }

            const node = try self.create_literal();
            _ = self.advance();
            return node;
        }

        const left = self.binary_operation_inner(current_priority + 1) catch unreachable;
        const current_priority_operators = self.priority_map.get(current_priority);

        for (current_priority_operators.?) |value| {
            if (self.peek().type == value) {
                const operator = self.peek().type;
                _ = self.advance();
                const right = self.binary_operation_inner(current_priority + 1) catch unreachable;

                const node = self.allocator.create(AstNode) catch unreachable;
                node.* = .{ .binary_op = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                } };

                return node;
            }
        }

        return left;
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

        const node = self.allocator.create(AstNode) catch unreachable;
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

        const node = self.allocator.create(AstNode) catch unreachable;
        node.* = .{ .literal = .{ .boolean = parsed } };

        return node;
    }

    fn create_number(self: *Parser) !*AstNode {
        const value = try std.fmt.parseFloat(f32, self.peek().value);

        const node = self.allocator.create(AstNode) catch unreachable;
        node.* = .{ .literal = .{ .number = value } };

        return node;
    }

    fn create_string(self: *Parser) !*AstNode {
        const value = self.peek().value;
        const str = value[1 .. value.len - 1];

        const node = self.allocator.create(AstNode) catch unreachable;
        node.* = .{ .literal = .{ .string = str } };

        return node;
    }

    /// check if next token is of token_type and advance, throw error with provided message if it's not
    fn expect_and_advance(self: *Parser, token_type: lx.TokenType, error_message: []const u8) !lx.Token {
        if (self.is_end() or self.peek().type != token_type) {
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

fn freeNode(allocator: std.mem.Allocator, node: *AstNode) void {
    switch (node.*) {
        .while_loop => |wl| {
            freeNode(allocator, wl.condition);
            freeAST(allocator, wl.body);
        },
        .if_statement => |ifs| {
            freeNode(allocator, ifs.condition);
            freeAST(allocator, ifs.then_branch);
            if (ifs.else_branch) |eb| {
                freeAST(allocator, eb);
            }
        },
        .function_definition => |fd| {
            freeAST(allocator, fd.body);
        },
        .binary_op => |bo| {
            freeNode(allocator, bo.left);
            freeNode(allocator, bo.right);
        },
        .assignment => |a| {
            freeNode(allocator, a.value);
        },
        .literal => {},
    }

    allocator.destroy(node);
}

pub fn freeAST(allocator: std.mem.Allocator, list: std.ArrayList(*AstNode)) void {
    for (list.items) |node| {
        freeNode(allocator, node);
    }
    var mutable = list;
    mutable.deinit(allocator);
}

test "while loop parsing" {
    const script = "while(a + b > c) {}";

    var lexer: lx.Lexer = .{
        .source = script,
    };

    var tokens: [11]lx.Token = .{undefined} ** 11;
    for (tokens, 0..) |_, i| {
        const token = lexer.tokenize() catch unreachable;
        tokens[i] = token;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var parser = Parser.init(gpa.allocator(), &tokens) catch unreachable;
    defer parser.deinit();

    const ast = try parser.parse();
    defer freeAST(gpa.allocator(), ast);

    try std.testing.expect(ast.items[0].* == .while_loop);
    const loop = ast.items[0].while_loop;
    try std.testing.expect(loop.condition.* == .binary_op);
    const binary_op = loop.condition.binary_op;
    try std.testing.expect(binary_op.operator == .more_than);
    try std.testing.expect(binary_op.right.* == .literal);
    try std.testing.expect(binary_op.left.* == .binary_op);
}

test "if statement parsing" {
    const script = "if(a > 5) {}";

    var lexer: lx.Lexer = .{
        .source = script,
    };

    var tokens: [9]lx.Token = .{undefined} ** 9;
    for (tokens, 0..) |_, i| {
        const token = lexer.tokenize() catch unreachable;
        tokens[i] = token;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var parser = Parser.init(gpa.allocator(), &tokens) catch unreachable;
    defer parser.deinit();

    const ast = try parser.parse();
    defer freeAST(gpa.allocator(), ast);

    try std.testing.expect(ast.items[0].* == .if_statement);
    const statement = ast.items[0].if_statement;
    try std.testing.expect(statement.condition.* == .binary_op);
    const binary_op = statement.condition.binary_op;
    try std.testing.expect(binary_op.left.literal == .identifier);
    try std.testing.expect(binary_op.right.literal == .number);
}
