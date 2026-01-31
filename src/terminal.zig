const std = @import("std");

const rl = @import("raylib");

const TERMINAL_BUFFER_SIZE = 128;

pub const Command = enum { quit, help, new_file };

pub const Terminal = struct {
    position: rl.Vector2,
    size: rl.Vector2,
    buffer: [TERMINAL_BUFFER_SIZE:0]u8 = .{0} ** TERMINAL_BUFFER_SIZE,
    text_index: usize = 0,
    last_command: ?Command = undefined,

    pub fn draw(self: *Terminal) void {
        rl.drawRectangleV(self.position, self.size, .gray);
        rl.drawText(">", @as(i32, @intFromFloat(self.position.x)) + 24, @as(i32, @intFromFloat(self.position.y)) + @divFloor(@as(i32, @intFromFloat(self.size.y)) - 24, 2), 24, .light_gray);
        rl.drawText(&self.buffer, @as(i32, @intFromFloat(self.position.x)) + 48, @as(i32, @intFromFloat(self.position.y)) + @divFloor(@as(i32, @intFromFloat(self.size.y)) - 24, 2), 24, .white);
    }

    pub fn update(self: *Terminal) void {
        const key = rl.getKeyPressed();
        const keyInt: c_int = @intFromEnum(key);
        if (keyInt >= 32 and keyInt <= 125) {
            self.buffer[self.text_index] = @intCast(keyInt);
            self.text_index += 1;
        }

        if (rl.isKeyReleased(.backspace) and self.text_index > 0) {
            self.buffer[self.text_index - 1] = 0;
            self.text_index -= 1;
        }

        if (rl.isKeyReleased(.enter)) {
            self.process_command();
            for (self.buffer, 0..) |_, i| {
                self.buffer[i] = 0;
            }
            self.text_index = 0;
        }
    }

    pub fn get_last_command(self: *Terminal) ?Command {
        const tmp = self.last_command;
        self.last_command = undefined;

        return tmp;
    }

    fn process_command(self: *Terminal) void {
        var command_iter = std.mem.splitScalar(u8, &self.buffer, ' ');
        const command = command_iter.next().?;
        if (self.command_equals(command, "QUIT")) {
            self.last_command = .quit;
        } else if (self.command_equals(command, "NEW-FILE")) {
            self.last_command = .new_file;
            const name = command_iter.next().?;
            std.debug.print("{s}", .{name});
        }
    }

    fn command_equals(_: *Terminal, command_slice: []const u8, literal: []const u8) bool {
        const command_end = if (command_slice.len > literal.len) literal.len else command_slice.len;
        return std.mem.eql(u8, command_slice[0..command_end], literal);
    }
};
