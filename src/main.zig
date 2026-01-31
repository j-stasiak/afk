const std = @import("std");

const rl = @import("raylib");

const terminal = @import("terminal.zig");

const screen_width = 800;
const screen_height = 450;

const line_height = 24;

pub fn main() void {
    rl.initWindow(screen_width, screen_height, "AFK Game");
    defer rl.closeWindow();

    rl.setTargetFPS(60);

    var term: terminal.Terminal = .{
        .size = .{ .x = screen_width, .y = line_height * 2 },
        .position = .{ .x = 0, .y = screen_height - line_height * 2 },
    };

    var show_terminal: bool = false;

    while (!rl.windowShouldClose()) {
        if (rl.isKeyReleased(.grave) and !show_terminal) {
            show_terminal = true;
        }

        if (show_terminal) {
            term.update();

            const last_command = term.get_last_command();
            if (last_command == .quit) {
                show_terminal = false;
            }
        }

        rl.beginDrawing();
        defer rl.endDrawing();

        if (show_terminal) {
            term.draw();
        }

        rl.clearBackground(.white);
    }
}
