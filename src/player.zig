const std = @import("std");

const rl = @import("raylib");

pub const Character = struct {
    level: u32,
    exp: u64,
    exp_to_next_level: u64,
    hp: u64,
};
