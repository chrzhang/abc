#!/usr/bin/env python3

import functools
from collections import defaultdict
from enum import Enum, unique, auto


@unique
class OpCode(Enum):
    ADD = "01"
    MULTIPLY = "02"
    INPUT = "03"
    OUTPUT = "04"
    JUMP_IF_TRUE = "05"
    JUMP_IF_FALSE = "06"
    LESS_THAN = "07"
    EQUALS = "08"
    ADJUST_REL_BASE = "09"
    HALT = "99"


class UnknownOpCodeError(Exception):
    pass


class UnknownModeError(Exception):
    pass


class WritingToImmediateError(Exception):
    pass


def number_of_values(opcode):
    if opcode in (OpCode.ADD, OpCode.MULTIPLY, OpCode.LESS_THAN, OpCode.EQUALS):
        return 4
    if opcode in (OpCode.INPUT, OpCode.OUTPUT, OpCode.ADJUST_REL_BASE):
        return 2
    if opcode in (OpCode.JUMP_IF_FALSE, OpCode.JUMP_IF_TRUE):
        return 3
    raise UnknownOpCodeError


@unique
class Mode(Enum):
    POSITION = "0"
    IMMEDIATE = "1"
    RELATIVE = "2"


class Modes:
    def __init__(self, modes):
        self._modes = [Mode(m) for m in modes]

    def first(self):
        return self._modes[2]

    def second(self):
        return self._modes[1]

    def third(self):
        return self._modes[0]


def parse_opcode_and_modes(first_value):
    padded = str(first_value).zfill(5)
    opcode = OpCode(padded[-2:])
    modes = Modes(padded[:3])
    return opcode, modes


class RelativeBase:
    def __init__(self, val):
        self._val = val

    def set_val(self, val):
        self._val = val

    def val(self):
        return self._val


def read_param(mode, param, states, relative_base):
    if mode == Mode.IMMEDIATE:
        return param
    if mode == Mode.POSITION:
        return states[param]
    if mode == Mode.RELATIVE:
        return states[param + relative_base.val()]


def get_params(modes, states, idx, relative_base):
    param_1 = read_param(modes.first(), states[idx + 1], states, relative_base)
    param_2 = read_param(modes.second(), states[idx + 2], states, relative_base)
    if modes.third() is Mode.IMMEDIATE:
        raise WritingToImmediateError
    if modes.third() is Mode.POSITION:
        pos_of_result = states[idx + 3]
    elif modes.third() is Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 3]
    return param_1, param_2, pos_of_result


def enact_add(modes, states, idx, relative_base):
    param_1, param_2, pos_of_result = get_params(modes, states, idx, relative_base)
    states[pos_of_result] = param_1 + param_2


def enact_multiply(modes, states, idx, relative_base):
    param_1, param_2, pos_of_result = get_params(modes, states, idx, relative_base)
    states[pos_of_result] = param_1 * param_2


def enact_input(modes, states, idx, input_getter, relative_base):
    if modes.first() == Mode.IMMEDIATE:
        raise WritingToImmediateError
    if modes.first() == Mode.POSITION:
        pos_of_result = states[idx + 1]
        states[pos_of_result] = input_getter()
    elif modes.first() == Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        states[pos_of_result] = input_getter()


def enact_output(modes, states, idx, output_setter, relative_base):
    if modes.first() is Mode.POSITION:
        pos_of_result = states[idx + 1]
        output_setter(states[pos_of_result])
    elif modes.first() is Mode.IMMEDIATE:
        output_setter(states[idx + 1])
    elif modes.first() is Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        output_setter(states[pos_of_result])
    else:
        raise UnknownModeError


def enact_jump_if_condition(condition, modes, states, idx, relative_base):
    param_1 = read_param(modes.first(), states[idx + 1], states, relative_base)
    if condition(param_1):
        return read_param(modes.second(), states[idx + 2], states, relative_base)
    return None


enact_jump_if_true = functools.partial(enact_jump_if_condition, lambda p: p)
enact_jump_if_false = functools.partial(enact_jump_if_condition, lambda p: not p)


def enact_less_than(modes, states, idx, relative_base):
    param_1, param_2, pos_of_result = get_params(modes, states, idx, relative_base)
    states[pos_of_result] = 1 if param_1 < param_2 else 0


def enact_equals(modes, states, idx, relative_base):
    param_1, param_2, pos_of_result = get_params(modes, states, idx, relative_base)
    states[pos_of_result] = 1 if param_1 == param_2 else 0


def enact_adjust_relative_base(modes, states, idx, relative_base):
    param_1 = read_param(modes.first(), states[idx + 1], states, relative_base)
    relative_base.set_val(param_1 + relative_base.val())


def enact_opcode(
    opcode, modes, states, idx, relative_base, output_setter, input_getter
):
    idx_target = None
    if opcode is OpCode.ADD:
        enact_add(modes, states, idx, relative_base)
    elif opcode is OpCode.MULTIPLY:
        enact_multiply(modes, states, idx, relative_base)
    elif opcode is OpCode.INPUT:
        enact_input(modes, states, idx, input_getter, relative_base)
    elif opcode is OpCode.OUTPUT:
        enact_output(modes, states, idx, output_setter, relative_base)
    elif opcode is OpCode.JUMP_IF_TRUE:
        idx_target = enact_jump_if_true(modes, states, idx, relative_base)
    elif opcode is OpCode.JUMP_IF_FALSE:
        idx_target = enact_jump_if_false(modes, states, idx, relative_base)
    elif opcode is OpCode.LESS_THAN:
        enact_less_than(modes, states, idx, relative_base)
    elif opcode is OpCode.EQUALS:
        enact_equals(modes, states, idx, relative_base)
    elif opcode is OpCode.ADJUST_REL_BASE:
        enact_adjust_relative_base(modes, states, idx, relative_base)
    else:
        raise UnknownOpCodeError
    if idx_target is None:
        return idx + number_of_values(opcode)
    else:
        return idx_target


def solve(read_states, input_getter, output_setter):
    states = defaultdict(int)
    for idx, val in enumerate(read_states):
        states[idx] = val
    relative_base = RelativeBase(0)
    i = 0
    while True:
        opcode, modes = parse_opcode_and_modes(states[i])
        if opcode is OpCode.HALT:
            break
        i = enact_opcode(
            opcode, modes, states, i, relative_base, output_setter, input_getter
        )


@unique
class TileType(Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    HORIZONTAL_PADDLE = 3
    BALL = 4


class Solver:
    def __init__(self):
        self.curr_x = None
        self.curr_y = None
        self.block_count = 0
        self.ball_pos = None
        self.paddle_pos = None
        self.last_output_score = None

    def input_getter(self):
        if self.paddle_pos[0] < self.ball_pos[0]:
            return 1
        elif self.paddle_pos[0] > self.ball_pos[0]:
            return -1
        return 0

    def reset_pos(self):
        self.curr_x = None
        self.curr_y = None

    def output_handler(self, v):
        if self.curr_x is None:
            self.curr_x = v
        elif self.curr_y is None:
            self.curr_y = v
        elif self.curr_x == -1 and self.curr_y == 0:
            self.last_output_score = v
            self.reset_pos()
        else:
            tile_type = TileType(v)
            curr_pos = (self.curr_x, self.curr_y)
            if tile_type is TileType.BLOCK:
                self.block_count += 1
            elif tile_type is TileType.BALL:
                self.ball_pos = curr_pos
            elif tile_type is TileType.HORIZONTAL_PADDLE:
                self.paddle_pos = curr_pos
            self.reset_pos()

    def run_bot(self, states):
        solve(states, self.input_getter, self.output_handler)


if __name__ == "__main__":
    with open("inputs/day13_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])

    def part_1():
        s = Solver()
        s.run_bot(read_states)
        assert 420 == s.block_count

    def part_2():
        s = Solver()
        lread_states = list(read_states)
        lread_states[0] = 2
        s.run_bot(lread_states)
        assert 21651 == s.last_output_score

    part_1()
    part_2()
