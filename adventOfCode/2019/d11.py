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
class Color(Enum):
    BLACK = 0
    WHITE = 1


@unique
class TurnDir(Enum):
    LEFT = 0
    RIGHT = 1


@unique
class Direction(Enum):
    UP = auto()
    DOWN = auto()
    LEFT = auto()
    RIGHT = auto()


@unique
class OutputMode(Enum):
    COLOR = auto()
    TURN = auto()


class Solver:
    def __init__(self):
        self.curr_pos = (0, 0)
        self.curr_dir = Direction.UP
        self.white_panels = set()
        self.curr_output_mode = OutputMode.COLOR
        self.panels_painted = set()

    def read_hull(self):
        return (
            Color.WHITE.value
            if self.curr_pos in self.white_panels
            else Color.BLACK.value
        )

    def paint_hull(self, color):
        self.panels_painted.add(self.curr_pos)
        if Color(color) is Color.BLACK and self.curr_pos in self.white_panels:
            self.white_panels.remove(self.curr_pos)
        elif Color(color) is Color.WHITE and self.curr_pos not in self.white_panels:
            self.white_panels.add(self.curr_pos)

    @staticmethod
    def turn(my_dir, turn_dir):
        if my_dir is Direction.UP:
            if turn_dir is TurnDir.RIGHT:
                return Direction.RIGHT
            elif turn_dir is TurnDir.LEFT:
                return Direction.LEFT
        elif my_dir is Direction.DOWN:
            if turn_dir is TurnDir.RIGHT:
                return Direction.LEFT
            elif turn_dir is TurnDir.LEFT:
                return Direction.RIGHT
        elif my_dir is Direction.RIGHT:
            if turn_dir is TurnDir.RIGHT:
                return Direction.DOWN
            elif turn_dir is TurnDir.LEFT:
                return Direction.UP
        elif my_dir is Direction.LEFT:
            if turn_dir is TurnDir.RIGHT:
                return Direction.UP
            elif turn_dir is TurnDir.LEFT:
                return Direction.DOWN

    @staticmethod
    def move(my_pos, my_dir):
        if my_dir is Direction.UP:
            return (my_pos[0], my_pos[1] - 1)
        elif my_dir is Direction.DOWN:
            return (my_pos[0], my_pos[1] + 1)
        elif my_dir is Direction.RIGHT:
            return (my_pos[0] + 1, my_pos[1])
        elif my_dir is Direction.LEFT:
            return (my_pos[0] - 1, my_pos[1])

    def turn_bot(self, turn_dir):
        self.curr_dir = self.turn(self.curr_dir, TurnDir(turn_dir))
        self.curr_pos = self.move(self.curr_pos, self.curr_dir)

    def output_handler(self, v):
        if self.curr_output_mode is OutputMode.COLOR:
            self.paint_hull(v)
            self.curr_output_mode = OutputMode.TURN
        elif self.curr_output_mode is OutputMode.TURN:
            self.turn_bot(v)
            self.curr_output_mode = OutputMode.COLOR

    def run_bot(self, states):
        solve(states, self.read_hull, self.output_handler)


if __name__ == "__main__":
    with open("inputs/day11_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    def part_1():
        s = Solver()
        s.run_bot(read_states)
        assert 1907 == len(s.panels_painted)
    def part_2():
        s = Solver()
        s.white_panels.add(s.curr_pos)
        s.run_bot(read_states)
        board_width = 1 + max([p[0] for p in s.white_panels])
        board_height = 1 + max([p[1] for p in s.white_panels])
        board = [[' ' for _ in range(board_width)] for __ in range(board_height)] 
        for p in s.white_panels:
            board[p[1]][p[0]] = '@'
        print('\n'.join([' '.join(r) for r in board]))
    part_1()
    part_2()
