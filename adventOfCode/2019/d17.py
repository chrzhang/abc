#!/usr/bin/env python3

import functools
from collections import defaultdict
from enum import Enum, unique


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


class InputMoreThanOnceError(Exception):
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
        states[pos_of_result] = next(input_getter)
    elif modes.first() == Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        states[pos_of_result] = next(input_getter)


def enact_output(modes, states, idx, outputs, relative_base):
    if modes.first() is Mode.POSITION:
        pos_of_result = states[idx + 1]
        outputs.append(states[pos_of_result])
    elif modes.first() is Mode.IMMEDIATE:
        outputs.append(states[idx + 1])
    elif modes.first() is Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        outputs.append(states[pos_of_result])
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


def enact_opcode(opcode, modes, states, idx, relative_base, outputs, input_getter):
    idx_target = None
    if opcode is OpCode.ADD:
        enact_add(modes, states, idx, relative_base)
    elif opcode is OpCode.MULTIPLY:
        enact_multiply(modes, states, idx, relative_base)
    elif opcode is OpCode.INPUT:
        enact_input(modes, states, idx, input_getter, relative_base)
    elif opcode is OpCode.OUTPUT:
        enact_output(modes, states, idx, outputs, relative_base)
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


def solve(input_getter, read_states):
    states = defaultdict(int)
    for idx, val in enumerate(read_states):
        states[idx] = val
    relative_base = RelativeBase(0)
    outputs = []
    i = 0
    while True:
        opcode, modes = parse_opcode_and_modes(states[i])
        if opcode is OpCode.HALT:
            break
        i = enact_opcode(opcode, modes, states, i, relative_base, outputs, input_getter)
    return states, outputs


def get_scaffold_positions(rows):
    scaffold_positions = set()
    for row_idx, r in enumerate(rows):
        for col_idx, e in enumerate(r):
            if e != ".":
                scaffold_positions.add((row_idx, col_idx))
    return scaffold_positions


def draw(rows):
    for row_idx, r in enumerate(rows):
        print(r)


def get_intersections(scaffold_positions):
    intersections = set()
    for scaffold_row, scaffold_col in scaffold_positions:
        ns = neighbors(scaffold_row, scaffold_col)
        if all([n in scaffold_positions for n in ns]):
            intersections.add((scaffold_row, scaffold_col))
    return intersections


def neighbors(row, col):
    return [
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1),
    ]


def find_curr_pos(rows):
    for row_idx, r in enumerate(rows):
        for col_idx, e in enumerate(r):
            if e in ["^", "<", ">", "v"]:
                return (row_idx, col_idx)


class Direction(Enum):
    UP = "^"
    LEFT = "<"
    RIGHT = ">"
    DOWN = "v"


def str_turn(direction):
    return {Direction.LEFT: "L", Direction.RIGHT: "R"}[direction]


def find_curr_dir(robot):
    return Direction(robot)


def relative_position(target, source):
    source_row, source_col = source
    if target == (source_row - 1, source_col):
        return Direction.UP
    if target == (source_row + 1, source_col):
        return Direction.DOWN
    if target == (source_row, source_col - 1):
        return Direction.LEFT
    if target == (source_row, source_col + 1):
        return Direction.RIGHT


def get_turn(curr_dir, rel_pos):
    if rel_pos is Direction.UP:
        if curr_dir is Direction.RIGHT:
            return Direction.LEFT
        elif curr_dir is Direction.LEFT:
            return Direction.RIGHT
        else:
            raise RuntimeError
    elif rel_pos is Direction.RIGHT:
        if curr_dir is Direction.UP:
            return Direction.RIGHT
        elif curr_dir is Direction.DOWN:
            return Direction.LEFT
        else:
            raise RuntimeError
    elif rel_pos is Direction.LEFT:
        if curr_dir is Direction.UP:
            return Direction.LEFT
        elif curr_dir is Direction.DOWN:
            return Direction.RIGHT
        else:
            raise RuntimeError
    elif rel_pos is Direction.DOWN:
        if curr_dir is Direction.RIGHT:
            return Direction.RIGHT
        elif curr_dir is Direction.LEFT:
            return Direction.LEFT
        else:
            raise RuntimeError


def get_unvisited_neighbor(curr_row, curr_col, rows, last_visited_pos):
    ns = neighbors(curr_row, curr_col)
    for n_row, n_col in ns:
        if (n_row, n_col) == last_visited_pos:
            continue
        try:
            if rows[n_row][n_col] == "#":
                return (n_row, n_col)
        except IndexError:
            pass
    return None


def get_path(rows):
    path = []
    curr_row, curr_col = find_curr_pos(rows)
    curr_dir = find_curr_dir(rows[curr_row][curr_col])
    last_visited_pos = None
    while True:
        unvisited_neighbor = get_unvisited_neighbor(
            curr_row, curr_col, rows, last_visited_pos
        )
        if unvisited_neighbor is None:
            break
        rel_pos = relative_position(unvisited_neighbor, (curr_row, curr_col))
        turn = get_turn(curr_dir, rel_pos)
        path.append(str_turn(turn))
        curr_dir = rel_pos
        step_ctr = 0
        try:
            if curr_dir is Direction.UP:
                while rows[curr_row - 1][curr_col] == "#":
                    last_visited_pos = (curr_row, curr_col)
                    step_ctr += 1
                    curr_row -= 1
            elif curr_dir is Direction.RIGHT:
                while rows[curr_row][curr_col + 1] == "#":
                    last_visited_pos = (curr_row, curr_col)
                    step_ctr += 1
                    curr_col += 1
            elif curr_dir is Direction.LEFT:
                while rows[curr_row][curr_col - 1] == "#":
                    last_visited_pos = (curr_row, curr_col)
                    step_ctr += 1
                    curr_col -= 1
            elif curr_dir is Direction.DOWN:
                while rows[curr_row + 1][curr_col] == "#":
                    last_visited_pos = (curr_row, curr_col)
                    step_ctr += 1
                    curr_row += 1
        except IndexError:
            pass
        path[-1] = path[-1] + str(step_ctr)
    return path


def part_1(rows):
    scaffold_positions = get_scaffold_positions(rows)
    intersections = get_intersections(scaffold_positions)
    total = 0
    for intersection_row, intersection_col in intersections:
        total += intersection_row * intersection_col
    return total


def part_2(rows, states):
    print(" ".join(get_path(rows)))
    # Figured out these by hand, TODO to automate
    movement_routine = "A,B,A,C,A,B,C,B,C,A\n"
    movement_function_A = "L,12,R,4,R,4,L,6\n"
    movement_function_B = "L,12,R,4,R,4,R,12\n"
    movement_function_C = "L,10,L,6,R,4\n"
    newsfeed_option = "n\n"
    inputs = (
        movement_routine
        + movement_function_A
        + movement_function_B
        + movement_function_C
        + newsfeed_option
    )

    def input_getter():
        for i in inputs:
            yield ord(i)

    states = list(states)
    states[0] = 2
    _, outputs = solve(input_getter(), states)
    return outputs[-1]


if __name__ == "__main__":
    with open("inputs/day17_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    _, outputs = solve(None, read_states)
    rows = "".join([chr(n) for n in outputs]).split("\n")
    draw(rows)
    assert 3292 == part_1(rows)
    assert 651043 == part_2(rows, read_states)
