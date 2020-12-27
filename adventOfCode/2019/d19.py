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


NUMBER_OF_VALUES = {
    OpCode.ADD: 4,
    OpCode.MULTIPLY: 4,
    OpCode.LESS_THAN: 4,
    OpCode.EQUALS: 4,
    OpCode.INPUT: 2,
    OpCode.OUTPUT: 2,
    OpCode.ADJUST_REL_BASE: 2,
    OpCode.JUMP_IF_FALSE: 3,
    OpCode.JUMP_IF_TRUE: 3,
}


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


@functools.cache
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
        return idx + NUMBER_OF_VALUES[opcode]
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


def make_input_getter(row, col):
    def input_getter():
        yield col
        yield row

    return input_getter()


def part1(read_states):
    counter = 0
    for row in range(50):
        leftmost_col = find_leftmost_pulled_col(read_states, row)
        if leftmost_col is None:
            continue
        for col in range(leftmost_col, 50):
            _, (output,) = solve(make_input_getter(row, col), read_states)
            counter += output
    return counter


def binary_search(low, high, checker):
    if low > high:
        raise RuntimeError("Binary search out of bounds.")
    mid = (high + low) // 2
    if checker(mid - 1):
        return binary_search(low, mid - 1, checker)
    if checker(mid):
        return mid
    return binary_search(mid + 1, high, checker)


def exponential_search(begin, checker):
    assert not checker(begin)
    power_of_two = 0
    while not checker(begin * (2 ** power_of_two)):
        power_of_two += 1
    lower = begin * (2 ** (power_of_two - 1))
    upper = begin * (2 ** (power_of_two))
    return binary_search(lower, upper, checker)


def find_leftmost_pulled_col(read_states, row):
    col = 0
    while True:
        # After visualizing the tractor beam, there are some skipped rows and columns
        if row in [1, 2, 3]:
            return None
        _, (output,) = solve(make_input_getter(row, col), read_states)
        if output:
            return col
        col += 1


def part2(read_states):
    BOX_WIDTH = 100

    def checker(row):
        left_col = find_leftmost_pulled_col(read_states, row)
        if not left_col:
            return False
        if row - (BOX_WIDTH - 1) <= 0:
            return False

        return (
            solve(make_input_getter(row, left_col + BOX_WIDTH - 1), read_states)[1][0]
            and solve(make_input_getter(row - (BOX_WIDTH - 1), left_col), read_states)[
                1
            ][0]
            and solve(
                make_input_getter(row - (BOX_WIDTH - 1), left_col + BOX_WIDTH - 1),
                read_states,
            )[1][0]
        )

    row_of_south_corner_of_box = exponential_search(1, checker)

    col_of_sw_corner_of_box = find_leftmost_pulled_col(
        read_states, row_of_south_corner_of_box
    )
    # After visualizing the tractor beam, there are some skipped rows and columns
    nw_corner_of_box = (
        row_of_south_corner_of_box - (BOX_WIDTH - 1) - 4,
        col_of_sw_corner_of_box - 3,
    )
    return nw_corner_of_box[0] + 10_000 * nw_corner_of_box[1]


if __name__ == "__main__":
    with open("inputs/day19_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    assert 192 == part1(read_states)
    assert 8381082 == part2(read_states)
