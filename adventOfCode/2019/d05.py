#!/usr/bin/python3

import functools
from enum import Enum, unique

with open("inputs/day5_input", "r") as f:
    (line,) = f.read().strip().split("\n")

read_states = tuple([int(x) for x in line.split(",")])


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
    if opcode in (OpCode.INPUT, OpCode.OUTPUT):
        return 2
    if opcode in (OpCode.JUMP_IF_FALSE, OpCode.JUMP_IF_TRUE):
        return 3
    raise UnknownOpCodeError


@unique
class Mode(Enum):
    POSITION = "0"
    IMMEDIATE = "1"


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


def read_param(mode, param, states):
    if mode == Mode.IMMEDIATE:
        return param
    if mode == Mode.POSITION:
        return states[param]


def get_params(modes, states, idx):
    param_1 = read_param(modes.first(), states[idx + 1], states)
    param_2 = read_param(modes.second(), states[idx + 2], states)
    if modes.third() != Mode.POSITION:
        raise WritingToImmediateError
    pos_of_result = states[idx + 3]
    return param_1, param_2, pos_of_result


def enact_add(modes, states, idx):
    param_1, param_2, pos_of_result = get_params(modes, states, idx)
    states[pos_of_result] = param_1 + param_2


def enact_multiply(modes, states, idx):
    param_1, param_2, pos_of_result = get_params(modes, states, idx)
    states[pos_of_result] = param_1 * param_2


def enact_input(modes, states, idx, input_value):
    if modes.first() != Mode.POSITION:
        raise WritingToImmediateError
    pos_of_result = states[idx + 1]
    states[pos_of_result] = input_value


def enact_output(modes, states, idx, outputs):
    if modes.first() is Mode.POSITION:
        pos_of_result = states[idx + 1]
        outputs.append(states[pos_of_result])
    elif modes.first() is Mode.IMMEDIATE:
        outputs.append(states[idx + 1])
    else:
        raise UnknownModeError


def enact_jump_if_condition(condition, modes, states, idx):
    param_1 = read_param(modes.first(), states[idx + 1], states)
    if condition(param_1):
        return read_param(modes.second(), states[idx + 2], states)
    return None


enact_jump_if_true = functools.partial(enact_jump_if_condition, lambda p: p)
enact_jump_if_false = functools.partial(enact_jump_if_condition, lambda p: not p)


def enact_less_than(modes, states, idx):
    param_1, param_2, pos_of_result = get_params(modes, states, idx)
    states[pos_of_result] = 1 if param_1 < param_2 else 0


def enact_equals(modes, states, idx):
    param_1, param_2, pos_of_result = get_params(modes, states, idx)
    states[pos_of_result] = 1 if param_1 == param_2 else 0


def enact_opcode(opcode, modes, states, idx, outputs, input_value):
    idx_target = None
    if opcode is OpCode.ADD:
        enact_add(modes, states, idx)
    elif opcode is OpCode.MULTIPLY:
        enact_multiply(modes, states, idx)
    elif opcode is OpCode.INPUT:
        enact_input(modes, states, idx, input_value)
    elif opcode is OpCode.OUTPUT:
        enact_output(modes, states, idx, outputs)
    elif opcode is OpCode.JUMP_IF_TRUE:
        idx_target = enact_jump_if_true(modes, states, idx)
    elif opcode is OpCode.JUMP_IF_FALSE:
        idx_target = enact_jump_if_false(modes, states, idx)
    elif opcode is OpCode.LESS_THAN:
        enact_less_than(modes, states, idx)
    elif opcode is OpCode.EQUALS:
        enact_equals(modes, states, idx)
    else:
        raise UnknownOpCodeError
    if idx_target is None:
        return idx + number_of_values(opcode)
    else:
        return idx_target


def solve(input_val, states):
    states = list(states)
    outputs = []
    i = 0
    input_used = False
    while True:
        opcode, modes = parse_opcode_and_modes(states[i])
        if opcode is OpCode.HALT:
            break
        if opcode is OpCode.INPUT:
            if input_used:
                raise InputMoreThanOnceError
            input_used = True
        i = enact_opcode(opcode, modes, states, i, outputs, input_val)
    return states, outputs


solve_part_1 = functools.partial(solve, 1)


assert [1002, 4, 3, 4, 99], [] == solve_part_1([1002, 4, 3, 4, 33])
assert [3, 0, 4, 0, 99], [1] == solve_part_1([3, 0, 4, 0, 99])
assert [1101, 100, -1, 4, 99], [] == solve_part_1([1101, 100, -1, 4, 0])

_, outputs = solve_part_1(read_states)
assert outputs[-1] == 6761139


solve_part_2 = functools.partial(solve, 5)
_, outputs = solve_part_2(read_states)
assert outputs == [9217546]
