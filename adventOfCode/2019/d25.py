#!/usr/bin/env python3

import functools
from collections import defaultdict, deque
from enum import Enum, unique
from itertools import chain, combinations


def powerset(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


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


@functools.lru_cache
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


def enact_input(modes, states, idx, input_value, relative_base):
    if modes.first() == Mode.IMMEDIATE:
        raise WritingToImmediateError
    if modes.first() == Mode.POSITION:
        pos_of_result = states[idx + 1]
        states[pos_of_result] = input_value
    elif modes.first() == Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        states[pos_of_result] = input_value


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


def enact_opcode(opcode, modes, states, idx, relative_base, outputs, input_value):
    idx_target = None
    if opcode is OpCode.ADD:
        enact_add(modes, states, idx, relative_base)
    elif opcode is OpCode.MULTIPLY:
        enact_multiply(modes, states, idx, relative_base)
    elif opcode is OpCode.INPUT:
        enact_input(modes, states, idx, input_value, relative_base)
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


def outputs_as_str(ascii_codes):
    return "".join([chr(a) for a in ascii_codes])


class DroidHaltedError(Exception):
    pass


class Droid:
    def __init__(self, read_states):
        self.states = defaultdict(int)
        for idx, val in enumerate(read_states):
            self.states[idx] = val
        self.relative_base = RelativeBase(0)
        self.i = 0

    def crunch(self, input_str):
        inputs = deque()
        for c in input_str:
            inputs.append(ord(c))
        if inputs:
            inputs.append(ord("\n"))
        outputs = []
        while True:
            opcode, modes = parse_opcode_and_modes(self.states[self.i])
            if opcode is OpCode.HALT:
                raise DroidHaltedError(f"Droid halted after: {outputs_as_str(outputs)}")
            input_val = None
            if opcode is OpCode.INPUT:
                if not inputs:
                    return outputs_as_str(outputs)
                input_val = inputs.popleft()
            self.i = enact_opcode(
                opcode,
                modes,
                self.states,
                self.i,
                self.relative_base,
                outputs,
                input_val,
            )


def go_and_fetch_all_items():
    moves = [
        "west",
        "take mug",
        "north",
        "take easter egg",
        "south",
        "east",
        "south",
        "east",
        "north",
        "take candy cane",
        "south",
        "west",
        "north",
        "east",
        "take coin",
        "north",
        "north",
        "take hypercube",
        "south",
        "east",
        "take manifold",
        "west",
        "south",
        "south",
        "east",
        "take pointer",
        "west",
        "west",
        "take astrolabe",
        "north",
        "east",
        "north",
        "inv",
    ]
    for m in moves:
        yield m


def drop_all_items():
    moves = [
        "drop hypercube",
        "drop manifold",
        "drop astrolabe",
        "drop easter egg",
        "drop mug",
        "drop coin",
        "drop pointer",
        "drop candy cane",
    ]
    for m in moves:
        yield m


def powerset_of_items():
    return powerset(
        [
            "hypercube",
            "manifold",
            "astrolabe",
            "easter egg",
            "mug",
            "coin",
            "pointer",
            "candy cane",
        ]
    )


if __name__ == "__main__":
    with open("inputs/day25_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    d = Droid(read_states)
    user_input = ""
    get_all_items_and_go_to_checkpoint = go_and_fetch_all_items()
    while True:
        print(d.crunch(user_input))
        try:
            user_input = next(get_all_items_and_go_to_checkpoint)
        except StopIteration:
            break

    drop_items = drop_all_items()
    user_input = ""
    while True:
        print(d.crunch(user_input))
        try:
            user_input = next(drop_items)
        except StopIteration:
            break

    for subset in powerset_of_items():

        def pick_up_subset():
            for m in [f"take {item}" for item in subset]:
                yield m

        def put_down_subset():
            for m in [f"drop {item}" for item in subset]:
                yield m

        pick_up = pick_up_subset()
        user_input = ""
        while True:
            print(d.crunch(user_input))
            try:
                user_input = next(pick_up)
            except StopIteration:
                break

        d.crunch("east")

        drop_stuff = put_down_subset()
        user_input = ""
        while True:
            print(d.crunch(user_input))
            try:
                user_input = next(drop_stuff)
            except StopIteration:
                break