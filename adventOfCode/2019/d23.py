#!/usr/bin/env python3

import functools
from collections import defaultdict, deque
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


@unique
class ComputerMode(Enum):
    NORMAL = 0
    HALTED = 1
    WAITING_FOR_INPUT = 2
    SENDING_OUTPUT = 4


class IntCodeComputer:
    def __init__(self, read_states):
        self.states = defaultdict(int)
        for idx, val in enumerate(read_states):
            self.states[idx] = val
        self.relative_base = RelativeBase(0)
        self.last_action = ComputerMode.NORMAL
        self.outputs = []
        self.input_value = None
        self.i = 0

    def halted(self):
        return self.last_action is ComputerMode.HALTED

    def needs_input(self):
        return self.last_action is ComputerMode.WAITING_FOR_INPUT

    def sent_output(self):
        return self.last_action is ComputerMode.SENDING_OUTPUT

    def set_next_input(self, input_val):
        self.input_value = input_val

    def crunch(self):
        # Runs the computer until IO or halt
        if self.last_action is ComputerMode.HALTED:
            raise RuntimeError("Computer already halted.")
        while True:
            opcode, modes = parse_opcode_and_modes(self.states[self.i])
            if opcode is OpCode.HALT:
                self.last_action = ComputerMode.HALTED
                return
            input_val = None
            if opcode is OpCode.INPUT:
                if self.input_value is None:
                    self.last_action = ComputerMode.WAITING_FOR_INPUT
                    return
                else:
                    input_val = self.input_value
                    self.input_value = None
                    self.last_action = ComputerMode.NORMAL
            self.i = enact_opcode(
                opcode,
                modes,
                self.states,
                self.i,
                self.relative_base,
                self.outputs,
                input_val,
            )
            if opcode is OpCode.OUTPUT:
                self.last_action = ComputerMode.SENDING_OUTPUT
                return


def part_1(read_states):
    COMPUTER_COUNT = 50
    computers = [IntCodeComputer(read_states) for _ in range(COMPUTER_COUNT)]
    message_queues = [deque([i]) for i in range(COMPUTER_COUNT)]
    computer_outputs = [[] for _ in range(COMPUTER_COUNT)]
    curr_computer_i = 0
    while True:
        computer = computers[curr_computer_i]
        assert not computer.halted()
        queue = message_queues[curr_computer_i]
        output = computer_outputs[curr_computer_i]
        computer.crunch()

        if computer.needs_input():
            input_value = -1
            if queue:
                input_value = queue.popleft()
            computer.set_next_input(input_value)
        elif computer.sent_output():
            output_val = computer.outputs[-1]
            output.append(output_val)
            if len(output) == 3:
                target, x, y = output
                if target == 255:
                    return y
                message_queues[target].append(x)
                message_queues[target].append(y)
                output[:] = []
        curr_computer_i = (curr_computer_i + 1) % COMPUTER_COUNT


def part_2(read_states):
    COMPUTER_COUNT = 50
    computers = [IntCodeComputer(read_states) for _ in range(COMPUTER_COUNT)]
    message_queues = [deque([i]) for i in range(COMPUTER_COUNT)]
    computer_outputs = [[] for _ in range(COMPUTER_COUNT)]
    last_nat_packet_x = last_nat_packet_y = None
    last_delivered_nat_packet_y = None
    first_delivered_nat_packet_y = None
    curr_computer_i = 0
    while True:
        computer = computers[curr_computer_i]
        assert not computer.halted()
        queue = message_queues[curr_computer_i]
        output = computer_outputs[curr_computer_i]
        computer.crunch()

        if computer.needs_input():
            input_value = -1
            if queue:
                input_value = queue.popleft()
            computer.set_next_input(input_value)

        elif computer.sent_output():
            output_val = computer.outputs[-1]
            output.append(output_val)
            if len(output) == 3:
                target, x, y = output
                if target == 255:
                    last_nat_packet_x = x
                    last_nat_packet_y = y
                else:
                    message_queues[target].append(x)
                    message_queues[target].append(y)
                output[:] = []

        if curr_computer_i == COMPUTER_COUNT - 1:
            # Run NAT
            if not any(message_queues) and all([c.needs_input() for c in computers]):
                message_queues[0].append(last_nat_packet_x)
                message_queues[0].append(last_nat_packet_y)

                if (
                    not (
                        last_delivered_nat_packet_y is None
                        or last_delivered_nat_packet_y == first_delivered_nat_packet_y
                    )
                    and last_nat_packet_y == last_delivered_nat_packet_y
                ):
                    return last_delivered_nat_packet_y

                if last_delivered_nat_packet_y is None:
                    first_delivered_nat_packet_y = last_nat_packet_y

                last_delivered_nat_packet_y = last_nat_packet_y
        curr_computer_i = (curr_computer_i + 1) % COMPUTER_COUNT


if __name__ == "__main__":
    with open("inputs/day23_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    assert 18604 == part_1(read_states)
    assert 11880 == part_2(read_states)
