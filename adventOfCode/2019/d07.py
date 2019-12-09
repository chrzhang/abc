#!/usr/bin/python3

from collections import deque
from itertools import cycle, permutations

from d05 import parse_opcode_and_modes, enact_opcode, OpCode


def solve(input_vals, states):
    states = list(states)
    outputs = []
    i = 0
    curr_input_i = 0
    while True:
        opcode, modes = parse_opcode_and_modes(states[i])
        if opcode is OpCode.HALT:
            break
        if curr_input_i >= len(input_vals):
            curr_input = None
        else:
            curr_input = input_vals[curr_input_i]
        i = enact_opcode(opcode, modes, states, i, outputs, curr_input)
        if opcode is OpCode.INPUT:
            curr_input_i += 1
    return states, outputs


def amp_settings():
    return permutations([0, 1, 2, 3, 4])


def get_highest_thruster_signal(states):
    highest_thruster_signal = None
    for amp_setting in amp_settings():
        current_input = 0
        current_phase_setting = amp_setting[0]
        for amp_i in range(5):
            _, outputs = solve([current_phase_setting, current_input], states)
            (output,) = outputs
            current_input = output
            if amp_i < 4:
                current_phase_setting = amp_setting[amp_i + 1]
        if highest_thruster_signal is None or current_input > highest_thruster_signal:
            highest_thruster_signal = current_input
    return highest_thruster_signal


class AmplifierHaltError(Exception):
    pass


def prev_amp(curr_amp):
    return {"A": "E", "B": "A", "C": "B", "D": "C", "E": "D"}[curr_amp]


def next_amp(curr_amp):
    return {"A": "B", "B": "C", "C": "D", "D": "E", "E": "A"}[curr_amp]


def get_output(
    curr_amp, amp_to_state_map, amp_to_iptr_map, amp_to_unused_inputs, outputs_of_amp_e
):
    iptr = amp_to_iptr_map[curr_amp]
    states = amp_to_state_map[curr_amp]
    unused_amp_inputs = amp_to_unused_inputs[curr_amp]
    while True:
        opcode, modes = parse_opcode_and_modes(states[iptr])
        if opcode is OpCode.HALT:
            raise AmplifierHaltError
        if opcode is OpCode.INPUT:
            if unused_amp_inputs:
                curr_input = unused_amp_inputs.popleft()
            else:
                curr_input = get_output(
                    prev_amp(curr_amp),
                    amp_to_state_map,
                    amp_to_iptr_map,
                    amp_to_unused_inputs,
                    outputs_of_amp_e,
                )
            iptr = enact_opcode(opcode, modes, states, iptr, None, curr_input)
        elif opcode is OpCode.OUTPUT:
            outputs = []
            iptr = enact_opcode(opcode, modes, states, iptr, outputs, None)
            amp_to_iptr_map[curr_amp] = iptr
            (output,) = outputs
            if curr_amp == "E":
                outputs_of_amp_e.append(output)
            amp_to_unused_inputs[next_amp(curr_amp)].append(output)
            return output
        else:
            iptr = enact_opcode(opcode, modes, states, iptr, None, None)


def loop_amp_settings():
    return permutations([5, 6, 7, 8, 9])


def get_highest_loop_thruster_signal(states):
    highest_thruster_signal = None
    for amp_setting in loop_amp_settings():
        thruster_signal = get_loop_thruster_signal(states, amp_setting)
        if highest_thruster_signal is None or thruster_signal > highest_thruster_signal:
            highest_thruster_signal = thruster_signal
    return highest_thruster_signal


def get_loop_thruster_signal(states, amp_setting):
    amp_to_state_map = {amp: list(states) for amp in "ABCDE"}
    amp_to_iptr_map = {amp: 0 for amp in "ABCDE"}
    amp_to_unused_inputs = {
        "A": deque([amp_setting[0], 0]),
        "B": deque([amp_setting[1]]),
        "C": deque([amp_setting[2]]),
        "D": deque([amp_setting[3]]),
        "E": deque([amp_setting[4]]),
    }
    outputs_of_amp_e = []
    for amp in cycle("ABCDE"):
        try:
            output = get_output(
                amp,
                amp_to_state_map,
                amp_to_iptr_map,
                amp_to_unused_inputs,
                outputs_of_amp_e,
            )
        except AmplifierHaltError:
            return outputs_of_amp_e[-1]


if __name__ == "__main__":
    with open("inputs/day7_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    assert 34686 == get_highest_thruster_signal(read_states)
    assert 36384144 == get_highest_loop_thruster_signal(read_states)
