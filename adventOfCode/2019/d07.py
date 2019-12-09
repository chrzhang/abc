#!/usr/bin/python3

from itertools import permutations

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
            output, = outputs
            current_input = output
            if amp_i < 4:
                current_phase_setting = amp_setting[amp_i + 1]
        if highest_thruster_signal is None or current_input > highest_thruster_signal:
            highest_thruster_signal = current_input
    return highest_thruster_signal


if __name__ == "__main__":
    with open("inputs/day7_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    assert 34686 == get_highest_thruster_signal(read_states)
