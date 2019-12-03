#!/usr/bin/python3

with open('inputs/day2_input', 'r') as f:
    line, = f.read().strip().split('\n')

read_states = tuple([int(x) for x in line.split(',')])

def solve(states, noun, verb):
    states = list(states)
    states[1] = noun
    states[2] = verb
    i = 0
    while True:
        opcode = states[i]
        if opcode == 99:
            break
        pos_of_operand1 = states[i + 1]
        pos_of_operand2 = states[i + 2]
        pos_of_output = states[i + 3]
        if opcode == 1:
            states[pos_of_output] = states[pos_of_operand1] + states[pos_of_operand2]
        elif opcode == 2:
            states[pos_of_output] = states[pos_of_operand1] * states[pos_of_operand2]
        else:
            raise RuntimeError(f'Unsupported opcode {states[i]}')
        i += 4
    return states[0]

assert 4090689 == solve(read_states, 12, 2)

from itertools import product
for possible_noun, possible_verb in product(range(100), range(100)):
    try:
        result = solve(read_states, possible_noun, possible_verb)
        if result == 19690720:
            assert 7733 == 100 * possible_noun + possible_verb
            break
    except RuntimeError:
        pass
