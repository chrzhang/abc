#!/usr/bin/env python3

import functools
from collections import defaultdict
from copy import deepcopy
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
        states[pos_of_result] = next(input_getter)
    elif modes.first() == Mode.RELATIVE:
        pos_of_result = relative_base.val() + states[idx + 1]
        states[pos_of_result] = next(input_getter)


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


def enact_opcode(opcode, modes, states, idx, relative_base, input_getter):
    idx_target = None
    if opcode is OpCode.ADD:
        enact_add(modes, states, idx, relative_base)
    elif opcode is OpCode.MULTIPLY:
        enact_multiply(modes, states, idx, relative_base)
    elif opcode is OpCode.INPUT:
        enact_input(modes, states, idx, input_getter, relative_base)
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


class State:
    def __init__(self, tape, iptr, relbase, pos):
        self.tape = tape
        self.iptr = iptr
        self.relbase = relbase
        self.pos = pos


@unique
class Direction(Enum):
    NORTH = 1
    SOUTH = 2
    WEST = 3
    EAST = 4


@unique
class OutputType(Enum):
    WALL = 0
    EMPTY = 1
    OXYGEN = 2


def move_pos(pos, direction):
    pos_x, pos_y = pos
    if direction is Direction.NORTH:
        return (pos_x, pos_y - 1)
    if direction is Direction.SOUTH:
        return (pos_x, pos_y + 1)
    if direction is Direction.WEST:
        return (pos_x - 1, pos_y)
    if direction is Direction.EAST:
        return (pos_x + 1, pos_y)


def get_state_after_move(prior_state, direction):
    def input_getter():
        yield direction.value

    curr_state = deepcopy(prior_state)
    while True:
        opcode, modes = parse_opcode_and_modes(curr_state.tape[curr_state.iptr])
        if opcode is OpCode.HALT:
            break
        elif opcode is OpCode.OUTPUT:
            if modes.first() is Mode.POSITION:
                pos_of_result = curr_state.tape[curr_state.iptr + 1]
                output = curr_state.tape[pos_of_result]
            elif modes.first() is Mode.IMMEDIATE:
                output = curr_state.tape[curr_state.iptr + 1]
            elif modes.first() is Mode.RELATIVE:
                pos_of_result = (
                    curr_state.relbase.val() + curr_state.tape[curr_state.iptr + 1]
                )
                output = curr_state.tape[pos_of_result]
            else:
                raise UnknownModeError
            curr_state.iptr += number_of_values(OpCode.OUTPUT)
            curr_state.pos = move_pos(curr_state.pos, direction)
            return curr_state, output
        curr_state.iptr = enact_opcode(
            opcode,
            modes,
            curr_state.tape,
            curr_state.iptr,
            curr_state.relbase,
            input_getter(),
        )


def get_unvisited_neighbor_states(s, visited, maze):
    unvisited_neighbor_states = []
    for direction in Direction:
        state_after_move, output = get_state_after_move(s, direction)
        assert state_after_move is not s
        if output == OutputType.OXYGEN.value:
            maze.oxygen_location = state_after_move.pos
        elif output == OutputType.WALL.value:
            maze.walls.add(state_after_move.pos)
            continue
        if output in (OutputType.EMPTY.value, OutputType.OXYGEN.value):
            if state_after_move.pos not in visited:
                unvisited_neighbor_states.append(state_after_move)
        else:
            raise RuntimeError
    return unvisited_neighbor_states


def visualize(maze):
    min_x = min([wall[0] for wall in maze.walls])
    max_x = max([wall[0] for wall in maze.walls])
    min_y = min([wall[1] for wall in maze.walls])
    max_y = max([wall[1] for wall in maze.walls])
    for y in range(min_y, max_y + 1):
        row = []
        for x in range(min_x, max_x + 1):
            if (x, y) == (0, 0):
                row.append("S")
            elif (x, y) == maze.oxygen_location:
                row.append("X")
            elif (x, y) in maze.walls:
                row.append("#")
            else:
                row.append(" ")
        print(" ".join(row))


class Maze:
    def __init__(self):
        self.walls = set()
        self.oxygen_location = None
        self.oxygen_steps = None


def draw_maze(states):
    tape = defaultdict(int)
    for idx, e in enumerate(states):
        tape[idx] = e
    start_pos = (0, 0)
    state = State(tape, 0, RelativeBase(0), start_pos)
    visited = {start_pos}
    frontier = [state]
    steps = 1
    maze = Maze()
    steps_to_reach_oxygen = None
    while frontier:
        next_frontier = []
        for s in frontier:
            for unvisited_neighbor_state in get_unvisited_neighbor_states(
                s, visited, maze
            ):
                visited.add(unvisited_neighbor_state.pos)
                next_frontier.append(unvisited_neighbor_state)
                if maze.oxygen_location and maze.oxygen_steps is None:
                    maze.oxygen_steps = steps
        frontier = next_frontier
        steps += 1
    return maze


def number_of_levels_in_bfs_from_oxygen(maze):
    frontier = [maze.oxygen_location]
    visited = set([maze.oxygen_location])
    levels = 0
    while frontier:
        next_frontier = []
        for loc in frontier:
            adjacent = [move_pos(loc, direction) for direction in Direction]
            neighbors = [n for n in adjacent if n not in maze.walls]
            unvisited_neighbors = [n for n in neighbors if n not in visited]
            for un in unvisited_neighbors:
                visited.add(un)
                next_frontier.append(un)
        frontier = next_frontier
        levels += 1
    return levels - 1


if __name__ == "__main__":
    with open("inputs/day15_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    read_states = tuple([int(x) for x in line.split(",")])
    maze = draw_maze(read_states)
    visualize(maze)
    assert 380 == maze.oxygen_steps
    assert 410 == number_of_levels_in_bfs_from_oxygen(maze)
