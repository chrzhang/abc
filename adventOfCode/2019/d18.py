#!/usr/bin/env python3

import string
import heapq
from collections import deque

POSSIBLE_DOORS = set(string.ascii_uppercase)
POSSIBLE_KEYS = set(string.ascii_lowercase)
POSSIBLE_TRAVERSABLE = set(".@" + string.ascii_lowercase)


def find_start(board):
    for row in range(len(board)):
        for col in range(len(board[0])):
            if board[row][col] == "@":
                yield (row, col)


def get_total_number_of_keys(board):
    counter = 0
    for row in range(len(board)):
        for col in range(len(board[0])):
            if board[row][col] in POSSIBLE_KEYS:
                counter += 1
    return counter


def get_neighbors(row, col, board):
    return [
        n
        for n in [(row - 1, col), (row + 1, col), (row, col + 1), (row, col - 1)]
        if 0 <= n[0] < len(board) and 0 <= n[1] < len(board[0])
    ]


def is_traversable(row, col, keys_gotten, board):
    return (board[row][col] in POSSIBLE_TRAVERSABLE) or (
        board[row][col] in POSSIBLE_DOORS and board[row][col].lower() in keys_gotten
    )


def is_new_key(row, col, keys_gotten, board):
    return board[row][col] in POSSIBLE_KEYS and board[row][col] not in keys_gotten


def get_all_keys_reachable_from(row, col, keys_gotten, board):
    q = deque([(row, col, 0)])
    discovered = {(row, col)}
    while q:
        q_row, q_col, q_cost = q.popleft()
        for neighbor in get_neighbors(q_row, q_col, board):
            if neighbor not in discovered:
                discovered.add(neighbor)
                if is_new_key(neighbor[0], neighbor[1], keys_gotten, board):
                    yield (
                        board[neighbor[0]][neighbor[1]],
                        q_cost + 1,
                        neighbor[0],
                        neighbor[1],
                    )
                    continue
                if is_traversable(neighbor[0], neighbor[1], keys_gotten, board):
                    q.append((neighbor[0], neighbor[1], q_cost + 1))


def dijkstra_shortest_path_part1(filename):
    with open(filename, "r") as f:
        board = f.read().strip().split("\n")
    STEP_COUNT = 0
    KEYS_GOTTEN = frozenset()
    min_heap_of_states_by_step_count = [
        (STEP_COUNT, next(find_start(board)), KEYS_GOTTEN)
    ]
    visited = set()
    while min_heap_of_states_by_step_count:
        step_count, position, keys_gotten = heapq.heappop(
            min_heap_of_states_by_step_count
        )
        if len(keys_gotten) == get_total_number_of_keys(board):
            return step_count
        if (position, keys_gotten) in visited:
            continue
        visited.add((position, keys_gotten))

        for key, cost, row, col in get_all_keys_reachable_from(
            position[0], position[1], keys_gotten, board
        ):
            heapq.heappush(
                min_heap_of_states_by_step_count,
                (
                    step_count + cost,
                    (row, col),
                    keys_gotten | frozenset([key]),
                ),
            )


assert 4510 == dijkstra_shortest_path_part1("inputs/day18_input")


def dijkstra_shortest_path_part2(filename):
    with open(filename, "r") as f:
        board = f.read().strip().split("\n")
    STEP_COUNT = 0
    KEYS_GOTTEN = frozenset()
    starts = tuple(find_start(board))
    KEY_COUNT = get_total_number_of_keys(board)
    min_heap_of_states_by_step_count = [(STEP_COUNT, starts, KEYS_GOTTEN)]
    visited = set()
    while min_heap_of_states_by_step_count:
        step_count, positions, keys_gotten = heapq.heappop(
            min_heap_of_states_by_step_count
        )
        if len(keys_gotten) == KEY_COUNT:
            return step_count
        if (positions, keys_gotten) in visited:
            continue
        visited.add((positions, keys_gotten))

        for i, (row, col) in enumerate(positions):
            for key, cost, row, col in get_all_keys_reachable_from(
                row, col, keys_gotten, board
            ):
                new_positions = positions[0:i] + ((row, col),) + positions[i + 1 :]
                heapq.heappush(
                    min_heap_of_states_by_step_count,
                    (
                        step_count + cost,
                        new_positions,
                        keys_gotten | frozenset([key]),
                    ),
                )


assert 1816 == dijkstra_shortest_path_part2("inputs/day18_input_2")