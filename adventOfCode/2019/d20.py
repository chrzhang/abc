#!/usr/bin/env python3

import math
import heapq
from collections import deque, defaultdict


with open("inputs/day20_input", "r") as f:
    board = f.read().split("\n")

HEIGHT = len(board)
WIDTH = len(board[0])


def get_entrance(letter_a, letter_b):
    for letter in (letter_a, letter_b):
        for d_row, d_col in [(-1, 0), (1, 0), (0, 1), (0, -1)]:
            neighbor = (letter[0] + d_row, letter[1] + d_col)
            if (
                0 <= neighbor[0] < HEIGHT
                and 0 <= neighbor[1] < WIDTH
                and board[neighbor[0]][neighbor[1]] == "."
            ):
                return neighbor


def get_portals():
    portals_to_positions = defaultdict(list)
    for row in range(HEIGHT):
        for col in range(WIDTH):
            if not board[row][col].isupper():
                continue
            has_letter_on_top = row > 0 and board[row - 1][col].isupper()
            has_letter_on_left = col > 0 and board[row][col - 1].isupper()
            if has_letter_on_top or has_letter_on_left:
                continue
            has_letter_on_right = col < WIDTH - 1 and board[row][col + 1].isupper()
            has_letter_on_bottom = row < HEIGHT - 1 and board[row + 1][col].isupper()
            assert has_letter_on_right != has_letter_on_bottom  # XOR
            if has_letter_on_right:
                portal_name = board[row][col] + board[row][col + 1]
                entrance = get_entrance((row, col), (row, col + 1))
            if has_letter_on_bottom:
                portal_name = board[row][col] + board[row + 1][col]
                entrance = get_entrance((row, col), (row + 1, col))
            portals_to_positions[portal_name].append(entrance)
    positions_to_portals = {}
    for portal, positions in portals_to_positions.items():
        for position in positions:
            positions_to_portals[position] = portal
    return portals_to_positions, positions_to_portals


def get_neighbors(pos):
    for d_row, d_col in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
        new_pos = (pos[0] + d_row, pos[1] + d_col)
        if 0 <= new_pos[0] < HEIGHT and 0 <= new_pos[1] < WIDTH:
            yield new_pos


def get_reachable_portals_from(position, portals_to_positions, positions_to_portals):
    q = deque([(position, 0)])
    visited = set()
    while q:
        e_pos, e_dist = q.popleft()
        if e_pos in visited:
            continue
        visited.add(e_pos)
        if e_dist and e_pos in positions_to_portals:
            portal = positions_to_portals[e_pos]
            yield ((portal, e_pos), e_dist)
        for n in get_neighbors(e_pos):
            if n in visited:
                continue

            if board[n[0]][n[1]] == ".":
                q.append((n, e_dist + 1))


def build_adj_list(portals_to_positions, positions_to_portals):
    adj_list = {}
    for portal, positions in portals_to_positions.items():
        for position in positions:
            adj_list[(portal, position)] = dict(
                get_reachable_portals_from(
                    position, portals_to_positions, positions_to_portals
                )
            )
    for portal, positions in portals_to_positions.items():
        if len(positions) == 2:
            adj_list[(portal, positions[0])][(portal, positions[1])] = 1
            adj_list[(portal, positions[1])][(portal, positions[0])] = 1

    return adj_list


def dijkstra_shortest_path():
    portals_to_positions, positions_to_portals = get_portals()
    adj_list = build_adj_list(portals_to_positions, positions_to_portals)

    START = ("AA", portals_to_positions["AA"][0])
    DEST = ("ZZ", portals_to_positions["ZZ"][0])

    dist = {}
    for portal in adj_list:
        dist[portal] = 0 if portal == START else math.inf
    min_heap = [(0, START)]
    visited = set()
    while min_heap:
        curr_dist, curr_portal = heapq.heappop(min_heap)
        if curr_portal in visited:
            continue
        visited.add(curr_portal)
        for neighbor, cost in adj_list[curr_portal].items():
            if neighbor in visited:
                continue
            alt = curr_dist + cost
            if alt < dist[neighbor]:
                dist[neighbor] = alt
                heapq.heappush(min_heap, (alt, neighbor))
    return dist[DEST]


assert 642 == dijkstra_shortest_path()
