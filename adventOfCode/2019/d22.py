#!/usr/bin/env python3

import re
from collections import deque


class Deck:
    def __init__(self, card_count):
        self.__cards = deque(list(range(card_count)))

    def deal_into_new_stack(self):
        self.__cards = deque(list(self.__cards)[::-1])

    def cut_cards(self, amount):
        self.__cards.rotate(-1 * amount)

    def deal_with_increment(self, amount):
        result = [None] * len(self.__cards)
        curr_result_index = 0
        while self.__cards:
            top_card = self.__cards.popleft()
            result[curr_result_index] = top_card
            curr_result_index = (curr_result_index + amount) % len(result)
        self.__cards = deque(result)

    def cards(self):
        return tuple(self.__cards)

    def __str__(self):
        return f"Deck contains {len(self.__cards)} cards: {', '.join([str(e) for e in self.__cards])}"


def apply_instruction_to_deck(deck, line):
    deal_into_new_stack_match = re.match(r"^deal into new stack$", line)
    if deal_into_new_stack_match:
        deck.deal_into_new_stack()
        return
    cut_n_cards_match = re.match(r"^cut (?P<amount>.*)$", line)
    if cut_n_cards_match:
        deck.cut_cards(int(cut_n_cards_match.group("amount")))
        return
    deal_with_increment_match = re.match(r"^deal with increment (?P<amount>\d+)$", line)
    if deal_with_increment_match:
        deck.deal_with_increment(int(deal_with_increment_match.group("amount")))
        return
    raise RuntimeError(f"Could not parse instruction from '{line}'.")


def part1():
    with open("inputs/day22_input", "r") as f:
        read_lines = f.read().strip().split("\n")

    d = Deck(10_007)
    for line in read_lines:
        apply_instruction_to_deck(d, line)

    return d.cards().index(2019)


assert 1498 == part1()


def position_before_deal_into_new_stack(position, deck_size):
    return deck_size - 1 - position


def position_before_cut_cards(position, cut_amount, deck_size):
    return (position + cut_amount + deck_size) % deck_size


def modular_inverse(a, b):
    return pow(a, -1, b)


def postion_before_deal_with_increment(position, amount, deck_size):
    return (modular_inverse(amount, deck_size) * position) % deck_size


def part2():
    """
    Each shuffle operation can be reversed and the reversal is linear.
    The function reverse_shuffle performs one complete reversal of our entire shuffle input.
    Because reverse_shuffle is made of linear transforms, the reverse_shuffle function
    is itself also linear, meaning it can be expressed as:

    reverse_shuffle(i) = Ai + B

    where A and B are unknowns and i means the input index of a card.

    Let our input be X = 2020.

    Let Y = reverse_shuffle(X) and Z = reverse_shuffle(Y).

    Y = AX + B
    Z = AY + B

    which, if we subtract the second equation from the first, gives:

    A = (Y - Z) / (X - Y)
    B = Y - AX

    The solution for the problem requires applying reverse_shuffle repeatedly.

    This reveals a pattern. Here is the reverse_shuffle happening 3 times, as an example.

    A(A(AX + B) + B) + B

    = A^3 * X + A^2 * B + AB + B

    where, if we use the formula for summing a geometric series, we get

    = A^n * X + B * (A^(n-1)) / (A - 1)

    Solving for this is computationally feasible, and if we use pow(), we also minimize
    the number of multiply operations.

    """
    with open("inputs/day22_input", "r") as f:
        reversed_lines = f.read().strip().split("\n")[::-1]

    def reverse_shuffle(pos, deck_size):
        for line in reversed_lines:
            deal_into_new_stack_match = re.match(r"^deal into new stack$", line)
            cut_n_cards_match = re.match(r"^cut (?P<amount>.*)$", line)
            deal_with_increment_match = re.match(
                r"^deal with increment (?P<amount>\d+)$", line
            )
            if deal_into_new_stack_match:
                pos = position_before_deal_into_new_stack(pos, deck_size)
            elif cut_n_cards_match:
                pos = position_before_cut_cards(
                    pos, int(cut_n_cards_match.group("amount")), deck_size
                )
            elif deal_with_increment_match:
                pos = postion_before_deal_with_increment(
                    pos, int(deal_with_increment_match.group("amount")), deck_size
                )
            else:
                raise RuntimeError(f"Could not parse instruction from '{line}'.")
        return pos

    DECK_SIZE = 119315717514047
    X = 2020
    Y = reverse_shuffle(X, DECK_SIZE)
    Z = reverse_shuffle(Y, DECK_SIZE)
    A = ((Y - Z) * modular_inverse(X - Y, DECK_SIZE)) % DECK_SIZE
    B = (Y - (A * X)) % DECK_SIZE
    SHUFFLE_COUNT = 101741582076661
    return (
        pow(A, SHUFFLE_COUNT, DECK_SIZE) * X
        + (pow(A, SHUFFLE_COUNT, DECK_SIZE) - 1) * modular_inverse(A - 1, DECK_SIZE) * B
    ) % DECK_SIZE


assert 74662303452927 == part2()
