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
