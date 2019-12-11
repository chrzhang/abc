#!/usr/bin/env python3

from more_itertools import chunked

WIDTH = 25
HEIGHT = 6


def count_digit_in_layer(layer, digit):
    return len([e for e in layer if e == digit])


def color_at_pixel(layer_idx, layers):
    for l in layers:
        if l[layer_idx] != "2":
            return l[layer_idx]
    raise RuntimeError


if __name__ == "__main__":
    with open("inputs/day8_input", "r") as f:
        (line,) = f.read().strip().split("\n")
    layers = tuple(chunked(line, WIDTH * HEIGHT))
    layer_with_least_zero_digits = None
    least_number_of_zero_digits = None
    for layer in layers:
        number_of_zero_digits = count_digit_in_layer(layer, "0")
        if (
            layer_with_least_zero_digits is None
            or number_of_zero_digits < least_number_of_zero_digits
        ):
            layer_with_least_zero_digits = layer
            least_number_of_zero_digits = number_of_zero_digits
    assert 1360 == (
        count_digit_in_layer(layer_with_least_zero_digits, "1")
        * count_digit_in_layer(layer_with_least_zero_digits, "2")
    )
    final_layer = []
    for layer_idx in range(WIDTH * HEIGHT):
        final_layer.append(color_at_pixel(layer_idx, layers))

    for row_idx in range(HEIGHT):
        row = ""
        for col_idx in range(WIDTH):
            row += final_layer[row_idx * WIDTH + col_idx]
        print("".join([" " if e == "0" else "*" for e in row]))
