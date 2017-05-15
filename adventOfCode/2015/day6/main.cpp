#include <iostream>
#include <fstream>
#include <string>
#include <regex> // Needs g++-4.9
#include <array>

/*
From http://adventofcode.com/2015/day/6

--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating
contest year after year, you've decided to deploy one million lights in a
1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed
you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
whether to turn on, turn off, or toggle various inclusive ranges given as
coordinate pairs. Each coordinate pair represents opposite corners of a
rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers
to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by
doing the instructions Santa sent you in order.

For example:

turn on 0,0 through 999,999 would turn on (or leave on) every light.  toggle
0,0 through 999,0 would toggle the first line of 1000 lights, turning off the
ones that were on, and turning on the ones that were off.  turn off 499,499
through 500,500 would turn off (or leave off) the middle four lights.  After
following the instructions, how many lights are lit?

--- Part Two ---

You just finish implementing your winning light pattern when you realize you
mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each
light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of
those lights by 1.

The phrase turn off actually means that you should decrease the brightness of
those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of
those lights by 2.

What is the total brightness of all lights combined after following Santa's
instructions?

For example:

turn on 0,0 through 0,0 would increase the total brightness by 1.  toggle 0,0
through 999,999 would increase the total brightness by 2000000.
*/

static const size_t HEIGHT = 1000;
static const size_t WIDTH = 1000;

void init(bool lights_on[HEIGHT][WIDTH],
          size_t brightness[HEIGHT][WIDTH]) {
    for (size_t i = 0; i < HEIGHT; ++i) {
        for (size_t j = 0; j < WIDTH; ++j) {
            lights_on[i][j] = false;
            brightness[i][j] = 0;
        }
    }
}

size_t num_on(bool lights_on[HEIGHT][WIDTH]) {
    size_t acc = 0;
    for (size_t i = 0; i < HEIGHT; ++i) {
        for (size_t j = 0; j < WIDTH; ++j) {
            acc += lights_on[i][j] ? 1 : 0;
        }
    }
    return acc;
}

size_t total_brightness(size_t brightness[HEIGHT][WIDTH]) {
    size_t acc = 0;
    for (size_t i = 0; i < HEIGHT; ++i) {
        for (size_t j = 0; j < WIDTH; ++j) {
            acc += brightness[i][j];
        }
    }
    return acc;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << "File not opened.\n";
        return 1;
    }
    static bool lights_on[1000][1000];
    static size_t brightness[1000][1000];
    init(lights_on, brightness);
    std::string line;
    while (std::getline(f, line)) {
        std::regex rgx("(turn off|turn on|toggle) (.*),(.*) through (.*),(.*)");
        std::smatch result;
        std::regex_search(line, result, rgx);
        unsigned long x1, x2, y1, y2;
        x1 = std::stoul(result.str(2), nullptr, 0);
        x2 = std::stoul(result.str(4), nullptr, 0);
        y1 = std::stoul(result.str(3), nullptr, 0);
        y2 = std::stoul(result.str(5), nullptr, 0);
        if (x1 > x2) {
            std::swap(x1, x2);
        }
        if (y1 > y2) {
            std::swap(y1, y2);
        }
        for (auto i = x1; i <= x2; ++i) {
            for (auto j = y1; j <= y2; ++j) {
                if ("turn off" == result.str(1)) {
                    lights_on[i][j] = false;
                    brightness[i][j] =
                        brightness[i][j] > 0 ? brightness[i][j] - 1 : 0;
                } else if ("turn on" == result.str(1)) {
                    lights_on[i][j] = true;
                    brightness[i][j] += 1;
                } else if ("toggle" == result.str(1)) {
                    lights_on[i][j] = lights_on[i][j] ? false : true;
                    brightness[i][j] += 2;
                } else {
                    std::cerr << "Unclear action.\n";
                    return 1;
                }
            }
        }
    }
    std::cout << num_on(lights_on) << " lights are on.\n";
    std::cout << total_brightness(brightness) << " is the total brightness.\n";
    return 0;
}
