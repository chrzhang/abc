#include <iostream>
#include <fstream>
#include <set>
#include <utility>

/*
From http://adventofcode.com/2015/day/3

--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and
then an elf at the North Pole calls him via radio and tells him where to move
next. Moves are always exactly one house to the north (^), south (v), east (>),
or west (<). After each move, he delivers another present to the house at his
new location.

However, the elf back at the north pole has had a little too much eggnog, and
so his directions are a little off, and Santa ends up visiting some houses more
than once. How many houses receive at least one present?

For example:

>c delivers presents to 2 houses: one at the starting location, and one to the
>east.
^>v< delivers presents to 4 houses in a square, including twice to the house at
his starting/ending location.  ^v^v^v^v^v delivers a bunch of presents to some
very lucky children at only 2 houses.

--- Part Two ---

The next year, to speed up the process, Santa creates a robot version of
himself, Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the
same starting house), then take turns moving based on instructions from the
elf, who is eggnoggedly reading from the same script as the previous year.

This year, how many houses receive at least one present?

For example:

^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa
goes south.  ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa
end up back where they started.  ^v^v^v^v^v now delivers presents to 11 houses,
with Santa going one direction and Robo-Santa going the other
*/

void part1(const std::string & filename) {
    std::ifstream f(filename);
    if (!f.is_open()) {
        std::cerr << "Unable to open " << filename << "\n";
        return;
    }
    int curr_y, curr_x;
    curr_y = curr_x = 0;
    std::set<std::pair<int, int>> visited_houses;
    visited_houses.insert({curr_x, curr_y});
    char c;
    while (f.get(c)) {
        switch (c) {
            case '^':
                ++curr_y;
                break;
            case '>':
                ++curr_x;
                break;
            case '<':
                --curr_x;
                break;
            case 'v':
                --curr_y;
                break;
            default:
                std::cerr << "Unrecognized character: " << c << std::endl;
                return;
        }
        visited_houses.insert({curr_x, curr_y});
    }
    std::cout << "Houses getting presents: " << visited_houses.size() << "\n";
}

void part2(const std::string & filename) {
    std::ifstream f(filename);
    if (!f.is_open()) {
        std::cerr << "Unable to open " << filename << "\n";
        return;
    }
    bool santa_mode = true;
    int santa_x, santa_y, bot_x, bot_y;
    santa_x = santa_y = bot_x = bot_y = 0;
    std::set<std::pair<int, int>> visited_houses;
    visited_houses.insert({santa_x, santa_y});
    char c;
    while (f.get(c)) {
        switch (c) {
            case '^':
                if (santa_mode) {
                    santa_y += 1;
                } else {
                    bot_y += 1;
                }
                break;
            case '>':
                if (santa_mode) {
                    santa_x += 1;
                } else {
                    bot_x += 1;
                }
                break;
            case 'v':
                if (santa_mode) {
                    santa_y -= 1;
                } else {
                    bot_y -= 1;
                }
                break;
            case '<':
                if (santa_mode) {
                    santa_x -= 1;
                } else {
                    bot_x -= 1;
                }
                break;
            default:
                std::cerr << "Unrecognized character: " << c << std::endl;
                break;
        }
        if (santa_mode) {
            visited_houses.insert({santa_x, santa_y});
        } else {
            visited_houses.insert({bot_x, bot_y});
        }
        santa_mode = santa_mode ? false : true;
    }
    std::cout << "Houses getting presents from Santa and RoboSanta: "
              << visited_houses.size() << "\n";
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    part1(argv[1]);
    part2(argv[1]);
    return 0;
}
