#include <iostream>
#include <fstream>
#include <sstream>

/*
From http://adventofcode.com/2015/day/1

--- Day 1: Not Quite Lisp ---

Santa was hoping for a white Christmas, but his weather machine's "snow"
function is powered by stars, and he's fresh out! To save Christmas, he needs
you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made available
on each day in the advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't
find the right floor - the directions he got are a little confusing. He starts
on the ground floor (floor 0) and then follows the instructions one character at
a time.

An opening parenthesis, (, means he should go up one floor, and a closing
parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will
never find the top or bottom floors.

For example:

(()) and ()() both result in floor 0.  ((( and (()(()( both result in floor
3.))((((( also results in floor 3.  (include <sstream>)) and ))( both result in
floor -1 (the first basement level).))) and )())()) both result in floor -3.  To
what floor do the instructions take Santa?

--- Part Two ---

Now, given the same instructions, find the position of the first character that
causes him to enter the basement (floor -1). The first character in the
instructions has position 1, the second character has position 2, and so on.

For example:

) causes him to enter the basement at character position 1.  ()()) causes him to
enter the basement at character position 5.  What is the position of the
character that causes Santa to first enter the basement?
*/

int main(int argc, char * argv[]) {
    if (argc == 1) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 0;
    }
    std::ifstream f(argv[1]);
    if (f.is_open()) {
        char c;
        int curr_floor = 0;
        size_t pos_enter_basement = 0;
        size_t curr_pos = 1;
        while (f.get(c)) {
            switch (c) {
                case ')':
                    curr_floor -= 1;
                    // First character causing him to enter the basement
                    if (pos_enter_basement == 0 &&
                        curr_floor == -1) {
                        pos_enter_basement = curr_pos;
                    }
                    break;
                case '(':
                    curr_floor += 1;
                    break;
                default:
                    break;
            }
            ++curr_pos;
        }
        std::cout << "Final floor: " << curr_floor << std::endl;
        std::cout << "Position of 1st char to enter basement: "
                  << pos_enter_basement << std::endl;
    }
    return 0;
}
