#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <regex>
#include <cassert>

/*
From http://adventofcode.com/2016/day/12

--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted glass
ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt
some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building - it's a
collection of buildings in the nearby area. They're all connected by a local
monorail, and there's another building not far from here! Unfortunately, being
night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the boot
sequence expects a password. The password-checking logic (your puzzle input) is
easy to extract, but the code it uses is strange: it's assembunny code designed
for the new computer you just assembled. You'll have to execute the code and
    get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and
d) that start at 0 and can hold any integer. However, it seems to make use of
only a few instructions:

cpy x y copies x (either an integer or the value of a register) into register y.
inc x increases the value of register x by one.
dec x decreases the value of register x by one.
jnz x y jumps to an instruction y away (positive means forward; negative means
backward), but only if x is not zero.
The jnz instruction moves relative to itself: an offset of -1 would continue at
the previous instruction, while an offset of 2 would skip over the next
instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would set register a to 41, increase its value by 2, decrease
its value by 1, and then skip the last dec a (because a is not zero, so the jnz
a 2 skips it), leaving register a at 42. When you move past the last
instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left in
register a?

--- Part Two ---

As you head down the fire escape to the monorail, you notice it didn't start;
register c needs to be initialized to the position of the ignition key.

If you instead initialize register c to be 1, what value is now left in
register a?
*/

bool isValidRegister(const std::string & reg) {
    return (reg == "a") || (reg == "b") || (reg == "c") || (reg == "d");
}

int toInt(const std::string & s) {
    int result = 0;
    std::stringstream ss(s);
    if (!(ss >> result)) {
        std::cerr << "Could not convert " << s << " to an int.\n";
        abort();
    }
    return result;
}

int solve(const std::vector<std::string> & allLines, int registers[4]) {
    std::regex cpyRegex("^cpy ([0-9abcd]+) (a|b|c|d)$");
    std::regex jnzRegex("^jnz ([0-9abcd]+) ([-+]?\\d+)$");
    std::regex incRegex("^inc (a|b|c|d)$");
    std::regex decRegex("^dec (a|b|c|d)$");
    for (size_t ii = 0; ii < allLines.size();) {
        const std::string & currentLine = allLines[ii];
        std::smatch result;
        if (std::regex_search(currentLine, result, cpyRegex)) {
            const std::string & src = result[1];
            const std::string & dest = result[2];
            assert(isValidRegister(dest));
            if (isValidRegister(src)) {
                registers[dest[0] - 'a'] = registers[src[0] - 'a'];
            } else {
                registers[dest[0] - 'a'] = toInt(src);
            }
            ++ii;
        } else if (regex_search(currentLine, result, jnzRegex)) {
            const std::string & condition = result[1];
            const std::string & offset = result[2];
            assert(toInt(offset) != 0); // Would go on forever!
            if (isValidRegister(condition)) {
                if (registers[condition[0] - 'a'] == 0) {
                    ++ii;
                } else {
                    ii += toInt(offset);
                }
            } else {
                if (toInt(condition) == 0) {
                    ++ii;
                } else {
                    ii += toInt(offset);
                }
            }
        } else if (regex_search(currentLine, result, incRegex)) {
            const std::string & reg = result[1];
            assert(isValidRegister(reg));
            registers[reg[0] - 'a']++;
            ++ii;
        } else if (regex_search(currentLine, result, decRegex)) {
            const std::string & reg = result[1];
            assert(isValidRegister(reg));
            registers[reg[0] - 'a']--;
            ++ii;
        } else {
            std::cerr << "Line " << currentLine << " is not supported.\n";
            abort();
        }
    }
    return registers[0];
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    std::string currentLine;
    std::vector<std::string> allLines;
    while (std::getline(inputFile, currentLine)) {
        allLines.push_back(currentLine);
    }
    int registers[4] = { 0, 0, 0, 0 };
    const int part1Result = solve(allLines, registers);
    assert(part1Result == 318020);
    std::cout << "Part 1: " << part1Result << std::endl;
    registers['a' - 'a'] = registers['b' - 'a'] = registers['d' - 'a'] = 0;
    registers['c' - 'a'] = 1;
    const int part2Result = solve(allLines, registers);
    assert(part2Result == 9227674);
    std::cout << "Part 2: " << part2Result << std::endl;
}
