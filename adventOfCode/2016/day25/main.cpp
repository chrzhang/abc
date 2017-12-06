#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <regex>
#include <cassert>

/*
From http://adventofcode.com/2016/day/25

--- Day 25: Clock Signal ---

You open the door and find yourself on the roof. The city sprawls away from you
for miles and miles.

There's not much time now - it's already Christmas, but you're nowhere near the
North Pole, much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution. After all, the
sleigh doesn't need the stars, exactly; it needs the timing data they provide,
and you happen to have a massive signal generator right here.

You connect the stars you have to your prototype computer, connect that to the
antenna, and begin the transmission.

Nothing happens.

You call the service number printed on the side of the antenna and quickly
explain the situation. "I'm not sure what kind of equipment you have connected
over there," he says, "but you need a clock signal." You try to explain that
this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer knows how
to read the data you're sending it. An endless, alternating pattern of 0, 1, 0,
1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency you would
need to use for the data from the stars. "There's no way it can! The only
antenna we've installed capable of that is on top of a top-secret Easter Bunny
installation, and you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code (your
puzzle input); it looks mostly compatible with code you worked on just
recently.

This antenna code, being a signal generator, uses one extra instruction:

out x transmits x (either an integer or the value of a register) as the next
value for the clock signal.  The code takes a value (via register a) that
describes the signal to generate, but you're not sure how it's used. You'll
have to find the input to produce the right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a
and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?
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
    std::regex outRegex("^out ([0-9abcd]+)$");
    std::vector<int> transmission;
    // Make sure the first THRESHOLD transmissions follow the pattern. This is
    // a fast heuristic that reached the desired result. For correctness,
    // here is a better algorithm:
    // - Store the state of the registers upon each transmission as a node.
    // - A state goes to another as the program executes, creating a list
    // - Treat the progression as a singly linked list
    // - Apply the 'loop detection' algorithm for linked list by having one
    //   pointer advance 1 node at a time, and another 2 nodes at a time
    // - Make sure the 0 1 0 1 ... pattern is transmitted as we advance
    // - If they meet (i.e. achieve the same state), a loop has been detected
    // - Having a loop guarantees the pattern will repeat forever
    const size_t THRESHOLD = 10;
    int expectedValue = 0;
    for (size_t ii = 0; ii < allLines.size() &&
                        transmission.size() < THRESHOLD;) {
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
        } else if (regex_search(currentLine, result, outRegex)) {
            const std::string & src = result[1];
            const int valueTransmitted =
                isValidRegister(src) ? registers[src[0] - 'a'] : toInt(src);
            if (valueTransmitted != expectedValue) {
                return -1;
            } else {
                transmission.push_back(expectedValue);
                expectedValue = expectedValue == 1 ? 0 : 1;
            }
            ++ii;
        } else {
            std::cerr << "Line " << currentLine << " is not supported.\n";
            abort();
        }
    }
    return 0;
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
    for (int i = 1;; ++i) {
        int registers[4] = { i, 0, 0, 0 };
        const int part1Result = solve(allLines, registers);
        if (part1Result != -1) {
            assert (158 == i);
            std::cout << "Part 1: " << i << std::endl;
            return 0;
        }
    }
}
