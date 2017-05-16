#include <iostream>
#include <fstream>
#include <regex>
#include <cassert>
#include <vector>
#include <sstream>

/*
From http://adventofcode.com/2015/day/23

--- Day 23: Opening the Turing Lock ---

Little Jane Marie just got her very first computer for Christmas from some
unknown benefactor. It comes with instructions and an example program, but the
computer itself seems to be malfunctioning. She's curious what the program
does, and would like you to help her run it.

The manual explains that the computer supports two registers and six
instructions (truly, it goes on to remind the reader, a state-of-the-art
technology). The registers are named a and b, can hold any non-negative
integer, and begin with a value of 0. The instructions are as follows:

hlf r sets register r to half its current value, then continues with the next
instruction.

tpl r sets register r to triple its current value, then continues with the next
instruction.

inc r increments register r, adding 1 to it, then continues with the next
instruction.

jmp offset is a jump; it continues with the instruction offset away relative to
itself.

jie r, offset is like jmp, but only jumps if register r is even ("jump if
even").

jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one",
not odd).

All three jump instructions work with an offset relative to that instruction.
The offset is always written with a prefix + or - to indicate the direction of
the jump (forward or backward, respectively). For example, jmp +1 would simply
continue with the next instruction, while jmp +0 would continuously jump back
to itself forever.

The program exits when it tries to run an instruction beyond the ones defined.

For example, this program sets a to 2, because the jio instruction causes it to
skip the tpl instruction:

inc a
jio a, +2
tpl a
inc a

What is the value in register b when the program in your puzzle input is
finished executing?

--- Part Two ---

The unknown benefactor is very thankful for releasi-- er, helping little Jane
Marie with her computer. Definitely not to distract you, what is the value in
register b after the program is finished executing if register a starts as 1
instead?
*/

struct Machine {
    int register_a = 0;
    int register_b = 0;
};

int to_int(const std::string & s) {
    std::stringstream ss(s);
    int i = 0;
    if (!(ss >> i)) {
        assert(false);
    }
    return i;
}

int solve(Machine & machine, const std::vector<std::string> & lines) {
    // Supported instructions
    std::regex half_regex("^hlf (a|b)$");
    std::regex triple_regex("^tpl (a|b)$");
    std::regex increment_regex("^inc (a|b)$");
    std::regex jump_regex("^jmp ([+-][0-9]*)$");
    std::regex jump_if_even_regex("^jie (a|b), ([+-][0-9]*)$");
    std::regex jump_if_one_regex("^jio (a|b), ([+-][0-9]*)$");
    int current_line_index = 0;
    while (current_line_index >= 0 &&
           current_line_index < (int) lines.size()) {
        const std::string & line = lines[current_line_index];
        std::smatch result;
        if (std::regex_search(line, result, half_regex)) {
            assert(result.size() == 2);
            const std::string & captured_register = result[1];
            int & r = captured_register == "a" ? machine.register_a :
                                                        machine.register_b;
            r /= 2;
            current_line_index += 1;
        } else if (std::regex_search(line, result, triple_regex)) {
            assert(result.size() == 2);
            const std::string & captured_register = result[1];
            int & r = captured_register == "a" ? machine.register_a :
                                                        machine.register_b;
            r *= 3;
            current_line_index += 1;
        } else if (std::regex_search(line, result, increment_regex)) {
            assert(result.size() == 2);
            const std::string & captured_register = result[1];
            int & r = captured_register == "a" ? machine.register_a :
                                                        machine.register_b;
            r += 1;
            current_line_index += 1;
        } else if (std::regex_search(line, result, jump_regex)) {
            assert(result.size() == 2);
            const std::string & captured_offset = result[1];
            current_line_index += to_int(captured_offset);
        } else if (std::regex_search(line, result, jump_if_even_regex)) {
            assert(result.size() == 3);
            const std::string & captured_register = result[1];
            const std::string & captured_offset = result[2];
            bool is_register_even =
                captured_register == "a" ? machine.register_a % 2 == 0 :
                                           machine.register_b % 2 == 0;
            current_line_index +=
                is_register_even ? to_int(captured_offset) : 1;
        } else if (std::regex_search(line, result, jump_if_one_regex)) {
            assert(result.size() == 3);
            const std::string & captured_register = result[1];
            const std::string & captured_offset = result[2];
            bool is_register_one =
                captured_register == "a" ? machine.register_a == 1 :
                                           machine.register_b == 1;
            current_line_index +=
                is_register_one ? to_int(captured_offset) : 1;
        } else {
            std::cerr << line << " is not a supported instruction.\n";
            return 1;
        }
    }
    return machine.register_b;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    Machine machine;
    // Read file
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(f, line)) {
        lines.push_back(line);
    }
    // Part 1
    assert(184 == solve(machine, lines));
    // Part 2
    machine.register_a = 1;
    machine.register_b = 0;
    assert(231 == solve(machine, lines));
    return 0;
}
