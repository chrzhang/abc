#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <string>
#include <cassert>

/*
From http://adventofcode.com/2016/day/23

--- Day 23: Safe Cracking ---

This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's
private office is here, complete with a safe hidden behind a painting, and who
wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry. A sticky note attached
to the safe has a password hint on it: "eggs". The painting is of a large
rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display; instead,
the keypad comes apart in your hands, apparently having been smashed. Behind it
is some kind of socket - one that matches a connector in your prototype
computer! You pull apart the smashed keypad and extract the logic circuit, plug
it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the
safe. You extract the assembunny code from the logic chip (your puzzle input).
The code looks like it uses almost the same architecture and instruction set
that the monorail computer used! You should be able to use the same assembunny
interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does:
positive means forward; negative means backward):

For one-argument instructions, inc becomes dec, and all other one-argument
instructions become inc.
For two-argument instructions, jnz becomes cpy, and all other two-instructions
become jnz.
The arguments of a toggled instruction are not affected.
If an attempt is made to toggle an instruction outside the program, nothing
happens.
If toggling produces an invalid instruction (like cpy 1 2) and an attempt is
later made to execute that instruction, skip it instead.
If tgl toggles itself (for example, if a is 0, tgl a would target itself and
become inc a), the resulting instruction is not executed until the next time it
is reached.
For example, given this program:

cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a

cpy 2 a initializes register a to 2.
The first tgl a toggles an instruction a (2) away from it, which changes the
third tgl a into inc a.
The second tgl a also modifies an instruction 2 away from it, which changes the
cpy 1 a into jnz 1 a.
The fourth line, which is now inc a, increments a to 3.
Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead,
skipping the dec a instructions.
In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry (the number of eggs,
7) in register a, run the code, and then send the value left in register a to
the safe.

What value should be sent to the safe?

--- Part Two ---

The safe doesn't open, but it does make several angry noises to express its
frustration.

You're quite sure your logic is working correctly, so the only other thing
is... you check the painting again. As it turns out, colored eggs are still
eggs. Now you count 12.

As you run the program with this new input, the prototype computer begins to
overheat. You wonder what's taking so long, and whether the lack of any
instruction more powerful than "add one" has anything to do with it. Don't
bunnies usually multiply?

Anyway, what value should actually be sent to the safe?
*/

std::vector<std::string> getAllLines(std::ifstream & inputFile) {
    std::vector<std::string> result;
    std::string currentLine;
    while (std::getline(inputFile, currentLine)) {
        result.push_back(currentLine);
    }
    return result;
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

bool isValidRegister(const std::string & reg) {
    return (reg == "a") || (reg == "b") || (reg == "c") || (reg == "d");
}

const int getValue(const std::string & s, const int registers[4]) {
    if (isValidRegister(s)) {
        return registers[s[0] - 'a'];
    } else {
        return toInt(s);
    }
}

struct Instruction {
    virtual int execute(int registers[4],
                        std::vector<Instruction *> & instructions,
                        size_t currInstructionIndex) = 0;
    virtual void toggle() = 0;
    virtual ~Instruction() {
    }
};

struct Copy : public Instruction {
    std::string m_source, m_destination;
    bool m_toggled;
    Copy(const std::string & source, const std::string & destination)
    : m_source(source), m_destination(destination), m_toggled(false) {
    }
    int execute(int registers[4],
                std::vector<Instruction *> & instructions,
                size_t currInstructionIndex) {
        if (!m_toggled) {
            assert(isValidRegister(m_destination));
            registers[m_destination[0] - 'a'] = getValue(m_source, registers);
            return 1;
        } else {
            if (getValue(m_source, registers)) {
                return getValue(m_destination, registers);
            }
            return 1;
        }
    }
    void toggle() {
        m_toggled = !m_toggled;
    }
};

struct Jump : public Instruction {
    std::string m_condition, m_offset;
    bool m_toggled;
    Jump(const std::string & condition, const std::string & offset)
    : m_condition(condition), m_offset(offset), m_toggled(false) {
    }
    int execute(int registers[4],
                std::vector<Instruction *> & instructions,
                size_t currInstructionIndex) {
        if (!m_toggled) {
            if (getValue(m_condition, registers)) {
                return getValue(m_offset, registers);
            }
            return 1;
        } else {
            assert(isValidRegister(m_offset));
            registers[m_offset[0] - 'a'] = getValue(m_condition, registers);
            return 1;
        }
    }
    void toggle() {
        m_toggled = !m_toggled;
    }
};

struct Increment : public Instruction {
    std::string m_register;
    bool m_toggled;
    Increment(const std::string & reg)
    : m_register(reg), m_toggled(false) {
    }
    int execute(int registers[4],
                std::vector<Instruction *> & instructions,
                size_t currInstructionIndex) {
        if (!m_toggled) {
            registers[m_register[0] - 'a']++;
            return 1;
        } else {
            registers[m_register[0] - 'a']--;
            return 1;
        }
    }
    void toggle() {
        m_toggled = !m_toggled;
    }
};

struct Decrement : public Instruction {
    std::string m_register;
    bool m_toggled;
    Decrement(const std::string & reg)
    : m_register(reg), m_toggled(false) {
    }
    int execute(int registers[4],
                std::vector<Instruction *> & instructions,
                size_t currInstructionIndex) {
        if (!m_toggled) {
            registers[m_register[0] - 'a']--;
            return 1;
        } else {
            registers[m_register[0] - 'a']++;
            return 1;
        }
    }
    void toggle() {
        m_toggled = !m_toggled;
    }
};

struct Toggle : public Instruction {
    std::string m_offset;
    bool m_toggled;
    Toggle(const std::string & offset)
    : m_offset(offset), m_toggled(false) {
    }
    int execute(int registers[4],
                std::vector<Instruction *> & instructions,
                size_t currInstructionIndex) {
        if (!m_toggled) {
            int toggleIndex = ((int) currInstructionIndex) +
                              getValue(m_offset, registers);
            if (toggleIndex >= 0 && toggleIndex < (int) instructions.size()) {
                Instruction * instructionToToggle = instructions[toggleIndex];
                instructionToToggle->toggle();
            }
            return 1;
        } else {
            registers[m_offset[0] - 'a']++;
            return 1;
        }
    }
    void toggle() {
        m_toggled = !m_toggled;
    }
};

int solve(int registers[4], const std::vector<std::string> allLinesInput) {
    std::vector<Instruction *> instructions;
    std::regex cpyRegex("^cpy ([-+]?[0-9abcd]+) (a|b|c|d)$");
    std::regex jnzRegex("^jnz ([-+]?[0-9abcd]+) ([-+]?[0-9abcd]+)$");
    std::regex incRegex("^inc (a|b|c|d)$");
    std::regex decRegex("^dec (a|b|c|d)$");
    std::regex tglRegex("^tgl ([-+]?[0-9abcd]+)$");
    for (const std::string & currentLine : allLinesInput) {
        std::smatch result;
        if (std::regex_search(currentLine, result, cpyRegex)) {
            instructions.push_back(new Copy(result[1], result[2]));
        } else if (regex_search(currentLine, result, jnzRegex)) {
            instructions.push_back(new Jump(result[1], result[2]));
        } else if (regex_search(currentLine, result, incRegex)) {
            instructions.push_back(new Increment(result[1]));
        } else if (regex_search(currentLine, result, decRegex)) {
            instructions.push_back(new Decrement(result[1]));
        } else if (std::regex_search(currentLine, result, tglRegex)) {
            instructions.push_back(new Toggle(result[1]));
        }
    }
    for (int ii = 0; ii < (int) instructions.size() && ii >= 0;) {
        const int offset = instructions[ii]->execute(registers,
                                                     instructions, ii);
        ii += offset;
    }
    for (Instruction * instruction_ptr : instructions) {
        delete instruction_ptr;
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
        std::cerr << argv[1] << " cannot be opened.\n";
        return 1;
    }
    const std::vector<std::string> & allLines = getAllLines(inputFile);
    {
        int registers1[4] = { 7, 0, 0, 0 };
        const int result1 = solve(registers1, allLines);
        assert(13776 == result1);
        std::cout << "Part 1: " << result1 << std::endl;
    }
    {
        int registers2[4] = {12, 0, 0, 0};
        const int result2 = solve(registers2, allLines);
        assert(479010336 == result2);
        std::cout << "Part 2: " << result2 << std::endl;
    }
    return 0;
}
