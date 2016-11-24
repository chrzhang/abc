#include <iostream>
#include <regex>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <cassert>
#include <cstdlib>

/*
From http://adventofcode.com/2015/day/7

--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic
gates! Unfortunately, little Bobby is a little under the recommended age range,
and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit
signal (a number from 0 to 65535). A signal is provided to each wire by a gate,
another wire, or some specific value. Each wire can only get a signal from one
source, but can provide its signal to multiple destinations. A gate provides no
signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together:
x AND y -> z means to connect wires x and y to an AND gate, and then connect
its output to wire z.

For example:

123 -> x means that the signal 123 is provided to wire x.  x AND y -> z means
that the bitwise AND of wire x and wire y is provided to wire z.  p LSHIFT 2 ->
q means that the value from wire p is left-shifted by 2 and then provided to
wire q.  NOT e -> f means that the bitwise complement of the value from wire e
is provided to wire f.  Other possible gates include OR (bitwise OR) and RSHIFT
(right-shift). If, for some reason, you'd like to emulate the circuit instead,
almost all programming languages (for example, C, JavaScript, or Python)
provide operators for these gates.

For example, here is a simple circuit:

123 -> x 456 -> y x AND y -> d x OR y -> e x LSHIFT 2 -> f y RSHIFT 2 -> g NOT
x -> h NOT y -> i After it is run, these are the signals on the wires:

d: 72 e: 507 f: 492 g: 114 h: 65412 i: 65079 x: 123 y: 456 In little Bobby's
kit's instructions booklet (provided as your puzzle input), what signal is
ultimately provided to wire a?

--- Part Two ---

Now, take the signal you got on wire a, override wire b to that signal, and
reset the other wires (including wire a). What new signal is ultimately
provided to wire a?
*/

class Circuitry {

    std::map<std::string, uint16_t> wires_to_values;
    std::set<std::string> overrides;

    bool isNumber(const std::string operand) {
        char * p;
        strtol(operand.c_str(), &p, 10);
        if (*p) {
            return false;
        }
        return true;
    }

    public:

        void override(const std::string & wire, int value) {
            wires_to_values[wire] = value;
            overrides.insert(wire);
        }

        void setValue(const std::string & wire, int value) {
            if (overrides.find(wire) != overrides.end()) {
                return;
            } else {
                wires_to_values[wire] = value;
            }
        }

        bool handleBinaryOperation(const std::string & operand_left,
                                   const std::string & operation,
                                   const std::string & operand_right,
                                   const std::string & target) {
            if ((wires_to_values.find(operand_left) == wires_to_values.end() &&
                !isNumber(operand_left)) ||
                (wires_to_values.find(operand_right) == wires_to_values.end() &&
                !isNumber(operand_right))) {
                return false;
            }
            int operand1 =
                isNumber(operand_left) ? std::stoi(operand_left) :
                                         wires_to_values[operand_left];
            int operand2 =
                isNumber(operand_right) ? std::stoi(operand_right) :
                                         wires_to_values[operand_right];
            if (operation == "AND") {
                setValue(target, operand1 & operand2);
            } else if (operation == "OR") {
                setValue(target, operand1 | operand2);
            } else if (operation == "LSHIFT") {
                setValue(target, operand1 << operand2);
            } else if (operation == "RSHIFT") {
                setValue(target, operand1 >> operand2);
            } else {
                assert(false);
            }
            return true;
        }

        bool handleUnaryOperation(const std::string & operation,
                                  const std::string & operand,
                                  const std::string & target) {
            if (wires_to_values.find(operand) == wires_to_values.end()
                && !isNumber(operand)) {
                return false;
            }
            int operand1 =
                isNumber(operand) ? std::stoi(operand) :
                                    wires_to_values[operand];
            if (operation == "NOT") {
                setValue(target, ~operand1);
            } else {
                assert(false);
            }
            return true;
        }

        bool handleSignal(const std::string & signal,
                          const std::string & target) {
            setValue(target, std::stoi(signal));
            return true;
        }

        bool handleForward(const std::string & source,
                           const std::string & target) {
            if (wires_to_values.find(source) == wires_to_values.end()) {
                return false;
            }
            setValue(target, wires_to_values[source]);
            return true;
        }

        const std::map<std::string, uint16_t> & values() const {
            return wires_to_values;
        }

        friend std::ostream & operator<<(std::ostream & os,
                                         const Circuitry & c);

};

std::ostream & operator<<(std::ostream & os, const Circuitry & c) {
    for (auto keyvalue: c.wires_to_values) {
        os << keyvalue.first << " | " << keyvalue.second << "\n";
    }
    return os;
}

int part(int level, char * argv[], int data = -1) {
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    Circuitry circuitry;
    if (level == 2) {
        circuitry.override("b", data);
    }
    std::vector<std::string> unfulfilledLines;
    std::string line;
    // Binary operator gates
    std::regex binary_op_rgx("(.+) (AND|OR|RSHIFT|LSHIFT) (.+) -> (.+)");
    // Unary operator gates
    std::regex unary_op_rgx("(NOT) (.+) -> (.+)");
    // Direct signal current
    std::regex signal_rgx("([0-9]+) -> (.+)");
    // Forward signal current
    std::regex forward_rgx("([a-zA-Z]+) -> (.+)");
    while (std::getline(f, line)) {
        std::smatch result;
        if (result.empty()) {
            std::regex_search(line, result, binary_op_rgx);
            if (!result.empty()) {
                assert(result.size() == 5);
                if (!(circuitry.handleBinaryOperation(result[1], result[2],
                                                      result[3], result[4]))) {
                    unfulfilledLines.push_back(line);
                }
            }
        }
        if (result.empty()) {
            std::regex_search(line, result, unary_op_rgx);
            if (!result.empty()) {
                assert(result.size() == 4);
                if (!(circuitry.handleUnaryOperation(result[1], result[2],
                                                     result[3]))) {
                    unfulfilledLines.push_back(line);
                }
            }
        }
        if (result.empty()) {
            std::regex_search(line, result, signal_rgx);
            if (!result.empty()) {
                assert(result.size() == 3);
                if (!(circuitry.handleSignal(result[1], result[2]))) {
                    unfulfilledLines.push_back(line);
                }
            }
        }
        if (result.empty()) {
            std::regex_search(line, result, forward_rgx);
            if (!result.empty()) {
                assert(result.size() == 3);
                if (!(circuitry.handleForward(result[1], result[2]))) {
                    unfulfilledLines.push_back(line);
                }
            }
        }
    }
    while (!unfulfilledLines.empty()) {
        for (size_t i = 0; i < unfulfilledLines.size(); ++i) {
            line = unfulfilledLines[i];
            std::smatch result;
            if (result.empty()) {
                std::regex_search(line, result, binary_op_rgx);
                if (!result.empty()) {
                    assert(result.size() == 5);
                    if (circuitry.handleBinaryOperation(result[1], result[2],
                                                        result[3], result[4])) {
                        unfulfilledLines.erase(
                            std::next(unfulfilledLines.begin(), i));
                        --i;
                    }
                }
            }
            if (result.empty()) {
                std::regex_search(line, result, unary_op_rgx);
                if (!result.empty()) {
                    assert(result.size() == 4);
                    if (circuitry.handleUnaryOperation(result[1], result[2],
                                                       result[3])) {
                        unfulfilledLines.erase(
                            std::next(unfulfilledLines.begin(), i));
                        --i;
                    }
                }
            }
            if (result.empty()) {
                std::regex_search(line, result, signal_rgx);
                if (!result.empty()) {
                    assert(result.size() == 3);
                    if (circuitry.handleSignal(result[1], result[2])) {
                        unfulfilledLines.erase(
                            std::next(unfulfilledLines.begin(), i));
                        --i;
                    }
                }
            }
            if (result.empty()) {
                std::regex_search(line, result, forward_rgx);
                if (!result.empty()) {
                    assert(result.size() == 3);
                    if (circuitry.handleForward(result[1], result[2])) {
                        unfulfilledLines.erase(
                            std::next(unfulfilledLines.begin(), i));
                        --i;
                    }
                }
            }
        }
    }
    std::cout << "Value of wire a: " << circuitry.values().at("a")
              << std::endl;
    return circuitry.values().at("a");
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    int valueOnA = part(1, argv);
    part(2, argv, valueOnA);
    return 0;
}
