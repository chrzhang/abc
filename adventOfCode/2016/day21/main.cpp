#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
#include <algorithm>
#include <regex>

/*
From http://adventofcode.com/2016/day/21

--- Day 21: Scrambled Letters and Hash ---

The computer system you're breaking into uses a weird scrambling function to
store its passwords. It shouldn't be much trouble to create your own scrambled
password so you can add it to the system; you just have to implement the
scrambler.

The scrambling function is a series of operations (the exact list is provided
in your puzzle input). Starting with the password to be scrambled, apply each
operation in succession to the string. The individual operations behave as
follows:

- swap position X with position Y means that the letters at indexes X and Y
  (counting from 0) should be swapped.
- swap letter X with letter Y means that the letters X and Y should be swapped
  (regardless of where they appear in the string).
- rotate left/right X steps means that the whole string should be rotated; for
  example, one right rotation would turn abcd into dabc.
- rotate based on position of letter X means that the whole string should be
  rotated to the right based on the index of letter X (counting from 0) as
  determined before this instruction does any rotations. Once the index is
  determined, rotate the string to the right one time, plus a number of times
  equal to that index, plus one additional time if the index was at least 4.
- reverse positions X through Y means that the span of letters at indexes X
  through Y (including the letters at X and Y) should be reversed in order.
- move position X to position Y means that the letter which is at index X
  should be removed from the string, then inserted such that it ends up at
  index Y.

For example, suppose you start with abcde and perform the following operations:

- swap position 4 with position 0 swaps the first and last letters, producing
  the input for the next step, ebcda.
- swap letter d with letter b swaps the positions of d and b: edcba.  reverse
  positions 0 through 4 causes the entire string to be reversed, producing
  abcde.
- rotate left 1 step shifts all letters left one position, causing the first
  letter to wrap to the end of the string: bcdea.
- move position 1 to position 4 removes the letter at position 1 (c), then
  inserts it at position 4 (the end of the string): bdeac.
- move position 3 to position 0 removes the letter at position 3 (a), then
  inserts it at position 0 (the front of the string): abdec.
- rotate based on position of letter b finds the index of letter b (1), then
  rotates the string right once plus a number of times equal to that index (2):
  ecabd.
- rotate based on position of letter d finds the index of letter d (4), then
  rotates the string right once, plus a number of times equal to that index,
  plus an additional time because the index was at least 4, for a total of 6
  right rotations: decab.

After these steps, the resulting scrambled password is decab.

Now, you just need to generate a new scrambled password and you can access the
system. Given the list of scrambling operations in your puzzle input, what is
the result of scrambling abcdefgh?

--- Part Two ---

You scrambled the password correctly, but you discover that you can't actually
modify the password file on the system. You'll need to un-scramble one of the
existing passwords by reversing the scrambling process.

What is the un-scrambled version of the scrambled password fbgdceah?
*/

int toInt(const std::string & someString) {
    int result = 0;
    std::stringstream ss(someString);
    if (!(ss >> result)) {
        std::cerr << "Cannot convert " << someString << " to a number.\n";
        abort();
    }
    return result;
}

class Password {

    std::string d_password;

    public:

        Password(const std::string & input) {
            d_password = input;
        }

        void swapCharactersAt_And_(const size_t ii, const size_t jj) {
            assert(ii < d_password.size() && jj < d_password.size());
            const char temp = d_password[ii];
            d_password[ii] = d_password[jj];
            d_password[jj] = temp;
        }

        void swapCharacters(const char aa, const char bb) {
            for (auto & cc : d_password) {
                if (cc == aa) {
                    cc = bb;
                } else if (cc == bb) {
                    cc = aa;
                }
            }
        }

        void rotateLeft(const int amount) {
            std::rotate(d_password.begin(),
                        d_password.begin() + (amount % d_password.size()),
                        d_password.end());
        }

        void rotateRight(const int amount) {
            std::rotate(d_password.rbegin(),
                        d_password.rbegin() + (amount % d_password.size()),
                        d_password.rend());
        }

        void reverseCharactersBetween_And_(const size_t ii, const size_t jj) {
            assert(ii < d_password.size() && jj < d_password.size());
            assert(ii < jj);
            std::reverse(std::next(d_password.begin(), ii), std::next(d_password.begin(), jj + 1));
        }

        void rotateBasedOnPositionOf(const char cc) {
            const size_t position = d_password.find(cc);
            assert(position != std::string::npos);
            rotateRight(1 + position + (position >= 4 ? 1 : 0));
        }

        void rotateReverseBasedOnPositionOf(const char cc) {
            const size_t currentLocation = d_password.find(cc);
            assert(currentLocation != std::string::npos);
            for (size_t possibleLocationBeforeRotation = 0;
                 possibleLocationBeforeRotation < d_password.size();
                 ++possibleLocationBeforeRotation) {
                const size_t rotationAmount =
                    1 + possibleLocationBeforeRotation +
                    (possibleLocationBeforeRotation >= 4 ? 1 : 0);
                if (currentLocation ==
                    (possibleLocationBeforeRotation + rotationAmount) % d_password.size()) {
                    rotateLeft(rotationAmount);
                    break;
                }
            }
        }

        void moveFrom_To_(const size_t ii, const size_t jj) {
            assert(ii < d_password.size() && jj < d_password.size());
            const char temp = d_password[ii];
            d_password.erase(ii, 1);
            d_password.insert(jj, 1, temp);
        }

        const std::string & getPassword() const {
            return d_password;
        }

};

void test() {
    {
        Password p("abcde");
        p.swapCharactersAt_And_(0, 4);
        assert(p.getPassword() == "ebcda");
        p.swapCharacters('d', 'b');
        assert(p.getPassword() == "edcba");
        p.reverseCharactersBetween_And_(0, 4);
        assert(p.getPassword() == "abcde");
        p.rotateLeft(1);
        assert(p.getPassword() == "bcdea");
        p.moveFrom_To_(1, 4);
        assert(p.getPassword() == "bdeac");
        p.moveFrom_To_(3, 0);
        assert(p.getPassword() == "abdec");
        p.rotateBasedOnPositionOf('b');
        assert(p.getPassword() == "ecabd");
        p.rotateBasedOnPositionOf('d');
        assert(p.getPassword() == "decab");
    }
    {
        Password p("decab");
        p.rotateReverseBasedOnPositionOf('d');
        p.rotateBasedOnPositionOf('d');
        assert(p.getPassword() == "decab");
    }
}

std::string part1(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << filename << std::endl;
        return "";
    }
    Password p("abcdefgh");
    std::string currentLine;
    std::regex swapIndicesRegex("^swap position ([0-9]+) with position ([0-9]+)$");
    std::regex swapLetterRegex("^swap letter ([a-z]) with letter ([a-z])$");
    std::regex rotateLeftRegex("^rotate left ([0-9]+) step[s]?$");
    std::regex rotateRightRegex("^rotate right ([0-9]+) step[s]?$");
    std::regex rotateBasedOnRegex("^rotate based on position of letter ([a-z])$");
    std::regex reverseIndicesRegex("^reverse positions ([0-9]+) through ([0-9]+)$");
    std::regex moveRegex("^move position ([0-9]+) to position ([0-9]+)$");
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        if (std::regex_search(currentLine, result, swapIndicesRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.swapCharactersAt_And_(ii, jj);
        } else if (std::regex_search(currentLine, result, swapLetterRegex)) {
            const auto c1 = std::string(result[1])[0];
            const auto c2 = std::string(result[2])[0];
            p.swapCharacters(c1, c2);
        } else if (std::regex_search(currentLine, result, rotateLeftRegex)) {
            const auto amount = toInt(result[1]);
            p.rotateLeft(amount);
        } else if (std::regex_search(currentLine, result, rotateRightRegex)) {
            const auto amount = toInt(result[1]);
            p.rotateRight(amount);
        } else if (std::regex_search(currentLine, result, rotateBasedOnRegex)) {
            const auto cc = std::string(result[1])[0];
            p.rotateBasedOnPositionOf(cc);
        } else if (std::regex_search(currentLine, result, reverseIndicesRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.reverseCharactersBetween_And_(ii, jj);
        } else if (std::regex_search(currentLine, result, moveRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.moveFrom_To_(ii, jj);
        } else {
            std::cerr << "Unrecognized line: " << currentLine << std::endl;
            return "";
        }
    }
    return p.getPassword();
}

std::string part2(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << filename << std::endl;
        return "";
    }
    Password p("fbgdceah");
    std::regex swapIndicesRegex("^swap position ([0-9]+) with position ([0-9]+)$");
    std::regex swapLetterRegex("^swap letter ([a-z]) with letter ([a-z])$");
    std::regex rotateLeftRegex("^rotate left ([0-9]+) step[s]?$");
    std::regex rotateRightRegex("^rotate right ([0-9]+) step[s]?$");
    std::regex rotateBasedOnRegex("^rotate based on position of letter ([a-z])$");
    std::regex reverseIndicesRegex("^reverse positions ([0-9]+) through ([0-9]+)$");
    std::regex moveRegex("^move position ([0-9]+) to position ([0-9]+)$");
    std::vector<std::string> instructions;
    {
        std::string currentLine;
        while (std::getline(inputFile, currentLine)) {
            instructions.push_back(currentLine);
        }
    }
    for (auto it = instructions.rbegin(); it != instructions.rend(); ++it) {
        const auto & currentLine = *it;
        std::smatch result;
        if (std::regex_search(currentLine, result, swapIndicesRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.swapCharactersAt_And_(ii, jj);
        } else if (std::regex_search(currentLine, result, swapLetterRegex)) {
            const auto c1 = std::string(result[1])[0];
            const auto c2 = std::string(result[2])[0];
            p.swapCharacters(c1, c2);
        } else if (std::regex_search(currentLine, result, rotateLeftRegex)) {
            const auto amount = toInt(result[1]);
            p.rotateRight(amount);
        } else if (std::regex_search(currentLine, result, rotateRightRegex)) {
            const auto amount = toInt(result[1]);
            p.rotateLeft(amount);
        } else if (std::regex_search(currentLine, result, rotateBasedOnRegex)) {
            const auto cc = std::string(result[1])[0];
            p.rotateReverseBasedOnPositionOf(cc);
        } else if (std::regex_search(currentLine, result, reverseIndicesRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.reverseCharactersBetween_And_(ii, jj);
        } else if (std::regex_search(currentLine, result, moveRegex)) {
            const auto ii = toInt(result[1]);
            const auto jj = toInt(result[2]);
            p.moveFrom_To_(jj, ii);
        } else {
            std::cerr << "Unrecognized line: " << currentLine << std::endl;
            return "";
        }
    }
    return p.getPassword();
}

int main(int argc, char * argv[]) {
    test();
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    const std::string part1Result = part1(argv[1]);
    assert(part1Result == "bfheacgd");
    std::cout << "Part 1: " << part1Result << std::endl;
    const std::string part2Result = part2(argv[1]);
    assert(part2Result == "gcehdbfa");
    std::cout << "Part 2: " << part2Result << std::endl;
}
