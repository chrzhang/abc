#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <regex>
#include <cassert>
#include <map>
#include <stdexcept>

/*
From http://adventofcode.com/2015/day/16

--- Day 16: Aunt Sue ---

Your Aunt Sue has given you a wonderful gift, and you'd like to send her a
thank you card. However, there's a small problem: she signed it "From, Aunt
Sue".

You have 500 Aunts named "Sue".

So, to avoid sending the card to the wrong person, you need to figure out which
Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the
gift. You open the present and, as luck would have it, good ol' Aunt Sue got
you a My First Crime Scene Analysis Machine! Just what you wanted. Or needed,
as the case may be.

The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few
specific compounds in a given sample, as well as how many distinct kinds of
those compounds there are. According to the instructions, these are what the
MFCSAM can detect:

    children, by human DNA age analysis.
    cats. It doesn't differentiate individual breeds.
    Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and
	vizslas.
    goldfish. No other kinds of fish.
    trees, all in one group.
    cars, presumably by exhaust or gasoline or something.
    perfumes, which is handy, since many of your Aunts Sue wear a few kinds.

In fact, many of your Aunts Sue have many of these. You put the wrapping from
the gift into the MFCSAM. It beeps inquisitively at you a few times and then
prints out a message on ticker tape:

children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1

You make a list of the things you can remember about each Aunt Sue. Things
missing from your list aren't zero - you simply don't remember the value.

What is the number of the Sue that got you the gift?

--- Part Two ---

As you're about to send the thank you note, something in the MFCSAM's
instructions catches your eye. Apparently, it has an outdated retroencabulator,
and so the output from the machine isn't exact values - some of them indicate
ranges.

In particular, the cats and trees readings indicates that there are greater
than that many (due to the unpredictable nuclear decay of cat dander and tree
pollen), while the pomeranians and goldfish readings indicate that there are
fewer than that many (due to the modial interaction of magnetoreluctance).

What is the number of the real Aunt Sue?
*/

int toInt(const std::string & s) {
    std::stringstream ss(s);
    int r;
    if (!(ss >> r)) {
        throw std::runtime_error("Cannot interpret " + s + " as a number.");
    }
    return r;
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
    std::string line;
    std::regex lineRegex("^Sue ([0-9]+): (.*)$");
    std::regex fieldRegex("([a-z]+: [0-9]+)");
    std::regex keyValueRegex("(.*): (.*)");
    std::map<std::string, std::map<std::string, std::string>> auntData;
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, lineRegex);
        assert(result.size() == 3);
        const std::string & fields = result[2];
        for (auto it = std::sregex_token_iterator(fields.begin(), fields.end(),
                                                  fieldRegex, 1);
             it != std::sregex_token_iterator(); ++it) {
            const std::string & keyValue = *it;
            std::smatch keyValueResult;
            std::regex_search(keyValue, keyValueResult, keyValueRegex);
            auntData[result[1]][keyValueResult[1]] = keyValueResult[2];
        }
    }
    const std::map<std::string, std::string> targetAunt = {
        {"children", "3"},
        {"cats", "7"},
        {"samoyeds", "2"},
        {"pomeranians", "3"},
        {"akitas", "0"},
        {"vizslas", "0"},
        {"goldfish", "5"},
        {"trees", "3"},
        {"cars", "2"},
        {"perfumes", "1"}
    };
    // Part 1
    for (auto p : auntData) {
        const auto & auntName = p.first;
        bool isCandidate = true;
        for (auto fp : p.second) {
            const auto & key = fp.first;
            const auto & value = fp.second;
            if (targetAunt.find(key) != targetAunt.end()) {
                int detected = toInt(targetAunt.at(key));
                int memory = toInt(value);
                if (detected != memory) {
                    isCandidate = false;
                    break;
                }
            }
        }
        if (isCandidate) {
            std::cout << "Part 1: Sue " << auntName << std::endl;
        }
    }
    // Part 2
    for (auto p : auntData) {
        const auto & auntName = p.first;
        bool isCandidate = true;
        for (auto fp : p.second) {
            const auto & key = fp.first;
            const auto & value = fp.second;
            if (targetAunt.find(key) != targetAunt.end()) {
                int detected = toInt(targetAunt.at(key));
                int actual = toInt(value);
                if (key == "cats" || key == "trees") {
                    if (detected >= actual) {
                        isCandidate = false;
                        break;
                    }
                } else if (key == "pomeranians" || key == "goldfish") {
                    if (detected <= actual) {
                        isCandidate = false;
                        break;
                    }
                } else {
                    if (detected != actual) {
                        isCandidate = false;
                        break;
                    }
                }
            }
        }
        if (isCandidate) {
            std::cout << "Part 2: Sue " << auntName << std::endl;
        }
    }
    return 0;
}
