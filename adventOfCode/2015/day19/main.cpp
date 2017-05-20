#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <map>
#include <vector>
#include <regex>
#include <cassert>
#include <climits>
#include <algorithm>
#include <cctype>

/*
From http://adventofcode.com/2015/day/19

--- Day 19: Medicine for Rudolph ---

Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
and he needs medicine.

Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer
chemistry isn't similar to regular reindeer chemistry, either.

The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission
plant, capable of constructing any Red-Nosed Reindeer molecule you need. It
works by starting with some input molecule and then doing a series of
replacements, one per step, until it has the right molecule.

However, the machine has to be calibrated before it can be used. Calibration
involves determining the number of molecules that can be generated in one step
from a given starting point.

For example, imagine a simpler machine that supports only the following
replacements:

H => HO
H => OH
O => HH

Given the replacements above and starting with HOH, the following molecules
could be generated:

HOOH (via H => HO on the first H).
HOHO (via H => HO on the second H).
OHOH (via H => OH on the first H).
HOOH (via H => OH on the second H).
HHHH (via O => HH).

So, in the example above, there are 4 distinct molecules (not five, because
HOOH appears twice) after one replacement from HOH. Santa's favorite molecule,
HOHOHO, can become 7 distinct molecules (over nine replacements: six from H,
and three from O).

The machine replaces without regard for the surrounding characters. For
example, given the string H2O, the transition H => OO would result in OO2O.

Your puzzle input describes all of the possible replacements and, at the
bottom, the medicine molecule for which you need to calibrate the machine. How
many distinct molecules can be created after all the different ways you can do
one replacement on the medicine molecule?

--- Part Two ---

Now that the machine is calibrated, you're ready to begin molecule fabrication.

Molecule fabrication always begins with just a single electron, e, and applying
replacements one at a time, just like the ones during calibration.

For example, suppose you have the following replacements:

e => H
e => O
H => HO
H => OH
O => HH

If you'd like to make HOH, you start with e, and then make the following
replacements:

e => O to get O O => HH to get HH H => OH (on the second H) to get HOH So, you
could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can be made in
6 steps.

How long will it take to make the medicine? Given the available replacements
and the medicine molecule in your puzzle input, what is the fewest number of
steps to go from e to the medicine molecule?
*/

std::string asString(const std::vector<std::string> & elements) {
    std::string result = "";
    for (const auto & e : elements) {
        result += e;
    }
    return result;
}

std::set<std::string>
getAllTransformedFrom(const std::string & source,
                      const std::multimap<std::string, std::string> &
                        transforms) {
    std::vector<std::string> elements;
    std::regex elementRegex("[A-Ze][a-z]{0,1}");
    for (auto it = std::sregex_token_iterator(source.begin(), source.end(),
                                              elementRegex);
         it != std::sregex_token_iterator(); ++it) {
        elements.push_back(*it);
    }
    std::set<std::string> results;
    for (auto & e : elements) {
        // Consider replacing it with every known mapping
        // Store original e value
        auto original = e;
        auto transformSeek = transforms.equal_range(e);
        for (auto it = transformSeek.first; it != transformSeek.second; ++it) {
            e = it->second;
            results.insert(asString(elements));
        }
        e = original;
    }
    return results;
}

void part1(const std::string & sequence,
           const std::multimap<std::string, std::string> & transforms) {
    auto results = getAllTransformedFrom(sequence, transforms);
    std::cout << "Number of results from one replacement: "
              << results.size() << std::endl;
}

int count_occ(const std::string & hay, const std::string & haystack) {
    int count = 0;
    size_t pos = haystack.find(hay, 0);
    while (pos != std::string::npos) {
        count += 1;
        pos = haystack.find(hay, pos + 1);
    }
    return count;
}

void part2(const std::string & molecule,
           const std::multimap<std::string, std::string> & transforms) {
    std::string starting_sequence = "e";
    int molecule_count = 0;
    for (const auto & c : molecule) {
        if (std::isupper(c)) {
            molecule_count += 1;
        }
    }
    // The symbols Rn, Ar, and Y appear only on the right side of the rules
    // Y is always followed by another element that also only appears on the
    // right side
    // To find the unique steps taken to transform from 'e', subtract these
    // "filler" symbols to filter them out and get only actual steps taken
    // to generate or alter the original sequence from e
    std::cout << "Steps to transform from e: ";
    std::cout << molecule_count - count_occ("Rn", molecule) -
                 count_occ("Ar", molecule) - 2 * count_occ("Y", molecule) - 1
              << std::endl;
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
    std::multimap<std::string, std::string> transforms;
    std::string line;
    std::regex transformRegex("^(.*) => (.*)$");
    std::regex sequenceRegex("^(.+)$");
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, transformRegex);
        if (!result.empty()) {
            assert(result.size() == 3);
            const auto & from = result[1];
            const auto & to = result[2];
            transforms.insert({from, to});
        } else {
            std::regex_search(line, result, sequenceRegex);
            if (!result.empty()) {
                assert(result.size() == 2);
                const std::string & sequence = result[1];
                part1(sequence, transforms);
                part2(sequence, transforms);
                return 0;
            }
        }
    }
    return 0;
}
