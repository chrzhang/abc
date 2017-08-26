#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <vector>
#include <array>
#include <climits>

/*
From http://adventofcode.com/2016

--- Day 6: Signals and Noise ---

Something is jamming your communications with Santa. Fortunately, your signal
is only partially jammed, and protocol in situations like this is to switch to
a simple repetition code to get the message through.

In this model, the same message is sent repeatedly. You've recorded the
repeating message signal (your puzzle input), but the data seems quite
corrupted - almost too badly to recover. Almost.

All you need to do is figure out which character is most frequent for each
position. For example, suppose you had recorded the following messages:

eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar

The most common character in the first column is e; in the second, a; in the
third, s, and so on. Combining these characters returns the error-corrected
message, easter.

Given the recording in your puzzle input, what is the error-corrected version
of the message being sent?

--- Part Two ---

Of course, that would be the message - if you hadn't agreed to use a modified
repetition code instead.

In this modified code, the sender instead transmits what looks like random
data, but for each character, the character they actually want to send is
slightly less likely than the others. Even after signal-jamming noise, you can
look at the letter distributions in each column and choose the least common
letter to reconstruct the original message.

In the above example, the least common character in the first column is a; in
the second, d, and so on. Repeating this process for the remaining characters
produces the original message, advent.

Given the recording in your puzzle input and this new decoding methodology,
what is the original message that Santa is trying to send?
*/

void processLine(std::vector<std::array<int, 26>> & counts,
                 const std::string & currentLine) {
    assert(currentLine.size() == counts.size());
    for (size_t characterIndex = 0; characterIndex < currentLine.size();
         ++characterIndex) {
        const char currentCharacter = currentLine[characterIndex];
        assert(currentCharacter >= 'a' && currentCharacter <= 'z');
        counts[characterIndex][currentCharacter - 'a'] += 1;
    }
}

void initCounts(std::vector<std::array<int, 26>> & counts) {
    for (auto & alphabetCounter : counts) {
        for (int i = 0; i < 26; ++i) {
            alphabetCounter[i] = 0;
        }
    }
}

char mostCommonCharacterIn(const std::array<int, 26> & alphabetCounter) {
    int maxIndex = 0;
    int maxCounter = -1;
    for (int currentIndex = 0; currentIndex < 26; ++currentIndex) {
        if (alphabetCounter[currentIndex] > maxCounter) {
            maxCounter = alphabetCounter[currentIndex];
            maxIndex = currentIndex;
        }
    }
    return 'a' + maxIndex;
}

char leastCommonCharacterIn(const std::array<int, 26> & alphabetCounter) {
    int minIndex = 0;
    int minCounter = INT_MAX;
    for (int currentIndex = 0; currentIndex < 26; ++currentIndex) {
        if (alphabetCounter[currentIndex] < minCounter) {
            minCounter = alphabetCounter[currentIndex];
            minIndex = currentIndex;
        }
    }
    return 'a' + minIndex;
}

std::string
getMostCommonCharacters(const std::vector<std::array<int, 26>> & counts) {
    std::string result;
    for (size_t characterIndex = 0; characterIndex < counts.size();
         ++characterIndex) {
        result += mostCommonCharacterIn(counts[characterIndex]);
    }
    return result;
}

std::string
getLeastCommonCharacters(const std::vector<std::array<int, 26>> & counts) {
    std::string result;
    for (size_t characterIndex = 0; characterIndex < counts.size();
         ++characterIndex) {
        result += leastCommonCharacterIn(counts[characterIndex]);
    }
    return result;
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
    if (!std::getline(inputFile, currentLine)) {
        std::cerr << "Cannot read " << argv[1] << std::endl;
    }
    std::vector<std::array<int, 26>> counts(currentLine.size(),
                                            std::array<int, 26>());
    initCounts(counts);
    processLine(counts, currentLine);
    while (std::getline(inputFile, currentLine)) {
        processLine(counts, currentLine);
    }
    std::cout << "Part 1: " << getMostCommonCharacters(counts) << std::endl;
    std::cout << "Part 2: " << getLeastCommonCharacters(counts) << std::endl;
    return 0;
}
