#include <iostream>
#include <regex>
#include <string>
#include <fstream>
#include <sstream>
#include <cassert>

/*
From http://adventofcode.com/2016/day/9

--- Day 9: Explosives in Cyberspace ---

Wandering around a secure area, you come across a datalink port to a new part
of the network. After briefly scanning it for interesting files, you find one
file in particular that catches your attention. It's compressed with an
experimental format, but fortunately, the documentation for the format is
nearby.

The format compresses a sequence of characters. Whitespace is ignored. To
indicate that some sequence should be repeated, a marker is added to the file,
like (10x2). To decompress this marker, take the subsequent 10 characters and
repeat them 2 times. Then, continue reading the file after the repeated data.
The marker itself is not included in the decompressed output.

If parentheses or other characters appear within the data referenced by a
marker, that's okay - treat it like normal data, not a marker, and then resume
looking for markers after the decompressed section.

For example:

ADVENT contains no markers and decompresses to itself with no changes,
resulting in a decompressed length of 6.
A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a
decompressed length of 7.
(3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a
decompressed length of 11.
(6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because
it's within a data section of another marker, it is not treated any differently
from the A that comes after it. It has a decompressed length of 6.
X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18),
because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped
and not processed further.

What is the decompressed length of the file (your puzzle input)? Don't count whitespace.

--- Part Two ---

Apparently, the file actually uses version two of the format.

In version two, the only difference is that markers within decompressed data
are decompressed. This, the documentation explains, provides much more
substantial compression capabilities, allowing many-gigabyte files to be stored
in only a few kilobytes.

For example:

(3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no
markers.
X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data
from the (8x2) marker is then further decompressed, thus triggering the (3x3)
marker twice for a total of six ABC sequences.
(27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated
241920 times.
(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters
long.

Unfortunately, the computer you brought probably doesn't have enough memory to
actually decompress the file; you'll have to come up with another way to get
its decompressed length.

What is the decompressed length of the file using this improved format?
*/

int toInt(const std::string & someString) {
    std::stringstream ss(someString);
    int result;
    if (!(ss >> result)) {
        std::cerr << "Cannot convert " << someString << " to int\n";
        return 0;
    }
    return result;
}

int getNumberOfTimesRepeated(const std::string & expander,
                             std::regex expanderRegex) {
    std::smatch result;
    std::regex_search(expander, result, expanderRegex);
    if (3 != result.size()) {
        std::cerr << "Line could not be interpreted by regex\n";
        return 1;
    }
    const auto & numberOfTimesRepeated = toInt(result[2]);
    return numberOfTimesRepeated;
}

int getNumberOfNextCharacters(const std::string & expander,
                              std::regex expanderRegex) {
    std::smatch result;
    std::regex_search(expander, result, expanderRegex);
    if (3 != result.size()) {
        std::cerr << "Line could not be interpreted by regex\n";
        return 1;
    }
    const auto & numberOfNextCharacters = toInt(result[1]);
    return numberOfNextCharacters;
}

unsigned getDecompressedLengthPart1(const std::string & encryptedLine) {
    std::regex expanderRegex("\\(([0-9]+)x([0-9]+)\\)");
    int currentStringIndex = 0;
    int totalDecompressedLength = encryptedLine.size();
    for (std::sregex_iterator it = std::sregex_iterator(encryptedLine.begin(),
                                                        encryptedLine.end(),
                                                        expanderRegex);
         it != std::sregex_iterator(); ++it) {
        const auto & expander = it->str();
        const auto & expanderStartPosition = it->position();
        assert(encryptedLine[expanderStartPosition] == '(');
        const auto & expanderEndPosition =
            expanderStartPosition + expander.size() - 1;
        assert(encryptedLine[expanderEndPosition] == ')');
        if (currentStringIndex > expanderStartPosition) {
            continue; // Current expander is not a true expander
        }
        totalDecompressedLength -= expander.size();
        const auto & numberOfNextCharacters =
            getNumberOfNextCharacters(expander, expanderRegex);
        const auto & numberOfTimesRepeated =
            getNumberOfTimesRepeated(expander, expanderRegex);
        const int amountOfExpandedCharacters =
            std::max(0, numberOfTimesRepeated - 1) * numberOfNextCharacters;
        totalDecompressedLength += amountOfExpandedCharacters;
        currentStringIndex = expanderEndPosition + 1 + numberOfNextCharacters;
    }
    return totalDecompressedLength;
}

bool hasExpander(const std::string & someString) {
    std::regex expanderRegex("\\(([0-9]+)x([0-9]+)\\)");
    std::smatch result;
    std::regex_search(someString, result, expanderRegex);
    return (3 == result.size());
}

unsigned long long getDecompressedLengthPart2(std::string encryptedLine) {
    std::regex expanderRegex("\\(([0-9]+)x([0-9]+)\\)");
    std::smatch result;
    std::regex_search(encryptedLine, result, expanderRegex);
    unsigned long long totalDecompressedLength = 0;
    while (result.size() == 3) {
        const std::string & expander = result[0];
        const auto & numberOfNextCharacters = toInt(result[1]);
        const auto & numberOfTimesRepeated = toInt(result[2]);
        int expanderStartPosition = result.position();
        int expanderEndPosition = expanderStartPosition + expander.size() - 1;
        // Condense characters before expander into counter
        encryptedLine = std::string(std::next(encryptedLine.begin(), expanderStartPosition), encryptedLine.end());
        totalDecompressedLength += expanderStartPosition;
        expanderEndPosition -= expanderStartPosition;
        expanderStartPosition = 0;
        assert(encryptedLine[expanderStartPosition] == '(');
        assert(encryptedLine[expanderEndPosition] == ')');
        const auto & chunkToRepeat = std::string(std::next(encryptedLine.begin(), expanderEndPosition + 1),
                                                 std::next(encryptedLine.begin(), expanderEndPosition + 1 + numberOfNextCharacters));
        if (hasExpander(chunkToRepeat)) { // Do the expansion
            totalDecompressedLength += numberOfTimesRepeated * getDecompressedLengthPart2(chunkToRepeat);
        } else { // Condense characters into counter
            totalDecompressedLength += numberOfNextCharacters * numberOfTimesRepeated;
        }
        encryptedLine =
            std::string(std::next(encryptedLine.begin(), expanderEndPosition + numberOfNextCharacters + 1),
                        encryptedLine.end());
        std::regex_search(encryptedLine, result, expanderRegex);
    }
    totalDecompressedLength += encryptedLine.size();
    return totalDecompressedLength;
}


void tests() {
    assert(getDecompressedLengthPart1("ADVENT") == std::string("ADVENT").size());
    assert(getDecompressedLengthPart1("A(1x5)BC") == std::string("ABBBBBC").size());
    assert(getDecompressedLengthPart1("(3x3)XYZ") == std::string("XYZXYZXYZ").size());
    assert(getDecompressedLengthPart1("A(2x2)BCD(2x2)EFG") == std::string("ABCBCDEFEFG").size());
    assert(getDecompressedLengthPart1("(6x1)(1x3)A") == std::string("(1x3)A").size());
    assert(getDecompressedLengthPart1("X(8x2)(3x3)ABCY") == std::string("X(3x3)ABC(3x3)ABCY").size());
    assert(getDecompressedLengthPart2("(3x3)XYZ") == std::string("XYZXYZXYZ").size());
    assert(getDecompressedLengthPart2("X(8x2)(3x3)ABCY") == std::string("XABCABCABCABCABCABCY").size());
    assert(getDecompressedLengthPart2("(27x12)(20x12)(13x14)(7x10)(1x12)A") == std::string(241920, 'A').size());
    assert(445 == getDecompressedLengthPart2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"));
}

int main(int argc, char * argv[]) {
    tests();
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    std::string encryptedLine;
    std::getline(inputFile, encryptedLine);
    const auto part1Answer = getDecompressedLengthPart1(encryptedLine);
    assert(part1Answer == 102239);
    std::cout << "Part 1: " << part1Answer << std::endl;
    const auto part2Answer = getDecompressedLengthPart2(encryptedLine);
    assert(part2Answer == 10780403063);
    std::cout << "Part 2: " << part2Answer << std::endl;
}
