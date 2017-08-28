#include <iostream>
#include <regex>
#include <string>
#include <fstream>
#include <sstream>
#include <cassert>

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

unsigned handleLine(const std::string & encryptedLine) {
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
        const auto & expanderEndPosition = expanderStartPosition + expander.size() - 1;
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

void tests() {
    assert(handleLine("ADVENT") == std::string("ADVENT").size());
    assert(handleLine("A(1x5)BC") == std::string("ABBBBBC").size());
    assert(handleLine("(3x3)XYZ") == std::string("XYZXYZXYZ").size());
    assert(handleLine("A(2x2)BCD(2x2)EFG") == std::string("ABCBCDEFEFG").size());
    assert(handleLine("(6x1)(1x3)A") == std::string("(1x3)A").size());
    assert(handleLine("X(8x2)(3x3)ABCY") == std::string("X(3x3)ABC(3x3)ABCY").size());
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
    std::cout << "Part 1: " << handleLine(encryptedLine) << std::endl;
}
