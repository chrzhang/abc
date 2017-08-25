#include <iostream>
#include <regex>
#include <fstream>
#include <sstream>
#include <string>
#include <array>
#include <algorithm>
#include <cassert>

/*
From http://adventofcode.com/2016/day/4

--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course,
the list is encrypted and full of decoy data, but the instructions to decode
the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes)
followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in
the encrypted name, in order, with ties broken by alphabetization. For example:

aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a
(5), b (3), and then a tie between x, y, and z, which are listed
alphabetically.
a-b-c-d-e-f-g-h-987[abcde] is a real room because although the
letters are all tied (1 of each), the first five are listed alphabetically.
not-a-real-room-404[oarel] is a real room.
totally-real-room-200[decoy] is
not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?

--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get
moving.

The room names are encrypted by a state-of-the-art shift cipher, which is
nearly unbreakable without the right software. However, the information kiosk
designers at Easter Bunny HQ were not expecting to deal with a master
cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a
number of times equal to the room's sector ID. A becomes B, B becomes C, Z
becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?
*/

std::string get5MostCommonCharacters(const std::string & someString) {
    std::array<int, 26> counts = { 0 };
    for (const auto & c : someString) {
        if (c >= 'a' && c <= 'z') {
            counts[c - 'a'] += 1;
        }
    }
    std::string alphabet = "abcdefghijklmnopqrstuvwxyz";
    std::sort(alphabet.begin(), alphabet.end(), [counts](char a, char b) {
        if (counts[a - 'a'] == counts[b - 'a']) {
            return  a < b;
        }
        return counts[a - 'a'] > counts[b - 'a'];
    });
    std::string result = "";
    for (int i = 0; i < 5; ++i) {
        result += alphabet[i];
    }
    return result;
}

bool match(const std::string & s1, const std::string & s2) {
    std::string copyS1(s1);
    std::string copyS2(s2);
    std::sort(copyS1.begin(), copyS1.end());
    std::sort(copyS2.begin(), copyS2.end());
    return copyS1 == copyS2;
}

int toInt(const std::string & someString) {
    std::stringstream ss(someString);
    int result;
    if (!(ss >> result)) {
        std::cerr << "Could not convert " << someString << " to int\n";
        return 0;
    }
    return result;
}

int part1(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << filename << " cannot be opened\n";
        return 1;
    }
    std::string currentLine = "";
    std::regex lineRegex("^([-a-z]+)([0-9]+)\\[(.*)\\]$");
    int sectorIDTotal = 0;
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        std::regex_search(currentLine, result, lineRegex);
        if (4 != result.size()) {
            std::cerr << "Line could not be interpreted by regex\n";
            return 1;
        }
        const std::string & encryptedName = result[1];
        const std::string & sectorID = result[2];
        const std::string & checksum = result[3];
        if (match(get5MostCommonCharacters(encryptedName), checksum)) {
            sectorIDTotal += toInt(sectorID);
        }
    }
    assert(sectorIDTotal == 158835);
    std::cout << "Part 1: " << sectorIDTotal << std::endl;
    return 0;
}

std::string decrypt(const std::string & encryptedString,
                    const int rotationAmount) {
    std::string result = "";
    for (const char & c : encryptedString) {
        if (c == '-') {
            result += ' ';
        } else if (c >= 'a' && c <= 'z') {
            int netRotation = rotationAmount % 26;
            const char rotatedChar = c + netRotation;
            assert(rotatedChar < 'z' + 26);
            if (rotatedChar > 'z') {
                result += 'a' + (rotatedChar - 'z' - 1);
            } else {
                result += rotatedChar;
            }
        } else {
            std::cerr << "Cannot rotate " << c << std::endl;
            return "";
        }
    }
    return result;
}

int part2(const char * filename) {
    std::ifstream inputFile(filename);
    if (!inputFile.is_open()) {
        std::cerr << filename << " cannot be opened\n";
        return 1;
    }
    std::string currentLine = "";
    std::regex lineRegex("^([-a-z]+)([0-9]+).*$");
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        std::regex_search(currentLine, result, lineRegex);
        if (3 != result.size()) {
            std::cerr << "Line could not be interpreted by regex\n";
            return 1;
        }
        const std::string & encryptedName = result[1];
        const std::string & sectorID = result[2];
        int rotationAmount = toInt(sectorID);
        if (decrypt(encryptedName, rotationAmount) == "northpole object storage ") {
            assert(rotationAmount == 993);
            std::cout << "Part 2: " << rotationAmount << std::endl;
            return 0;
        }
    }
    return 0;
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    part1(argv[1]);
    assert(decrypt("qzmt-zixmtkozy-ivhz", 343) == "very encrypted name");
    part2(argv[1]);
}
