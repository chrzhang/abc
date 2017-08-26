#include <iostream>
#include <fstream>
#include <cassert>
#include <string>
#include <regex>
#include <set>
#include <array>

/*
From http://adventofcode.com/2016/day/7

--- Day 7: Internet Protocol Version 7 ---

While snooping around the local network of EBHQ, you compile a list of IP
addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to
figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
An ABBA is any four-character sequence which consists of a pair of two
different characters followed by the reverse of that pair, such as xyyx or
abba. However, the IP also must not have an ABBA within any hypernet sequences,
which are contained by square brackets.

For example:

abba[mnop]qrst supports TLS (abba outside square brackets).
abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even
though xyyx is outside square brackets).
aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters
must be different).
ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though
it's within a larger string).

How many IPs in your puzzle input support TLS?

--- Part Two ---

You would also like to know which IPs support SSL (super-secret listening).

An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in
the supernet sequences (outside any square bracketed sections), and a
corresponding Byte Allocation Block, or BAB, anywhere in the hypernet
sequences. An ABA is any three-character sequence which consists of the same
character twice with a different character between them, such as xyx or aba. A
corresponding BAB is the same characters but in reversed positions: yxy and
bab, respectively.

For example:

aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab
within square brackets).
xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet;
the aaa sequence is not related, because the interior character must be
different).
zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a
corresponding bzb, even though zaz and zbz overlap).

How many IPs in your puzzle input support SSL?
*/

bool inBrackets(const std::string & someString) {
    assert(someString.size() > 0);
    return !(someString[0] == ']' || someString[someString.size() - 1] == '[');
}

bool hasABBA(const std::string & someString) {
    if (someString.size() < 4) {
        return false;
    }
    for (size_t i = 0; i < someString.size() - 3; ++i) {
        if (someString[i] == someString[i + 3] &&
            someString[i + 1] == someString[i + 2] &&
            someString[i] != someString[i + 1]) {
            return true;
        }
    }
    return false;
}

bool supportsTLS(const std::string & currentLine) {
    std::regex squareBracketsRegex("[\\]]*([^\\[\\]]+)[\\[]*");
    bool foundABBA = false;
    for (auto it = std::sregex_token_iterator(currentLine.begin(),
                                              currentLine.end(),
                                              squareBracketsRegex);
         it != std::sregex_token_iterator(); ++it) {
        if (inBrackets(*it) && hasABBA(*it)) {
            return false;
        }
        if (!(foundABBA)) {
            if (hasABBA(*it)) {
                foundABBA = true;
            }
        }
    }
    return foundABBA;
}

void store010s(const std::string & haystack,
               std::set<std::array<char, 3>> & tripletSet) {
    for (size_t i = 0; i < haystack.size() - 2; ++i) {
        if (haystack[i] == haystack[i + 2] &&
            haystack[i] != haystack[i + 1]) {
            tripletSet.insert({haystack[i], haystack[i + 1], haystack[i + 2]});
        }
    }
}

void storeBABs(const std::string & haystack,
               std::set<std::array<char, 3>> & babSet) {
    store010s(haystack, babSet);
}

void storeABAs(const std::string & haystack,
               std::set<std::array<char, 3>> & abaSet) {
    store010s(haystack, abaSet);
}


bool supportsSSL(const std::string & currentLine) {
    std::regex squareBracketsRegex("[\\]]*([^\\[\\]]+)[\\[]*");
    std::set<std::array<char, 3>> babSet, abaSet;
    for (auto it = std::sregex_token_iterator(currentLine.begin(),
                                              currentLine.end(),
                                              squareBracketsRegex);
         it != std::sregex_token_iterator(); ++it) {
        if (inBrackets(*it)) {
            storeBABs(*it, babSet);
        } else {
            storeABAs(*it, abaSet);
        }
    }
    for (const auto & bab : babSet) {
        if (abaSet.find({bab[1], bab[0], bab[1]}) != abaSet.end()) {
            return true;
        }
    }
    return false;
}

void test() {
    assert(supportsTLS("abba[mnop]qrst"));
    assert(!supportsTLS("abcd[bddb]xyyx"));
    assert(!supportsTLS("aaaa[qwer]tyui"));
    assert(supportsTLS("ioxxoj[asdfgh]zxcvbn"));
}

int main(int argc, char * argv[]) {
    test();
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
    int tlsSupporterCount = 0;
    int sslSupporterCount = 0;
    while (std::getline(inputFile, currentLine)) {
        if (supportsTLS(currentLine)) {
            ++tlsSupporterCount;
        }
        if (supportsSSL(currentLine)) {
            ++sslSupporterCount;
        }
    }
    assert(tlsSupporterCount == 118);
    std::cout << "Part 1: " << tlsSupporterCount << std::endl;
    assert(sslSupporterCount == 260);
    std::cout << "Part 2: " << sslSupporterCount << std::endl;
    return 0;
}
