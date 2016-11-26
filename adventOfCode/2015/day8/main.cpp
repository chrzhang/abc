#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <cassert>

/*
From http://adventofcode.com/2015/day/8

--- Day 8: Matchsticks ---

Space on the sleigh is limited this year, and so Santa will be bringing his
list as a digital copy. He needs to know how much space it will take up when
stored.

It is common in many programming languages to provide a way to escape special
characters in strings. For example, C, JavaScript, Perl, Python, and even PHP
handle special characters in very similar ways.

However, it is important to realize the difference between the number of
characters in the code representation of the string literal and the number of
characters in the in-memory string itself.

For example:

"" is 2 characters of code (the two double quotes), but the string contains
zero characters.  "abc" is 5 characters of code, but 3 characters in the string
data.  "aaa\"aaa" is 10 characters of code, but the string itself contains six
"a" characters and a single, escaped quote character, for a total of 7
characters in the string data.  "\x27" is 6 characters of code, but the string
itself contains just one - an apostrophe ('), escaped using hexadecimal
notation.  Santa's list is a file that contains many double-quoted string
literals, one on each line. The only escape sequences used are \\ (which
represents a single backslash), \" (which represents a lone double-quote
character), and \x plus two hexadecimal characters (which represents a single
character with that ASCII code).

Disregarding the whitespace in the file, what is the number of characters of
code for string literals minus the number of characters in memory for the
values of the strings in total for the entire file?

For example, given the four strings above, the total number of characters of
string code (2 + 5 + 10 + 6 = 23) minus the total number of characters in
memory for string values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.

--- Part Two ---

Now, let's go the other way. In addition to finding the number of characters of
code, you should now encode each code representation as a new string and find
the number of characters of the new encoded representation, including the
surrounding double quotes.

For example:

"" encodes to "\"\"", an increase from 2 characters to 6.  "abc" encodes to
"\"abc\"", an increase from 5 characters to 9.  "aaa\"aaa" encodes to
"\"aaa\\\"aaa\"", an increase from 10 characters to 16.  "\x27" encodes to
"\"\\x27\"", an increase from 6 characters to 11.  Your task is to find the
total number of characters to represent the newly encoded strings minus the
number of characters of code in each original string literal. For example, for
the strings above, the total encoded length (6 + 9 + 16 + 11 = 42) minus the
characters in the original code representation (23, just like in the first part
of this puzzle) is 42 - 23 = 19.
*/

int partOne(char * argv[]) {
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::string line;
    std::regex quotedLineRegex  ("^\"(.*)\"$");
    std::regex escapedRawRegex  ("(\\\\[^x])");
    std::regex escapedAsciiRegex("(\\\\x[0-9a-fA-F]{2})");
    int difference = 0;
    while (std::getline(f, line)) {
        const int charsInCode = line.size();
        { // Check that line begins and ends with "
            std::smatch result;
            std::regex_search(line, result, quotedLineRegex);
            if (result.size() != 2) {
                std::cerr << "Line does not begin and end with \"\n";
                return 1;
            }
            line = result[1];
        }
        int charsInMemory = charsInCode - 2;
        { // Match with escape sequences that are not ASCII codes
            // For every \c, where c is any character but x,
            // \c constitutes only 1 char in memory but takes up
            // 2 chars in code
            charsInMemory -=
                 std::distance(std::sregex_token_iterator(line.begin(),
                                                          line.end(),
                                                          escapedRawRegex, 1),
                               std::sregex_token_iterator());
        }
        { // Match with escaped ASCII codes
          // For every \xab, where a, b is any character,
          // \xab constitues only 1 char in memory but tkaes up
          // 3 chars in code
            charsInMemory -=
                3 * std::distance(std::sregex_token_iterator(line.begin(),
                                                             line.end(),
                                                             escapedAsciiRegex,
                                                             1),
                                  std::sregex_token_iterator());
        }
        difference += charsInCode - charsInMemory;
    }
    std::cout << "Part 1 Difference: " << difference << std::endl;
    return 0;
}

int partTwo(char * argv[]) {
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::string line;
    std::regex quoteOrSlashRegex("([\\\\|\"])");
    int difference = 0;
    while (std::getline(f, line)) {
        difference += 2; // New surrounding quotes
        difference += std::distance(std::sregex_token_iterator(line.begin(),
                                                               line.end(),
                                                               quoteOrSlashRegex,
                                                               1),
                                    std::sregex_token_iterator());
    }
    std::cout << "Part 2 Difference: " << difference << std::endl;
    return 0;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    partOne(argv);
    partTwo(argv);
    return 0;
}
