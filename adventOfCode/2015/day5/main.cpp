#include <iostream>
#include <fstream>
#include <string>
#include <cctype>
#include <set>
#include <map>

/*
From http://adventofcode.com/2015/day/5

--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or
nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or
aeiouaeiouaeiou.  It contains at least one letter that appears twice in a row,
like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).  It does not contain the
strings ab, cd, pq, or xy, even if they are part of one of the other
requirements.  For example:

ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a
double letter (...dd...), and none of the disallowed substrings.  aaa is nice
because it has at least three vowels and a double letter, even though the
letters used by different rules overlap.  jchzalrnumimnmhp is naughty because
it has no double letter.  haegwjzuvuyypxyu is naughty because it contains the
string xy.  dvszwmarrgswjxmb is naughty because it contains only one vowel.
How many strings are nice?

--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of
determining whether a string is naughty or nice. None of the old rules apply,
as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

It contains a pair of any two letters that appears at least twice in the string
without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa,
but it overlaps).  It contains at least one letter which repeats with exactly
one letter between them, like xyx, abcdefeghi (efe), or even aaa.  For example:

qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a
letter that repeats with exactly one letter between them (zxz).  xxyxx is nice
because it has a pair that appears twice and a letter that repeats with one
between, even though the letters used by each rule overlap.  uurcxstgmygtbstg
is naughty because it has a pair (tg) but no repeat with a single letter
between them.  ieodomkazucvgmuy is naughty because it has a repeating letter
with one between (odo), but no pair that appears twice.  How many strings are
nice under these new rules?
*/

bool is_vowel(const char c) {
    static const std::string vowels = "aeiou";
    return vowels.find(c) != std::string::npos;
}

struct Part1 {
    static bool is_bad_pair(const std::string & s) {
        static const std::set<std::string> bad_pairs = {"ab", "cd", "pq", "xy"};
        return bad_pairs.find(s) != bad_pairs.end();
    }
    static bool is_nice(const std::string & s) {
        unsigned vowel_count = 0;
        bool has_two_in_a_row_letter = false;
        for (auto it = s.begin(); it != s.end(); ++it) {
            if (is_vowel(*it)) {
                vowel_count += 1;
            }
            auto next_it = std::next(it);
            if (next_it != s.end()) {
                if (*next_it == *it) {
                    has_two_in_a_row_letter = true;
                }
                std::string two_letters;
                two_letters += *it;
                two_letters += *next_it;
                if (is_bad_pair(two_letters)) {
                    return false;
                }
            }
        }
        return vowel_count >= 3 && has_two_in_a_row_letter;
    }
};

struct Part2 {
    static bool add_to_set(std::set<size_t> & indices_set,
                           size_t i) {
        bool non_overlap = false;
        for (auto element : indices_set) {
            if (i - element > 1) {
                non_overlap = true;
            }
        }
        indices_set.insert(i);
        return non_overlap;
    }
    static bool is_nice(const std::string & s) {
        std::map<std::pair<char, char>, std::set<size_t>> pairs_to_indices;
        bool has_pair = false;
        for (size_t i = 0; i + 1 < s.size() && !has_pair; i += 1) {
            bool non_overlap =
                add_to_set(pairs_to_indices[{s[i], s[i + 1]}], i);
            if (non_overlap) {
                has_pair = true;
            }
        }
        bool has_between = false;
        for (size_t i = 0; i < s.size() && !has_between; i += 1) {
            if (i + 2 >= s.size()) {
                break;
            }
            if (s[i] == s[i + 2]) {
                has_between = true;
            }
        }
        return has_pair && has_between;
    }
};

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << "File not opened\n";
        return 1;
    }
    unsigned nice_count1 = 0;
    unsigned nice_count2 = 0;
    std::string line;
    while (std::getline(f, line)) {
        if (Part1::is_nice(line)) {
            ++nice_count1;
        }
        if (Part2::is_nice(line)) {
            ++nice_count2;
        }
    }
    std::cout << "Part 1: " << nice_count1 << " strings are nice.\n";
    std::cout << "Part 2: " << nice_count2 << " strings are nice.\n";
    return 0;
}
