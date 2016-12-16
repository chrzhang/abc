#include <iostream>
#include <string>
#include <cassert>
#include <vector>

/*
From http://adventofcode.com/2015/day/11

--- Day 11: Corporate Policy ---

Santa's previous password expired, and he needs help choosing a new one.

To help him remember his new password after the old one expires, Santa has
devised a method of coming up with a password based on the previous one.
Corporate policy dictates that passwords must be exactly eight lowercase
letters (for security reasons), so he finds his new password by incrementing
his old password string repeatedly until it is valid.

Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
Increase the rightmost letter one step; if it was z, it wraps around to a, and
repeat with the next letter to the left until one doesn't wrap around.

Unfortunately for Santa, a new Security-Elf recently started, and he has
imposed some additional password requirements:

Passwords must include one increasing straight of at least three letters, like
abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't
count.  Passwords may not contain the letters i, o, or l, as these letters can
be mistaken for other characters and are therefore confusing.  Passwords must
contain at least two different, non-overlapping pairs of letters, like aa, bb,
or zz.  For example:

hijklmmn meets the first requirement (because it contains the straight hij) but
fails the second requirement requirement (because it contains i and l).
abbceffg meets the third requirement (because it repeats bb and ff) but fails
the first requirement.  abbcegjk fails the third requirement, because it only
has one double letter (bb).  The next password after abcdefgh is abcdffaa.  The
next password after ghijklmn is ghjaabcc, because you eventually skip all the
passwords that start with ghi..., since i is not allowed.  Given Santa's
current password (your puzzle input), what should his next password be?

--- Part Two ---

Santa's password expired again. What's the next one?
*/

const int PW_SIZE = 8;

int toCode(char c) {
    assert(c >= 'a' && c <= 'z');
    return c - 'a';
}

char toChar(int c) {
    assert(c >= 0 && c <= 26);
    return c + 'a';
}

void increment(std::string & password) {
    assert(password.size() == PW_SIZE);
    int carry = 0;
    int index = PW_SIZE - 1;
    do {
        char & c = password[index];
        int colSum = toCode(c) + carry;
        if (index == PW_SIZE - 1) {
            colSum += 1;
        }
        carry = colSum / 26;
        c = toChar(colSum % 26);
        --index;
    } while (carry && PW_SIZE >= 0);
}

bool hasIncreasingStraight(const std::string & password) {
    assert(password.size() == PW_SIZE);
    const int MIN_STRAIGHT_LEN = 3;
    for (size_t ii = 0; ii < password.size() - MIN_STRAIGHT_LEN + 1; ++ii) {
        bool foundStraight = true;
        for (size_t jj = ii + 1; jj < ii + MIN_STRAIGHT_LEN; ++jj) {
            if (password[jj] != password[jj - 1] + 1) {
                foundStraight = false;
            }
        }
        if (foundStraight) {
            return true;
        }
    }
    return false;
}

bool doesNotContainIOL(const std::string & password) {
    assert(password.size() == PW_SIZE);
    for (auto x : password) {
        switch (x) {
            case 'i':
            case 'o':
            case 'l':
                return false;
            default:
                break;
        }
    }
    return true;
}

// With each pair found, compare it with all past pairs and if they do not
// overlap, return true
bool consider(std::vector<std::pair<int, int>> & allMatchingPairsIndices,
              int i1, int i2, const std::string & password) {
    for (auto p : allMatchingPairsIndices) {
        if (p.first == i1 ||
            p.first == i2 ||
            p.second == i1 ||
            p.second == i2) {
            allMatchingPairsIndices.push_back({i1, i2});
            return false;
        }
    }
    for (auto p : allMatchingPairsIndices) {
        if (password[p.first] != password[i1]) {
            return true;
        }
    }
    allMatchingPairsIndices.push_back({i1, i2});
    return false;
}

bool hasTwoPairs(const std::string & password) {
    assert(password.size() == PW_SIZE);
    // Find all pairs of matching letters and store their indices
    std::vector<std::pair<int, int>> allMatchingPairsIndices;
    for (size_t ii = 0; ii < password.size() - 1; ii += 2) {
        if (password[ii] == password[ii + 1]) {
            if (consider(allMatchingPairsIndices, ii, ii + 1, password)) {
                return true;
            }
        }
    }
    for (size_t ii = 1; ii < password.size() - 2; ii += 2) {
        if (password[ii] == password[ii + 1]) {
            if (consider(allMatchingPairsIndices, ii, ii + 1, password)) {
                return true;
            }
        }
    }
    return false;
}

bool passesRequirements(const std::string & password) {
    assert(password.size() == PW_SIZE);
    return hasIncreasingStraight(password) &&
           doesNotContainIOL(password) &&
           hasTwoPairs(password);
}

void run(std::string & password) {
    do {
        increment(password);
    } while (!passesRequirements(password));
}

int main() {
    std::string password = "vzbxkghb";
    run(password);
    std::cout << password << std::endl;
    run(password);
    std::cout << password << std::endl;
    return 0;
}
