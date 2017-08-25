#include <iostream>
#include <string>
#include <sstream>
#include <cassert>
#include <cstring>
#include <openssl/md5.h>

/*
From http://adventofcode.com/2016/day/5

--- Day 5: How About a Nice Game of Chess? ---

You are faced with a security door designed by Easter Bunny engineers that seem
to have acquired most of their security knowledge by watching hacking movies.

The eight-character password for the door is generated one character at a time
by finding the MD5 hash of some Door ID (your puzzle input) and an increasing
integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal
representation starts with five zeroes. If it does, the sixth character in the
hash is the next character of the password.

For example, if the Door ID is abc:

The first index which produces a hash that starts with five zeroes is 3231929,
which we find by hashing abc3231929; the sixth character of the hash, and thus
the first character of the password, is 1.  5017308 produces the next
interesting hash, which starts with 000008f82..., so the second character of
the password is 8.  The third time a hash starts with five zeroes is for
abc5278568, discovering the character f.  In this example, after continuing
this search a total of eight times, the password is 18f47a30.

rt Two ---

As the door slides open, you are presented with a second door that uses a
slightly more inspired security mechanism. Clearly unimpressed by the last
version (in what movie is the password decrypted in order?!), the Easter Bunny
engineers have worked out a better solution.

Instead of simply filling in the password from left to right, the hash now also
indicates the position within the password to fill. You still look for hashes
that begin with five zeroes; however, now, the sixth character represents the
position (0-7), and the seventh character is the character to put in that
position.

A hash result of 000001f means that f is the second character in the password.
Use only the first result for each position, and ignore invalid positions.

For example, if the Door ID is abc:

The first interesting hash is from abc3231929, which produces 0000015...; so, 5
goes in position 1: _5______.  In the previous method, 5017308 produced an
interesting hash; however, it is ignored, because it specifies an invalid
position (8).  The second interesting hash is at index 5357525, which produces
000004e...; so, e goes in position 4: _5__e___.  You almost choke on your
popcorn as the final character falls into place, producing the password
05ace8e3.

Given the actual Door ID and this new method, what is the password? Be extra
proud of your solution if it uses a cinematic "decrypting" animation.  Given
the actual Door ID, what is the password?
*/

bool beginsWith5Zeroes(const char * someString) {
    // If someString does not have 5 readable bytes, behavior is undefined
    for (int i = 0; i < 5; ++i) {
        if (someString[i] != '0') {
            return false;
        }
    }
    return true;
}

void md5(const char * msg, char * result) {
    unsigned char digest[16];
    MD5_CTX md5_ctx;
    MD5_Init(&md5_ctx);
    MD5_Update(&md5_ctx, msg, strlen(msg));
    MD5_Final(digest, &md5_ctx);
    for (int i = 0; i < 16; ++i) {
        sprintf(&result[i*2], "%02x", (unsigned int) digest[i]);
    }
}

int part1() {
    char hashResult[33] = { 0 };
    const std::string input = "reyedfim";
    int characterIndex = 0;
    std::string password = "";
    for (int increasingIntegerIndex = 0; characterIndex < 8; ++increasingIntegerIndex) {
        std::string thingToHash = input + std::to_string(increasingIntegerIndex);
        md5(thingToHash.c_str(), hashResult);
        if (beginsWith5Zeroes(hashResult)) {
            std::cout << "Decrypting password (" << characterIndex + 1 << " / 8) @ " << increasingIntegerIndex << std::endl;
            password += hashResult[5];
            ++characterIndex;
        }
    }
    assert(password == "f97c354d");
    std::cout << "Part 1: " << password << std::endl;
    return 0;
}

int toInt(const char c) {
    assert(c >= '0' && c <= '9');
    return c - '0';
}

bool allSet(const bool beenSet[8]) {
    for (int i = 0; i < 8; ++i) {
        if (!beenSet[i]) {
            return false;
        }
    }
    return true;
}

int part2() {
    char hashResult[33] = { 0 };
    const std::string input = "reyedfim";
    std::string password(8, '-');
    bool beenSet[8] = { false };
    for (int increasingIntegerIndex = 0; !allSet(beenSet); ++increasingIntegerIndex) {
        std::string thingToHash = input + std::to_string(increasingIntegerIndex);
        md5(thingToHash.c_str(), hashResult);
        if (beginsWith5Zeroes(hashResult)) {
            if (hashResult[5] >= '0' && hashResult[5] <= '7') {
                if (!beenSet[toInt(hashResult[5])]) {
                    std::cout << "Decrypting password ("
                              << hashResult[5] << " / 8) @ "
                              << increasingIntegerIndex << std::endl;
                    beenSet[toInt(hashResult[5])] = true;
                    password[toInt(hashResult[5])] = hashResult[6];
                }
            }
        }
    }
    assert(password == "863dde27");
    std::cout << "Part 2: " << password << std::endl;
    return 0;
}

int main() {
    part1();
    part2();
    return 0;
}
