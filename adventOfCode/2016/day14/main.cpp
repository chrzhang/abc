#include <iostream>
#include <string>
#include <map>
#include <cassert>
#include <cstring>
#include <openssl/md5.h>

static const std::string salt = "ahsbgdzn";
static const int HASH_LEN = 33;

/*
From http://adventofcode.com/2016/day/14

--- Day 14: One-Time Pad ---

In order to communicate securely with Santa while you're on this mission,
you've been using a one-time pad that you generate using a pre-agreed
algorithm. Unfortunately, you've run out of keys in your one-time pad, and so
you need to generate some more.

To generate keys, you first get a stream of random data by taking the MD5 of a
pre-arranged salt (your puzzle input) and an increasing integer index (starting
with 0, and represented in decimal); the resulting MD5 hash should be
represented as a string of lowercase hexadecimal digits.

However, not all of these MD5 hashes are keys, and you need 64 new keys for
your one-time pad. A hash is a key only if:

It contains three of the same character in a row, like 777. Only consider the
first such triplet in a hash.  One of the next 1000 hashes in the stream
contains that same character five times in a row, like 77777.  Considering
future hashes for five-of-a-kind sequences does not cause those hashes to be
skipped; instead, regardless of whether the current hash is a key, always
resume testing for keys starting with the very next hash.

For example, if the pre-arranged salt is abc:

The first index which produces a triple is 18, because the MD5 hash of abc18
contains ...cc38887a5.... However, index 18 does not count as a key for your
one-time pad, because none of the next thousand hashes (index 19 through index
1018) contain 88888.
The next index which produces a triple is 39; the hash of abc39 contains eee.
It is also the first key: one of the next thousand hashes (the one at index
816) contains eeeee.
None of the next six triples are keys, but the one after that, at index 92, is:
it contains 999 and index 200 contains 99999.
Eventually, index 22728 meets all of the criteria to generate the 64th key.
So, using our example salt of abc, index 22728 produces the 64th key.

Given the actual salt in your puzzle input, what index produces your 64th
one-time pad key?

--- Part Two ---

Of course, in order to make this process even more secure, you've also
implemented key stretching.

Key stretching forces attackers to spend more time generating hashes.
Unfortunately, it forces everyone else to spend more time, too.

To implement key stretching, whenever you generate a hash, before you use it,
you first find the MD5 hash of that hash, then the MD5 hash of that hash, and
so on, a total of 2016 additional hashings. Always use lowercase hexadecimal
representations of hashes.

For example, to find the stretched hash for index 0 and salt abc:

Find the MD5 hash of abc0: 577571be4de9dcce85a041ba0410f29f.
Then, find the MD5 hash of that hash: eec80a0c92dc8a0777c619d9bb51e910.
Then, find the MD5 hash of that hash: 16062ce768787384c81fe17a7a60c7e3.
...repeat many times...
Then, find the MD5 hash of that hash: a107ff634856bb300138cac6568c0f24.
So, the stretched hash for index 0 in this situation is a107ff.... In the end,
you find the original hash (one use of MD5), then find the
hash-of-the-previous-hash 2016 times, for a total of 2017 uses of MD5.

The rest of the process remains the same, but now the keys are entirely
different. Again for salt abc:

The first triple (222, at index 5) has no matching 22222 in the next thousand
hashes.
The second triple (eee, at index 10) hash a matching eeeee at index 89, and so
it is the first key.
Eventually, index 22551 produces the 64th key (triple fff with matching fffff
at index 22859.

Given the actual salt in your puzzle input and using 2016 extra MD5 calls of
key stretching, what index now produces your 64th one-time pad key?
*/

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

char getCharRepeatedThrice(const std::string & md5hash) {
    assert(md5hash.size() == HASH_LEN);
    for (int ii = 0; ii < HASH_LEN - 2; ++ii) {
        if (md5hash[ii] == md5hash[ii + 1] &&
            md5hash[ii] == md5hash[ii + 2]) {
            return md5hash[ii];
        }
    }
    return 0;
}

bool containsFiveOf(const char c, const std::string & md5hash) {
    assert(md5hash.size() == HASH_LEN);
    static const int five = 5;
    return md5hash.find(std::string(five, c)) != std::string::npos;
}

const std::string getHash(const int currentIndex,
                          std::map<int, std::string> & hashes,
                          const int iterations = 1) {
    const auto seekHash = hashes.find(currentIndex);
    if (seekHash == hashes.end()) {
        const std::string saltSuffixedWithIndex = salt + std::to_string(currentIndex);
        char md5hash[HASH_LEN];
        md5(saltSuffixedWithIndex.c_str(), md5hash);
        for (int iter = 1; iter < iterations; ++iter) {
            char temp[HASH_LEN];
            strncpy(temp, md5hash, HASH_LEN);
            md5(temp, md5hash);
        }
        const std::string hash(md5hash, HASH_LEN);
        hashes[currentIndex] = hash;
        return hash;
    } else {
        return seekHash->second;
    }
}

int part1() {
    int counter = 0;
    std::map<int, std::string> hashes;
    int result = -1;
    for (int currentIndex = 0; counter <= 64; ++currentIndex) {
        const std::string & currentHash = getHash(currentIndex, hashes);
        const char charRepeatedThrice = getCharRepeatedThrice(currentHash);
        if (0 != charRepeatedThrice) {
            for (int nextIndex = currentIndex + 1; nextIndex <= currentIndex + 1000; ++nextIndex) {
                const std::string & nextHash = getHash(nextIndex, hashes);
                if (containsFiveOf(charRepeatedThrice, nextHash)) {
                    ++counter;
                    if (64 == counter) {
                        result = currentIndex;
                        break;
                    }
                }
            }
        }
        hashes.erase(currentIndex);
    }
    return result;
}

int part2() {
    int counter = 0;
    std::map<int, std::string> hashes;
    int result = -1;
    for (int currentIndex = 0; counter <= 64; ++currentIndex) {
        const std::string & currentHash = getHash(currentIndex, hashes, 2017);
        const char charRepeatedThrice = getCharRepeatedThrice(currentHash);
        if (0 != charRepeatedThrice) {
            for (int nextIndex = currentIndex + 1; nextIndex <= currentIndex + 1000; ++nextIndex) {
                const std::string & nextHash = getHash(nextIndex, hashes, 2017);
                if (containsFiveOf(charRepeatedThrice, nextHash)) {
                    ++counter;
                    if (64 == counter) {
                        result = currentIndex;
                        break;
                    }
                }
            }
        }
        hashes.erase(currentIndex);
    }
    return result;
}

int main() {
    const int part1Result = part1();
    assert(part1Result == 23890);
    std::cout << "Part 1: " << part1Result << std::endl;
    const int part2Result = part2();
    assert(part2Result == 22696);
    std::cout << "Part 2: " << part2Result << std::endl;
    return 0;
}
