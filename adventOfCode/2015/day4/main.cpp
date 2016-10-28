#include <iostream>
#include <openssl/md5.h>
#include <string>
#include <iomanip>
#include <stdio.h>
#include <string.h>

/*
From http://adventofcode.com/2015/day/4

--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins, you
must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
that produces such a hash.

For example:

If your secret key is abcdef, the answer is 609043, because the MD5 hash of
abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
such number to do so.  If your secret key is pqrstuv, the lowest number it
combines with to make an MD5 hash starting with five zeroes is 1048970; that
is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

--- Part Two ---

Now find one that starts with six zeroes.
*/

const std::string secret_key = "ckczppom";

void md5(const char * msg, char * result) {
    unsigned char digest[16];
    MD5_CTX md5_ctx;
    MD5_Init(&md5_ctx);
    MD5_Update(&md5_ctx, msg, strlen(msg));
    MD5_Final(digest, &md5_ctx);
    for(int i = 0; i < 16; ++i) {
        sprintf(&result[i*2], "%02x", (unsigned int)digest[i]);
    }
}

void solve(unsigned num_leading_zeroes) {
    unsigned curr_suffix = 1;
    for (;;) {
        std::string input = secret_key + std::to_string(curr_suffix++);
        const char * string = input.c_str();
        char result[33];
        md5(string, result);
        unsigned i = 0;
        while (i < num_leading_zeroes) {
            if (result[i] == '0') {
                ++i;
            } else {
                break;
            }
        }
        if (i == num_leading_zeroes) {
            std::cout << "Suffix causing " << num_leading_zeroes
                      << " zeroes in hash: " << curr_suffix - 1 << std::endl;
            break;
        }
    }
}

int main() {
    solve(5);
    solve(6);
    return 0;
}
