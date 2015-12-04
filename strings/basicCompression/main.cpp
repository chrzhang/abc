#include <stdio.h>
#include <string.h>
#include <string>
#include <cstring>
#include <cstdlib>
#include <iostream>

#define TEST_STR1 "aabcccccaaa"
//                "a2b1c5a3"
// Compress a string with consecutively alike letters so aaabb becomes a3b2
// If the compressed string is not smaller than the original, return original

void compress(char * s) {
    // Do not replace as we iterate (might end up being longer than original)
    std::string buffer; // For brevity, use std::string to buffer final result
    char * p = s;
    while (*p) { // p stays on a unique letter, q finds count
        char * q = p + 1;
        int count = 1;
        while (*q && *q == *p) {
            ++count;
            ++q;
        }
        buffer.append(*p + std::to_string(count));
        p += count; // Jump to next different letter
    }
    if (buffer.size() > strlen(s)) {
        return; // Don't compress if final result is longer than original
    }
    strcpy(s, buffer.c_str());
}

int main() {
    // Assume the string is big enough (at least twice the length of the
    // original (in the case of abc becoming a1b1c1)
    char str[] = TEST_STR1;
    compress(str);
    printf("Compressed |%s| into |%s|.\n", TEST_STR1, str);
}
