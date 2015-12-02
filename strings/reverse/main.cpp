#include <stdio.h>
#include <cstring>
#include <assert.h>
#define TEST_STR "Hello, world!"

// Reverse a null-terminated string

void swap(char * l, char * r) {
    char temp = *l;
    *l = *r;
    *r = temp;
}

void reverse0(char * s) {
    size_t len = 0;
    char * p = s;
    while (*p) {
        ++len;
        ++p;
    }
    if (len <= 1) {
        return;
    }
    for (size_t i = 0; i < len / 2; ++i) {
        size_t j = (len - 1) - i;
        swap(s + i, s + j);
    }
}

void reverse1(char * s) { // Doesn't need to store length
    char * p, * q;
    p = q = s;
    while (*p) {
        ++p;
    }
    --p; // Last character
    if (p <= s) {
        return;
    }
    while (q < p) {
        swap(q, p);
        --p;
        ++q;
    }
}

int main() {
    { // reverse1 test
        char str[] = TEST_STR;
        reverse1(str);
        printf("%s\n", str);
        reverse1(str);
        assert(0 == strcmp(str, TEST_STR)); // Does not test case of palindromes
    }
    { // reverse0 test
        char str[] = TEST_STR;
        reverse0(str);
        printf("%s\n", str);
        reverse0(str);
        assert(0 == strcmp(str, TEST_STR)); // Does not test case of palindromes
    }
    return 0;
}
