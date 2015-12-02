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

void reverse(char * s) {
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

int main() {
    char str[] = TEST_STR;
    reverse(str);
    printf("%s\n", str);
    reverse(str);
    assert(0 == strcmp(str, TEST_STR));
    return 0;
}
