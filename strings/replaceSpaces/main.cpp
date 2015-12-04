#include <stdio.h>

#define TEST_STR1 " Hello, w orld! "
#define REPLACEMENT "%20"

// Replace all instances of spaces with %20 assuming string is big enough

void replaceSpaces(char * str, int length) {
    char * p = str;
    size_t final_len = 0;
    while (*p) {
        if (*p == ' ') {
            final_len += (sizeof REPLACEMENT) / (sizeof REPLACEMENT[0]) - 1;
        } else {
            ++final_len;
        }
        ++p;
    }
    int final_index = final_len - 1;
    int end_index = length - 1;
    while (final_index >= 0) {
        if (str[end_index] == ' ') {
            str[final_index] = '0';
            --final_index;
            str[final_index] = '2';
            --final_index;
            str[final_index] = '%';
        } else {
            str[final_index] = str[end_index];
        }
        --end_index;
        --final_index;
    }
}

int main() {
    char str1[100] = TEST_STR1;
    int sizeStr1 = (sizeof TEST_STR1) / (sizeof TEST_STR1[0]);
    replaceSpaces(str1, sizeStr1 - 1); // True length ignores the null escape
    printf("Replacing spaces in |%s| yields |%s|\n", TEST_STR1, str1);
    return 0;
}
