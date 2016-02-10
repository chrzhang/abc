#include <iostream>
#include <cmath>
#include <cassert>

// Check whether a number is a palindrome

bool isPalindrome(int x) {
    int original = x;
    x = abs(x);
    int result = 0;
    while (x) {
        result *= 10;
        result += x % 10;
        x /= 10;
    }
    return result == original;
}

int main() {
    assert(isPalindrome(1));
    assert(isPalindrome(11));
    assert(isPalindrome(121));
    assert(isPalindrome(12321));
    assert(isPalindrome(123321));
    assert(isPalindrome(1233321));
    assert(!isPalindrome(-1233321));
    assert(!isPalindrome(12));
    assert(!isPalindrome(1211));
    return 0;
}
