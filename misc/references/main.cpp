#include <cassert>

char & func(char * c_ptr) {
    *c_ptr = 'z';
    return *c_ptr;
}

void mod(char & arg) {
    arg = 'y';
}

int main() {
    // Uninitialized reference is not allowed
    // char & c;
    // Changing a reference is not allowed
    char d = 'd';
    char e = 'e';
    char & d_r = d;
    d_r = e;
    d_r = 'f'; // Try to change re-seated reference
    assert(e == 'e'); // It didn't work
    // Null references cannot exist in a well-defined program because
    // de-referencing it would be the undefined de-referencing of a null pointer
    // Modify variable via a pointer and return as a reference
    char c = 'a';
    char & c_r = func(&c);
    assert(c_r == 'z'); // Make sure char was modified
    c_r = 'b'; // Make sure return value is still a reference
    assert(c == 'b');
    // Modify variable via a reference
    char g = 'g';
    mod(g);
    assert(g == 'y');
    return 0;
}
