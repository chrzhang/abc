#include <cassert>

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
    return 0;
}
