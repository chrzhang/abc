#include <cassert>
#define F1 (x) (x + 1)
#define F2(x) (x + 1)

// Correct macro

int main() {
    assert(2 == F2(1));
    assert(0 == F2(-1));
    return 0;
}
