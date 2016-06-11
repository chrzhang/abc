#include <cassert>

// Overload operators

struct Obj {
    int i;
    Obj(int j) : i(j) {}
    Obj operator+(const Obj & o) {
        Obj tempObj(i);
        tempObj.i += o.i;
        return tempObj;
    }
    Obj & operator++() { // Prefix
        ++i;
        return *this;
    }
    Obj operator++(int) { // Postfix
        Obj temp = *this;
        ++*this;
        return temp; // Return non-inc version
    }
};


int main() {
    Obj o_1(1);
    Obj o_2(2);
    Obj o_3 = o_1 + o_2;
    assert(o_3.i == 3);
    o_1++;
    assert(o_1.i == 2);
    ++o_1;
    assert(o_1.i == 3);
    return 0;
}
