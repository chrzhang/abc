#include <iostream>

using namespace std;

// Call member functions, default the arguments, and see differences

struct ObjA {
    void f() {}
    void f(char a) {}
    void f(char a, char b) {}
    void f(char a, char b, char c) {}
};

struct ObjB {
    void f(char a = 'x', char b = 'y', char c = 'z') {
        cout << "a = " << a << " b = " << b << " c = " << c << endl;
    }
};

int main(int argc, char * argv[]) {
    ObjA o_a;
    o_a.f();
    o_a.f('a');
    o_a.f('a', 'b');
    o_a.f('a', 'b', 'c');
    ObjB o_b;
    o_b.f();
    o_b.f('a'); // Default arguments work left to right
    o_b.f('a', 'b'); // Only the third argument is defaulted
    o_b.f('a', 'b', 'c'); // No arguments defaulted
    return 0;
}
