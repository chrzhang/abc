#include <iostream>

using namespace std;

// Print lines when ctor, dtor called
// Create object and maintain a member variable

struct Obj {
    int member;
    Obj(int i) {
        member = i;
        cout << "Obj constructor called, member is " << member << ".\n";
    }
    ~Obj() {
        cout << "Obj destructor called, member is " << member << "\n";
    }
};

int main() {
    Obj o(42);
    return 0;
}
