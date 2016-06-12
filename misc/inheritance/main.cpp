#include <iostream>

using namespace std;

// Create a 3-level hierarchy with default ctors and dtors that
// print to cout

struct X {
    X() {
        cout << "X ctor\n";
    }
    ~X() {
        cout << "X dtor\n";
    }
};

struct Y : public X {
    Y() {
        cout << "Y ctor\n";
    }
    ~Y() {
        cout << "Y dtor\n";
    }
};

struct Z : public Y {
    Z() {
        cout << "Z ctor\n";
    }
    ~Z() {
        cout << "Z dtor\n";
    }
};

int main() {
    // Constructors are called from the top-level parent class first to the
    // most derived class
    {
        cout << "Creating top-level X object\n";
        X x;
    }
    {
        cout << "Creating second-level Y object\n";
        Y y;
    }
    {
        cout << "Creating most-derived Z object\n";
        Z z;
    }
    // Destructors are called from the most derived class to the top-level class
    return 0;
}
