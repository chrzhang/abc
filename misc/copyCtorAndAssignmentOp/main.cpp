#include <iostream>
#include <string>
#include <cassert>

using namespace std;

// Create two classes with copy constructors and assignment operators
// Have a derived class from one of the classes

struct Traveler {
    string str;
    Traveler(const string & s) : str(s) {}
    Traveler(const Traveler & otherTraveler) {
        str = otherTraveler.str;
    }
    Traveler & operator=(const Traveler & otherTraveler) {
        str = otherTraveler.str;
        return *this;
    }
};

struct Pager {
    string str;
    Pager(const string & s) : str(s) {}
    Pager(const Pager & otherPager) {
        str = otherPager.str;
    }
    Pager & operator=(const Pager & otherPager) {
        str = otherPager.str;
        return *this;
    }
};

struct BusinessTraveler : public Traveler {
    Pager pag;
    BusinessTraveler(const string & s) : Traveler(s), pag(s) {}
};

int main() {
    Traveler t("hello");
    Traveler tCopy(t); // Copy ctor
    assert(tCopy.str == t.str);
    Traveler tAssign("world");
    tAssign = t; // Assignment operator
    assert(tAssign.str == t.str);
    BusinessTraveler bt("foo");
    assert(bt.pag.str == "foo");
    assert(bt.str == "foo");
    return 0;
}
