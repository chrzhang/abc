#include <iostream>
#include <cassert>
#include <fstream>

using namespace std;

bool isItOn(const unsigned int nn) {
    bool isItOn = false;
    for (unsigned int ii = 1; ii <= nn; ++ii) {
        if (nn % ii == 0) {
            isItOn = !isItOn;
        }
    }
    return isItOn;
}

string solve(const unsigned int nn) {
    return isItOn(nn) ? "yes" : "no";
}

int main() {
    assert("no" == solve(3));
    assert("yes" == solve(6241));
    assert("no" == solve(8191));
    ifstream inFile("input.txt");
    unsigned int xx;
    while (inFile >> xx) {
        if (xx == 0) {
            break;
        }
        cout << solve(xx) << endl;
    }
    return 0;
}
