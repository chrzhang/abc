#include <iostream>
#include <cassert>
#include <fstream>
#include <cmath>

using namespace std;

bool isItOn(const unsigned int nn) {
    // Switch gets toggled for every factor it has
    // To count factors, take advantage of the fact that there
    // are an equal number of factors on either side of the square root
    // Because every factor is in a pair, the switch is toggled twice and
    // nothing happens
    // This problem then boils down to looking at the square root (which has
    // no pair)
    // If the sqrt is integral, then there was one toggle and the switch is on
    return sqrt(nn) == (int) sqrt(nn);
}

string solve(const unsigned int nn) {
    return isItOn(nn) ? "yes" : "no";
}

int main() {
    assert("no" == solve(3));
    assert("yes" == solve(6241));
    assert("no" == solve(8191));
    assert("no" == solve(4294967295));
    assert("yes" == solve(4294705156));
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
