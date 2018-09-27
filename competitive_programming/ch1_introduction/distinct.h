#include <set>
#include <vector>
#include <iostream>
using namespace std;
void print_distinct(const vector<int> & v) {
    const set<int> s(v.begin(), v.end());
    for (const int & i : s) { cout << i << " "; }
    cout << endl;
}
