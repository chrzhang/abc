#include <iostream>
#include <vector>
#include <string>
#include <cassert>

#define NUM_PAIRS 5

using namespace std;

// Build all valid arrangements of N pairs of ()

// no - number of opening (s left
// nc - number of closing )s left
void gaux(const int no, const int nc, const string & s,
          vector<string> & result) {
	if (no + nc == 0) {
		result.push_back(s);
		return;
	}
	const int bal = nc - no; // Since no - nc == bal
	assert(bal >= 0);
	if (nc && bal > 0) {
		gaux(no, nc - 1, s + ")", result);
	}
	if (no) {
		gaux(no - 1, nc, s + "(", result);
	}
}

vector<string> generateParenthesis(int n) {
	vector<string> result;
	gaux(n, n, "", result);
	return result;
}

int main() {
    for (int n = 0; n <= NUM_PAIRS; ++n) {
        std::cout << "Arrangements of " << n << " pairs:\n";
        for (const auto & a : generateParenthesis(n)) {
            cout << a << endl;
        }
    }
    return 0;
}
