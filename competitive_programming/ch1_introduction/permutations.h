#include <algorithm>
#include <vector>
using namespace std;
template <typename T>
void all_permutations_of(vector<T> v) {
    int counter = 0;
    do {
        ++counter; // Do as you will here
    } while (next_permutation(v.begin(), v.end()));
    cout << "There are " << counter << " permutations!" << endl;
}
