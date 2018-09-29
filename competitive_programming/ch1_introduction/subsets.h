#include <vector>
using namespace std;
template <typename T>
void subset_of_aux(const vector<T> & v, const size_t i, vector<T> tv, int & counter) {
    if (i >= v.size()) {
        ++counter;
        return;
    }
    tv.push_back(v[i]);
    subset_of_aux(v, i + 1, tv, counter);
    tv.pop_back();
    subset_of_aux(v, i + 1, tv, counter);
}

template <typename T>
void subsets_of(const vector<T> & v) {
    vector<T> tv;
    int counter = 0;
    subset_of_aux(v, 0, tv, counter);
    cout << "There are " << counter << " subsets!" << endl;
}
