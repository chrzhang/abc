#include <vector>
using namespace std;
template <typename T>
int search_sorted(const vector<T> & v, const T & needle,
                  const int begin, const int end) {
    if (v.empty() || begin > end) { return -1; }
    const int mid = (begin + end) / 2;
    if (v[mid] == needle) { return mid; }
    else if (v[mid] < needle) { return search_sorted(v, needle, mid + 1, end); }
    else { return search_sorted(v, needle, begin, mid - 1); }
}
