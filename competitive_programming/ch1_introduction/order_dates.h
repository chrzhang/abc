#include <utility>
#include <vector>
#include <algorithm>
using namespace std;
struct Birthdate {
    short y, m, d;
    Birthdate(const short y, const short m, const short d)
    : y(y), m(m), d(d) {}
};
inline bool operator<(const Birthdate & b1, const Birthdate & b2) {
    return tie(b1.m, b1.d, b1.y) < tie(b2.m, b2.d, b2.y);
}
void my_sort(vector<Birthdate> & v) {
    sort(v.begin(), v.end());
}
