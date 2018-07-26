#include <iostream>
#include <map>
#include <vector>
#include <algorithm>
#include <cassert>

using namespace std;

struct Elephant {
    int kg, iq, id;
    Elephant(const int kg, const int iq, const int id)
    : kg(kg), iq(iq), id(id) {}
};

ostream & operator<<(ostream & os, const Elephant & elephant) {
    os << elephant.kg << " " << elephant.iq << " #" << elephant.id;
    return os;
}

vector<int> solve(vector<Elephant> elephants) {
    // Solve by sorting elephants into decreasing weight and finding the longest
    // IQ-increasing subsequence.
    sort(elephants.begin(), elephants.end(), [](const Elephant & e1, const Elephant & e2) {
        return e1.kg > e2.kg;
    });
    vector<int> lengths(elephants.size(), 1); // Longest subsequence ending at i
    vector<int> pred(elephants.size(), -1); // For building the chain
    for (int i = 1; i < (int) elephants.size(); ++i) {
        int max_length = 0;
        for (int j = i - 1; j >= 0; --j) {
            // Because of the strict ordering, do not bother comparing
            // elephants of the same weight or IQ.
            if (elephants[j].iq < elephants[i].iq &&
                elephants[j].kg > elephants[i].kg) {
                if (lengths[j] > max_length) {
                    max_length = lengths[j];
                    pred[i] = j;
                }
            }
        }
        lengths[i] = 1 + max_length;
    }
    // Find position of longest chain
    int max_i = 0;
    int max_length = 0;
    for (size_t i = 0; i < lengths.size(); ++i) {
        if (lengths[i] > max_length) {
            max_length = lengths[i];
            max_i = i;
        }
    }
    int current = max_i;
    vector<int> result;
    do {
        result.push_back(elephants[current].id);
        current = pred[current];
    } while (current != -1);
    assert((int) result.size() == max_length);
    return vector<int>(result.rbegin(), result.rend());
}

int main() {
    vector<Elephant> elephants({Elephant(6008, 1300, 1),
                                Elephant(6000, 2100, 2),
                                Elephant(500, 2000, 3),
                                Elephant(1000, 4000, 4),
                                Elephant(1100, 3000, 5),
                                Elephant(6000, 2000, 6),
                                Elephant(8000, 1400, 7),
                                Elephant(6000, 1200, 8),
                                Elephant(2000, 1900, 9)});
    const auto result = solve(elephants);
    cout << result.size() << endl;
    for (const auto e : result) {
        cout << e << endl;
    }
    return 0;
}
