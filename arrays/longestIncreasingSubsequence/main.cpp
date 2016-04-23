#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>
#include <vector>
#include <algorithm>

#define NUM_PEOPLE 10

// Find highest stack of people (only shorter and lighter people on top)

struct Person {
    static size_t unique;
    size_t height, weight;
    size_t id;
    Person(size_t h, size_t w) : height(h), weight(w) {
        id = ++unique;
    }
};

bool operator<(const Person & p1, const Person & p2) {
    if (p1.height < p2.height &&
        p1.weight < p2.weight) {
        return true;
    }
    return false;
}

struct {
    bool operator()(const Person & p1, const Person & p2) {
        return p1.height < p2.height;
    }
} PersonHeightCompare;

std::ostream & operator<<(std::ostream & os, const Person & p) {
    os << "(id" << p.id << "|h" << p.height << ",w" << p.weight << ")";
    return os;
}

size_t Person::unique = 0;

bool canStackOnto(const Person & p1, const Person & p2) {
    if (p1 < p2 && !(p2 < p1)) { return true; }
    return false;
}

size_t max(size_t s1, size_t s2) {
    return s1 > s2 ? s1 : s2;
}

int main() {
    srand(time(0));
    std::vector<Person> people;
    for (int i = 0; i < NUM_PEOPLE; ++i) {
        people.push_back(Person(rand() % 20, rand() % 20));
    }
    // Sort by height, then find the longest increasing subsequence of weights
    std::sort(people.begin(), people.end(), PersonHeightCompare);
    std::cout << "Everyone:\n";
    for (auto it = people.begin(); it != people.end(); ++it) {
        std::cout << *it << std::endl;
    }
    // Keep track of the longest increasing subsequence ending at each index
    int lissLengths[NUM_PEOPLE];
    int predecessorIndices[NUM_PEOPLE];
    for (int i = 0; i < NUM_PEOPLE; ++i) {
        lissLengths[i] = 1;
        predecessorIndices[i] = -1;
    }
    // For each number, check all its predecessors (if any are less than it)
    // use the pre-calculated result + 1 as a lower bound for the current
    // length of the longest increasing subsequence
    for (auto it = people.begin(); it != people.end(); ++it) {
        auto currX = it->weight;
        for (auto pre = people.begin(); pre != it; ++pre) {
            auto preX = pre->weight;
            if (preX < currX) {
                if (lissLengths[pre - people.begin()] + 1 >
                    lissLengths[it - people.begin()]) {
                    lissLengths[it - people.begin()] =
                        lissLengths[pre - people.begin()] + 1;
                    // Keep track of which predecessor was less than
                    // the current value as a way to enumerate the LISS later
                    predecessorIndices[it - people.begin()] =
                        pre - people.begin();
                }
            }
        }
    }
    int max = -1;
    int indexOfMax = -1;
    for (int i = 0; i < NUM_PEOPLE; ++i) {
        if (lissLengths[i] > max) {
            max = lissLengths[i];
            indexOfMax = i;
        }
    }
    std::cout << "Most people tower, bottom first, tallest/heaviest ->"
              << " shortest/lightest:\n";
    auto currIndex = indexOfMax;
    std::cout << people[currIndex];
    while (predecessorIndices[currIndex] != -1) {
        currIndex = predecessorIndices[currIndex];
        std::cout << ", " << people[currIndex];
    }
    std::cout << std::endl;
    return 0;
}
