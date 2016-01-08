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

std::ostream & operator<<(std::ostream & os, const Person & p) {
    os << "(id" << p.id << "|h" << p.height << ",w" << p.weight << ")";
    return os;
}

size_t Person::unique = 0;

bool canStackOnto(const Person & p1, const Person & p2) {
    if (p1 < p2 && !(p2 < p1)) { return true; }
    return false;
}

int main() {
    srand(time(0));
    std::vector<Person> people;
    for (int i = 0; i < NUM_PEOPLE; ++i) {
        people.push_back(Person(rand() % 20, rand() % 20));
    }
    std::sort(people.begin(), people.end());
    auto lastChosen = *people.begin();
    for (auto it = people.begin(); it != people.end(); ++it) {
        if (it != people.begin() ) {
            if (canStackOnto(lastChosen, *it)) {
                std::cout << *it << std::endl;
                lastChosen = *it;
            }
        } else {
            std::cout << *it << std::endl;
        }
    }
    return 0;
}
