#include <iostream>
#include <list>
#include <string>
#include <cassert>

/*
From http://adventofcode.com/2016/day/19

--- Day 19: An Elephant Named Joseph ---

The Elves contact you over a highly secure emergency channel. Back at the North
Pole, the Elves are busy misunderstanding White Elephant parties.

Each Elf brings a present. They all sit in a circle, numbered starting with
position 1. Then, starting with the first Elf, they take turns stealing all the
presents from the Elf to their left. An Elf with no presents is removed from
the circle and does not take turns.

For example, with five Elves (numbered 1 to 5):

  1
5   2
 4 3

Elf 1 takes Elf 2's present.
Elf 2 has no presents and is skipped.
Elf 3 takes Elf 4's present.
Elf 4 has no presents and is also skipped.
Elf 5 takes Elf 1's two presents.
Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
Elf 3 takes Elf 5's three presents.
So, with five Elves, the Elf that sits starting in position 3 gets all the
presents.

With the number of Elves given in your puzzle input, which Elf gets all the
presents?

--- Part Two ---

Realizing the folly of their present-exchange rules, the Elves agree to instead
steal presents from the Elf directly across the circle. If two Elves are across
the circle, the one on the left (from the perspective of the stealer) is stolen
from. The other rules remain unchanged: Elves with no presents are removed from
the circle entirely, and the other elves move in slightly to keep the circle
evenly spaced.

For example, with five Elves (again numbered 1 to 5):

The Elves sit in a circle; Elf 1 goes first:

  1
5   2
 4 3

Elves 3 and 4 are across the circle; Elf 3's present is stolen, being the one
to the left. Elf 3 leaves the circle, and the rest of the Elves move in:

  1           1
5   2  -->  5   2
 4 -          4

Elf 2 steals from the Elf directly across the circle, Elf 5:

  1         1
-   2  -->     2
  4         4

Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:

 -          2
    2  -->
 4          4

Finally, Elf 2 steals from Elf 4:

 2
    -->  2
 -
So, with five Elves, the Elf that sits starting in position 2 gets all the
presents.

With the number of Elves given in your puzzle input, which Elf now gets all the
presents?
*/

struct Elf {
    size_t index;
    Elf(const size_t index)
    : index(index) {
    }
};

void advanceIt(const size_t amount, std::list<Elf> & elves,
               std::list<Elf>::iterator & it) {
    assert(it != elves.end());
    for (size_t i = 0; i < amount; ++i) {
        it = std::next(it);
        if (it == elves.end()) {
            it = elves.begin();
        }
    }
}

int part1() {
    std::list<Elf> elves;
    const int NUMBER_OF_ELVES = 3001330;
    for (int i = 0; i < NUMBER_OF_ELVES; ++i) {
        elves.push_back(Elf(i + 1));
    }
    std::list<Elf>::iterator currentElfIt = elves.begin();
    // Since std::list::size() is O(n) prior to C++11
    size_t currentNumberOfElves = NUMBER_OF_ELVES;
    while (currentNumberOfElves > 1) {
        std::list<Elf>::iterator nextElfIt = currentElfIt;
        advanceIt(1, elves, nextElfIt);
        assert(nextElfIt != currentElfIt);
        elves.erase(nextElfIt);
        --currentNumberOfElves;
        advanceIt(1, elves, currentElfIt);
    }
    return elves.begin()->index;
}

int part2() {
    std::list<Elf> elves;
    const int NUMBER_OF_ELVES = 3001330;
    for (int i = 0; i < NUMBER_OF_ELVES; ++i) {
        elves.push_back(Elf(i + 1));
    }
    std::list<Elf>::iterator currentElfIt = elves.begin();
    std::list<Elf>::iterator nextElfIt = currentElfIt;
    size_t currentNumberOfElves = NUMBER_OF_ELVES;
    advanceIt(currentNumberOfElves / 2, elves, nextElfIt);
    while (currentNumberOfElves > 1) {
        assert(nextElfIt != currentElfIt);
        std::list<Elf>::iterator nextNextIt = nextElfIt;
        advanceIt(currentNumberOfElves % 2 ? 2 : 1, elves, nextNextIt);
        elves.erase(nextElfIt);
        --currentNumberOfElves;
        nextElfIt = nextNextIt;
        advanceIt(1, elves, currentElfIt);
    }
    return elves.begin()->index;
}

int main() {
    const int part1Answer = part1();
    assert(1808357 == part1Answer);
    std::cout << "Part 1: " << part1Answer << std::endl;
    const int part2Answer = part2();
    assert(1407007 == part2Answer);
    std::cout << "Part 2: " << part2Answer << std::endl;
}
