#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdexcept>
#include <vector>
#include <cassert>
#include <algorithm>

/*
From http://adventofcode.com/2015/day/17

--- Day 17: No Such Thing as Too Much ---

The elves bought too much eggnog again - 150 liters this time. To fit it all
into your refrigerator, you'll need to move it into smaller containers. You
take an inventory of the capacities of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
If you need to store 25 liters, there are four ways to do it:

15 and 10
20 and 5 (the first 5)
20 and 5 (the second 5)
15, 5, and 5

Filling all containers entirely, how many different combinations of containers
can exactly fit all 150 liters of eggnog?

--- Part Two ---

While playing with all the containers in the kitchen, another load of eggnog
arrives! The shipping and receiving department is requesting as many containers
as you can spare.

Find the minimum number of containers that can exactly fit all 150 liters of
eggnog. How many different ways can you fill that number of containers and
still hold exactly 150 litres?

In the example above, the minimum number of containers was two. There were
three ways to use that many containers, and so the answer there would be 3.
*/

const int LITERS_OF_EGGNOG = 150;

int toInt(const std::string & s) {
    std::stringstream ss(s);
    int r;
    if (!(ss >> r)) {
        throw std::runtime_error("Cannot interpet " + s + " as a number.");
    }
    return r;
}

void solve(int currentIndex, const std::vector<int> & capacities,
           std::vector<bool> & used, int amountLeft,
           std::vector<std::vector<bool>> & ways) {
    assert(capacities.size() == used.size());
    if (amountLeft == 0) {
        ways.push_back(used);
        return;
    }
    if (currentIndex >= capacities.size()) {
        return;
    }
    if (capacities[currentIndex] <= amountLeft) {
        used[currentIndex] = true;
        solve(currentIndex + 1, capacities, used,
              amountLeft - capacities[currentIndex], ways);
    }
    used[currentIndex] = false;
    solve(currentIndex + 1, capacities, used, amountLeft, ways);
}

int containersUsed(const std::vector<bool> & way) {
    int numContainersUsed = 0;
    for (auto containerUsed : way) {
        if (containerUsed) {
            ++numContainersUsed;
        }
    }
    return numContainersUsed;
}

int minContainersUsed(const std::vector<std::vector<bool>> & ways) {
    int minimum = ways.size() + 1;
    for (auto way : ways) {
        minimum = std::min(minimum, containersUsed(way));
    }
    return minimum;
}

int numWaysUsingNumContainers(const std::vector<std::vector<bool>> & ways,
                              int numContainers) {
    int numWays = 0;
    for (auto way : ways) {
        if (numContainers == containersUsed(way)) {
            ++numWays;
        }
    }
    return numWays;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
    }
    std::string line;
    std::vector<int> capacities;
    std::vector<bool> used;
    while (std::getline(f, line)) {
        capacities.push_back(toInt(line));
        used.push_back(false);
    }
    std::vector<std::vector<bool>> validWays;
    solve(0, capacities, used, LITERS_OF_EGGNOG, validWays);
    std::cout << "There are " << validWays.size() << " to store "
              << LITERS_OF_EGGNOG << " liters of eggnog.\n";
    int minimumContainersNecessary = minContainersUsed(validWays);
    std::cout << "There are "
              << numWaysUsingNumContainers(validWays,
                                           minimumContainersNecessary)
              << " ways to store " << LITERS_OF_EGGNOG
              << " liters of eggnog in the minimum amount of containers.\n";
    return 0;
}
