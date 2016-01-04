#include <iostream>
#include <iomanip>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <string>
#include <assert.h>

// Count and enumerate ways an amount of money can be broken into coins

class Coin {
    size_t val;
    std::string name;
    public:
        Coin(size_t val, std::string name) {
            this->val = val;
            this->name = name;
        }
        size_t getVal() const {
            return val;
        }
        std::string getName() const {
            return name;
        }
};

static const Coin coins[] = { Coin(25, "Quarters"), Coin(10, "Dimes"),
                              Coin(5, "Nickels"), Coin(1, "Pennies") };

void printEnumeration(const std::vector<int> & counts) {
    size_t total = 0;
    for (auto it = counts.begin(); it != counts.end(); ++it) {
        std::cout << std::setw(4) << *it << " "
                  << coins[it - counts.begin()].getName() << " ";
        total += *it * coins[it - counts.begin()].getVal();
    }
    std::cout << "= $" << total / 100.0 << std::endl;
}

void findWaysToMake(size_t amount, size_t & ways, std::vector<int> counts) {
    auto it = counts.begin();
    for (; it != counts.end(); ++it) {
        if (*it == -1) {
            break;
        }
    }
    if (it + 1 == counts.end()) { // Pennies
        *it = amount;
        ++ways;
        printEnumeration(counts);
        return;
    }
    size_t weight = coins[it - counts.begin()].getVal();
    for (int i = 0; i <= amount / weight; ++i) {
        auto countsCopy = counts;
        countsCopy[it - counts.begin()] = i;
        findWaysToMake(amount - (i * weight), ways, countsCopy);
    }
}

int main() {
    srand(time(0));
    size_t amount = rand() % 100;
    std::cout << "Amount: " << amount << std::endl;
    size_t ways = 0;
    findWaysToMake(amount, ways,
                   std::vector<int>((sizeof(coins) / sizeof(Coin)), -1));
    std::cout << ways << " ways." << std::endl;
    return 0;
}
