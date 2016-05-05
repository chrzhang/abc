#include <iostream>
#include <cassert>
#include <vector>

// Find the subset of items where the total value is optimized for a total
// weight under a given constraint

struct Item {
    int weight, value;
    Item(int w, int v) : weight(w), value(v) {}
};

std::ostream & operator<<(std::ostream & os, const Item & i) {
    os << "(" << i.weight << ", " << i.value << ")";
    return os;
}

void solve(const std::vector<Item> & items, const int & c) {
    int n = items.size();
    // An entry in table[i, j] is the maximum total value attained for the
    // items up to index i using total weight less than or equal to j
    auto table =
        std::vector<std::vector<int>>(n + 1, std::vector<int>(c + 1, 0));
    for (int itemIndex = 1; itemIndex <= n; ++itemIndex) {
        for (int weight = 0; weight <= c; ++weight) {
            if (items[itemIndex - 1].weight > weight) { // Item alone too heavy
                table[itemIndex][weight] = table[itemIndex - 1][weight];
            } else {
                // Find what has more value, not including the current item or
                // including it
                table[itemIndex][weight] =
                    std::max(table[itemIndex - 1][weight],
                             table[itemIndex - 1][weight -
                                                  items[itemIndex - 1].weight] +
                                items[itemIndex - 1].value);
            }
        }
    }
    std::cout << "Maximum value possible: "
              << table[n][c] << std::endl;
    // Backtrace upwards in the table to find the choices made along the way
    int weight = c;
    for (int itemIndex = n; itemIndex > 0; --itemIndex) {
        if (table[itemIndex][weight] != table[itemIndex - 1][weight]) { // Pick
            std::cout << items[itemIndex - 1] << std::endl;
            weight -= items[itemIndex - 1].weight;
        }
    }
}

int main() {
    solve({Item(10, 60), Item(20, 100), Item(30, 120)}, 50);
    solve({Item(1, 1), Item(2, 6), Item(4, 8), Item(5, 9)}, 8);
    return 0;
}
