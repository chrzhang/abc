#include <iostream>
#include <cassert>
#include <vector>

// Find the subset of items where the total value is optimized for a total
// weight under a given constraint

struct Item {
    int weight, value;
    Item(int w, int v) : weight(w), value(v) {}
};

int main() {
    // Store weights and values of items
    std::vector<Item> items = {Item(10, 60), Item(20, 100), Item(30, 120)};
    int capacityOfKnapsack = 50;
    // An entry in table[i, j] is the maximum total value attained for the
    // items up to index i using total weight less than or equal to j
    auto table =
        std::vector<std::vector<int>>(items.size() + 1,
                                      std::vector<int>(capacityOfKnapsack + 1,
                                                       0));
    for (int itemIndex = 1; itemIndex <= (int) items.size(); ++itemIndex) {
        for (int weight = 0; weight <= capacityOfKnapsack; ++weight) {
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
              << table[items.size()][capacityOfKnapsack] << std::endl;
    return 0;
}
