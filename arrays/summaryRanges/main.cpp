#include <iostream>
#include <vector>
#include <string>

// Summarize ranges of a sorted array

std::vector<std::string> summaryRanges(const std::vector<int> & nums) {
    std::vector<std::string> result;
    for (auto it = nums.begin(); it != nums.end();) {
        auto b = *it;
        auto walker = next(it);
        if (nums.end() == walker) {
            result.push_back(std::to_string(b));
            return result;
        }
        while (*walker == *prev(walker) + 1) {
            ++walker;
        }
        walker = prev(walker);
        if (b != *walker) {
            result.push_back(std::to_string(b) + "->" +
                             std::to_string(*walker));
        } else {
            result.push_back(std::to_string(b));
        }
        it = next(walker);
    }
    return result;
}

int main() {
    auto r = summaryRanges({ 1, 2, 4, 5, 7 });
    for (auto e : r) {
        std::cout << e << " ";
    }
    std::cout << "\n";
    return 0;
}
