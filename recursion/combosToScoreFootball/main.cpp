#include <iostream>

// Football scores increment by either 2, 3, or 7 points. Find all possible
// combinations of scores which add up to that score.

void findCombosThatScore(int target) {
    std::cout << "-----\nCombos that score to " << target << "\n";
    if (target < 0) { return; }
    for (int num2s = 0; 2 * num2s <= target; ++num2s) {
        for (int num3s = 0; 2 * num2s + 3 * num3s <= target; ++num3s) {
            for (int num7s = 0; 2 * num2s + 3 * num3s + 7 * num7s <= target;
                 ++num7s) {
                if (2 * num2s + 3 * num3s + 7 * num7s == target) {
                    std::cout << "2-pointers: " << num2s << "\t";
                    std::cout << "3-pointers: " << num3s << "\t";
                    std::cout << "7-pointers: " << num7s << "\n";
                }
            }
        }
    }
    std::cout << "-----\n";
}

int main() {
    findCombosThatScore(1);
    findCombosThatScore(4);
    findCombosThatScore(9);
    findCombosThatScore(11);
    findCombosThatScore(75);
    return 0;
}
