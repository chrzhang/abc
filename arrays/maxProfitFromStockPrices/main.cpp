#include <iostream>
#include <vector>
#include <cassert>

// Given a vector of stock prices where index represents time, find the most
// profit to be gained by buying and selling a stock

int maxProfit(const std::vector<int> & prices) {
    if (prices.empty()) { return 0; }
    int currProfit = 0;
    int currMin = prices.front();
    for (auto p : prices) {
        if (p < currMin) {
            currMin = p;
        }
        if (p - currMin > currProfit) {
            currProfit = p - currMin;
        }
    }
    return currProfit;
}

int main() {
    std::vector<int> v = {1, 2, 3, 4, 5};
    assert(maxProfit(v) == 4);
    v = {4, 1, 2};
    assert(maxProfit(v) == 1);
    v = {5, 4, 3, 2, 1};
    assert(maxProfit(v) == 0);
    return 0;
}
