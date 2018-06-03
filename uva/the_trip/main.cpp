#include <iostream>
#include <cassert>
#include <vector>
#include <cmath>
#include <cfloat>

using namespace std;

int sumAmounts(const vector<int> & amounts) {
    int sum = 0;
    for (const auto & amount : amounts) {
        sum += amount;
    }
    return sum;
}

int findExchange_(const vector<int> & amounts) {
    const int totalSum = sumAmounts(amounts);
    const int perStudent = totalSum / (int) amounts.size();
    int underpaidTotal = 0;
    int overpaidTotal = 0;
    int exchange = 0;
    for (const auto & amount : amounts) {
        if (perStudent == amount) {
            continue;
        } else if (amount < perStudent) { // Underpaid
            underpaidTotal += perStudent - amount;
        } else { // Overpaid
            overpaidTotal += amount - perStudent;
        }
        const int currentExchange = min(underpaidTotal, overpaidTotal);
        exchange += currentExchange;
        underpaidTotal -= currentExchange;
        overpaidTotal -= currentExchange;
    }
    return exchange;
}

double findExchange(const vector<double> & amounts) {
    vector<int> centAmounts;
    for (const auto dollarAmount : amounts) {
        centAmounts.push_back((int) (100 * dollarAmount));
    }
    return (double) findExchange_(centAmounts);
}

int main() {
    assert(1199 == findExchange({15, 15.01, 3, 3.01}));
    assert(1000 == findExchange({10, 20, 30}));
    assert(7 == findExchange({9999.1, 9999.1, 9999.0, 9999.1}));
    return 0;
}

