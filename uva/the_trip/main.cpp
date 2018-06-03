#include <iostream>
#include <cassert>
#include <vector>
#include <cmath>
#include <cfloat>

using namespace std;

double sumAmounts(const vector<double> & amounts) {
    double sum = 0;
    for (const auto & amount : amounts) {
        sum += amount;
    }
    return sum;
}

double findExchange(const vector<double> & amounts) {
    const double totalSum = sumAmounts(amounts);
    const double perStudent = totalSum / (int) amounts.size();
    double underpaidTotal = 0;
    double overpaidTotal = 0;
    for (const auto & amount : amounts) {
        const double diff = (long) ((amount - perStudent) * 100.0) / 100.0;
        if (diff <= 0) { // Underpaid
            underpaidTotal += -1 * diff;
        } else { // Overpaid
            overpaidTotal += diff;
        }
    }
    return max(underpaidTotal, overpaidTotal);
}

int main() {
    assert(10 == findExchange({10, 20, 30}));
    assert(11.99 == findExchange({15, 15.01, 3, 3.01}));
    assert(0.07 == findExchange({9999.1, 9999.1, 9999.0, 9999.1}));
    return 0;
}

