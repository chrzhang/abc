#include <iostream>
#include <iomanip>
using namespace std;
void print_pi(const int num_digits) {
    cout << fixed << setprecision(num_digits) << 3.141592653589793 << endl;
}
int main() {
    for (int i = 0; i <= 15; ++i) { print_pi(i); }
}
