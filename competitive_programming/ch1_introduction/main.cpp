#include "pi_printer.h"
#include "day_of_week.h"
#include "distinct.h"
#include "order_dates.h"
#include "binary_search.h"
#include <cassert>
int main() {
    for (int i = 0; i <= 15; ++i) {
        print_pi(i);
    }
    cout << "2000 1 1 is a " << day_of_week(2000, 1, 1) << endl;
    cout << "2018 9 26 is a " << day_of_week(2018, 9, 26) << endl;
    cout << "2010 8 9 is a " << day_of_week(2010, 8, 9) << endl;
    print_distinct({100, 2, 3, 4, 50, 1, 2, 3});
    vector<Birthdate> v = {Birthdate(2000, 2, 3), Birthdate(2010, 2, 3), Birthdate(2010, 5, 3), Birthdate(2002, 10, 3), Birthdate(2002, 10, 2)};
    my_sort(v);
    for (const auto & b : v) {
        cout << b.m << " " << b.d << " " << b.y << endl;
    }
    vector<int> haystack;
    for (int i = 0; i < 1000000; ++i) {
        haystack.push_back(i);
    }
    for (int needle = 0; needle < 1000000; ++needle) {
        assert(needle ==
               search_sorted(haystack, needle, 0, haystack.size() - 1));
    }
}
