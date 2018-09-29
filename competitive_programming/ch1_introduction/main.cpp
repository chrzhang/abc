#include "pi_printer.h"
#include "day_of_week.h"
#include "distinct.h"
#include "order_dates.h"
#include "binary_search.h"
#include "permutations.h"
#include "subsets.h"
#include <cassert>
int main() {
    { // 1.2.3.2
        for (int i = 0; i <= 15; ++i) {
            print_pi(i);
        }
    }
    { // 1.2.3.3
        cout << "2000 1 1 is a " << day_of_week(2000, 1, 1) << endl;
        cout << "2018 9 26 is a " << day_of_week(2018, 9, 26) << endl;
        cout << "2010 8 9 is a " << day_of_week(2010, 8, 9) << endl;
    }
    { // 1.2.3.4
        print_distinct({100, 2, 3, 4, 50, 1, 2, 3});
    }
    { // 1.2.3.5
        vector<Birthdate> v = {Birthdate(2000, 2, 3), Birthdate(2010, 2, 3), Birthdate(2010, 5, 3), Birthdate(2002, 10, 3), Birthdate(2002, 10, 2)};
        my_sort(v);
        for (const auto & b : v) {
            cout << b.m << " " << b.d << " " << b.y << endl;
        }
    }
    { // 1.2.3.6
        vector<int> haystack;
        for (int i = 0; i < 1000000; ++i) {
            haystack.push_back(i);
        }
        for (int needle = 0; needle < 1000000; ++needle) {
            assert(needle ==
                search_sorted(haystack, needle, 0, haystack.size() - 1));
        }
    }
    { // 1.2.3.7
        vector<char> v;
        for (char c = 'A'; c <= 'J'; ++c) {
            v.push_back(c);
        }
        all_permutations_of(v); // 10!
    }
    { // 1.2.3.8
        vector<int> v;
        for (int i= 0; i < 20; ++i) {
            v.push_back(i);
        }
        subsets_of(v); // 2^20
    }
}
