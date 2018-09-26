#include "pi_printer.h"
#include "day_of_week.h"
int main() {
    for (int i = 0; i <= 15; ++i) {
        print_pi(i);
    }
    cout << "2000 1 1 is a " << day_of_week(2000, 1, 1) << endl;
    cout << "2018 9 26 is a " << day_of_week(2018, 9, 26) << endl;
    cout << "2010 8 9 is a " << day_of_week(2010, 8, 9) << endl;
}
