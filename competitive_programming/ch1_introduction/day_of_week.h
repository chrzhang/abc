#include <cmath>
using namespace std;
int day_of_week(const int year, const int month, const int day) { // Gauss Algo
    const int Y = (month == 1 || month == 2) ? year - 1 : year;
    const int y = Y % 100;
    const int c = Y / 100;
    static const int shifted_m[] = {11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    const int m = shifted_m[month - 1];
    return (day + int(floor(2.6 * m - 0.2)) + y + int(floor(y / 4)) + int(floor(c / 4)) - 2 * c) % 7;
}
