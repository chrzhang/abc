#include <iostream>

// Find the weekday of a given date in [1990, 1999]
enum Month { january, february, march, april, may, june, july, august,
             september, october, november, december };

enum Day { monday, tuesday, wednesday, thursday, friday, saturday, sunday };

std::ostream & operator<<(std::ostream & os, const Day & d) {
    switch (d) {
        case monday:
            os << "monday";
            break;
        case tuesday:
            os << "tuesday";
            break;
        case wednesday:
            os << "wednesday";
            break;
        case thursday:
            os << "thursday";
            break;
        case friday:
            os << "friday";
            break;
        case saturday:
            os << "saturday";
            break;
        case sunday:
            os << "sunday";
            break;
    }
    return os;
}

void next(Day & d) {
    switch (d) {
        case monday:
            d = tuesday;
            break;
        case tuesday:
            d = wednesday;
            break;
        case wednesday:
            d = thursday;
            break;
        case thursday:
            d = friday;
            break;
        case friday:
            d = saturday;
            break;
        case saturday:
            d = sunday;
            break;
        case sunday:
            d = monday;
            break;
    }
}

Day incDay(Day d, int amt) {
    for (int i = 0; i < amt % 7; ++i) {
        next(d);
    }
    return d;
}

Day getWeekDay(Month m, Day d, int y) {
    return monday; // Placeholder
}

int main() {
    Day d = monday;
    for (int i = 0; i < 20; ++i) {
        std::cout << d << std::endl;
        next(d);
    }
    return 0;
}
