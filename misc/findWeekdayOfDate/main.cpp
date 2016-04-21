#include <iostream>
#include <cassert>

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

void next(Month & m) {
    switch (m) {
        case january:
            m = february;
            break;
        case february:
            m = march;
            break;
        case march:
            m = april;
            break;
        case april:
            m = may;
            break;
        case may:
            m = june;
            break;
        case june:
            m = july;
            break;
        case july:
            m = august;
            break;
        case august:
            m = september;
            break;
        case september:
            m = october;
            break;
        case october:
            m = november;
            break;
        case november:
            m = december;
            break;
        case december:
            m = january;
            break;
    }
}

void incDay(Day & d, int amt) {
    for (int i = 0; i < amt % 7; ++i) {
        next(d);
    }
}

bool isLeapYear(int y) {
    return y == 1992 || y == 1996;
}

Day getWeekDay(Month m, int d, int y) {
    if (y < 1990 || y > 1999) {
        std::cout << "Year " << y << " not in range [1990, 1999].\n";
        assert(false);
    }
    int daysInEachMonthYear[10][12];
    for (int year = 1990; year < 2000; ++year) {
        daysInEachMonthYear[year - 1990][january] =
        daysInEachMonthYear[year - 1990][march] =
        daysInEachMonthYear[year - 1990][may] =
        daysInEachMonthYear[year - 1990][july] =
        daysInEachMonthYear[year - 1990][august] =
        daysInEachMonthYear[year - 1990][october] =
        daysInEachMonthYear[year - 1990][december] = 31;
        daysInEachMonthYear[year - 1990][april] =
        daysInEachMonthYear[year - 1990][june] =
        daysInEachMonthYear[year - 1990][september] =
        daysInEachMonthYear[year - 1990][november] = 30;
        daysInEachMonthYear[year - 1990][february] = isLeapYear(year) ? 29 : 28;
    }
    int year = 1990;
    int daysPassed = 0;
    while (year != y) {
        for (int i = 0; i < 12; ++i) {
            daysPassed += daysInEachMonthYear[year - 1990][i];
        }
        ++year;
    }
    for (Month mi = january; mi != m; next(mi)) {
        daysPassed += daysInEachMonthYear[y - 1990][mi];
    }
    daysPassed += d - 1;
    Day start = monday;
    incDay(start, daysPassed);
    return start;
}

int main() {
    assert(monday == getWeekDay(april, 26, 1999));
    return 0;
}
