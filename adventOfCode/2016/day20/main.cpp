#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <regex>
#include <cassert>
#include <list>

/*
From http://adventofcode.com/2016/day/20

--- Day 20: Firewall Rules ---

You'd like to set up a small hidden computer here so you can use it to get back
into the network later. However, the corporate firewall only allows
communication with certain external IP addresses.

You've retrieved the list of blocked IPs from the firewall, but the list seems
to be messy and poorly maintained, and it's not clear which IPs are allowed.
Also, rather than being written in dot-decimal notation, they are written as
plain 32-bit integers, which can have any value from 0 through 4294967295,
inclusive.

For example, suppose only the values 0 through 9 were valid, and that you
retrieved the following blacklist:

5-8
0-2
4-7

The blacklist specifies ranges of IPs (inclusive of both the start and end
value) that are not allowed. Then, the only IPs that this firewall allows are 3
and 9, since those are the only numbers not in any range.

Given the list of blocked IPs you retrieved from the firewall (your puzzle
input), what is the lowest-valued IP that is not blocked?

--- Part Two ---

How many IPs are allowed by the blacklist?
*/

unsigned long toULong(const std::string & s) {
    std::stringstream ss(s);
    unsigned long result = 0;
    assert(ss >> result);
    return result;
}

class Range {

    unsigned long lower;
    unsigned long upper;
    bool value;

    public:
        Range(const unsigned long lower, const unsigned long upper,
              const bool value)
        : lower(lower), upper(upper), value(value) {
            if (lower > upper) {
                std::cerr << "Cannot make range where " << lower << " > "
                          << upper << std::endl;
                abort();
            }
        }

        const unsigned long size() const {
            return upper - lower + 1;
        }

        const unsigned long getLower() const {
            return lower;
        }

        const unsigned long getUpper() const {
            return upper;
        }

        void setLower(const unsigned newLower) {
            lower = newLower;
            if (lower > upper) {
                std::cerr << "Cannot set lower where " << lower << " > "
                          << upper << std::endl;
                abort();
            }
        }

        void setUpper(const unsigned newUpper) {
            upper = newUpper;
            if (upper < lower) {
                std::cerr << "Cannot set upper where " << lower << " > "
                          << upper << std::endl;
                abort();
            }
        }

        void setValue(const bool newValue) {
            value = newValue;
        }

        const bool getValue() const {
            return value;
        }

};

class RangeList {

    std::list<Range> ranges;

    std::list<Range>::iterator findRangeContaining(const unsigned long bound) {
        for (std::list<Range>::iterator rangeIt = ranges.begin();
             rangeIt != ranges.end(); ++rangeIt) {
            if (rangeIt->getLower() <= bound && rangeIt->getUpper() >= bound) {
                return rangeIt;
            }
        }
        return ranges.end();
    }

    public:

        RangeList(const unsigned long lower, const unsigned long upper) {
            ranges.push_back(Range(lower, upper, true));
        }

        unsigned long valueCount(const bool value) const {
            unsigned long result = 0;
            for (const auto & range : ranges) {
                if (range.getValue() == value) {
                    result += range.size();
                }
            }
            return result;
        }

        unsigned long smallestValue(const bool value) const {
            for (const auto & range : ranges) {
                if (range.getValue() == value) {
                    return range.getLower();
                }
            }
            abort();
            return 0;
        }

        void setRange(const unsigned long lower, const unsigned long upper,
                      const bool value) {
            assert(lower <= upper);
            const std::list<Range>::iterator lowerRangeIt =
                findRangeContaining(lower);
            const std::list<Range>::iterator upperRangeIt =
                findRangeContaining(upper);
            if (lowerRangeIt == upperRangeIt) {
                const auto & rangeIt = lowerRangeIt;
                if (rangeIt->getValue()!= value) {
                    if (rangeIt->getLower() + 1 <= lower) {
                        assert(lower > 0);
                        const Range beforeRange(rangeIt->getLower(), lower - 1,
                                                rangeIt->getValue());
                        ranges.insert(rangeIt, beforeRange);
                    }
                    if (upper + 1 <= rangeIt->getUpper()) {
                        const Range afterRange(upper + 1, rangeIt->getUpper(),
                                               rangeIt->getValue());
                        ranges.insert(std::next(rangeIt), afterRange);
                    }
                    rangeIt->setLower(lower);
                    rangeIt->setUpper(upper);
                    rangeIt->setValue(value);
                }
            } else {
                if (std::next(lowerRangeIt) != upperRangeIt) {
                    ranges.erase(std::next(lowerRangeIt), upperRangeIt);
                }
                if (lowerRangeIt->getValue() == value &&
                    upperRangeIt->getValue() == value) {
                    lowerRangeIt->setUpper(upperRangeIt->getUpper());
                    ranges.erase(upperRangeIt);
                } else if (lowerRangeIt->getValue() != value &&
                           upperRangeIt->getValue() == value) {
                    assert(lower > 0);
                    if (lowerRangeIt->getLower() <= lower - 1) {
                        lowerRangeIt->setUpper(lower - 1);
                    } else {
                        ranges.erase(lowerRangeIt);
                    }
                    if (lower <= upperRangeIt->getUpper()) {
                        const Range currentRange(lower,
                                                upperRangeIt->getUpper(), value);
                        const auto & nextUpperRangeIt = std::next(upperRangeIt);
                        ranges.erase(upperRangeIt);
                        ranges.insert(nextUpperRangeIt, currentRange);
                    }
                } else if (lowerRangeIt->getValue() == value &&
                           upperRangeIt->getValue() != value) {
                    if (upperRangeIt->getUpper() >= upper + 1) {
                        upperRangeIt->setLower(upper + 1);
                    } else {
                        ranges.erase(upperRangeIt);
                    }
                    if (lowerRangeIt->getLower() <= upper) {
                        const Range currentRange(lowerRangeIt->getLower(), upper,
                                                value);
                        const auto & nextLowerRangeIt = std::next(lowerRangeIt);
                        ranges.erase(lowerRangeIt);
                        ranges.insert(nextLowerRangeIt, currentRange);
                    }
                } else if (lowerRangeIt->getValue() != value &&
                           upperRangeIt->getValue() != value) {
                    assert(lower > 0);
                    auto currentRangeDestination = upperRangeIt;
                    if (lower - 1 >= lowerRangeIt->getLower()) {
                        lowerRangeIt->setUpper(lower - 1);
                    } else {
                        ranges.erase(lowerRangeIt);
                    }
                    if (upper + 1 <= upperRangeIt->getUpper()) {
                        upperRangeIt->setLower(upper + 1);
                    } else {
                        currentRangeDestination = std::next(upperRangeIt);
                        ranges.erase(upperRangeIt);
                    }
                    const Range currentRange(lower, upper, value);
                    ranges.insert(currentRangeDestination, currentRange);
                }
            }
        }

        friend std::ostream & operator<<(std::ostream & os,
                                         const RangeList & rangeList);

};

std::ostream & operator<<(std::ostream & os, const RangeList & rangeList) {
    for (const auto & range : rangeList.ranges) {
        os << range.getLower() << "-" << range.getUpper() << ": "
           << range.getValue() << "\n";
    }
    return os;
}

void tests() {
    RangeList rangeList(1, 20);
    assert(rangeList.valueCount(true) == 20);
    rangeList.setRange(9, 12, true);
    assert(rangeList.valueCount(true) == 20);
    rangeList.setRange(9, 12, false);
    assert(rangeList.valueCount(true) == 16);
    rangeList.setRange(3, 12, true);
    assert(rangeList.valueCount(true) == 20);
    rangeList.setRange(6, 18, true);
    assert(rangeList.valueCount(true) == 20);
    rangeList.setRange(2, 4, false);
    assert(rangeList.valueCount(true) == 17);
    rangeList.setRange(5, 7, true);
    assert(rangeList.valueCount(true) == 17);
    rangeList.setRange(9, 19, true);
    assert(rangeList.valueCount(true) == 17);
    rangeList.setRange(1, 2, false);
    assert(rangeList.valueCount(true) == 16);
    rangeList.setRange(13, 20, false);
    assert(rangeList.valueCount(true) == 8);
}

int main(int argc, char * argv[]) {
    tests();
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    while (!inputFile.is_open()) {
        std::cerr << "Cannot open file\n";
        return 1;
    }
    RangeList rangeList(0, 4294967295);
    std::string currentLine;
    while (std::getline(inputFile, currentLine)) {
        std::regex lineRegex("^([0-9]+)-([0-9]+)$");
        std::smatch result;
        std::regex_search(currentLine, result, lineRegex);
        assert(result.size() == 3);
        const unsigned long lower = toULong(result[1]);
        const unsigned long upper = toULong(result[2]);
        assert(lower <= upper);
        rangeList.setRange(lower, upper, false);
    }
    const auto & part1 = rangeList.smallestValue(true);
    assert(part1 == 14975795);
    std::cout << "Part 1: " << part1 << std::endl;
    const auto & part2 = rangeList.valueCount(true);
    assert(part2 == 101);
    std::cout << "Part 2: " << part2 << std::endl;
    return 0;
}
