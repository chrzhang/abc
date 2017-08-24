#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <set>
#include <cassert>

/*
From http://adventofcode.com/2016/day/1

--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and the
clock's oscillator is regulated by stars. Unfortunately, the stars have been
stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve
all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
unfortunately, is as close as you can get - the instructions on the Easter
Bunny Recruiting Document the Elves intercepted start here, and nobody had time
to work them out further.

The Document indicates that you should start at the given coordinates (where
you just landed) and face North. Then, follow the provided sequence: either
turn left (L) or right (R) 90 degrees, then walk forward the given number of
blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you
take a moment and work out the destination. Given that you can only walk on the
street grid of the city, how far is the shortest path to the destination?

For example:

Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2
blocks away.  R5, L5, R5, R3 leaves you 12 blocks away.  How many blocks away
is Easter Bunny HQ?

--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting
Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you
visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
*/

enum Direction {
    NORTH,
    EAST,
    WEST,
    SOUTH
};

struct Player {
    Direction currentDirection;
    std::set<std::pair<int, int>> visitHistory;
    int xPosition;
    int yPosition;
    int distanceToFirstRevisitedPoint;
    Player()
    : currentDirection(NORTH), xPosition(0), yPosition(0) {
        visitHistory.insert({xPosition, yPosition});
        distanceToFirstRevisitedPoint = -1;
    }
    int distance() const {
        return abs(xPosition) + abs(yPosition);
    }
    void handleHistoryInsertionResult(const std::pair<std::set<std::pair<int, int>>::const_iterator, bool> & result) {
        if (result.second == false && distanceToFirstRevisitedPoint == -1) {
            distanceToFirstRevisitedPoint = abs((result.first)->first) +
                                            abs((result.first)->second);
        }
    }
    void updateVisitHistoryGoing(const Direction & direction,
                                 const int xPosition,
                                 const int yPosition,
                                 const int amount) {
        assert(amount >= 0);
        if (direction == NORTH) {
            for (int i = 1; i <= amount; ++i) {
                handleHistoryInsertionResult(visitHistory.insert({xPosition,
                                                                  yPosition + i}));
            }
        } else if (direction == SOUTH) {
            for (int i = 1; i <= amount; ++i) {
                handleHistoryInsertionResult(visitHistory.insert({xPosition,
                                                                  yPosition - i}));
            }
        } else if (direction == EAST) {
            for (int i = 1; i <= amount; ++i) {
                handleHistoryInsertionResult(visitHistory.insert({xPosition + i,
                                                                  yPosition}));
            }
        } else if (direction == WEST) {
            for (int i = 1; i <= amount; ++i) {
                handleHistoryInsertionResult(visitHistory.insert({xPosition - i,
                                                                  yPosition}));
            }
        }
    }
    void goWest(const int amount) {
        currentDirection = WEST;
        updateVisitHistoryGoing(WEST, xPosition, yPosition, amount);
        xPosition -= amount;
    }
    void goEast(const int amount) {
        currentDirection = EAST;
        updateVisitHistoryGoing(EAST, xPosition, yPosition, amount);
        xPosition += amount;
    }
    void goNorth(const int amount) {
        currentDirection = NORTH;
        updateVisitHistoryGoing(NORTH, xPosition, yPosition, amount);
        yPosition += amount;
    }
    void goSouth(const int amount) {
        currentDirection = SOUTH;
        updateVisitHistoryGoing(SOUTH, xPosition, yPosition, amount);
        yPosition -= amount;
    }
    void goLeft(const int amount) {
        if (currentDirection == NORTH) {
            goWest(amount);
        } else if (currentDirection == EAST) {
            goNorth(amount);
        } else if (currentDirection == WEST) {
            goSouth(amount);
        } else if (currentDirection == SOUTH) {
            goEast(amount);
        }
    }
    void goRight(int amount) {
        if (currentDirection == NORTH) {
            goEast(amount);
        } else if (currentDirection == EAST) {
            goSouth(amount);
        } else if (currentDirection == WEST) {
            goNorth(amount);
        } else if (currentDirection == SOUTH) {
            goWest(amount);
        }
    }
};

int toInt(const std::string & numberString) {
    std::stringstream ss(numberString);
    int value = 0;
    if (!(ss >> value)) {
        std::cerr << "Could not convert " << numberString << " to an int.\n";
    }
    return value;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    Player player;
    std::string wordFromFile;
    while (std::getline(inputFile, wordFromFile, ',')) {
        wordFromFile.erase(std::remove_if(wordFromFile.begin(), wordFromFile.end(), isspace), wordFromFile.end());
        if (wordFromFile[0] == 'L') {
            player.goLeft(toInt(std::string(wordFromFile.begin() + 1,
                                            wordFromFile.end())));
        } else if (wordFromFile[0] == 'R') {
            player.goRight(toInt(std::string(wordFromFile.begin() + 1,
                                             wordFromFile.end())));
        }
    }
    assert(279 == player.distance());
    std::cout << "Part 1: " << player.distance() << std::endl;
    assert(163 == player.distanceToFirstRevisitedPoint);
    std::cout << "Part 2: " << player.distanceToFirstRevisitedPoint << std::endl;
    return 0;
}
