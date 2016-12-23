#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <cassert>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <algorithm>
#include <climits>

/*
From http://adventofcode.com/2015/day/14

--- Day 14: Reindeer Olympics ---

This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
rest occasionally to recover their energy. Santa would like to know which of
his reindeer is fastest, and so he has them race.

Reindeer can only either be flying (always at their top speed) or resting (not
moving at all), and always spend whole seconds in either state.

For example, suppose you have the following Reindeer:

    Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten
seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh
second, Comet begins resting (staying at 140 km), and Dancer continues on for a
total distance of 176 km. On the 12th second, both reindeer are resting. They
continue to rest until the 138th second, when Comet flies for another ten
seconds. On the 174th second, Dancer flies for another 11 seconds.

In this example, after the 1000th second, both reindeer are resting, and Comet
is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
So, in this situation, Comet would win (if the race ended at 1000 seconds).

Given the descriptions of each reindeer (in your puzzle input), after exactly
2503 seconds, what distance has the winning reindeer traveled?

--- Part Two ---

Seeing how reindeer move in bursts, Santa decides he's not pleased with the old
scoring system.

Instead, at the end of each second, he awards one point to the reindeer
currently in the lead. (If there are multiple reindeer tied for the lead, they
each get one point.) He keeps the traditional 2503 second time limit, of
course, as doing otherwise would be entirely ridiculous.

Given the example reindeer from above, after the first second, Dancer is in the
lead and gets one point. He stays in the lead until several seconds into
Comet's second burst: after the 140th second, Comet pulls into the lead and
gets his first point. Of course, since Dancer had been in the lead for the 139
seconds before that, he has accumulated 139 points by the 140th second.

After the 1000th second, Dancer has accumulated 689 points, while poor Comet,
our old champion, only has 312. So, with the new scoring system, Dancer would
win (if the race ended at 1000 seconds).

Again given the descriptions of each reindeer (in your puzzle input), after
exactly 2503 seconds, how many points does the winning reindeer have?
*/

const int RACE_LENGTH_SECONDS = 2503;

class Reindeer {

    std::string m_name;
    int m_speed;
    int m_flyDuration;
    int m_restDuration;
    int m_points;

    int total() const {
        return m_flyDuration + m_restDuration;
    }

    public:

        Reindeer(const std::string & name, int speed, int flyDuration,
                 int restDuration)
        : m_name(name), m_speed(speed), m_flyDuration(flyDuration),
          m_restDuration(restDuration), m_points(0) {
        }

        void givePoint() {
            ++m_points;
        }

        const int points() const {
            return m_points;
        }

        const std::string & name() const {
            return m_name;
        }

        // Return the distance traveled after alternating between flying and
        // resting over 'duration' seconds
        int dist(int duration) const {
            return (duration / total()) * (m_flyDuration * m_speed) +
                   std::min((duration % total()), m_flyDuration) * m_speed;
        }

};

int toInt(const std::string & s) {
    std::stringstream ss(s);
    int r;
    if (!(ss >> r)) {
        throw std::runtime_error("Cannot convert " + s + " to an integer.\n");
    }
    return r;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::string line;
    std::regex lineRegex("^(.*) can fly (.*) km/s for (.*) seconds, but then "
                         "must rest for (.*) seconds.$");
    std::vector<Reindeer> fleet;
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, lineRegex);
        assert(result.size() == 5);
        const std::string & reindeerName = result[1];
        const std::string & speedString = result[2];
        const std::string & flyDurationString = result[3];
        const std::string & restDurationString = result[4];
        int speed, flyDuration, restDuration;
        speed = toInt(speedString);
        flyDuration = toInt(flyDurationString);
        restDuration = toInt(restDurationString);
        fleet.push_back(Reindeer(reindeerName, speed, flyDuration,
                                 restDuration));
    }
    // Part 1
    int maxDistance = INT_MIN;
    for (auto reindeer : fleet) {
        maxDistance = std::max(maxDistance, reindeer.dist(RACE_LENGTH_SECONDS));
    }
    std::cout << "Winning reindeer travelled " << maxDistance << " km."
              << std::endl;
    // Part 2
    for (int sec = 1; sec <= RACE_LENGTH_SECONDS; ++sec) { // After each second
        // Find the leader(s)
        std::vector<int> leaders;
        int maxDistance = INT_MIN;
        for (size_t i = 0; i < fleet.size(); ++i) {
            const auto & reindeer = fleet[i];
            int distanceTravelled = reindeer.dist(sec);
            if (distanceTravelled > maxDistance) {
                leaders.clear();
                leaders.push_back(i);
                maxDistance = distanceTravelled;
            } else if (distanceTravelled == maxDistance) {
                leaders.push_back(i);
            }
        }
        for (auto i : leaders) {
            fleet[i].givePoint();
        }
    }
    int maxPoints = INT_MIN;
    std::string winner;
    for (auto reindeer : fleet) {
        if (reindeer.points() > maxPoints) {
            maxPoints = reindeer.points();
            winner = reindeer.name();
        }
    }
    std::cout << winner << " won the points-based race with " << maxPoints
              << " points.\n";
    return 0;
}
