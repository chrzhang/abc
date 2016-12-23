#include <iostream>
#include <string>
#include <regex>
#include <fstream>
#include <cassert>
#include <sstream>
#include <map>
#include <climits>
#include <algorithm>

/*
From http://adventofcode.com/2015/day/13

--- Day 13: Knights of the Dinner Table ---

In years past, the holiday feast with your family hasn't gone so well. Not
everyone gets along! This year, you resolve, will be different. You're going to
find the optimal seating arrangement and avoid all those awkward conversations.

You start by writing up a list of everyone invited and the amount their
happiness would increase or decrease if they were to find themselves sitting
next to each other person. You have a circular table that will be just big
enough to fit everyone comfortably, and so each person will have exactly two
neighbors.

For example, suppose you have only four attendees planned, and you calculate
their potential happiness as follows:

Alice would gain 54 happiness units by sitting next to Bob.  Alice would lose
79 happiness units by sitting next to Carol.  Alice would lose 2 happiness
units by sitting next to David.  Bob would gain 83 happiness units by sitting
next to Alice.  Bob would lose 7 happiness units by sitting next to Carol.  Bob
would lose 63 happiness units by sitting next to David.  Carol would lose 62
happiness units by sitting next to Alice.  Carol would gain 60 happiness units
by sitting next to Bob.  Carol would gain 55 happiness units by sitting next to
David.  David would gain 46 happiness units by sitting next to Alice.  David
would lose 7 happiness units by sitting next to Bob.  David would gain 41
happiness units by sitting next to Carol.

Then, if you seat Alice next to David, Alice would lose 2 happiness units
(because David talks so much), but David would gain 46 happiness units (because
Alice is such a good listener), for a total change of 44.

If you continue around the table, you could then seat Bob next to Alice (Bob
gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol
gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The
arrangement looks like this:

     +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
     -7  +83

After trying every other seating arrangement in this hypothetical scenario, you
find that this one is the most optimal, with a total change in happiness of
330.

What is the total change in happiness for the optimal seating arrangement of
the actual guest list?

--- Part Two ---

In all the commotion, you realize that you forgot to seat yourself. At this
point, you're pretty apathetic toward the whole thing, and your happiness
wouldn't really go up or down regardless of who you sit next to. You assume
everyone else would be just as ambivalent about sitting next to you, too.

So, add yourself to the list, and give all happiness relationships that involve
you a score of 0.

What is the total change in happiness for the optimal seating arrangement that
actually includes yourself?
*/

const auto MY_NAME = "me";

void solve(const std::map<std::string, std::map<std::string, int>> & joy) {
    std::vector<std::string> people;
    for (auto p : joy) {
        people.push_back(p.first);
    }
    if (joy.size() < 2) {
        std::cerr << "The joy of solitude is unknown.\n";
        return;
    }
    std::vector<int> peopleIndices;
    for (size_t i = 0; i < joy.size(); ++i) {
        peopleIndices.push_back(i);
    }
    // Entertain every ordering of people and find the total happiness
    int maxHappiness = INT_MIN;
    do {
        int totalHappiness = 0;
        for (auto it = peopleIndices.begin(); it != peopleIndices.end(); ++it) {
            const std::string & currPerson = people[*it];
            if (it == peopleIndices.begin()) {
                const std::string & neighbor1 = people[*std::next(it)];
                const std::string & neighbor2 =
                    people[*std::prev(peopleIndices.end())];
                totalHappiness += joy.at(currPerson).at(neighbor1);
                totalHappiness += joy.at(currPerson).at(neighbor2);
            } else if (std::next(it) == peopleIndices.end()) {
                const std::string & neighbor1 = people[*std::prev(it)];
                const std::string & neighbor2 = people[*peopleIndices.begin()];
                totalHappiness += joy.at(currPerson).at(neighbor1);
                totalHappiness += joy.at(currPerson).at(neighbor2);
            } else {
                const std::string & neighbor1 = people[*std::prev(it)];
                const std::string & neighbor2 = people[*std::next(it)];
                totalHappiness += joy.at(currPerson).at(neighbor1);
                totalHappiness += joy.at(currPerson).at(neighbor2);
            }
        }
        maxHappiness = std::max(maxHappiness, totalHappiness);
    } while (std::next_permutation(peopleIndices.begin(), peopleIndices.end()));
    std::cout << maxHappiness << std::endl;
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
    std::regex lineRegex("^(.*) would (.*) (.*) happiness units by sitting "
                         "next to (.*).$");
    std::map<std::string, std::map<std::string, int>> joy;
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, lineRegex);
        assert(result.size() == 5);

        const std::string & person = result[1];
        const std::string & gainOrLose = result[2];
        const std::string & amountString = result[3];
        const std::string & neighbor = result[4];

        std::stringstream ss(amountString);
        int amount = 0;
        if (!(ss >> amount)) {
            std::cerr << "Could not interpret " << amountString
                      << " as a number.\n";
            return 1;
        }
        if (gainOrLose == "gain") {
        } else if (gainOrLose == "lose") {
            amount *= -1;
        } else {
            std::cerr << "Could not interpret " << gainOrLose
                      << " as gain or lose.\n";
            return 1;
        }
        joy[person][neighbor] = amount;
    }
    solve(joy);
    if (joy.find(MY_NAME) != joy.end()) {
        std::cerr << "Someone shares your name. Please use a different one.\n";
        return 1;
    }
    for (auto p : joy) {
        joy[p.first][MY_NAME] = 0;
        joy[MY_NAME][p.first] = 0;
    }
    solve(joy);
    return 0;
}
