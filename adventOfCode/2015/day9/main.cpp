#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <map>
#include <set>
#include <algorithm>
#include <climits>

/*
From http://adventofcode.com/2015/day/9

--- Day 9: All in a Single Night ---

Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have provided
him the distances between every pair of locations. He can start and end at any
two (different) locations he wants, but he must visit each location exactly
once. What is the shortest distance he can travel to achieve this?

For example, given the following distances:

London to Dublin = 464 London to Belfast = 518 Dublin to Belfast = 141 The
possible routes are therefore:

Dublin -> London -> Belfast = 982 London -> Dublin -> Belfast = 605 London ->
Belfast -> Dublin = 659 Dublin -> Belfast -> London = 659 Belfast -> Dublin ->
London = 605 Belfast -> London -> Dublin = 982 The shortest of these is London
-> Dublin -> Belfast = 605, and so the answer is 605 in this example.

What is the distance of the shortest route?

--- Part Two ---

The next year, just to show off, Santa decides to take the route with the
longest distance instead.

He can still start and end at any two (different) locations he wants, and he
still must visit each location exactly once.

For example, given the distances above, the longest route would be 982 via (for
example) Dublin -> London -> Belfast.

What is the distance of the longest route?
*/

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
    std::regex routeRegex("(.*) to (.*) = (.*)");
    std::map<std::string, std::map<std::string, int>> routes;
    std::set<std::string> places;
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, routeRegex);
        const auto & src = result[1];
        const auto & dest = result[2];
        const auto & dist = std::stoi(result[3]);
        routes[src][dest] = dist;
        routes[dest][src] = dist;
        places.insert(src);
        places.insert(dest);
    }
    std::vector<std::string> indexablePlaces(places.begin(), places.end());
    places.clear();
    std::vector<int> indices;
    for (size_t i = 0; i < indexablePlaces.size(); ++i) {
        indices.push_back(i);
    }
    int maxDistanceSoFar = INT_MIN;
    int minDistanceSoFar = INT_MAX;
    do {
        int currDistance = 0;
        for (size_t i = 1; i < indices.size(); ++i) {
            currDistance += routes[indexablePlaces[indices[i]]]
                                  [indexablePlaces[indices[i - 1]]];
        }
        maxDistanceSoFar = std::max(maxDistanceSoFar, currDistance);
        minDistanceSoFar = std::min(minDistanceSoFar, currDistance);
    } while (std::next_permutation(indices.begin(), indices.end()));
    std::cout << "Max: " << maxDistanceSoFar << std::endl;
    std::cout << "Min: " << minDistanceSoFar << std::endl;
    return 0;
}
