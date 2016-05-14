#include <iostream>
#include <cassert>
#include <string>
#include <set>
#include <algorithm>
#include <vector>

// Given classes that have pre-requisites, find a valid ordering to take the
// classes (if there is no strict ordering between two or more classes, use the
// class number to order and then if the numbers match, the lexicographic order
// of the department)

struct Class {
    std::string dept;
    int num;
    Class(const std::string & s) {
        auto it = s.begin();
        while (isalpha(*it) && isupper(*it)) { ++it; }
        dept = std::string(s.begin(), it);
        num = std::stoi(std::string(it, s.end()));
    }
    operator std::string() const {
        return dept + std::to_string(num);
    }
};

bool operator<(const Class & c1, const Class & c2) {
    if (c1.num != c2.num) {
        return c1.num < c2.num;
    } else {
        return std::lexicographical_compare(c1.dept.begin(), c1.dept.end(),
                                            c2.dept.begin(), c2.dept.end());
    }
}

std::ostream & operator<<(std::ostream & os, const Class & c) {
    os << c.dept << c.num;
    return os;
}

struct Graph {
    std::set<Class> classes; // Vertices
    std::vector<std::vector<bool>> adjMat; // Adjacency matrix
    int numEdges = 0;
    void expand() {
        adjMat = std::vector<std::vector<bool>>(classes.size(),
                    std::vector<bool>(classes.size(), false));
    }
    void connect(std::set<Class>::iterator & targetIt,
                 std::set<Class>::iterator & prereqIt) {
        int targetIndex = std::distance(classes.begin(), targetIt);
        int prereqIndex = std::distance(classes.begin(), prereqIt);
        if (!adjMat[prereqIndex][targetIndex]) {
            adjMat[prereqIndex][targetIndex] = true;
            ++numEdges;
        } else {
            assert(0);
        }
    }
    std::pair<std::set<Class>::iterator, bool> addNode(const Class & c) {
        auto r = classes.insert(c);
        return r;
    }
    void print(std::ostream & os) const {
        for (auto targetIt = classes.begin(); targetIt != classes.end();
             ++targetIt) {
            std::cout << *targetIt << ": ";
            for (auto prereqIt = classes.begin(); prereqIt != classes.end();
                 ++prereqIt) {
                int targetIndex = std::distance(classes.begin(), targetIt);
                int prereqIndex = std::distance(classes.begin(), prereqIt);
                if (adjMat[prereqIndex][targetIndex]) {
                    std::cout << *prereqIt << " ";
                }
            }
            std::cout << std::endl;
        }
    }
    std::vector<std::string> toposort() {
        std::vector<std::string> result;
        std::vector<std::set<Class>::iterator> classItsWithNoPrereqs;
        for (auto targetIt = classes.begin(); targetIt != classes.end();
             ++targetIt) {
            bool hasPrereq = false;
            for (auto potentialPrereqIt = classes.begin();
                 potentialPrereqIt != classes.end(); ++potentialPrereqIt) {
                int potentialPrereqIndex = std::distance(classes.begin(),
                                                         potentialPrereqIt);
                int targetIndex = std::distance(classes.begin(), targetIt);
                if (adjMat[potentialPrereqIndex][targetIndex]) {
                    hasPrereq = true;
                    break;
                }
            }
            if (!hasPrereq) {
                classItsWithNoPrereqs.push_back(targetIt);
            }
        }
        while (!classItsWithNoPrereqs.empty()) {
            // Find a class with no pre-reqs (resolve ties by # then dept)
            std::sort(classItsWithNoPrereqs.begin(),
                      classItsWithNoPrereqs.end(),
                      [](std::set<Class>::iterator c1,
                         std::set<Class>::iterator c2) { return *c1 < *c2; });
            auto firstClassIt = classItsWithNoPrereqs.begin();
            auto prereqIt = *firstClassIt;
            int prereqIndex = std::distance(classes.begin(), prereqIt);
            result.push_back(*prereqIt); // Calls std::string() implicitly
            classItsWithNoPrereqs.erase(firstClassIt);
            for (auto potentialTargetIt = classes.begin();
                 potentialTargetIt != classes.end(); ++potentialTargetIt) {
                int potentialTargetIndex = std::distance(classes.begin(),
                                                         potentialTargetIt);
                if (adjMat[prereqIndex][potentialTargetIndex]) {
                    adjMat[prereqIndex][potentialTargetIndex] = false;
                    --numEdges;
                    // Check for other incoming edges to the target
                    bool hasOtherIncomingEdges = false;
                    for (auto potentialPrereqIt = classes.begin();
                         potentialPrereqIt != classes.end();
                         ++potentialPrereqIt) {
                        int potentialPrereqIndex =
                            std::distance(classes.begin(),
                                          potentialPrereqIt);
                        if (adjMat[potentialPrereqIndex]
                                  [potentialTargetIndex]) {
                            hasOtherIncomingEdges = true;
                            break;
                        }
                    }
                    if (!hasOtherIncomingEdges) {
                        classItsWithNoPrereqs.push_back(potentialTargetIt);
                    }
                }
            }
        }
        if (0 != numEdges) {
            return {};
        }
        return result;
    }
};

std::ostream & operator<<(std::ostream & os, const Graph & g) {
    g.print(os);
    return os;
}

// Departments are 3 or 4 characters of capital letters
bool isValidDept(const std::string & s) {
    if (s.size() < 3 || s.size() > 4) { return false; }
    for (auto c : s) {
        if (!isalpha(c) || !isupper(c)) { return false; }
    }
    return true;
}

// Class numbers are in [100, 999]
bool isValidNum(int n) {
    return n >= 100 && n <= 999;
}

bool isValidListing(const std::string & s) {
    auto it = s.begin();
    while (isalpha(*it) && isupper(*it)) { ++it; }
    if (!isValidDept(std::string(s.begin(), it))) { return false; }
    return isValidNum(std::stoi(std::string(it, s.end())));
}

bool isValidInputLine(const std::string & s) {
    if (s.empty()) { return false; }
    bool colonFound = false;
    auto it = s.begin();
    while (it != s.end()) {
        if (*it == ':') {
            if (!isValidListing(std::string(s.begin(), it))) { return false; }
            colonFound = true;
            auto sit = std::next(it); // Check for any extraneous colons
            while (sit != s.end()) {
                if (*sit == ':') { return false; }
                ++sit;
            }
            ++it;
            break;
        }
        ++it;
    }
    if (!colonFound) { return false; }
    if (it == s.end()) { return true; }
    while (it != s.end()) {
        if (*it != ' ') { return false; }
        ++it;
        auto endit = it;
        while (endit != s.end() && *endit != ' ') { ++endit; }
        if (!isValidListing(std::string(it, endit))) { return false; }
        it = endit;
    }
    return true;
}

void readValidInputLine(const std::string & s, Graph & g) {
    auto it = s.begin();
    while (it != s.end()) {
        if (*it == ':') {
            std::string target(s.begin(), it);
            g.addNode(Class(target));
            ++it;
            break;
        }
        ++it;
    }
    while (it != s.end()) {
        ++it;
        auto endit = it;
        while (endit != s.end() && *endit != ' ') { ++endit; }
        std::string prereq(std::string(it, endit));
        g.addNode(Class(prereq));
        it = endit;
    }
}

void connectPrereqsToTargets(const std::string & s, Graph & g) {
    auto it = s.begin();
    std::set<Class>::iterator targetIt;
    while (it != s.end()) {
        if (*it == ':') {
            std::string target(s.begin(), it);
            targetIt = g.classes.find(Class(target));
            assert(targetIt != g.classes.end());
            ++it;
            break;
        }
        ++it;
    }
    while (it != s.end()) {
        ++it;
        auto endit = it;
        while (endit != s.end() && *endit != ' ') { ++endit; }
        std::string prereq(std::string(it, endit));
        std::set<Class>::iterator prereqIt =
            g.classes.find(Class(prereq));
        assert(prereqIt != g.classes.end());
        g.connect(targetIt, prereqIt);
        it = endit;
    }
}

std::vector<std::string> reorder(const std::vector<std::string> &
                                 classSchedule) {
    Graph g;
    for (auto cl : classSchedule) {
        if (!isValidInputLine(cl)) { return {}; }
        readValidInputLine(cl, g);
    }
    g.expand();
    for (auto cl : classSchedule) {
        connectPrereqsToTargets(cl, g);
    }
    std::cout << "----GRAPH----\n" << g << "---------\n";
    // Kahn's algorithm for topological sort assumes graph is acyclic, which
    // is logical since a circular dependency for class requirements makes no
    // sense
    return g.toposort();
}

int main() {
    assert(isValidInputLine("CSE111: CSE110 MATH101"));
    assert(isValidInputLine("CSE110:"));
    assert(!isValidInputLine("CS117:"));
    assert(!isValidInputLine("cs117:"));
    assert(!isValidInputLine("CS9E11:"));
    assert(!isValidInputLine("CSE110: "));
    assert(!isValidInputLine("CSE110: CSE101 "));
    assert(!isValidInputLine("MATH211: MAM2222"));
    assert(!isValidInputLine("MATH211: MAM22"));
    assert(!isValidInputLine("ENGIN517: MATH211"));
    {
        std::vector<std::string> classSchedule =
            {
                "CSE121: CSE110",
                "CSE110:",
                "MATH122:"
            };
        auto result = reorder(classSchedule);
        std::cout << "ORDER: ";
        for (auto c : result) {
            std::cout << c << " ";
        }
        std::cout << std::endl;
    }
    {
        std::vector<std::string> classSchedule =
            {
                "CSE258: CSE244 CSE243 INTR100",
                "CSE221: CSE254 INTR100",
                "CSE254: CSE111 MATH210 INTR100",
                "CSE244: CSE243 MATH210 INTR100",
                "MATH210: INTR100",
                "CSE101: INTR100",
                "CSE111: INTR100",
                "ECE201: CSE111 INTR100",
                "ECE111: INTR100",
                "CSE243: CSE254",
                "INTR100:"
            };
        auto result = reorder(classSchedule);
        std::cout << "ORDER: ";
        for (auto c : result) {
            std::cout << c << " ";
        }
        std::cout << std::endl;
    }
    {
        // Topological sort will "fail" when there's a cycle, which is the
        // intended behavior
        std::vector<std::string> classSchedule =
            {
                "ENGL111: ENGL110",
                "ENGL110: ENGL111"
            };
        auto result = reorder(classSchedule);
        std::cout << "ORDER: ";
        for (auto c : result) {
            std::cout << c << " ";
        }
        std::cout << std::endl;
    }
    return 0;
}
