#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <cassert>
#include <set>

// Implement a finite state machine

struct Symbol {
    std::string sym;
    Symbol() {}
    Symbol(const std::string & s) {
        assert(!s.empty());
        for (auto c : s) {
            assert(isalpha(c));
        }
        sym = s;
    }
};

bool operator==(const Symbol & s1, const Symbol & s2) {
    return s1.sym.compare(s2.sym) == 0;
}

std::ostream & operator<<(std::ostream & os, const Symbol & s) {
    os << s.sym;
    return os;
}

struct State {
    std::string name;
    State() {}
    State(const std::string & s) {
        assert(!s.empty());
        for (auto c : s) {
            assert(isalpha(c));
        }
        name = s;
    }
};

std::ostream & operator<<(std::ostream & os, const State & s) {
    os << s.name;
    return os;
}

struct Func {
    std::string f;
    Func() {}
    Func(const std::string & s) {
        assert(!s.empty());
        for (auto c : s) {
            assert(isalpha(c));
        }
        f = s;
    }
};

std::ostream & operator<<(std::ostream & os, const Func & fn) {
    os << fn.f;
    return os;
}

struct Edge {
    State source, sink;
    Symbol symbol;
    Func fn;
    Edge() {}
    Edge(const std::string & s) {
        assert(!s.empty());
        for (auto c : s) {
            assert(isalpha(c) || isspace(c));
        }
        std::istringstream iss(s);
        std::string sub;
        iss >> sub;
        source = State(sub);
        iss >> sub;
        sink = State(sub);
        iss >> sub;
        symbol = Symbol(sub);
        iss >> sub;
        fn = Func(sub);
    }
};

std::ostream & operator<<(std::ostream & os, const Edge & e) {
    os << e.source << " -> " << e.sink << " " << e.symbol << "|" << e.fn;
    return os;
}

std::vector<Symbol> toSymbols(const std::string & symbols) {
    std::vector<Symbol> syms;
    std::istringstream iss(symbols);
    do {
        std::string sub;
        iss >> sub;
        if (!sub.empty()) { syms.push_back(Symbol(sub)); }
    } while (iss);
    return syms;
}

bool operator<(const Edge & e1, const Edge & e2) {
    return e1.source.name.compare(e2.source.name) < 0;
}

std::vector<State> toStates(const std::string & states) {
    std::vector<State> sts;
    std::istringstream iss(states);
    do {
        std::string sub;
        iss >> sub;
        if (!sub.empty()) { sts.push_back(State(sub)); }
    } while (iss);
    return sts;
}

std::multiset<Edge> toEdges(const std::vector<std::string> & rawEdges) {
    std::multiset<Edge> edges;
    for (auto s : rawEdges) {
        edges.insert(Edge(s));
    }
    return edges;
}

// Gets the functions called by walking through the symbols to the FSM
std::vector<std::string> runFSM(const std::string & rawSymbols,
                                const std::string & rawStates,
                                const std::vector<std::string> & rawEdges) {
    std::vector<std::string> output;
    auto symbols = toSymbols(rawSymbols);
    auto states = toStates(rawStates);
    auto edges = toEdges(rawEdges);
    auto currStateIt = states.begin();
    for (auto currSymbIt = symbols.begin(); currSymbIt != symbols.end();
         ++currSymbIt) {
        // Look at edges leaving the current state
        auto ret = edges.equal_range(currStateIt->name);
        auto currEdgeIt = edges.end();
        for (auto it = ret.first; it != ret.second; ++it) {
            if (*currSymbIt == it->symbol) {
                currEdgeIt = it;
                break;
            }
        }
        if (currEdgeIt == edges.end()) { output.push_back("ERROR"); continue; }
        output.push_back(currEdgeIt->fn.f + "(" + currSymbIt->sym + ")");
        for (auto it = states.begin(); it != states.end(); ++it) {
            if (it->name == currEdgeIt->sink.name) {
                currStateIt = it;
                break;
            }
        }
    }
    return output;
}

int main() {
    std::string states =
        "Zerocents Fivecents Tencents Fifteencents Twentycents";
    std::vector<std::string> edges = {
        "Zerocents  Fivecents  nickel needMoreOne",
        "Zerocents  Tencents dime   needMoreTwo",
        "Fivecents  Fifteencents dime   needMoreThree",
        "Fivecents  Tencents nickel needMoreFour",
        "Tencents Fifteencents nickel needMoreFive",
        "Tencents Twentycents dime   releasingCandyOne",
        "Fifteencents Twentycents nickel releasingCandyTwo",
        "Fifteencents Twentycents dime   releasingCandyThree",
        "Twentycents Fivecents  nickel needMoreSix",
        "Twentycents Tencents dime   needMoreSeven"
    };
    assert(runFSM("nickel dime dime", states, edges) ==
           std::vector<std::string>({"needMoreOne(nickel)",
                                     "needMoreThree(dime)",
                                     "releasingCandyThree(dime)"}));
    assert(runFSM("dime oops dime", states, edges) ==
           std::vector<std::string>({"needMoreTwo(dime)",
                                     "ERROR",
                                     "releasingCandyOne(dime)"}));
    return 0;
}
