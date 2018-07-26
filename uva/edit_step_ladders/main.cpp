#include <iostream>
#include <vector>
#include <cassert>
#include <set>
#include <climits>
#include <fstream>

using namespace std;

struct State {
    string word;
    mutable int maxlen_ladder_starting_from;
    State(const string & word) : word(word), maxlen_ladder_starting_from(1) {}
};

bool operator<(const State & lhs, const State & rhs) {
    return lhs.word < rhs.word;
}

bool can_transform_add_or_remove(const string & a, const string & b) {
    if (a.size() == b.size()) return false;
    const string & longer = a.size() > b.size() ? a : b;
    const string & shorter = a.size() > b.size() ? b : a;
    if (longer.size() - shorter.size() != 1) return false;
    for (auto s_it = shorter.begin(), l_it = longer.begin();
         s_it != shorter.end() && l_it != longer.end(); ++s_it, ++l_it) {
        if (*s_it != *l_it) {
            return string(s_it, shorter.end()) == string(next(l_it), longer.end());
        }
    }
    return true;
}

bool can_transform_change(const string & a, const string & b) {
    if (a.size() != b.size()) return false;
    for (auto a_it = a.begin(), b_it = b.begin(); a_it != a.end(), b_it != b.end(); ++a_it, ++b_it) {
        if (*a_it != *b_it) {
            return string(next(a_it), a.end()) == string(next(b_it), b.end());
        }
    }
    return true;
}

bool can_transform(const string & a, const string & b) {
    assert(!a.empty() && !b.empty());
    if (a == b) return false;
    return can_transform_add_or_remove(a, b) ||
           can_transform_change(a, b);
}

int solve(const set<string> dictionary) {
    /* Each word is a state in an implicit graph with the transforms being
     * edges. Solving requires knowing the longest step ladder from each word.
     * Knowing longest step ladder from each word also helps us decide whether
     * to transform to another word as an intermediary state to maximize length.
     * Since the ladder must ascend lexicographically, the only states to
     * consider jumping to must come after the source when sorted.
     */
     if (dictionary.size() < 2) { return dictionary.size(); }
     set<State> words_with_lens;
     for (const string & word : dictionary) {
        words_with_lens.insert(word);
     }
     for (auto curr_start = words_with_lens.rbegin(); curr_start != words_with_lens.rend(); ++curr_start) {
        const auto & word = curr_start->word;
        int & maxlen_ladder_starting_from = curr_start->maxlen_ladder_starting_from;
        for (auto succ_it = curr_start; ; --succ_it) {
            if (can_transform(word, succ_it->word)) {
                if (1 + succ_it->maxlen_ladder_starting_from >
                    maxlen_ladder_starting_from) {
                    maxlen_ladder_starting_from = 1 + succ_it->maxlen_ladder_starting_from;
                }
            }
            if (succ_it == words_with_lens.rbegin()) break;
        }
        // If successor's length is greater AND we can transform to it
        // Use successor's length
     }
     int max_len = INT_MIN;
     for (auto it = words_with_lens.begin(); it != words_with_lens.end(); ++it) {
        max_len = max(it->maxlen_ladder_starting_from, max_len);
     }
     return max_len;
}

int main() {
    assert(can_transform_change("dog", "dig"));
    assert(!can_transform("dog", "dim"));
    assert(can_transform_add_or_remove("dog", "do"));
    assert(can_transform_add_or_remove("dog", "doge"));
    assert(!can_transform("fine", "fog"));
    assert(can_transform("fine", "wine"));
    assert(can_transform_add_or_remove("aaaaaah", "aaaaah"));
    assert(5 == solve({"cat", "dig", "dog", "fig", "fin", "fine", "fog", "log", "wine"}));
    ifstream inFile("input.txt");
    string word;
    set<string> dict;
    while (inFile >> word) {
        dict.insert(word);
    }
    assert(11 == solve(dict));
}
