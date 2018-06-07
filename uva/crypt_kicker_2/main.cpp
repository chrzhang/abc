#include <iostream>
#include <cassert>
#include <vector>
#include <stdexcept>

using namespace std;

static const string reference = "the quick brown fox jumps over the lazy dog";

bool matchesKey(const string & s) {
    if (s.size() != reference.size()) {
        return false;
    }
    for (const auto c : s) {
        if ((c < 'a' || c > 'z') && c != ' ') {
            return false;
        }
    }
    bool letterFound[26] = {false};
    for (size_t ii = 0; ii < s.size(); ++ii) {
        if (reference[ii] == ' ' && s[ii] != ' ') {
            return false;
        }
        letterFound[s[ii] - 'a'] = true;
    }
    for (size_t ii = 0; ii < 26; ++ii) {
        if (!letterFound[ii]) {
            return false;
        }
    }
    return true;
}

void makeRosetta(const string & key, char rosetta[26]) {
    assert(matchesKey(key));
    for (size_t ii = 0; ii < key.size(); ++ii) {
        rosetta[key[ii] - 'a'] = reference[ii];
    }
}

void solve(const vector<string> & lines) {
    char rosetta[26] = {0};
    for (const auto & line : lines) {
        if (matchesKey(line)) {
            makeRosetta(line, rosetta);
            break;
        }
    }
    for (const auto & line : lines) {
        string decoded;
        for (const auto c : line) {
            if (c == ' ') {
                decoded += ' ';
            } else {
                decoded += rosetta[c - 'a'];
            }
        }
        cout << decoded << endl;
    }
}

int main() {
    assert(matchesKey("xnm ceuob lrtzv ita hegfd tsmr xnm ypwq ktj"));
    solve({"vtz ud xnm xugm itr pyy jttk gmv xt otgm xt xnm puk ti xnm fprxq",
           "xnm ceuob lrtzv ita hegfd tsmr xnm ypwq ktj",
           "frtjrpgguvj otvxmdxd prm iev prmvx xnmq"});
}
