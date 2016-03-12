#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <cassert>

// Determine whether a string can be formed by interleaving two given strings

bool isInterleaveAux(const std::string & s1,
                     const std::string & s2,
                     const std::string & s3,
                     int s1i, int s2i, int s3i,
                     std::map<std::vector<int>, bool> & results) {
    auto seek = results.find(std::vector<int>{s1i, s2i, s3i});
    if (seek != results.end()) {
        return seek->second;
    }
    if (s1i >= (int) s1.size() &&
        s2i >= (int) s2.size() &&
        s3i >= (int) s3.size()) {
        results[std::vector<int>{s1i, s2i, s3i}] = true;
        return true;
    }
    if (s3i >= (int) s3.size()) { return false; }
    if (s1i < (int) s1.size()) {
        if ((s3[s3i] == s1[s1i]) &&
             isInterleaveAux(s1, s2, s3, s1i + 1, s2i, s3i + 1, results)) {
            results[std::vector<int>{s1i, s2i, s3i}] = true;
            return true;
        }
    }
    if (s2i < (int) s2.size()) {
        if ((s3[s3i] == s2[s2i]) &&
             isInterleaveAux(s1, s2, s3, s1i, s2i + 1, s3i + 1, results)) {
            results[std::vector<int>{s1i, s2i, s3i}] = true;
            return true;
        }
    }
    results[std::vector<int>{s1i, s2i, s3i}] = false;
    return false;
}

bool isInterleave(const std::string & s1,
                  const std::string & s2,
                  const std::string & s3) {
    if (s3.size() != s1.size() + s2.size()) { return false; }
    std::map<std::vector<int>, bool> results;
    return isInterleaveAux(s1, s2, s3, 0, 0, 0, results);
}

int main() {
    assert( isInterleave("aabcc", "dbbca", "aadbbcbcac"));
    assert(!isInterleave("aabcc", "dbbca", "aadbbbaccc"));
    assert( isInterleave("xxy", "xxz", "xxyxxz"));
    assert( isInterleave("xxy", "xxz", "xxxxzy"));
    assert( isInterleave("xxy", "xxz", "xxxxyz"));
    return 0;
}
