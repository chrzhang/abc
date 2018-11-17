class Solution {
public:
    int romanToInt(string s)
    {
        std::map<char, int> romanNums = { { 'I', 1 }, { 'V', 5 }, { 'X', 10 }, { 'L', 50 }, { 'C', 100 }, { 'D', 500 },
            { 'M', 1000 } };
        int currAmt = 0;
        for (auto it = s.begin(); it != s.end(); ++it) {
            auto after = std::next(it);
            if (after != s.end() && romanNums[*after] > romanNums[*it]) {
                currAmt -= romanNums[*it];
                currAmt += romanNums[*after];
                ++it; // Must get incremented twice
                continue;
            }
            currAmt += romanNums[*it];
        }
        return currAmt;
    }
};