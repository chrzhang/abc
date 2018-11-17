class Solution {
public:
    bool isIsomorphic(string s, string t)
    {
        if (s.empty() && t.empty()) {
            return true;
        }
        int sLastMapping, tLastMapping;
        sLastMapping = tLastMapping = 0;
        unordered_map<char, int> sIds;
        unordered_map<char, int> tIds;
        auto sit = s.begin();
        auto tit = t.begin();
        while (sit != s.end()) {
            auto seekS = sIds.find(*sit);
            auto seekT = tIds.find(*tit);
            if (seekS == sIds.end()) {
                sIds[*sit] = ++sLastMapping;
            } else {
                sLastMapping = seekS->second;
            }
            if (seekT == tIds.end()) {
                tIds[*tit] = ++tLastMapping;
            } else {
                tLastMapping = seekT->second;
            }
            if (tLastMapping != sLastMapping) {
                return false;
            }
            ++sit;
            ++tit;
        }
        return true;
    }
};