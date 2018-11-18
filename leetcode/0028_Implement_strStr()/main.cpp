class Solution {
public:
    // TODO Use Boyer-Moore to make this fast like grep
    bool match(const string& t, size_t i, const string& p)
    {
        for (size_t j = 0; j < p.size(); ++j) {
            if (i >= t.size())
                return false;
            if (t[i++] != p[j])
                return false;
        }
        return true;
    }
    int strStr(string haystack, string needle)
    {
        if (needle.empty())
            return 0;
        for (size_t i = 0; i < haystack.size(); ++i) {
            if (match(haystack, i, needle))
                return i;
        }
        return -1;
    }
};