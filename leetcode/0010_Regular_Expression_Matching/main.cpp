class Solution {
public:
    bool isMatchStar(const char c, const char* regexp, const char* text)
    {
        do {
            if (isMatchAux(text, regexp)) {
                return true;
            }
        } while (*text && (*text++ == c || c == '.'));
        return 0;
    }

    bool isMatchAux(const char* text, const char* regexp)
    {
        if (regexp[0] == 0) {
            return text[0] == 0;
        }
        if (regexp[1] == '*') {
            return isMatchStar(regexp[0], regexp + 2, text);
        }
        if (text[0] != 0 && (regexp[0] == '.' || regexp[0] == *text)) {
            return isMatchAux(text + 1, regexp + 1);
        }
        return false;
    }

    bool isMatch(const string& s, const string& p)
    {
        return isMatchAux(s.c_str(), p.c_str());
    }
};