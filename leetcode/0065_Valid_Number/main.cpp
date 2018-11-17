class Solution {
public:
    bool isNumber(const string& s)
    {
        regex rx("^[\\s]*([+-]?[0-9]+(\\.[0-9]*)?)(e[+-]?[0-9]+)?[\\s]*$");
        regex rx2("^[\\s]*[+-]?(\\.[0-9]+)(e[+-]?[0-9]+)?[\\s]*$");
        smatch result;
        if (regex_search(s, result, rx)) {
            return true;
        }
        if (regex_search(s, result, rx2)) {
            return true;
        }
        return false;
    }
};