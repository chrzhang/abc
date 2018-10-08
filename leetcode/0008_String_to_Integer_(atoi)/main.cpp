class Solution {
public:
    int myAtoi(string str) {
        if (str.empty()) { return 0; }
        auto it = str.begin();
        while (*it == ' ') {++it;}
        if (it == str.end()) { return 0; }
        if (*it != '+' && *it != '-' && (*it < '0' || *it > '9')) { return 0; }
        const bool is_pos = *it == '-' ? false : true;
        const auto start = (*it == '-' || *it == '+') ? it + 1 : it;
        if (start == str.end()) { return 0; }
        it = start;
        while (it != str.end() && (*it >= '0' && *it <= '9')) { ++it; }
        if (it <= start) { return 0; }
        --it;
        int x = 0;
        int place = 0;
        for (;;) {
            const int consider = (*it - '0') * pow(10, place);
            if (x > INT_MAX - consider) {
                return is_pos ? INT_MAX : INT_MIN;
            }
            x += consider;
            ++place;
            if (it == start) { return is_pos ? x : -1 * x; }
            --it;
        }
    }
};