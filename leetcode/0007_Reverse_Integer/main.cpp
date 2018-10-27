class Solution {
public:
    int reverse(int x) {
        auto s = std::to_string(abs(x));
        s = std::string(s.rbegin(), s.rend());
        try {
            if (x < 0) { s.insert(0, 1, '-'); }
            return std::stoi(s);
        } catch (const std::out_of_range & oor) {
            return 0;
        }
    }
};