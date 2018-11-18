class Solution {
    bool allFound(const int d_enc[128], const int d_alpha[128])
    {
        for (int i = 0; i < 128; ++i) {
            if (d_alpha[i] > d_enc[i]) {
                return false;
            }
        }
        return true;
    }

public:
    string minWindow(const string& v, const string& d)
    {
        if (d.empty()) {
            return "";
        }
        if (v.empty()) {
            return "";
        }
        // Slide a window [b, e) from the left rightward until all chars in d are found
        // Then shift b right by 1, and keep shifting e rightward from the last e
        // Since we consider every beginning and every possible ending, solution will be found
        int best_begin(0), best_end(v.size() + 1), b(0), e(0);
        int d_alpha[128] = { 0 };
        for (const char c : d) {
            d_alpha[c]++;
        }
        int d_enc[128] = { 0 }; // Char counter of encountered
        while (e <= v.size() && b < v.size()) {
            while (!allFound(d_enc, d_alpha) && e < v.size()) {
                d_enc[v[e++]]++;
            }
            if (!allFound(d_enc, d_alpha)) {
                break;
            }
            if (e - b < best_end - best_begin) {
                best_end = e;
                best_begin = b;
            }
            if (b == e - 1)
                break; // No window can be smaller than 1
            d_enc[v[b++]]--;
        }
        cout << best_end << endl;
        if (best_end <= v.size()) {
            return string(v, best_begin, best_end - best_begin);
        }
        return "";
    }
};
