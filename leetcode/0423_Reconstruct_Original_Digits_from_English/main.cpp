class Solution {
public:
    vector<int> words_possible(const vector<string> & words, const int alpha_ctr[26]) {
        vector<int> result;
        for (int i = 0; i < words.size(); ++i) {
            const string & curr_word = words[i];
            int word_alpha[26] = {0};
            for (const char c : curr_word) {
                word_alpha[c - 'a']++;
            }
            bool possible = true;
            for (int j = 0; j < 26; ++j) {
                if (word_alpha[j] > alpha_ctr[j]) {
                    possible = false;
                    break;
                }
            }
            if (possible) {
                result.push_back(i);
            }
        }
        return result;
    }
    void adj_ctr_rem(int alpha_ctr[26], const string & word) {
        for (const char c : word) {
            alpha_ctr[c - 'a']--;
        }
    }
    void adj_ctr_add(int alpha_ctr[26], const string & word) {
        for (const char c : word) {
            alpha_ctr[c - 'a']++;
        }
    }
    bool all_used(const int alpha_ctr[26]) {
        for (int i = 0; i < 26; ++i) {
            if (alpha_ctr[i]) {
                return false;
            }
        }
        return true;
    }
    bool solve_aux(const vector<string> & words, int alpha_ctr[26], list<int> & nums) {
        if (all_used(alpha_ctr)) return true;
        vector<int> possibilities = words_possible(words, alpha_ctr);
        for (const int word_i : possibilities) {
            const string & w = words[word_i];
            int minOcc = INT_MAX;
            for (auto c : w) {
                minOcc = min(alpha_ctr[c - 'a'], minOcc);
            }
            for (int i = 0; i < minOcc; ++i) {
                adj_ctr_rem(alpha_ctr, w);
                nums.push_back(word_i);
            }
            if (solve_aux(words, alpha_ctr, nums)) {
                return true;
            }
            for (int i = 0; i < minOcc; ++i) {
                adj_ctr_add(alpha_ctr, words[word_i]);
                nums.pop_back();
            }

        }
        return false;
    }
    string originalDigits(string s) {
        int alpha_ctr[26] = {0};
        for (const char c : s) {
            alpha_ctr[c - 'a']++;
        }
        static const vector<string> words({
            "zero",
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine"
        });
        list<int> nums;
        solve_aux(words, alpha_ctr, nums);
        string result = "";
        for (const int i : nums) {
            result += to_string(i);
        }
        sort(result.begin(), result.end());
        return result;
        
    }
};