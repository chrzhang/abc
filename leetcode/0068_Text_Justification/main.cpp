class Solution {
string jaux(const int cwi, const int lwi, const vector<string> & words, const int WIDTH) {
    string result;
    if (lwi == cwi + 1) {
        result = words[cwi];
        return result + string(WIDTH - result.size(), ' ');
    }
    if (lwi == (int) words.size()) {
        for (int i = cwi; i < lwi; ++i) {
            result += words[i];
            if (i < lwi - 1) {
                result += " ";
            }
        }
        return result + string(WIDTH - result.size(), ' ');
    }
    vector<int> gaps(lwi - cwi - 1, 1);
    int text_amount = 0;
    for (int i = cwi; i < lwi; ++i) {
        text_amount += words[i].size();
    }
    int extra_whitespace = WIDTH - text_amount - (lwi - cwi - 1);
    int j = 0;
    for (int i = 0; i < extra_whitespace; ++i) {
        gaps[j] += 1;
        j++;
        if (j == (int) gaps.size()) { j = 0; }
    }
    for (int wi = cwi, gi = 0; wi < lwi; ++wi, ++gi) {
        string t = words[wi];
        if (gi < (int) gaps.size()) {
            t += string(gaps[gi], ' ');
        }
        result += t;
    }
    return result;
}
public:
vector<string> fullJustify(const vector<string> & words, const int WIDTH) {
    vector<string> result;
    if (words.empty()) { return result; }
    int clw(0), cwi(0), lwi(1);
    while (cwi < (int) words.size()) {
        clw = words[cwi].size();
        for (; lwi < words.size() && clw + 1 + (int) words[lwi].size() <= WIDTH; ++lwi) {
            clw += 1 + words[lwi].size();
        }
        result.push_back(jaux(cwi, lwi, words, WIDTH));
        cwi = lwi;
        lwi = cwi + 1;
    }
    return result;
}
};