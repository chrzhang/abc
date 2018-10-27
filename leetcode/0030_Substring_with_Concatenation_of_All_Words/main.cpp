class Solution {
public:
bool occurs_at(const size_t i, const string & s, const string & canonical, const int N) {
    string ss = s.substr(i, canonical.size());
    assert(ss.size() == canonical.size());
    vector<size_t> indices;
    for (size_t l = 0; l < ss.size() / N; ++l) {
        indices.push_back(l);
    }
    sort(indices.begin(), indices.end(), [&](int j, int k) {
        return ss.substr(j * N, N) < ss.substr(k * N, N);
    });
    string css;
    for (size_t i : indices) {
        css += ss.substr(i * N, N);
    }
    return css == canonical;

}

vector<int> findSubstring(const string & s, vector<string> words) {
    vector<int> result;
    string canonical;
    sort(words.begin(), words.end());
    for (const string & word : words) {
        canonical += word;
    }
    if (canonical.size() == 0) { return result; }
    if (canonical.size() > s.size()) { return result; }
    for (size_t i = 0; i <= s.size() - canonical.size(); ++i) {
        if (occurs_at(i, s, canonical, words[0].size())) {
            result.push_back(i);
        }
    }
    return result;
}

};