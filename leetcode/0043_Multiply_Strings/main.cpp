class Solution {
public:
string add(const string & a, const string & b) {
    if (a.empty()) { return b; }
    if (b.empty()) { return a; }
    string result;
    int carry = 0;
    auto at = a.rbegin();
    auto bt = b.rbegin();
    while (at != a.rend() || bt != b.rend() || carry) {
        const int aval = at == a.rend() ? 0 : *at - '0';
        const int bval = bt == b.rend() ? 0 : *bt - '0';
        const int dsum = aval + bval + carry;
        result.push_back(dsum % 10 + '0');
        carry = dsum / 10;
        if (at != a.rend()) { ++at; }
        if (bt != b.rend()) { ++bt; }
    }
    return string(result.rbegin(), result.rend());
}
string multd(const char d, const string & s) {
    auto it = s.rbegin();
    int carry = 0;
    const int dval = d - '0';
    string result;
    while (carry || it != s.rend()) {
        const int ival = it == s.rend() ? 0 : *it - '0';
        const int dprod = dval * ival + carry;
        result.push_back(dprod % 10 + '0');
        carry = dprod / 10;
        if (it != s.rend()) { ++it; }
    }
    return string(result.rbegin(), result.rend());
}
void trim(string & s) {
    if (s.size() < 2) { return; }
    auto it = s.begin();
    while (it != prev(s.end()) && it != s.end() && *it == '0') {
        ++it;
    }
    if (it != s.end()) {
        s = string(it, s.end());
    }
}
string multiply(const string & a, const string & b) {
    string shorter, longer;
    if (a.size() <= b.size()) {
        shorter = a;
        longer = b;
    } else {
        shorter = b;
        longer = a;
    }
    string result;
    int i = 0;
    for (auto it = shorter.rbegin(); it != shorter.rend(); ++it, ++i) {
        string prod = multd(*it, longer);
        prod.append(i, '0');
        result = add(result, prod);
    }
    trim(result);
    return result;
}
};