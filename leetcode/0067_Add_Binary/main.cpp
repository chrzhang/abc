class Solution {
public:
    string addBinary(string a, string b)
    {
        string r;
        auto aIt = a.rbegin();
        auto bIt = b.rbegin();
        int carry = 0;
        while (!(aIt == a.rend() && bIt == b.rend())) {
            char c1, c2;
            c1 = c2 = '0';
            if (aIt != a.rend()) {
                c1 = *aIt;
                ++aIt;
            } else {
                assert(c1 == '0');
            }
            if (bIt != b.rend()) {
                c2 = *bIt;
                ++bIt;
            } else {
                assert(c2 == '0');
            }
            int i1 = c1 == '0' ? 0 : 1;
            int i2 = c2 == '0' ? 0 : 1;
            switch (i1 + i2 + carry) {
            case 0: {
                r.push_back('0');
                carry = 0;
                break;
            }
            case 1: {
                r.push_back('1');
                carry = 0;
                break;
            }
            case 2: {
                r.push_back('0');
                carry = 1;
                break;
            }
            case 3: {
                r.push_back('1');
                carry = 1;
                break;
            }
            default: {
                assert(0);
            }
            };
        }
        if (carry) {
            r.push_back('1');
        }
        return string(r.rbegin(), r.rend());
    }
};
