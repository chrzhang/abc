class Solution {
public:
    void swap(int& a, int& b, int& c, int& d)
    {
        int ta(a), tb(b), tc(c), td(d);
        a = td;
        b = ta;
        c = tb;
        d = tc;
    }

    void rotate(vector<vector<int>>& v)
    {
        const int N = (int)v.size();
        if (N < 2)
            return;
        int l = 0;
        for (;;) {
            pair<int, int> ul = make_pair(l, l);
            pair<int, int> ur = make_pair(l, N - 1 - l);
            pair<int, int> ll = make_pair(N - 1 - l, l);
            pair<int, int> lr = make_pair(N - 1 - l, N - 1 - l);
            if (ul.second >= ur.second)
                return;
            for (int i = 0; i < N - (2 * l) - 1; ++i) {
                swap(v[ul.first][ul.second],
                    v[ur.first][ur.second],
                    v[lr.first][lr.second],
                    v[ll.first][ll.second]);
                ul.second += 1;
                ur.first += 1;
                lr.second -= 1;
                ll.first -= 1;
            }
            ++l;
        }
    }
};