class Solution {
public:
int jump(const vector<int> & v) {
    if (v.size() < 2) { return 0; }
    // Walk the vector, finding out which step # we are currently in
    int liicl = 0; // last index in current level
    int cl = 0; // current level
    int i = 0; // to walk through all of v
    for (;;) {
        int firfcl = liicl; // furthest i reachable from curr level
        for (; i <= liicl; ++i) { // walk through curr level
            firfcl = max(firfcl, v[i] + i); // how far our next level goes
        }
        ++cl;
        if (firfcl >= (int) (v.size() - 1)) { return cl; } // can reach last item
        liicl = firfcl;
    }
}
};