class Solution {
public:
    // Do a BFS traversal over the array
    bool canJump(vector<int>& a)
    {
        if (a.size() < 2) {
            return true;
        }
        int i = 0;
        int f = a[i]; // furthest reachable in current level
        for (;;) {
            if (f >= a.size() - 1) {
                return true;
            } // can reach the end
            int nf = i; // to figure out how far the next level goes
            for (; i <= f; ++i) { // note that the last ++i will make i > f
                nf = max(nf, a[i] + i);
            }
            f = nf;
            if (f == i - 1) {
                return false;
            } // the next level gets us nowhere (i - 1) because i is currently > f
        }
        return false;
    }
};