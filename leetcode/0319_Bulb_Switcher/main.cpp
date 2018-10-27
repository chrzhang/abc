class Solution {
public:
    int bulbSwitch(int n) {
        if (n == 0) { return 0; }
        if (n == 1) { return n; }
        return sqrt(n);
        vector<bool> lights(n, true);
        for (int round = 2; round <= n; ++round) {
            for (int i = round; i <= n; i += round) {
                lights[i - 1] = lights[i - 1] ? false : true;
            }
        }
        int acc = 0;
        for (auto x : lights) {
            if (x) { ++acc; }
        }
        return acc;
    }
};