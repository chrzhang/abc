#include <iostream>
#include <vector>
#include <cassert>
#include <climits>
#include <algorithm>

// Three runners are in a race where there are n checkpoints. Each time a runner
// crosses a checkpoint, their time is recorded. Find the checkpoint at which
// the runners are closest in the race.

int checkpointClosest(std::vector<int> runner1times,
                      std::vector<int> runner2times,
                      std::vector<int> runner3times) {
    if (!(runner1times.size() == runner2times.size() &&
          runner2times.size() == runner3times.size()) ||
        (runner1times.size() == 0)) {
        return -1;
    }
    int checkpoint = -1;
    int minDiff = INT_MAX;
    for (size_t i = 0; i < runner1times.size(); ++i) {
        int potentialNewMinDiff = std::max({runner1times[i], runner2times[i],
                                            runner3times[i]}) -
                                  std::min({runner1times[i], runner2times[i],
                                              runner3times[i]});
        if (potentialNewMinDiff <= minDiff) {
            minDiff = potentialNewMinDiff;
            checkpoint = i;
        }
    }
    return checkpoint;
}

int main() {
    assert(3 == checkpointClosest(std::vector<int>({3, 4, 5, 8}),
                                  std::vector<int>({3, 4, 6, 8}),
                                  std::vector<int>({3, 5, 7, 8})));
    assert(2 == checkpointClosest(std::vector<int>({10, 40, 90}),
                                  std::vector<int>({12, 37, 87}),
                                  std::vector<int>({15, 32, 88})));
    return 0;
}
