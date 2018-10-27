class Solution {
public:
struct PairCompare {
    bool operator()(const std::pair<int, int> & p1,
                    const std::pair<int, int> & p2) const {
        return (p1.first + p1.second) < (p2.first + p2.second);
    }
};

std::vector<std::pair<int, int>>
kSmallestPairs( std::vector<int> & a1,
                std::vector<int> & a2, int k) {
    std::vector<std::pair<int, int>> heap;
    for (size_t i = 0; i < a1.size(); ++i) {
        for (size_t j = 0; j < a2.size(); ++j) {
            if ((int) heap.size() < k) {
                heap.push_back(std::make_pair(a1[i], a2[j]));
                if ((int) heap.size() == k) {
                    std::make_heap(heap.begin(), heap.end(), PairCompare());
                }
            } else if (a1[i] + a2[j] <
                       heap.front().first + heap.front().second) {
                heap[0] = std::make_pair(a1[i], a2[j]);
                std::make_heap(heap.begin(), heap.end(), PairCompare());
            }
        }
    }
    std::sort_heap(heap.begin(), heap.end(), PairCompare());
    return heap;
}
};