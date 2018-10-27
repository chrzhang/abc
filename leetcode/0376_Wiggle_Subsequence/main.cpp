class Solution {
public:
size_t wiggleMaxLength(const std::vector<int> & numbers) {
    if (numbers.size() < 2) {
        return numbers.size();
    }
    size_t count = 1;
    enum SEEKING { ANY, UP, DOWN };
    SEEKING target = ANY;
    for (size_t i = 1; i < numbers.size(); ++i) {
        if (numbers[i - 1] < numbers[i] &&
            (target == ANY || target == DOWN)) {
            ++count;
            target = UP;
        } else if (numbers[i - 1] > numbers[i]
                   && (target == ANY || target == UP)) {
            ++count;
            target = DOWN;
        }
    }
    return count;
}
};