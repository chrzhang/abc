class Solution {
public:
int find_row(const vector<vector<int>> & matrix, const int target, const int begin, const int end) {
    if (begin > end) { return -1; }
    const int mid = (begin + end) / 2;
    if (*matrix[mid].begin() <= target && *matrix[mid].rbegin() >= target) {
        return mid;
    } else if (*matrix[mid].rbegin() < target) {
        return find_row(matrix, target, mid + 1, end);
    } else {
        return find_row(matrix, target, begin, mid - 1);
    }
}

int find_col(const vector<int> & row, const int target, const int begin, const int end) {
    if (begin > end) { return -1; }
    const int mid = (begin + end) / 2;
    if (row[mid] == target) { return mid; }
    if (row[mid] < target) { return find_col(row, target, mid + 1, end); }
    return find_col(row, target, begin, mid - 1);
}

bool searchMatrix(const vector<vector<int>> & matrix, const int target) {
    if (matrix.empty()) { return false; }
    if (matrix[0].empty()) { return false; }
    const int r = find_row(matrix, target, 0, (int) matrix.size() - 1);
    if (r == -1) { return false; }
    const int c = find_col(matrix[r], target, 0, (int) matrix[0].size() - 1);
    if (c == -1) { return false; }
    return true;
}
};