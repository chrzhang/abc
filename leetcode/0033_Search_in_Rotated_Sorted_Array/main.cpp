class Solution {
public:
int faux(const vector<int> & v,
         const int begin,
         const int end,
         const int n) {
    if (begin > end) { return -1; }
    const int mid = (begin + end) / 2;
    if (v[mid] == n) { return mid; }
    if (v[begin] <= v[mid - 1]) {
        if (begin < mid && n >= v[begin] && n <= v[mid - 1]) {
            return faux(v, begin, mid - 1, n);
        } else {
            return faux(v, mid + 1, end, n);
        }
    } else if (v[mid + 1] <= v[end]) {
        if (n >= v[mid + 1]  && n <= v[end]) {
            return faux(v, mid + 1, end, n);
        } else {
            return faux(v, begin, mid - 1, n);
        }
    } else {
        return -1; 
    }
}
int search(const vector<int> & v, const int n) {
    return faux(v, 0, v.size() - 1, n);
}
};