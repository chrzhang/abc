class Solution {
public:
    void sumNumbersAux(TreeNode* n, int& sumSoFar, std::string s)
    {
        if (!n) {
            return;
        }
        s += to_string(n->val);
        if (!n->left && !n->right) {
            sumSoFar += stoi(s);
            return;
        }
        if (n->left) {
            sumNumbersAux(n->left, sumSoFar, s);
        }
        if (n->right) {
            sumNumbersAux(n->right, sumSoFar, s);
        }
    }
    int sumNumbers(TreeNode* root)
    {
        int sum = 0;
        sumNumbersAux(root, sum, "");
        return sum;
    }
};