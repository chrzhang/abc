class Solution {
public:
    bool isSameTree(TreeNode* p, TreeNode* q) {
        if (!p) { return !q; }
        if (!q) { return !p; }
        if (p->val != q->val) { return false; }
        return isSameTree(p->left, q->left) && isSameTree(p->right, q->right);
    }
};