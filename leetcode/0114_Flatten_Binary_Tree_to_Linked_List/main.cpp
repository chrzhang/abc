class Solution {
public:
    void insertRight(TreeNode * parent, TreeNode * n) {
        if (!(parent &&n)) { return; }
        if (parent->right) {
            insertRight(parent->right, n);
        } else {
            parent->right = n;
        }
    }
    void flattenAux(TreeNode * & otherTreeRoot, TreeNode * root) {
        if (!root) { return; }
        auto temp = root->left;
        auto temp2 = root->right;
        root->left = nullptr;
        root->right = nullptr;
        if (!otherTreeRoot) {
            otherTreeRoot = root;
        } else {
            insertRight(otherTreeRoot, root);
        }
        flattenAux(otherTreeRoot, temp);
        flattenAux(otherTreeRoot, temp2);
    }
    void flatten(TreeNode * root) {
        if (!root) { return; }
        TreeNode * otherTreeRoot = nullptr;
        flattenAux(otherTreeRoot, root);
    }
};
