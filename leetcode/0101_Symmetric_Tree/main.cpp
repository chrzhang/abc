class Solution {
public:
    bool isSymmetricAux(TreeNode * n1, TreeNode * n2) {
        if (!n1 && !n2) { return true; }
        if (!n1 && n2) { return false; }
        if (n1 && !n2) { return false; }
        if (n1->val != n2->val) { return false; }
        return isSymmetricAux(n1->left, n2->right) && isSymmetricAux(n1->right, n2->left);
    }
    bool isSymmetric(TreeNode* root) {
        if (!root)  { return true; }
        return isSymmetricAux(root->left, root->right);
    }
};