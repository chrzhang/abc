class Solution {
public:
    void findPathsAux(TreeNode * parent, vector<string> & result, string currPath) {
        if (!parent) {
            return;
        }
        if (currPath.empty()) {
            currPath += to_string(parent->val);
        } else {
            currPath += "->" + to_string(parent->val);
        }
        findPathsAux(parent->left, result, currPath);
        findPathsAux(parent->right, result, currPath);
        if (!parent->left && !parent->right) {
            if (!currPath.empty()) { result.push_back(currPath); }
        }
    }
    vector<string> binaryTreePaths(TreeNode* root) {
        vector<string> result;
        findPathsAux(root, result, "");
        return result;
    }
};