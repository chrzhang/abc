#include <iostream>
#include <string>
#include <cassert>

// Paths from root to leaf of a tree with nodes of numeric digits represent
// numbers (1->2->3 is 123)
// Find the sum of all paths

struct Node {
    int val;
    Node * left, * right;
    Node(int v) : val(v), left(nullptr), right(nullptr) {}
};

void sumNumbersAux(Node * n, int & sumSoFar, std::string s) {
    if (!n) { return; }
    s += std::to_string(n->val);
    if (!n->left && !n->right) {
        sumSoFar += std::stoi(s);
        return;
    }
    if (n->left) {
        sumNumbersAux(n->left, sumSoFar, s);
    }
    if (n->right) {
        sumNumbersAux(n->right, sumSoFar, s);
    }
}

int sumNumbers(Node * root) {
    int sum = 0;
    sumNumbersAux(root, sum, "");
    return sum;
}

int main() {
    Node * n1 = new Node(1);
    n1->left = new Node(2);
    n1->right = new Node(3);
    assert(sumNumbers(n1) == 12 + 13);
    delete n1->left;
    delete n1->right;
    delete n1;
    return 0;
}
