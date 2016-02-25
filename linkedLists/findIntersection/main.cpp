#include <iostream>
#include <cassert>
#include <cmath>

// Find first shared node of two intersecting singly linked lists

struct Node {
    int val;
    Node * next;
    Node(int v) : val(v), next(nullptr) {}
};

struct List {
    Node * head, * tail;
    List() : head(nullptr) {}
    ~List() {
        Node * curr = head;
        while (curr) {
            Node * temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(int val) {
        Node * n = new Node(val);
        if (!head) {
            head = tail = n;
        } else {
            n->next = head;
            head = n;
        }
    }
    void print() const {
        Node * curr = head;
        while (curr) {
            std::cout << curr->val << " ";
            curr = curr->next;
        }
        std::cout << std::endl;
    }
};

int findLen(Node * n) {
    int r = 0;
    while (n) {
        ++r;
        n = n->next;
    }
    return r;
}

Node * getIntersection(Node * headA, Node * headB) {
    // Get lengths of both lists, traverse the longer one an offset so if there
    // is an intersection it will be reached if we advance through the list
    // in lock-step
    int lenA = findLen(headA);
    int lenB = findLen(headB);
    Node * headLonger = lenA > lenB ? headA : headB;
    Node * headShorter = lenA > lenB ? headB : headA;
    for (int i = 0; i < abs(lenA - lenB); ++i) {
        headLonger = headLonger->next;
    }
    while (headLonger && headShorter) {
        if (headLonger == headShorter) { return headLonger; }
        headLonger = headLonger->next;
        headShorter = headShorter->next;
    }
    return nullptr;
}

int main() {
    for (int amtToAdvance = 0; amtToAdvance < 9; ++amtToAdvance) {
        printf("amt to advance: %d\n", amtToAdvance);
        List l;
        for (int i = 0; i < 10; ++i) {
            l.add(i);
        }
        List otherList;
        for (int i = 0; i < 5; ++i) {
            otherList.add(i);
        }
        // Temporarily connect otherList to a node in l
        Node * nodeToConnectTo = l.head;
        for (int i = 0; i < amtToAdvance; ++i) {
            nodeToConnectTo = nodeToConnectTo->next;
        }
        otherList.tail->next = nodeToConnectTo;
        l.print();
        otherList.print();
        assert(getIntersection(l.head, otherList.head) == nodeToConnectTo);
        otherList.tail->next = nullptr;
    }
    return 0;
}
