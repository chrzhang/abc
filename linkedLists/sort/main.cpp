#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cassert>

#define N 1000

// Sort a list

struct Node {
    int val;
    Node * next;
    Node(int v) : val(v), next(nullptr) {}
};

struct List {
    Node * head, * tail;
    List() : head(nullptr), tail(nullptr) {}
    ~List() {
        auto curr = head;
        while (curr) {
            auto temp = curr->next;
            delete curr;
            curr = temp;
        }
    }
    void add(int v) {
        auto n = new Node(v);
        if (!head) {
            head = tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
    }
    void sort() {
        if (!head) { return; }
        // Using insertion sort, the list is sorted by expanding a sorted
        // sublist from empty to the entire size of the list
        Node * endOfSorted = head; // One past the tail of the sorted sublist
        Node * tailOfSorted = nullptr;
        while (endOfSorted) {
            if (endOfSorted->next == nullptr) {
                tail = endOfSorted;
            }
            Node * temp = endOfSorted;
            Node * pred = nullptr;
            Node * currUnsortedMinNode = temp;
            Node * currUnsortedMinNodePred = nullptr;
            while (temp) {
                if (temp->val < currUnsortedMinNode->val) {
                    currUnsortedMinNode = temp;
                    currUnsortedMinNodePred = pred;
                }
                pred = temp;
                temp = temp->next;
            }
            if (currUnsortedMinNode == endOfSorted) {
                if (!tailOfSorted) { head = endOfSorted; }
                tailOfSorted = endOfSorted;
                endOfSorted = endOfSorted->next;
            } else {
                assert(currUnsortedMinNodePred);
                // Transplanting currUnsortedMinNode->val
                currUnsortedMinNodePred->next = currUnsortedMinNode->next;
                if (tailOfSorted) {
                    tailOfSorted->next = currUnsortedMinNode;
                    tailOfSorted = tailOfSorted->next;
                } else {
                    tailOfSorted = currUnsortedMinNode;
                    head = tailOfSorted;
                }
                tailOfSorted->next = endOfSorted;
            }
        }
    }
    bool isSorted() const {
        if (!head) { return true; }
        Node * pred = nullptr;
        Node * curr = head;
        while (curr) {
            if (pred) {
                if (curr->val < pred->val) { return false; }
            }
            pred = curr;
            curr = curr->next;
        }
        return true;
    }
};

std::ostream & operator<<(std::ostream & os, const List & l) {
    auto curr = l.head;
    while (curr) {
        os << curr->val << " ";
        if (curr->next == nullptr) {
            assert(l.tail == curr);
        }
        curr = curr->next;
    }
    os << "\n";
    return os;
}

int main() {
    srand(time(0));
    for (int sizeOfList = 0; sizeOfList < N; ++sizeOfList) {
        std::cout << "Testing list of length " << sizeOfList << std::endl;
        List l;
        for (int i = 0; i < sizeOfList; ++i) {
            l.add(rand() % 1000000);
        }
        //std::cout << l;
        l.sort();
        //std::cout << l;
        assert(l.isSorted());
    }
    return 0;
}
