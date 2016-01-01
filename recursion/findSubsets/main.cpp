#include <iostream>
#include <assert.h>
#include <vector>
#include <cmath>
#include <cstdlib>
#include <ctime>

#define SET_SIZE 10
#define NUM_ITERATIONS 10

template<typename T>
struct ListNode {
    T val;
    ListNode<T> * next;
    ListNode(T val) {
        this->val = val;
        next = nullptr;
    }
};

template<typename T>
struct List {
    ListNode<T> * head, * tail;
    size_t size;
    List() {
        head = tail = nullptr;
        size = 0;
    }
    List(const List<T> & otherList) {
        head = tail = nullptr;
        size = 0;
        ListNode<T> * nptr = otherList.head;
        while (nptr) {
            pushBack(new ListNode<T>(nptr->val));
            nptr = nptr->next;
        }
    }
    ~List() {
        ListNode<T> * nptr = head;
        while (nptr) {
            ListNode<T> * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void pushBack(ListNode<T> * n) {
        if (!n) { return; }
        ++size;
        if (!head) {
            head = tail = n;
        } else {
            tail->next = n;
            tail = n;
        }
    }
    ListNode<T> * popFront() {
        if (!head) { assert(size == 0); return nullptr; }
        --size;
        ListNode<T> * nptr = head;
        head = head->next;
        if (!head) { tail = head; }
        return nptr;
    }
};

template<typename T>
std::ostream & operator<<(std::ostream & os, const List<T> & l) {
    os << "[";
    ListNode<int> * nptr = l.head;
    if (nptr) {
        os << nptr->val;
        nptr = nptr->next;
    }
    while (nptr) {
        os << ", " << nptr->val;
        nptr = nptr->next;
    }
    os << "]";
    return os;
}

template<typename T>
std::vector<List<T>> genSubsets(List<T> l) {
    // Remove first element, generate all subsets of rest, append first to each
    std::vector<List<T>> subsets;
    if (l.size <= 0) {
        subsets.push_back(List<T>());
        return subsets;
    }
    if (l.size == 1) {
        subsets.push_back(l);
        subsets.push_back(List<T>());
        return subsets;
    }
    ListNode<T> * front = l.popFront();
    subsets = genSubsets(l);
    std::vector<List<T>> addition;
    for (auto it = subsets.begin(); it != subsets.end(); ++it) {
        List<T> copyL = *it;
        copyL.pushBack(new ListNode<T>(front->val));
        addition.push_back(copyL);
    }
    subsets.insert(subsets.end(), addition.begin(), addition.end());
    delete front;
    return subsets;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        List<int> l;
        int setSize = rand() % SET_SIZE;
        for (int i = 1; i <= setSize; ++i) {
            l.pushBack(new ListNode<int>(i));
        }
        std::cout << l;
        std::vector<List<int>> allSubsets = genSubsets(l);

        std::cout << " contains " << allSubsets.size() << " subsets: "
                  << std::endl;
        for (auto it = allSubsets.begin(); it != allSubsets.end(); ++it) {
            std::cout << *it << " ";
        }
        std::cout << std::endl << std::endl;
        // n things have 2^n subsets
        assert(pow(2, l.size) == allSubsets.size());
    }
    return 0;
}
