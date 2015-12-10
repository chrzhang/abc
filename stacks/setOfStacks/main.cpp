#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>
#include <deque>
#include <unordered_map>

#define SUB_STACK_MAX_HEIGHT 5
#define NUM_ITERATIONS 1000
#define NUM_STACK_OPS 1000

// Build a set of stacks that acts (has same API) as a normal stack

struct Node {
    intptr_t val;
    bool isSentinel;
    Node * next;
    Node(intptr_t val, bool isSentinel = false) {
        this->val = val;
        next = nullptr;
        this->isSentinel = isSentinel;
    }
};

struct SetOfStacks {
    Node * top;
    // Maintain a list of sentinels (signifying the end of a substack)
    std::unordered_map<int, intptr_t> sentinelAddressesMap; // For popAt
    size_t currentSubstackOffset; // How far in a substack the stack is
    size_t currentSubstack;
    SetOfStacks() {
        top = nullptr;
        currentSubstackOffset = 0;
        currentSubstack = 0;
    }
    ~SetOfStacks() {
        Node * nptr = top;
        while (nptr) {
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void push(Node * n) {
        if (currentSubstackOffset == SUB_STACK_MAX_HEIGHT) {
            // Build and insert a sentinel
            Node * sentinel = new Node(-1, true);
            sentinel->next = top;
            // Store the sentinel's address in listOfSentinelAddresses
            assert(sentinelAddressesMap.find(currentSubstack)
                   == sentinelAddressesMap.end());
            sentinelAddressesMap[currentSubstack] = (intptr_t) sentinel;
            top = sentinel;
            // Reset the offset in our new substack
            currentSubstackOffset = 0;
            ++currentSubstack;
        }
        // Insert the node n
        n->next = top;
        top = n;
        // Increment the offset in the substack
        ++currentSubstackOffset;
    }
    Node * pop() {
        if (!top) return nullptr;
        assert(!top->isSentinel);
        Node * temp1 = top;
        top = top->next;
        if (top && top->isSentinel) {
            --currentSubstack;
            sentinelAddressesMap.erase(currentSubstack);
            Node * temp2 = top->next;
            delete top;
            top = temp2;
        }
        if (currentSubstackOffset == 1 && top) {
            currentSubstackOffset = SUB_STACK_MAX_HEIGHT;
        } else {
            --currentSubstackOffset;
        }
        return temp1;
    }
    Node * popAt(int subStackIndex) {
        if (subStackIndex == currentSubstack + 1) {
            return pop();
        }
        assert(sentinelAddressesMap.find(subStackIndex) !=
               sentinelAddressesMap.end());
        // Remove the node after the sentinel for the substack
        // Remove the node before the sentinel and put it as next
        // Swap all sentinels afterwards with their predecessors
        return nullptr;
    }
};

std::ostream & operator<<(std::ostream & os, const SetOfStacks & sos) {
    os << "top <- ";
    Node * nptr = sos.top;
    while (nptr) {
        if (nptr->isSentinel) {
            os << "s ";
        } else {
            os << nptr->val << " ";
        }
        nptr = nptr->next;
    }
    os << std::endl;
    return os;
}

bool matches(const std::deque<int> & d, const SetOfStacks & sos) {
    Node * nptr = sos.top;
    for (auto it = d.begin(); it != d.end(); ++it, nptr = nptr->next) {
        if (nptr->isSentinel) { nptr = nptr->next; } // Skip sentinels
        if (*it != nptr->val) { return false; }
    }
    assert(!nptr); // Same length
    return true;
}

void printSentinels(const std::unordered_map<int, intptr_t> & map) {
    for (auto it = map.begin(); it != map.end(); ++it) {
        // TODO Print information about predecessor and successor for debugging
    }
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        SetOfStacks sos;
        std::deque<int> standardStack;
        for (int stackop = 0; stackop < NUM_STACK_OPS; ++stackop) {
            switch (rand() % 2) { // Either push or pop
                case 0 : {
                    int randval = rand() % 100;
                    sos.push(new Node(randval));
                    standardStack.push_front(randval);
                    assert(matches(standardStack, sos));
                    // std::cout << "+ Pushed " << sos;
                    printSentinels(sos.sentinelAddressesMap);
                    break;
                }
                case 1: {
                    Node * popped = sos.pop();
                    delete popped;
                    if (!standardStack.empty()) {
                        standardStack.pop_front();
                    }
                    assert(matches(standardStack, sos));
                    // std::cout << "- Popped " << sos;
                    printSentinels(sos.sentinelAddressesMap);
                    break;
                }
            }
        }
    }
    return 0;
}
