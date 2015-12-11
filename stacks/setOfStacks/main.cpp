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
    Node * prev;
    Node(intptr_t val, bool isSentinel = false) {
        this->val = val;
        next = prev = nullptr;
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
            top->prev = sentinel;
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
        if (top) { top->prev = n; }
        top = n;
        assert(top->prev == nullptr);
        // Increment the offset in the substack
        ++currentSubstackOffset;
    }
    Node * pop() {
        if (!top) return nullptr;
        assert(!top->isSentinel);
        Node * temp1 = top;
        top = top->next;
        if (top) { top->prev = nullptr; }
        if (top && top->isSentinel) {
            --currentSubstack;
            sentinelAddressesMap.erase(currentSubstack);
            Node * temp2 = top->next;
            delete top;
            top = temp2;
            top->prev = nullptr;
        }
        if (currentSubstackOffset == 1 && top) {
            currentSubstackOffset = SUB_STACK_MAX_HEIGHT;
        } else {
            --currentSubstackOffset;
        }
        return temp1;
    }
    // Pop from a specific substack and "shift" everything over with ptr manip
    Node * popAt(size_t subStackIndex) {
        if (subStackIndex == currentSubstack) {
            return pop();
        } else if (subStackIndex > currentSubstack) {
            // Asking for a substack that does not exist
            std::cout << "substack with index " << subStackIndex
                      << " does not exist." << std::endl;
            return nullptr;
        }
        assert(sentinelAddressesMap.find(subStackIndex) !=
               sentinelAddressesMap.end());
        Node * sentinel = (Node *) sentinelAddressesMap[subStackIndex];
        assert(sentinel);
        std::cout << "current substack: " << currentSubstack << std::endl;
        // assert(sentinel->prev->prev);
        // Remove the node after the sentinel for the substack
        Node * n = sentinel->next;
        assert(n);
        sentinel->next = sentinel->next->next;
        sentinel->next->prev = sentinel;
        // Swap all sentinels afterwards with their predecessors
        for (int i = subStackIndex; sentinelAddressesMap.find(i) !=
                                    sentinelAddressesMap.end(); ++i) {
            Node * sentinel = (Node *) sentinelAddressesMap[i];
            assert(sentinel);
            Node * pn = sentinel->prev;
            Node * sn = sentinel->next;
            if (!pn->prev) {
                assert(top == pn);
                pn->next = sn;
                sn->prev = pn;
                delete sentinel;
                sentinelAddressesMap.erase(i);
                --currentSubstack;
                break;
            }
            pn->prev->next = sentinel;
            sentinel->prev = pn->prev;
            sentinel->next = pn;
            pn->prev = sentinel;
            pn->next = sn;
            sn->prev = pn;
        }
        --currentSubstackOffset;
        n->prev = n->next = nullptr; // Cut it off before returning
        return n;
    }
};

std::ostream & operator<<(std::ostream & os, const SetOfStacks & sos) {
    int ct = sos.currentSubstack - 1;
    os << "top <- ";
    Node * nptr = sos.top;
    while (nptr) {
        if (nptr->isSentinel) {
            os << "[stack " << ct-- << "] ";
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
        std::cout << ((Node *) (it->second))->prev->val << " s" << it->first
                  << " " << ((Node *) (it->second))->next->val << std::endl;
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
                    //std::cout << "+ Pushed " << sos;
                    //printSentinels(sos.sentinelAddressesMap);
                    break;
                }
                case 1: {
                    Node * popped = sos.pop();
                    delete popped;
                    if (!standardStack.empty()) {
                        standardStack.pop_front();
                    }
                    assert(matches(standardStack, sos));
                    //std::cout << "- Popped " << sos;
                    //printSentinels(sos.sentinelAddressesMap);
                    break;
                }
            }
        }
    }
    std::cout << "Testing popAt(stackIndex)\n";
    // Tested with a size of 14 and trying 0 repeatedly until past empty
    // Tested with a size of 14 and trying 1 repeatedly until only stack 0 left
    // Tested with a size of 5 and trying 1,2... (for stacks that DNE)
    SetOfStacks sos;
    std::string input;
    int length = 0;
    while (length <= 0) {
        std::cout << "Enter a pos. length for the total set of stacks: ";
        getline(std::cin, input);
        length = stoi(input);
    }
    for (int i = 0; i < stoi(input); ++i) {
        sos.push(new Node(rand() % 100));
    }
    // Each stack is separated by s, to denote a sentinel
    std::cout << "Generated a new set of stacks:\n" << sos;
    input.clear();
    int subStackIndex = 1;
    while (subStackIndex >= 0) {
        std::cout << "Enter a substack index to popAt (-1 to quit): ";
        getline(std::cin, input);
        subStackIndex = stoi(input);
        delete sos.popAt(subStackIndex);
        std::cout << sos;
    }
    return 0;
}
