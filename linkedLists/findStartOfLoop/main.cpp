#include <iostream>
#include <ctime>
#include <cstdlib>
#include <assert.h>

#define MAX_NUM_NODES 10
#define MAX_NODE_VAL 100
#define NUM_ITERATIONS 1000

// Given a linked list, find the start of a loop (if any)

struct Node {
    int val;
    Node * next;
    Node(int val) {
        this->val = val;
        next = nullptr;
    }
};

Node * findStartOfLoop(Node * root) {
    if (!root) return nullptr;
    // Walk through the list with a slow and fast walker
    Node * slowWalker, * fastWalker;
    slowWalker = fastWalker = root;
    while (slowWalker && fastWalker) {
        slowWalker = slowWalker->next;
        fastWalker = fastWalker->next->next;
        assert(slowWalker && fastWalker);
        if (slowWalker == fastWalker) {
            // Detect a loop with a collision
            break;
        }
    }
    // slowWalker has traveled k nodes
    // fastWalker has traveled 2k nodes
    // If root is ax nodes away from the start of the loop, the collision point
    // is x nodes away from the start of the loop where x can be 1,2,3,...
    // Since root is a multiple of x away we can walk from the point of
    // collision and root in lock step until they match to find the start
    slowWalker = root;
    while (slowWalker != fastWalker) {
        slowWalker = slowWalker->next;
        fastWalker = fastWalker->next;
    }
    return slowWalker;
}

void printList(Node * root, Node * tail) {
    Node * nptr = root;
    while (nptr) {
        std::cout << nptr->val << " ";
        if (nptr == tail) {
            break;
        }
        nptr = nptr->next;
    }
}

int main() {
    srand(time(0));
    for (int k = 0; k < NUM_ITERATIONS; ++k) {
        Node * root = new Node((rand() % (MAX_NODE_VAL - 1)) + 1); // 1 to 100
        int numNodes = rand() % (MAX_NUM_NODES + 1);
        Node * nodeToLoopBackTo = root;
        Node * temp = root;
        if (numNodes) {
            // Pick which node to loop to
            int indexOfNodeToLoopBackTo = rand() % numNodes;
            for (int i = 0; i < numNodes; ++i) {
                if (i == indexOfNodeToLoopBackTo) {
                    nodeToLoopBackTo = temp;
                }
                temp->next = new Node((rand() % (MAX_NODE_VAL - 1)) + 1);
                temp = temp->next;
            }
        }
        Node * tail = temp; // For testing, give print the non-loop part
        temp->next = nodeToLoopBackTo;
        printList(root, tail);
        std::cout << " -> loops to " << nodeToLoopBackTo->val << std::endl;
        assert(nodeToLoopBackTo == findStartOfLoop(root)); // Test the fn
        // Free the nodes
        Node * nptr = root;
        while (nptr) {
            if (nptr == tail) {
                delete nptr;
                break;
            }
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    return 0;
}
