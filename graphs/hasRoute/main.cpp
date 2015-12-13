#include <iostream>
#include <unordered_map>
#include <ctime>
#include <cstdlib>
#include <iomanip>
#include <assert.h>

#define NUM_NODES 5
#define NUM_ITERATIONS 100

// Check if there is a route from one node to another in a directed graph

struct Node {
    int val;
    Node * prev, * next;
    Node(int val) {
        this->val = val;
        this->prev = this->next = nullptr;
    }
};

struct Queue { // For BFS
    Node * head, * tail;
    Queue() {
        head = tail = nullptr;
    }
    ~Queue() {
        Node * nptr = head;
        while (nptr) {
            assert(false);
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void enqueue(Node * n) {
        if (!n) { return; }
        n->prev = n->next = nullptr;
        if (!head) {
            head = tail = n;
        } else {
            tail->prev = n;
            n->next = tail;
            tail = n;
        }
    }
    Node * dequeue() {
        if (!head) { return nullptr; }
        Node * temp = head;
        head = head->prev;
        if (head) {
            head->next = nullptr;
        } else {
            tail = nullptr;
        }
        temp->next = temp->prev = nullptr;
        return temp;
    }
    bool isEmpty() const {
        return (head == nullptr);
    }
};

std::ostream & operator<<(std::ostream & os, const Queue & q) {
    os << "tail -> ";
    Node * nptr = q.tail;
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
    os << "-> head";
    return os;
}

struct Graph {
    bool adjMatrix[NUM_NODES][NUM_NODES];
    std::unordered_map<int, Node *> allNodes;
    Graph() {
        // Fill the graph with random valued nodes
        for (int i = 0; i < NUM_NODES; ++i) {
            allNodes[i] = new Node(i);
        }
        // Randomly fill adjacency matrix
        for (int i = 0; i < NUM_NODES; ++i) {
            for (int j = 0; j < NUM_NODES; ++j) {
                adjMatrix[i][j] = rand() % 2;
            }
        }
    }
    ~Graph() {
        for (auto it = allNodes.begin(); it != allNodes.end(); ++it) {
            delete it->second;
        }
    }
    bool hasRouteBFS(int srcIndex, int destIndex) {
        if (allNodes.find(srcIndex) == allNodes.end() ||
            allNodes.find(destIndex) == allNodes.end()) {
            std::cout << "Source or destination is not in the graph."
                      << std::endl;
            return false;
        }
        bool visited[NUM_NODES] = { false };
        Queue q;
        // Enqueue the start node
        visited[srcIndex] = true;
        q.enqueue(allNodes[srcIndex]);
        while (!q.isEmpty()) {
            std::cout << "Queue: " << q << std::endl;
            // Dequeue, check if it's destination, and enqueue neighbors
            Node * n = q.dequeue();
            std::cout << "Looking at node " << n->val << " and marking as visited." << std::endl;
            assert(n);
            if (n->val == destIndex) {
                std::cout << "\t\t\t\tFound destination!" << std::endl;
                while (!q.isEmpty()) {
                    q.dequeue();
                }
                return true;
            }
            for (int j = 0; j < NUM_NODES; ++j) {
                if (adjMatrix[n->val][j]) {
                    if (!visited[j]) {
                        std::cout << "Enqueueing neighbor node " << allNodes[j]->val << std::endl;
                        visited[j] = true;
                        q.enqueue(allNodes[j]);
                    }
                }
            }
        }
        std::cout << "\t\t\t\tCould not reach destination." << std::endl;
        return false;
    }
};

std::ostream & operator<<(std::ostream & os, const Graph & g) {
    os << std::setw(4) << "";
    for (int i = 0; i < NUM_NODES; ++i) {
        os << std::setw(4) << i;
    }
    os << std::endl;
    for (int i = 0; i < NUM_NODES; ++i) {
        os << std::setw(4) << i;
        for (int j = 0; j < NUM_NODES; ++j) {
            os << std::setw(4) << g.adjMatrix[i][j];
        }
        os << std::endl;
    }
    return os;
}

int main() {
    srand(time(0));
    for (int iteration = 0; iteration < NUM_ITERATIONS; ++iteration) {
        Graph g;
        std::cout << g << std::endl;
        g.hasRouteBFS(0, NUM_NODES - 1);
        std::cout << std::endl;
    }
}
