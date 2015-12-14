#include <iostream>
#include <unordered_map>
#include <ctime>
#include <cstdlib>
#include <iomanip>
#include <assert.h>

#define NUM_NODES 100
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

struct Stack { // For DFS
    Node * top;
    Stack() {
        top = nullptr;
    }
    ~Stack() {
        Node * nptr = top;
        while (nptr) {
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void push(Node * n) {
        if (!n) { return; }
        n->next = n->prev = nullptr;
        if (!top) {
            top = n;
        } else {
            top->prev = n;
            n->next = top;
            top = n;
        }
    }
    Node * pop() {
        if (!top) { return nullptr; }
        Node * temp = top;
        top = top->next;
        if (top) { top->prev = nullptr; }
        temp->prev = temp->next = nullptr;
        return temp;
    }
    bool isEmpty() const {
        return (top == nullptr);
    }
};

std::ostream & operator<<(std::ostream & os, const Stack & s) {
    Node * nptr = s.top;
    std::cout << "top -> ";
    while (nptr) {
        os << nptr->val << " ";
        nptr = nptr->next;
    }
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
            // Dequeue, check if it's destination, and enqueue neighbors
            Node * n = q.dequeue();
            assert(n);
            if (n->val == destIndex) {
                while (!q.isEmpty()) {
                    q.dequeue();
                }
                return true;
            }
            for (int j = 0; j < NUM_NODES; ++j) {
                if (adjMatrix[n->val][j]) {
                    if (!visited[j]) {
                        visited[j] = true;
                        q.enqueue(allNodes[j]);
                    }
                }
            }
        }
        return false;
    }
    bool hasRouteDFS(int srcIndex, int destIndex) {
        if (allNodes.find(srcIndex) == allNodes.end() ||
            allNodes.find(destIndex) == allNodes.end()) {
            std::cout << "Source or destination is not in the graph."
                      << std::endl;
            return false;
        }
        bool visited[NUM_NODES] = { false };
        Stack s;
        visited[srcIndex] = true;
        s.push(allNodes[srcIndex]);
        while (!s.isEmpty()) {
            Node * n = s.pop();
            assert(n);
            if (n->val == destIndex) {
                while (!s.isEmpty()) {
                    s.pop();
                }
                return true;
            }
            for (int j = 0; j < NUM_NODES; ++j) {
                if (adjMatrix[n->val][j]) {
                    if (!visited[j]) {
                        visited[j] = true;
                        s.push(allNodes[j]);
                    }
                }
            }
        }
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
        int randNodeIndex = rand() % NUM_NODES;
        assert(g.hasRouteBFS(0, randNodeIndex) ==
               g.hasRouteDFS(0, randNodeIndex));
    }
}
