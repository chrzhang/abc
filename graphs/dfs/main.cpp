#include <iostream>
#include <list>
#include <vector>
#include <cassert>
#include <stack>

// Perform a depth-first traversal over a graph

struct Graph {
    unsigned numVertices;
    std::vector<std::list<unsigned>> adjLists;
    Graph() : numVertices(0) {}
    Graph(size_t size) {
        numVertices = size;
        adjLists = { size, std::list<unsigned>() };
    }
    void addEdge(unsigned v1, unsigned v2) { // Adds edge both ways
        assert(v1 < numVertices && v2 < numVertices);
        adjLists[v1].push_back(v2);
        adjLists[v2].push_back(v1);
    }
    void addVertex() {
        ++numVertices;
        adjLists.push_back(std::list<unsigned>());
    }
    void dfs(unsigned originIndex) {
        assert(originIndex < numVertices);
        std::vector<bool> visited(adjLists.size(), false );
        std::stack<int> needProcessing;
        needProcessing.push(originIndex);
        while (!needProcessing.empty()) {
            int e = needProcessing.top();
            needProcessing.pop();
            if (!visited[e]) {
                visited[e] = true;
                // Process information here
                std::cout << e << " ";
                std::list<unsigned> & adjList = adjLists[e];
                for (auto it = adjList.begin(); it != adjList.end(); ++it) {
                    if (!visited[*it]) {
                        needProcessing.push(*it);
                    }
                }
            }
        }
        std::cout << "\n";
    }
};

int main() {
    Graph g(6);
    g.addEdge(0, 4);
    g.addEdge(0, 1);
    g.addEdge(1, 4);
    g.addEdge(4, 3);
    g.addEdge(2, 1);
    g.addEdge(2, 3);
    g.addEdge(5, 3);
    g.dfs(0);
    return 0;
}
