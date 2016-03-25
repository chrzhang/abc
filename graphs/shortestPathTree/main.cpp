#include <iostream>
#include <iomanip>
#include <algorithm>
#include <set>
#include <unordered_map>
#include <climits>
#include <vector>
#include <cassert>

#define WIDTH 5

// Find the shortest-path tree, a spanning tree such that the path distance from
// the chosen root vertex to any other vertex is the shortest possible
// Note the program does not create a graph but outputs the minimal distance
// between a chosen root and every other vertex

struct Edge;

struct Vertex {
    int id;
    std::set<Edge> adjList;
    Vertex(int i) : id(i) {}
};

struct Edge {
    Vertex * destination;
    unsigned weight;
    Edge(Vertex * d, unsigned w) : destination(d), weight(w) {}
};

bool operator<(const Edge & e1, const Edge & e2) {
    if (e1.destination == e2.destination) {
        return e1.weight < e2.weight;
    } else {
        return e1.destination < e2.destination;
    }
}

struct Graph {
    std::unordered_map<int, Vertex *> allVertices;
    int currVertexIndex;
    Graph() : currVertexIndex(0) {}
    ~Graph() {
        for (auto idVertex : allVertices) {
            delete idVertex.second;
        }
    }
    void addVertex(size_t amount) {
        for (size_t v = 0; v < amount; ++v) {
            int id = currVertexIndex++;
            auto seek = allVertices.find(id);
            if (seek != allVertices.end()) {
                std::cout << "Prevented adding a duplicate vertex.\n";
                return;
            }
            allVertices[id] = new Vertex(id);
        }
    }
    void connect(int id1, int id2, unsigned weight) {
        auto seek1 = allVertices.find(id1);
        auto seek2 = allVertices.find(id2);
        if (weight == 0 || seek1 == allVertices.end() ||
            seek2 == allVertices.end()) {
            std::cout << "Invalid arguments to adding an edge.\n";
            return;
        }
        Vertex * v1 = seek1->second;
        Vertex * v2 = seek2->second;
        v1->adjList.insert(Edge(v2, weight));
        v2->adjList.insert(Edge(v1, weight));
    }
};

void genShortestPath(int idRoot, const Graph * g) {
    if (!g || idRoot < 0 || idRoot >= (int) g->allVertices.size()) {
        return;
    }
    // Create a set of all vertices in the shortest path tree
    std::set<int> shortestPathTreeSet; // Starts empty
    // Assign a distance to all vertices in the graph (INF, 0 at source)
    std::vector<unsigned> distances (g->allVertices.size(), UINT_MAX);
    distances[idRoot] = 0;
    // While not all nodes are accounted for
    while (shortestPathTreeSet.size() < g->allVertices.size()) {
        // Pick a vertex that is not accounted for with minimum distance value
        int smallestIndex = -1;
        unsigned smallestVal = UINT_MAX;
        for (size_t i = 0; i < distances.size(); ++i) {
            auto seek = shortestPathTreeSet.find(i);
            if (seek != shortestPathTreeSet.end()) { continue; }
            if (distances[i] < smallestVal) {
                smallestVal = distances[i];
                smallestIndex = (int) i;
            }
        }
        if (smallestIndex == -1) { assert(false); }
        // Add vertex to set of nodes accounted for
        shortestPathTreeSet.insert(smallestIndex);
        // Update distance of all adjacent nodes by giving it the value of
        // min(currDistanceValue, currDistanceValueOfSource + edgeWeight)
        for (auto e : g->allVertices.at(smallestIndex)->adjList) {
            Vertex * dest = e.destination;
            distances[dest->id] = std::min(distances[dest->id], smallestVal +
                                           e.weight);
        }
    }
    for (size_t i = 0; i < distances.size(); ++i) {
        std::cout << "Shortest path from " << idRoot << " to " << i << " is "
                  << distances[i] << "\n";
    }
}

unsigned isConnectedTo(Vertex * v1, Vertex * v2) {
    if (!v1 || !v2) { return 0; }
    std::set<Edge> & adjList = v1->adjList;
    for (auto e : adjList) {
        if (e.destination == v2) { return e.weight; }
    }
    return 0;
}

std::ostream & operator<<(std::ostream & os, const Graph & g) {
    os << "Adjacency list:\n";
    for (auto it = g.allVertices.begin(); it != g.allVertices.end(); ++it) {
        os << it->first << ": ";
        for (auto edge : it->second->adjList) {
            os << edge.destination->id << " (" << edge.weight
               << ") ";
        }
        os << "\n";
    }
    os << "Adjacency matrix:\n";
    for (int i = -1; i < (int) g.allVertices.size(); ++i) {
        for (int j = -1; j < (int) g.allVertices.size(); ++j) {
            if (i == -1 && j == -1) {
                std::cout << std::setw(WIDTH) << " " << " ";
            } else if (i == -1) {
                std::cout << std::setw(WIDTH) << j << " ";
            } else if (j == -1) {
                std::cout << std::setw(WIDTH) << i << " ";
            } else {
                auto r = isConnectedTo(g.allVertices.at(i),
                                       g.allVertices.at(j));
                os << std::setw(WIDTH)
                   << (r ? std::to_string(r) : " ") << " ";
            }
        }
        os << "\n";
    }
    return os;
}

int main() {
    Graph g;
    g.addVertex(9);
    g.connect(0, 1, 4);
    g.connect(1, 2, 8);
    g.connect(2, 3, 7);
    g.connect(3, 4, 9);
    g.connect(4, 5, 10);
    g.connect(5, 6, 2);
    g.connect(6, 7, 1);
    g.connect(7, 0, 8);
    g.connect(1, 7, 11);
    g.connect(7, 8, 7);
    g.connect(6, 8, 6);
    g.connect(2, 8, 2);
    g.connect(2, 5, 4);
    g.connect(3, 5, 14);
    std::cout << g;
    genShortestPath(0, &g);
    return 0;
}
