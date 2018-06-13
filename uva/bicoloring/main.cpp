#include <iostream>
#include <map>
#include <vector>
#include <cassert>
#include <list>
#include <fstream>

using namespace std;
using vert_t = int;

struct Graph {
    map<vert_t, vector<vert_t>> adj_list;
    void add_edge(const vert_t & from,
                  const vert_t & toward) {
        adj_list[from].push_back(toward);
        adj_list[toward].push_back(from);
    }
    bool bicolorable() const {
        if (adj_list.size() < 3) return true;
        enum COLOR { RED, BLUE };
        map<vert_t, COLOR> vertex_colors;
        list<vert_t> traversal_queue;
        vert_t curr_vertex = adj_list.begin()->first;
        traversal_queue.push_front(curr_vertex);
        vertex_colors[curr_vertex] = RED;
        while (!traversal_queue.empty()) {
            curr_vertex = traversal_queue.back();
            traversal_queue.pop_back();
            const COLOR curr_color = vertex_colors[curr_vertex];
            for (const vert_t & neighbor : adj_list.at(curr_vertex)) {
                auto seek = vertex_colors.find(neighbor);
                if (seek == vertex_colors.end()) {
                    vertex_colors[neighbor] = curr_color == RED ? BLUE : RED;
                    traversal_queue.push_front(neighbor);
                } else if (seek->second == curr_color) {
                    return false;
                }
            }
        }

        return true;
    }
};

ostream & operator<<(ostream & os, const Graph & graph) {
    for (const auto & mpair : graph.adj_list) {
        const vert_t & vertex = mpair.first;
        const vector<vert_t> & edge_list = mpair.second;
        os << vertex << " | ";
        for (const vert_t & edge_dest : edge_list) {
            os << edge_dest << " ";
        }
        os << endl;
    }
    return os;
}

int main() {
    {
        Graph g;
        g.add_edge(0, 1);
        g.add_edge(1, 2);
        g.add_edge(2, 0);
        assert(!g.bicolorable());
    }
    {
        Graph g;
        g.add_edge(0, 1);
        g.add_edge(1, 2);
        assert(g.bicolorable());
    }
    {
        Graph g;
        g.add_edge(0, 1);
        g.add_edge(0, 2);
        g.add_edge(0, 3);
        g.add_edge(0, 4);
        g.add_edge(0, 5);
        g.add_edge(0, 6);
        g.add_edge(0, 7);
        g.add_edge(0, 8);
        assert(g.bicolorable());
    }
    ifstream inFile("input.txt");
    int node_ct, edge_ct;
    while (inFile >> node_ct && inFile >> edge_ct) {
        Graph g;
        for (int edge_i = 0; edge_i < edge_ct; ++edge_i) {
            vert_t from, toward;
            if (!(inFile >> from && inFile >> toward)) {
                assert(false);
            }
            g.add_edge(from, toward);
        }
        cout << (g.bicolorable() ? "BICOLORABLE." : "NOT BICOLORABLE.") << endl;
    }
}
