#include <iostream>
#include <vector>
#include <map>
#include <climits>
#include <cassert>

using namespace std;

struct Edge {
    int to;
    int weight;
    Edge(const int to, const int weight)
    : to(to), weight(weight) {}
};

struct Graph {
    map<int, vector<Edge>> adj_list;
    Graph() {}
    void connect(const int a, const int b, const int weight) {
        adj_list[a].push_back(Edge(b, weight));
        adj_list[b].push_back(Edge(a, weight));
    }
    size_t size() const {
        return adj_list.size();
    }
    int weight(const int from, const int to) const {
        if (from == to) return 0;
        const vector<Edge> & edges = adj_list.at(from);
        for (const auto & edge : edges) {
            if (edge.to == to) return edge.weight;
        }
        return INT_MAX;
    }
};

int get_best_route(const Graph & cities, const int start_city,
                   const int end_city, const int passenger_ct) {
    // The best route will be bottlenecked by its smallest edge, which also
    // defines the number of trips to be taken.
    // Finding the best route means finding the route where the smallest edge
    // encountered can fit the most people, overall reducing our bottleneck.
    // Let N be the number of cities
    // Create a N x N matrix M where M[r][c] is the biggest bottleneck between
    // r and c.
    vector<vector<int>> best_paths_matrix;
    for (size_t row_i = 0; row_i < cities.size(); ++row_i) {
        vector<int> row;
        for (size_t col_i = 0; col_i < cities.size(); ++col_i) {
            const int weight = cities.weight(row_i, col_i);
            row.push_back(weight == INT_MAX ? 0 : weight); // Because of max()
        }
        best_paths_matrix.push_back(row);
    }
    for (size_t k = 0; k < cities.size(); ++k) { // Intermediate city
        for (size_t r = 0; r < cities.size(); ++r) { // Start city
            for (size_t c = 0; c < cities.size(); ++c) { // End city
                if (r == c || r == k || c == k) continue;
                // Going through k means updating our bottleneck
                const int bottleneck_thru_k = min(best_paths_matrix[r][k],
                                                  best_paths_matrix[k][c]);
                const int curr_bottleneck = best_paths_matrix[r][c];
                if (bottleneck_thru_k > curr_bottleneck) {
                    // Prefer the bigger bottleneck
                    best_paths_matrix[r][c] = bottleneck_thru_k;
                }
            }
        }
    }
    const int biggest_bottleneck = best_paths_matrix[start_city][end_city];
    // Account for tour guide him/herself on the bus
    if (passenger_ct % (biggest_bottleneck - 1)) { // Can't cleanly fill bus
        return passenger_ct / (biggest_bottleneck - 1) + 1;
    }
    return passenger_ct / (biggest_bottleneck - 1);
}

int main() {
    Graph g;
    g.connect(0, 1, 30);
    g.connect(0, 3, 10);
    g.connect(0, 2, 15);
    g.connect(1, 3, 25);
    g.connect(1, 4, 60);
    g.connect(2, 3, 40);
    g.connect(2, 5, 20);
    g.connect(3, 6, 35);
    g.connect(4, 6, 20);
    g.connect(5, 6, 30);
    assert(5 == get_best_route(g, 0, 6, 100));
}
