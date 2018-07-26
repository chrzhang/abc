#include <iostream>
#include <vector>
#include <cassert>
#include <list>

#define MAX_COLOR 51

using namespace std;

struct Bead {
    int colorA, colorB;
    Bead(const int a, const int b) : colorA(a), colorB(b) {}
};

ostream & operator<<(ostream & os, const Bead & bead) {
    os << bead.colorA << " " << bead.colorB;
    return os;
}

bool is_necklace(const vector<Bead> & beads) {
    // Beads are edges between color vertices
    // Check for an Eulerian circuit (https://en.wikipedia.org/wiki/Eulerian_path)
    // by checking if degree of every vertex is even
    int degrees[MAX_COLOR] = {0};
    for (const auto & bead : beads) {
        degrees[bead.colorA]++;
        degrees[bead.colorB]++;
    }
    for (size_t i = 0; i < MAX_COLOR; ++i) {
        if (degrees[i] % 2 == 1) return false;
    }
    return true;
}

list<int> mark_tour_from(const int start_color, const vector<Bead> & beads,
                         const vector<int> edges_per_color[MAX_COLOR], vector<bool> & visited) {
    list<int> tour_i;
    int curr_vertex_color = start_color;
    bool returned = false;
    while (!returned) {
        for (const int edge : edges_per_color[curr_vertex_color]) {
            if (visited[edge]) continue;
            visited[edge] = true;
            tour_i.push_back(edge);
            curr_vertex_color = beads[edge].colorA == curr_vertex_color ? beads[edge].colorB : beads[edge].colorA;
            break;
        }
        if (curr_vertex_color == start_color) {
            returned = true;
        }
    }
    return tour_i;
}

const list<Bead> find_necklace(const vector<Bead> & beads) {
    if (!is_necklace(beads)) { return list<Bead>(); }
    if (beads.size() < 2) { return list<Bead>(); }
    // Use Hierholzer's Algorithm
    vector<bool> visited(beads.size(), false);
    vector<int> edges_per_color[MAX_COLOR];
    for (size_t bead_i = 0; bead_i < beads.size(); ++bead_i) {
        const auto & bead = beads[bead_i];
        edges_per_color[bead.colorA].push_back(bead_i);
        edges_per_color[bead.colorB].push_back(bead_i);
    }
    // Choose any starting vertex and follow a trail of edges until returning
    const int starting_vertex_color = beads[0].colorA;
    list<int> tour_i = mark_tour_from(starting_vertex_color,
                                      beads,
                                      edges_per_color,
                                      visited);
    // While a vertex u in the tour has an unvisited edge from it, start a new
    // tour from u back to u with unused edges and join the tour
    for (;;) {
        bool has_unvisited_edge_from_tour_member = false;
        list<int>::const_iterator tour_member = tour_i.end();
        int color = -1;
        bool go_before = false;
        for (auto it = tour_i.begin(); it != tour_i.end(); ++it) {
            const int bead_i = *it;
            assert(visited[bead_i]);
            const vector<int> & edges_per_color_a = edges_per_color[beads[bead_i].colorA];
            const vector<int> & edges_per_color_b = edges_per_color[beads[bead_i].colorB];
            for (const auto & edge : edges_per_color_a) {
                if (!visited[edge]) {
                    has_unvisited_edge_from_tour_member = true;
                    tour_member = it;
                    color = beads[edge].colorA;
                    go_before = true;
                    break;
                }
            }
            if (has_unvisited_edge_from_tour_member) break;
            for (const auto & edge : edges_per_color_b) {
                if (!visited[edge]) {
                    has_unvisited_edge_from_tour_member = true;
                    tour_member = it;
                    color = beads[edge].colorB;
                    break;
                }
            }
        }
        if (!has_unvisited_edge_from_tour_member) break;
        const list<int> sub_tour = mark_tour_from(color, beads, edges_per_color, visited);
        tour_i.insert(go_before ? tour_member : next(tour_member), sub_tour.begin(), sub_tour.end());
    }
    list<Bead> tour;
    for (const int i : tour_i) {
        tour.push_back(beads[i]);
    }
    return tour;
}

void solve(const vector<Bead> & beads, int case_n) {
    cout << "Case #" << case_n << endl;
    const auto result = find_necklace(beads);
    if (result.empty()) {
        cout << "some beads may be lost" << endl;
    }
    for (const auto & bead : result) {
        cout << bead << endl;
    }
    cout << endl;
}

int main() {
    assert(!is_necklace({Bead(1, 2), Bead(2, 3), Bead(3, 4), Bead(4, 5), Bead(5, 6)}));
    assert(is_necklace({Bead(2, 1), Bead(2, 2), Bead(3, 4), Bead(3, 1), Bead(2, 4)}));
    solve({Bead(1, 2), Bead(2, 3), Bead(3, 4), Bead(4, 5), Bead(5, 6)}, 1);
    solve({Bead(2, 1), Bead(2, 2), Bead(3, 4), Bead(3, 1), Bead(2, 4)}, 2);
    solve({Bead(2, 2), Bead(3, 4), Bead(2, 1), Bead(3, 1), Bead(2, 4)}, 3);
    solve({Bead(2, 2), Bead(3, 4), Bead(3, 1), Bead(2, 1), Bead(2, 4)}, 4);
    solve({Bead(2, 2), Bead(3, 4), Bead(3, 1), Bead(2, 4), Bead(2, 1)}, 5);
}
