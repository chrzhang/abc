#include <iostream>
#include <vector>
#include <cassert>
#include <cmath>
#include <cfloat>
#include <iomanip>
#include <fstream>

using namespace std;

struct Point {
    double x, y;
    Point(const double x, const double y) : x(x), y(y) {}
    double dist_to(const Point & p) const {
        return sqrt(pow((p.x - x), 2) + pow((p.y - y), 2));
    }
};

ostream & operator<<(ostream & os, const Point & p) {
    os << p.x << "," << p.y;
    return os;
}

struct Edge {
    Point from, to;
    double weight;
    Edge(const Point & from, const Point & to) : from(from), to(to) {}
};

bool operator<(const Edge & lhs, const Edge & rhs) {
    return lhs.weight < rhs.weight;
}

double solve(const vector<Point> & points) {
    if (points.size() < 2) { return 0; }
    vector<vector<double>> dists(points.size(), vector<double>(points.size(), -1));
    for (size_t from = 0; from < points.size(); ++from) {
        for (size_t to = 0; to < points.size(); ++to) {
            dists[from][to] = points[from].dist_to(points[to]);
        }
    }
    vector<bool> visited(points.size(), false);
    size_t visited_ct = 1;
    visited[0] = true;
    double min_total_len = 0;
    while (visited_ct != visited.size()) {
        double min_dist = DBL_MAX;
        int min_to = -1;
        for (size_t from = 0; from < points.size(); ++from) {
            if (!visited[from]) continue;
            for (size_t to = 0; to < points.size(); ++to) {
                if (visited[to]) continue;
                const auto dist = points[from].dist_to(points[to]);
                if (dist < min_dist) {
                    min_dist = dist;
                    min_to = to;
                }
            }
        }
        assert(min_to != -1);
        assert(!visited[min_to]);
        visited[min_to] = true;
        ++visited_ct;
        min_total_len += min_dist;
    }
    return min_total_len;
}

int main() {
    ifstream inFile("input.txt");
    int num_inputs;
    inFile >> num_inputs;
    for (int input_i = 0; input_i < num_inputs; ++input_i) {
        int num_points;
        inFile >> num_points;
        vector<Point> points;
        for (int point_i = 0; point_i < num_points; ++point_i) {
            double x, y;
            inFile >> x;
            inFile >> y;
            points.push_back(Point(x, y));
        }
        cout << setprecision(2) << fixed << solve(points) << endl << endl;
    }
}
