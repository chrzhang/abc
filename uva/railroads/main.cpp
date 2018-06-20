#include <iostream>
#include <vector>
#include <cassert>
#include <fstream>
#include <map>
#include <set>
#include <climits>

using namespace std;

struct Edge {
    string dest_city;
    int time_leave_source_city, time_arrive_dest_city;
    Edge(const string & dest_city, const int time_leave, const int time_arrive)
    : dest_city(dest_city), time_leave_source_city(time_leave),
      time_arrive_dest_city(time_arrive) {}
};

struct Graph {
    map<string, vector<Edge>> cities_to_neighbors;
    Graph(const vector<string> & all_cities) {
        for (const auto & city : all_cities) {
            cities_to_neighbors[city] = {};
        }
    }
    void add_route(const string & from, const string & to,
                   const int time_leave, const int time_arrive) {
        cities_to_neighbors[from].push_back(Edge(to, time_leave, time_arrive));
    }
    bool is_visited(const string & city, const set<string> & visited) const {
        return visited.find(city) != visited.end();
    }
    pair<int, int> fastest_trip(const string & from, const string & to,
                                const int start_time) const {
        // Use Dijkstra's algorithm but instead of minimizing summed edge
        // weights, minimize the time of arrival at the destination
        map<string, int> best_arrival_times;
        set<string> visited;
        for (const auto & p : cities_to_neighbors) {
            best_arrival_times[p.first] = INT_MAX;
        }
        best_arrival_times[from] = start_time;
        visited.insert(from);
        string curr_city = from;
        while (curr_city != to) {
            // Consider all of curr_city's unvisited neighbors
            // A city is only a neighbor if there is a valid train
            // going to it with a departure >= the curr_city's curr best_arrival time
            for (const Edge & edge : cities_to_neighbors.at(curr_city)) {
                if (is_visited(edge.dest_city, visited)) continue;
                if (edge.time_leave_source_city < best_arrival_times[curr_city]) continue;
                // If time going across the train to the neighbor gets us there
                // faster, update the best arrival time
                if (edge.time_arrive_dest_city < best_arrival_times[edge.dest_city]) {
                    best_arrival_times[edge.dest_city] = edge.time_arrive_dest_city;
                }
            }
            visited.insert(curr_city);
            int curr_min_distance = INT_MAX;
            string curr_min_distance_city = "";
            for (const auto & p : cities_to_neighbors) {
                if (!is_visited(p.first, visited)) {
                    if (best_arrival_times[p.first] <= curr_min_distance) {
                        curr_min_distance = best_arrival_times[p.first];
                        curr_min_distance_city = p.first;
                    }
                }
            }
            curr_city = curr_min_distance_city;
        }
        int arrival = best_arrival_times[to];
        if (arrival == INT_MAX) {
            return make_pair(INT_MAX, INT_MAX);
        }
        // The departure is one of the departures from city 'from'
        // We would like to use the latest one, provided it gets us onto
        // a path that can reach city 'to' at the desired arrival time
        int departure = 0;
        return make_pair(departure, arrival);
    }
};

struct InputEdge {
    string from, to;
    int arrival, departure;
    InputEdge(const string & from, const string & to, const int arrival, const int departure)
    : from(from), to(to), arrival(arrival), departure(departure) {}
};

void solve(const vector<string> & cities, const vector<InputEdge> & edges,
           const string & from, const string & to, const int start) {
    // Make a directed graph where nodes are cities and edges are made from
    // trains' itineraries
    Graph g(cities);
    for (const auto & edge : edges) {
        g.add_route(edge.from, edge.to, edge.arrival, edge.departure);
    }
    const auto result = g.fastest_trip(from, to, start);
    static int scenario_i = 1;
    cout << "Scenario " << scenario_i++ << endl;
    if (result.first == INT_MAX || result.second == INT_MAX) {
        cout << "No connection" << endl << endl;
        return;
    }
    cout << "Departure " << result.first << " " << from << endl;
    cout << "Arrival   " << result.second << " " << to << endl << endl;
}

int main() {
    ifstream inFile("input.txt");
    int scenario_ct;
    inFile >> scenario_ct;
    for (int scenario_i = 0; scenario_i < scenario_ct; ++scenario_i) {
        // Part 1
        int city_ct;
        inFile >> city_ct;
        vector<string> cities;
        for (int city_i = 0; city_i < city_ct; ++city_i) {
            string city;
            inFile >> city;
            cities.push_back(city);
        }
        // Part 2
        int road_ct;
        inFile >> road_ct;
        vector<InputEdge> edges;
        for (int road_i = 0; road_i < road_ct; ++road_i) {
            int stop_ct;
            inFile >> stop_ct;
            string last_stop = "";
            int last_time = -1;
            for (int stop_i = 0; stop_i < stop_ct; ++stop_i) {
                int time;
                inFile >> time;
                string station;
                inFile >> station;
                if (!last_stop.empty() && last_time != -1) {
                    edges.push_back(InputEdge(last_stop, station, last_time, time));
                }
                last_stop = station;
                last_time = time;
            }
        }
        // Part 3
        int start_time;
        string start_city, end_city;
        inFile >> start_time;
        inFile >> start_city;
        inFile >> end_city;
        solve(cities, edges, start_city, end_city, start_time);
    }
}
