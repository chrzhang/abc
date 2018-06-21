#include <iostream>
#include <vector>
#include <cassert>
#include <set>
#include <algorithm>
#include <fstream>

using namespace std;

struct Table {
    int id, seat_ct;
    set<int> occupants;
    Table(const int id, const int seat_ct)
    : id(id), seat_ct(seat_ct) {}
    int occupancy() const {
        return occupants.size();
    }
};

bool operator<(const Table & lhs, const Table & rhs) {
    return lhs.seat_ct < rhs.seat_ct;
}

bool can_share_tables(const vector<int> & team_sizes,
                      const vector<int> & table_sizes) {
    // A greedy algorithm that fills the table with the most spots proves
    // optimal because it keeps the most table options open by filling up
    // the biggest tables first.
    vector<Table> tables;
    for (size_t table_i = 0; table_i < table_sizes.size(); ++table_i) {
        // Use 1 indexing
        tables.push_back(Table(table_i + 1, table_sizes[table_i]));
    }
    sort(tables.begin(), tables.end());
    for (size_t team_i = 0; team_i < team_sizes.size(); ++team_i) {
        int team_members_left = team_sizes[team_i];
        for (auto table_it = tables.rbegin();
             team_members_left && table_it != tables.rend(); ++table_it) {
            if (table_it->occupancy() < table_it->seat_ct) {
                // use 1 indexing
                table_it->occupants.insert(team_i + 1);
                --team_members_left;
            }
        }
        if (team_members_left) {
            cout << "0" << endl;
            return false;
        }
    }
    cout << "1" << endl;
    for (size_t team_i = 0; team_i < team_sizes.size(); ++team_i) {
        for (size_t table_i = 0; table_i < table_sizes.size(); ++table_i) {
            const auto & occupants = tables[table_i].occupants;
            if (occupants.find(team_i + 1) != occupants.end()) {
                cout << table_i + 1 << " ";
            }
        }
        cout << endl;
    }
    return true;
}

int main() {
    ifstream inFile("input.txt");
    int team_ct, table_ct;
    while (inFile >> team_ct && inFile >> table_ct) {
        vector<int> team_sizes, table_sizes;
        for (int team_i = 0; team_i < team_ct; ++team_i) {
            int team_size;
            inFile >> team_size;
            team_sizes.push_back(team_size);
        }
        for (int table_i = 0; table_i < table_ct; ++table_i) {
            int table_size;
            inFile >> table_size;
            table_sizes.push_back(table_size);
        }
        can_share_tables(team_sizes, table_sizes);
    }
}
