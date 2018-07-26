#include <iostream>
#include <vector>
#include <cassert>
#include <set>
#include <stdexcept>

#define MAX_COLOR_CT 101 // To avoid having to offset indices

using namespace std;

struct Cube {
    int front_color;
    int back_color;
    int left_color;
    int right_color;
    int top_color;
    int bottom_color;
    explicit Cube(const int front, const int back,
                  const int left, const int right,
                  const int top, const int bottom)
    : front_color(front), back_color(back),
      left_color(left), right_color(right),
      top_color(top), bottom_color(bottom) {}
    const set<int> possible_tops(const int bottom) const {
        set<int> result;
        if (top_color == bottom) result.insert(bottom_color);
        if (bottom_color == bottom) result.insert(top_color);
        if (right_color == bottom) result.insert(left_color);
        if (left_color == bottom) result.insert(right_color);
        if (front_color == bottom) result.insert(back_color);
        if (back_color == bottom) result.insert(front_color);
        return result;
    }
    bool has_base(const int color) const {
        return front_color == color || back_color == color ||
               top_color == color || bottom_color == color ||
               left_color == color || right_color == color;
    }
    string face_colored(const int color) const {
        if (front_color == color) return "front";
        if (back_color == color) return "back";
        if (right_color == color) return "right";
        if (left_color == color) return "left";
        if (top_color == color) return "top";
        if (bottom_color == color) return "bottom";
        throw invalid_argument(string("Color is not on cube."));
    }
};

struct CubeAndColor {
    int cube, color;
    CubeAndColor()
    : cube(-1), color(-1) {}
    CubeAndColor(const int cube, const int color)
    : cube(cube), color(color) {}
};

vector<pair<int, string>> solve(const vector<Cube> & cubes) {
    if (cubes.empty()) { return vector<pair<int, string>>(); }
    // Cubes are provided in order of ascending weight
    // Store the height of the highest tower possible with each possible base
    // Since we need the actual tower itself, also store which smaller cube
    // sits on the cube to make the tower (the pair's second field)
    vector<vector<pair<int, CubeAndColor>>> tower_heights; // cube by color
    for (size_t i = 0; i < cubes.size(); ++i) {
        tower_heights.push_back(vector<pair<int, CubeAndColor>>(MAX_COLOR_CT, make_pair(0, CubeAndColor())));
    }
    for (size_t cube_i = 0; cube_i < cubes.size(); ++cube_i) {
        for (size_t color_i = 0; color_i < MAX_COLOR_CT; ++color_i) {
            tower_heights[cube_i][color_i] = cubes[cube_i].has_base(color_i) ? make_pair(1, CubeAndColor()) : make_pair(0, CubeAndColor());
        }
    }
    for (size_t cube_i = 0; cube_i < cubes.size(); ++cube_i) {
        for (size_t color_i = 0; color_i < MAX_COLOR_CT; ++color_i) {
            // Consider using cube_i as a base with color_i on the bottom
            if (tower_heights[cube_i][color_i].first == 0) continue; // Not viable
            assert(tower_heights[cube_i][color_i].first == 1);
            auto possible_tops = cubes[cube_i].possible_tops(color_i);
            // Consider cubes of lower weight
            int biggest_smaller_cube_tower_height = 0;
            int biggest_smaller_cube_tower_cube = -1;
            int biggest_smaller_cube_tower_color = -1;
            for (int smaller_cube_i = cube_i - 1; smaller_cube_i >= 0; --smaller_cube_i) {
                for (size_t smaller_color_i = 0; smaller_color_i < MAX_COLOR_CT; ++smaller_color_i) {
                    if (tower_heights[smaller_cube_i][smaller_color_i].first == 0) continue; // Not viable
                    if (possible_tops.find(smaller_color_i) == possible_tops.end()) continue;
                    if (tower_heights[smaller_cube_i][smaller_color_i].first > biggest_smaller_cube_tower_height) {
                        biggest_smaller_cube_tower_height = tower_heights[smaller_cube_i][smaller_color_i].first;
                        biggest_smaller_cube_tower_cube = smaller_cube_i;
                        biggest_smaller_cube_tower_color = smaller_color_i;
                    }
                }
            }
            tower_heights[cube_i][color_i].first += biggest_smaller_cube_tower_height;
            tower_heights[cube_i][color_i].second = CubeAndColor(biggest_smaller_cube_tower_cube, biggest_smaller_cube_tower_color);
        }
    }
    vector<pair<int, string>> result; // cube and color on top per cube
    int biggest_tower_height = -1;
    CubeAndColor curr, small;
    for (size_t cube = 0; cube < tower_heights.size(); ++cube) {
        for (size_t color = 0; color < MAX_COLOR_CT; ++color) {
            if (tower_heights[cube][color].first > biggest_tower_height) {
                biggest_tower_height = tower_heights[cube][color].first;
                curr = CubeAndColor(cube, color);
                small = CubeAndColor(tower_heights[cube][color].second);
            }
        }
    }
    while (curr.cube != -1) {
        if (small.color == -1) {
            auto possible_tops = cubes[curr.cube].possible_tops(curr.color);
            assert(!possible_tops.empty());
            result.push_back(make_pair(curr.cube, cubes[curr.cube].face_colored(*possible_tops.begin())));
        } else {
            result.push_back(make_pair(curr.cube, cubes[curr.cube].face_colored(small.color)));
        }
        curr = small;
        if (curr.cube != -1 && curr.color != -1) {
            small = CubeAndColor(tower_heights[curr.cube][curr.color].second);
        } else {
            break;
        }
    }
    return vector<pair<int, string>>(result.rbegin(), result.rend());
}

void test() {
    Cube c(1, 2, 3, 4, 5, 6);
    assert(set<int>({1}) == c.possible_tops(2));
    assert(set<int>({2}) == c.possible_tops(1));
    assert(set<int>({3}) == c.possible_tops(4));
    assert(set<int>({4}) == c.possible_tops(3));
    assert(set<int>({5}) == c.possible_tops(6));
    assert(set<int>({6}) == c.possible_tops(5));
    assert(c.has_base(1));
    assert(c.has_base(2));
    assert(c.has_base(3));
    assert(c.has_base(4));
    assert(c.has_base(5));
    assert(c.has_base(6));
    assert(!c.has_base(7));
}

void printResult(const vector<pair<int, string>> & result) {
    static int counter = 1;;
    cout << "Case #" << counter++ << endl;
    cout << result.size() << endl;
    for (const auto x : result) {
        cout << 1 + x.first << " " << x.second << endl;
    }
    cout << endl;
}

int main() {
    test();
    printResult(solve({Cube(1, 2, 2, 2, 1, 2),
                       Cube(3, 3, 3, 3, 3, 3),
                       Cube(3, 2, 1, 1, 1, 1)}));
    printResult(solve({Cube(1, 5, 10, 3, 6, 5),
                       Cube(2, 6, 7, 3, 6, 9),
                       Cube(5, 7, 3, 2, 1, 9),
                       Cube(1, 3, 3, 5, 8, 10),
                       Cube(6, 6, 2, 2, 4, 4),
                       Cube(1, 2, 3, 4, 5, 6),
                       Cube(10, 9, 8, 7, 6, 5),
                       Cube(6, 1, 2, 3, 4, 7),
                       Cube(1, 2, 3, 3, 2, 1),
                       Cube(3, 2, 1, 1, 2, 3)}));
}
