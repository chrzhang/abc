#include <iostream>
#include <string>
#include <cassert>

// After specifying a start point and a series of Up, Down, Left, Right motions,
// find the area enclosed by the resulting path

#define MINX -10
#define MAXX 10
#define MINY -10
#define MAXY 10
#define WIDTH (MAXX - MINX + 1)
#define HEIGHT (MAXY - MINY + 1)

struct Grid {
    static const int h = HEIGHT + (MAXY - MINY);
    static const int w = WIDTH + (MAXX - MINX);
    // To display coordinates and the cells themselves
    char loc[h][w];
    int convertX(int x) {
        return 2 * (x - MINX);
    }
    int convertY(int y) {
        return (MAXX - MINY) - 2 * y;
    }
    bool inBoundsX(int x) {
        return x >= MINX && x <= MAXX;
    }
    bool inBoundsY(int y) {
        return y >= MINY && y <= MAXY;
    }
    void setPoint(int x, int y) { // Map Cartesian coordinate to the arr indices
        loc[convertY(y)][convertX(x)] = '*';
    }
    Grid(int startX, int startY, const std::string & moves) {
        for (int i = 0; i < h; ++i) {
            for (int j = 0; j < w; ++j) {
                loc[i][j] = ' ';
            }
        }
        setPoint(startX, startY);
        for (auto c : moves) {
            switch (c) {
                case 'U':
                    startY += 1;
                    setPoint(startX, startY);
                    loc[convertY(startY) + 1][convertX(startX)] = '|';
                    break;
                case 'D':
                    startY -= 1;
                    setPoint(startX, startY);
                    loc[convertY(startY) - 1][convertX(startX)] = '|';
                    break;
                case 'L':
                    startX -= 1;
                    setPoint(startX, startY);
                    loc[convertY(startY)][convertX(startX) + 1] = '-';
                    break;
                case 'R':
                    startX += 1;
                    setPoint(startX, startY);
                    loc[convertY(startY)][convertX(startX) - 1] = '-';
                    break;
                default:
                    assert(false);
            }
        }
    }
};

std::ostream & operator<<(std::ostream & os, const Grid & g) {
    for (auto i = 0; i < g.h; ++i) {
        for (auto j = 0; j < g.w; ++j) {
            os << g.loc[i][j] << " ";
        }
        os << "\n";
    }
    return os;
}

int main() {
    Grid g(0, 0, "RRRRUUULLD");
    std::cout << g << std::endl;
    return 0;
}
