#include <iostream>
#include <string>
#include <cassert>
#include <cmath>

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
    double areaEnclosed;
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
    Grid(const int startX, const int startY, const std::string & moves) {
        areaEnclosed = 0;
        for (int i = 0; i < h; ++i) {
            for (int j = 0; j < w; ++j) {
                loc[i][j] = ' ';
            }
        }
        int priorX = startX;
        int priorY = startY;
        int currX = startX;
        int currY = startY;
        int leftSum, rightSum;
        leftSum = rightSum = 0;
        setPoint(currX, currY);
        // Use shoelace formula or Gauss's area formula to figure out area as
        // we go along (only works when steps form a valid polygon)
        for (auto c : moves) {
            switch (c) {
                case 'U':
                    currY += 1;
                    if (!(inBoundsX(currX) && inBoundsY(currY))) {
                        std::cerr << "Invalid movement outside upper edge.\n";
                        return;
                    }
                    setPoint(currX, currY);
                    loc[convertY(currY) + 1][convertX(currX)] = '|';
                    break;
                case 'D':
                    currY -= 1;
                    if (!(inBoundsX(currX) && inBoundsY(currY))) {
                        std::cerr << "Invalid movement outside lower edge.\n";
                        return;
                    }
                    setPoint(currX, currY);
                    loc[convertY(currY) - 1][convertX(currX)] = '|';
                    break;
                case 'L':
                    currX -= 1;
                    if (!(inBoundsX(currX) && inBoundsY(currY))) {
                        std::cerr << "Invalid movement outside left edge.\n";
                        return;
                    }
                    setPoint(currX, currY);
                    loc[convertY(currY)][convertX(currX) + 1] = '-';
                    break;
                case 'R':
                    currX += 1;
                    if (!(inBoundsX(currX) && inBoundsY(currY))) {
                        std::cerr << "Invalid movement outside right edge.\n";
                        return;
                    }
                    setPoint(currX, currY);
                    loc[convertY(currY)][convertX(currX) - 1] = '-';
                    break;
                default:
                    assert(false);
            }
            leftSum += priorX * currY;
            rightSum += priorY * currX;
            priorX = currX;
            priorY = currY;
        }
        areaEnclosed = 0.5 * abs(leftSum - rightSum);
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
    {
        Grid g(0, 0, "RRUULLDD");
        std::cout << g << std::endl;
        assert(g.areaEnclosed == 4);
        std::cout << g.areaEnclosed << std::endl;
    }
    {
        Grid g(0, 0, "RRULLD");
        std::cout << g << std::endl;
        assert(g.areaEnclosed == 2);
        std::cout << g.areaEnclosed << std::endl;
    }
    {
        Grid g(0, 0, "RRRRUULDLULDLD");
        std::cout << g << std::endl;
        assert(g.areaEnclosed == 6);
        std::cout << g.areaEnclosed << std::endl;
    }
    return 0;
}
