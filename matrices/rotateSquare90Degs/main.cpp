#include <iostream>

#define N 3

// Rotate a N x N matrix 90 degrees where each pixel is 4 bytes

struct Pixel {
    int i;
    Pixel() {
    }
    Pixel(int val) {
        i = val;
    }
};

void printMatrix(Pixel matrix[N][N]) {
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            std::cout << matrix[i][j].i << " ";
        }
        std::cout << std::endl;
    }
}

void rotateMatrix(Pixel matrix[N][N]) {
    // Start off on the outmost layer and descend in
    for (int depth = 0; depth < (N + 1) / 2; ++depth) {
        Pixel * nw, * ne, * sw, * se;
        nw = &matrix[depth][depth];
        ne = &matrix[depth][N - 1 - depth];
        sw = &matrix[N - 1 - depth][depth];
        se = &matrix[N - 1 - depth][N - 1 - depth];
        int i = 0; // Keep count of how many 4-way swaps to do
        while (i < (N - 2 * depth) - 1) {
            // Swap
            Pixel temp = *nw;
            *nw = *sw;
            *sw = *se;
            *se = *ne;
            *ne = temp;
            // Shift 4 pointers
            nw = nw + 1; // right
            ne = ne + N; // down
            sw = sw - N; // up
            se = se - 1; // left
            ++i; // Counter for how many times the 4 pointers shift
        }
    }
}

int main() {
    Pixel matrix[N][N];
    int val = 1;
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            matrix[i][j] = Pixel(val++);
        }
    }
    printMatrix(matrix);
    std::cout << "Rotate 90 degrees clockwise." << std::endl;
    rotateMatrix(matrix);
    printMatrix(matrix);
    return 0;
}
