#include <iostream>
#include <vector>

// Generate Pascal's triangle of a given # of rows

void printTriangle(const std::vector<std::vector<int>> & t) {
    for (auto row : t) {
        for (auto e : row) {
            std::cout << e << " ";
        }
        std::cout << "\n";
    }
}

std::vector<std::vector<int>> generate(int numRows) {
    std::vector<std::vector<int>> result;
    for (int rowNum = 0; rowNum < numRows; ++rowNum) {
        std::vector<int> row;
        row.push_back(1);
        for (int k = 0; k < rowNum; ++k) {
            row.push_back(row.back() * (rowNum - k) / (double) (k + 1));
        }
        result.push_back(row);
    }
    return result;
}

int main() {
    auto triangle = generate(15);
    printTriangle(triangle);
    return 0;
}
