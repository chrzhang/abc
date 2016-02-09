#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>

int reverse(int x) {
    auto s = std::to_string(abs(x));
    s = std::string(s.rbegin(), s.rend());
    if (x < 0) { s.insert(0, 1, '-'); }
    return std::stoi(s);
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cout << "Usage: ./p <Number>" << std::endl;
        return 1;
    }
    try {
        std::cout << reverse(std::stoi(argv[1])) << std::endl;
    } catch (const std::out_of_range & oor) {
        std::cout << 0 << std::endl;
    }
    return 0;
}
