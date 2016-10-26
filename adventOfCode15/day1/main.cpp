#include <iostream>
#include <fstream>
#include <sstream>

int main(int argc, char * argv[]) {
    if (argc == 1) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 0;
    }
    std::ifstream f(argv[1]);
    if (f.is_open()) {
        char c;
        int curr_floor = 0;
        while (f.get(c)) {
            switch (c) {
                case ')':
                    curr_floor -= 1;
                    break;
                case '(':
                    curr_floor += 1;
                    break;
                default:
                    break;
            }
        }
        std::cout << "Final floor: " << curr_floor << std::endl;
    }
    return 0;
}
