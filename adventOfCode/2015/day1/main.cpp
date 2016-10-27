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
        size_t pos_enter_basement = 0;
        size_t curr_pos = 1;
        while (f.get(c)) {
            switch (c) {
                case ')':
                    curr_floor -= 1;
                    // First character causing him to enter the basement
                    if (pos_enter_basement == 0 &&
                        curr_floor == -1) {
                        pos_enter_basement = curr_pos;
                    }
                    break;
                case '(':
                    curr_floor += 1;
                    break;
                default:
                    break;
            }
            ++curr_pos;
        }
        std::cout << "Final floor: " << curr_floor << std::endl;
        std::cout << "Position of 1st char to enter basement: "
                  << pos_enter_basement << std::endl;
    }
    return 0;
}
