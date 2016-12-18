#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <string>
#include <stdexcept>

int toNum(const std::string & s) {
    std::stringstream ss(s);
    int num;
    if (!(ss >> num)) {
        throw std::runtime_error("Cannot convert " + s + " to int.");
    }
    return num;
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::string line;
    std::getline(f, line);
    std::regex intRegex("([-+]?[0-9]+)");
    std::smatch result;
    std::regex_search(line, result, intRegex);
    int sum = 0;
    for (auto it = std::sregex_token_iterator(line.begin(), line.end(),
                                              intRegex);
         it != std::sregex_token_iterator(); ++it) {
        sum += toNum(*it);
    }
    std::cout << sum << std::endl;
    return 0;
}
