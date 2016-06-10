#include <iostream>
#include <string>
#include <fstream>
#include <vector>

using namespace std;

// Copy lines in file into a vector of strings

int main(int argc, const char * argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    vector<string> vec;
    ifstream in(argv[1]);
    string line;
    while (getline(in, line)) {
        vec.push_back(line);
    }
    // Print lines from last to first
    for (int i = (int) vec.size() - 1; i >= 0; --i) {
        cout << i << ": " << vec[i] << endl;
    }
    return 0;
}
