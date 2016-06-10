#include <iostream>
#include <fstream>
#include <string>

using namespace std;

// Count whitespace separated words in a file
// Test against 'wc -w <filename>'

int countFile(const char * filename) {
    int count = 0;
    string word;
    ifstream in(filename);
    while (in >> word) {
        ++count;
    }
    return count;
}

int main(int argc, const char * argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    cout << countFile(argv[1]) << " " << argv[1] << endl;
    return 0;
}
