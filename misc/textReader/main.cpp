#include <iostream>
#include <fstream>
#include <string>

using namespace std;

// Write a class that can read a file and store its contents

class Text {
    string member;
    public:
        Text() {}
        Text(const string & filename) {
            ifstream in(filename);
            string line;
            while (getline(in, line)) {
                member += line + '\n';
            }
        }
        const string & contents() const {
            return member;
        }
};

int main(int argc, const char * argv[]) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    Text t(argv[1]);
    cout << t.contents() << endl;
    return 0;
}
