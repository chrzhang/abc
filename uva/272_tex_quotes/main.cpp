#include <iostream>
#include <string>
using namespace std;
int main() {
    string curr_line;
    bool opening = true;
    while (getline(cin, curr_line)) {
        for (const char c : curr_line) {
            if (c == '\"') {
                if (opening) {
                    cout << "``";
                } else {
                    cout << "''";
                }
                opening = !opening;
            } else {
                cout << c;
            }
        }
        cout << endl;
    }
}
