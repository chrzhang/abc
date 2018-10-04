#include <iostream>
#include <regex>
#include <string>
using namespace std;
string replace_special(const string & s) {
    regex re("\\b([a-z][0-9]{2})\\b");
    string result;
    regex_replace(back_inserter(result), s.begin(), s.end(), re, "***");
    return result;
}
