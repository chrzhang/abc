class Solution {
public:
string simplifyPath(const string & path) {
    list<string> stack;
    string dir;
    for (auto it = path.begin(); it != path.end(); ++it) {
        if (next(it) == path.end() && *it != '/') {
            dir.append(1, *it);
        }
        if (*it == '/' || next(it) == path.end()) {
            if (!dir.empty()) {
                if (dir == ".") {
                } else if (dir == "..") {
                    if (!stack.empty()) {
                        stack.pop_back();
                    }
                } else {
                    stack.push_back(dir);
                }
                dir.clear();
            }
        } else {
            dir.append(1, *it);
        }
    }
    if (stack.empty()) { return "/"; }
    string result;
    for (const auto & d : stack) {
        result += string(1, '/') + d;
    }
    return result;
}
};