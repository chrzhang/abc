#include <iostream>

using namespace std;

// Experiment with structs and their sizes

struct EmptyStruct {};
struct OneMemberStruct {
    char member;
};
struct TwoMemberStruct {
    char member0;
    char member1;
};
struct FuncMemberStruct {
    char member;
    void func0() const;
    void func1() const;
    void func2() const;
    void func3() const;
    void func4() const;
    void func5() const;
};

int main() {
    EmptyStruct es;
    cout << "size of empty struct: " << sizeof(es) << endl;
    // An empty struct cannot have size 0, or subsequent definitions may
    // occupy the same address (thus bloat is considered part of struct)
    OneMemberStruct oms;
    cout << "size of one member struct: " << sizeof(oms) << endl;
    // In this case, the one character member is smaller than the initial bloat
    // of an empty struct and the size does not change
    TwoMemberStruct tms;
    cout << "size of two member struct: " << sizeof(tms) << endl;
    // 2
    FuncMemberStruct fms;
    cout << "size of function member struct: " << sizeof(fms) << endl;
    // Functions considered members are not part of the size of a struct
    return 0;
}
