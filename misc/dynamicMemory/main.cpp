#include <iostream>

using namespace std;

// Experiment with dynamic memory allocation (use valgrind to check)

int main() {
    int * iPtr = new int;
    printf("%p\n", (void *) iPtr);

    long * lPtr = new long;
    printf("%p\n", (void *) lPtr);

    char * charArray = new char[100];
    printf("%p\n", (void *) charArray);

    float * floatArray = new float[100];
    printf("%p\n", (void *) floatArray);

    delete iPtr;
    delete lPtr;
    delete [] charArray;
    delete [] floatArray;
    return 0;
}
