#include <iostream>

using namespace std;

// Verify behavior of virtual functions

struct Shape {
    virtual void draw() {
        cout << "Shape draw\n";
    }
    virtual ~Shape() {
        cout << "Shape dtor\n";
    }
};

struct Circle : public Shape {
    virtual void draw() {
        cout << "Circle draw\n";
    }
    virtual ~Circle() {
        cout << "Circle dtor\n";
    }
};

struct Square : public Shape {
    virtual void draw() {
        cout << "Square draw\n";
    }
    virtual ~Square() {
        cout << "Square dtor\n";
    }
};

struct Triangle : public Shape {
    virtual void draw() {
        cout << "Triangle draw\n";
    }
    virtual ~Triangle() {
        cout << "Triangle dtor\n";
    }
};

int main() {
    Shape * s[4];
    s[0] = new Shape();
    s[1] = new Circle();
    s[2] = new Square();
    s[3] = new Triangle();
    for (int i = 0; i < 4; ++i) {
        s[i]->draw();
        delete s[i];
    }
    return 0;
}
