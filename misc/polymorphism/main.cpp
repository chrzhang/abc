#include <iostream>

using namespace std;

// Verify behavior of virtual functions

struct Shape {
    // Since draw() is pure virtual, Shape cannot be created alone
    virtual void draw() = 0; // Cannot be given a definition (/overloaded)
    Shape() {
        // Cannot be called since draw()'s behavior is undefined
        // draw();
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
    Shape * s[3];
    s[0] = new Circle();
    s[1] = new Square();
    s[2] = new Triangle();
    for (int i = 0; i < 3; ++i) {
        s[i]->draw();
        delete s[i];
    }
    return 0;
}
