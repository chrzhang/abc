#include <iostream>
#include <list>
#include <cassert>

// Determine whether 3-D points lie on one plane

struct Vect3D {
    int x, y, z;
    Vect3D() : x(0), y(0), z(0) {}
    Vect3D(int x, int y, int z) : x(x), y(y), z(z) {}
};

std::ostream & operator<<(std::ostream & os, const Vect3D & v) {
    os << "(" << v.x << "," << v.y << "," << v.z << ")\n";
    return os;
}

bool sharePlane(const std::list<Vect3D *> vertices) {
    if (vertices.size() <= 3) { return true; }
    int index = 0;
    // Take first 3 vertices to define a plane
    Vect3D v1, v2, v3;
    // Coefficients in the equation of such plane
    int a, b, c, d;
    for (auto vertexPtr : vertices) {
        switch (index) {
            case 0: {
                v1 = *vertexPtr;
                break;
            }
            case 1: {
                v2 = *vertexPtr;
                break;
            }
            case 2: {
                v3 = *vertexPtr;
                Vect3D slope1(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z);
                Vect3D slope2(v3.x - v1.x, v3.y - v1.y, v3.z - v1.z);
                Vect3D cross(slope1.y * slope2.z - slope1.z * slope2.y,
                             slope1.z * slope2.x - slope1.x * slope2.z,
                             slope1.x * slope2.y - slope1.y * slope2.x);
                d = 0;
                d -= (cross.x * (-v1.x));
                d -= (cross.y * (-v1.y));
                d -= (cross.z * (-v1.z));
                a = cross.x;
                b = cross.y;
                c = cross.z;
                break;
            }
            default: {
                // Check to see if it fits the equation
                if (a * vertexPtr->x + b * vertexPtr->y +
                    c * vertexPtr->z != d) {
                    return false;
                }
                break;
            }
        }
        ++index;
    }
    std::cout << "Plane: " << d << " = " << a << "x + " << b << "y + "
                           << c << "z\n";
    return true;
}

int main() {
    std::list<Vect3D *> vertices;
    Vect3D v1( 1, -2,  0);
    Vect3D v2( 3,  1,  4);
    Vect3D v3( 0, -1,  2);
    Vect3D v4( 9,  0,  0);
    vertices = { &v1, &v2, &v3, &v4 };
    assert(sharePlane(vertices));
    return 0;
}
