#include <iostream>
#include <cstdlib>
#include <ctime>

#define NUM_ANIMALS 20

// Implement an adoption center with a FIFO logic for dogs and cats but users
// can specify if they want the [cat/dog], [dog], or [cat] that has been there
// the longest

enum PetType { Dog, Cat };

struct Pet {
    int timeEntered;
    PetType type;
    Pet() {
        timeEntered = -1;
    }
    Pet(int timeEntered, PetType type) {
        this->timeEntered = timeEntered;
        this->type = type;
    }
};

struct Node {
    Pet p;
    Node * next, * prev;
    Node(Pet p) {
        this->p = p;
        next = prev = nullptr;
    }
};

struct Queue {
    Node * head, * tail;
    Queue() {
        head = tail = nullptr;
    }
    ~Queue() {
        Node * nptr = tail;
        while (nptr) {
            Node * temp = nptr->next;
            delete nptr;
            nptr = temp;
        }
    }
    void enqueue(Node * n) { // Push onto tail
        if (!n) { return; }
        if (!head) {
            head = tail = n;
        } else {
            tail->prev = n;
            n->next = tail;
            tail = n;
        }
    }
    Node * dequeue() { // Pop from head
        if (!head) { return nullptr; }
        Node * temp = head;
        head = head->prev;
        if (head) {
            head->next = nullptr;
        } else {
            tail = nullptr;
        }
        temp->prev = temp->next = nullptr;
        return temp;
    }
    bool isEmpty() {
        return (head == nullptr);
    }
};

struct PetQueue {
    Queue catQ, dogQ;
    void enqueueCat(Node * n) {
        catQ.enqueue(n);
    }
    void enqueueDog(Node * n) {
        dogQ.enqueue(n);
    }
    Node * dequeueDog() {
        return dogQ.dequeue();
    }
    Node * dequeueCat() {
        return catQ.dequeue();
    }
    Node * dequeueAny() {
        if (catQ.head && dogQ.head) {
            if (catQ.head->p.timeEntered < dogQ.head->p.timeEntered) {
                return dequeueCat();
            } else {
                return dequeueDog();
            }
        } else if (!catQ.head && !dogQ.head) {
            return nullptr;
        }
        if (catQ.head) { return dequeueCat(); }
        return dequeueDog();
    }
};

std::ostream & operator<<(std::ostream & os, const Queue & q) {
    os << "tail -> ";
    Node * nptr = q.tail;
    while (nptr) {
        if (nptr->p.type == Cat) {
            os << "Cat:";
        } else if (nptr->p.type == Dog) {
            os << "Dog:";
        }
        os << nptr->p.timeEntered << " ";
        nptr = nptr->next;
    }
    return os;
}

int main() {
    unsigned clock = 0;
    srand(time(0));
    PetQueue petQ;
    for (int i = 0; i < NUM_ANIMALS; ++i) {
        switch(rand() % 2) {
            case 0: { // On the cat queue
                petQ.enqueueCat(new Node(Pet(clock++, Cat)));
                break;
            }
            case 1: { // On the dog queue
                petQ.enqueueDog(new Node(Pet(clock++, Dog)));
                break;
            }
        }
    }
    std::cout << "Dog queue: " << petQ.dogQ << std::endl;
    std::cout << "Cat queue: " << petQ.catQ << std::endl << std::endl;
    while (!petQ.catQ.isEmpty() || !petQ.dogQ.isEmpty()) {
        switch(rand() % 3) {
            case 0: { // Dequeue dog
                Node * n = petQ.dequeueDog();
                if (n) {
                    std::cout << "Dequeued Dog:" << n->p.timeEntered
                              << std::endl;
                    delete n;
                    std::cout << petQ.dogQ << std::endl << std::endl;
                }
                break;
            }
            case 1: { // Dequeue cat
                Node * n = petQ.dequeueCat();
                if (n) {
                    std::cout << "Dequeued Cat:" << n->p.timeEntered
                              << std::endl;
                    delete n;
                    std::cout << petQ.catQ << std::endl << std::endl;
                }
                break;
            }
            case 2: { // Dequeue any
                Node * n = petQ.dequeueAny();
                if (n) {
                    std::cout << "Dequeued Any ";
                    if (n->p.type == Cat) {
                        std::cout << "Cat:";
                    } else {
                        std::cout << "Dog:";
                    }
                    std::cout << n->p.timeEntered << std::endl;
                    delete n;
                }
                break;
            }
        }
    }
    return 0;
}
