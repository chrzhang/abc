#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <unordered_map>

// Encode XML

struct End {
    std::string getEncoding() {
        return "0";
    }
};

struct Value {
    // String
    std::string val;
    Value(const std::string & v) : val(v) {}
    std::string getEncoding() {
        return val;
    }
};

struct Tag {
    // Mapping to int
    int encoding;
    Tag() {
        encoding = -1;
    }
    Tag(const std::string & s,
        std::unordered_map<std::string, int> & table) {
        assert(table.find(s) != table.end());
        encoding = table[s];
    }
    std::string getEncoding() {
        return std::to_string(encoding);
    }
};

struct Attribute {
    // Tag
    Tag tag;
    // Value
    Value val;
    Attribute(Tag t, Value v) : tag(t), val(v) {}
    std::string getEncoding() {
        return (tag.getEncoding() + " " + val.getEncoding());
    }
};

struct Element {
    // Tag
    Tag t;
    // Attributes
    std::vector<Attribute> attrs;
    // End
    End e;
    // Children
    std::vector<Element> children;
    // End
    Element() {}
    std::string getEncoding() {
        std::string tagEncoding = t.getEncoding();
        std::string attrsEncoding = "";
        for (auto it = attrs.begin(); it != attrs.end(); ++it) {
            attrsEncoding += it->getEncoding() + " ";
        }
        std::string childrenEncoding = "";
        for (auto it = children.begin(); it != children.end(); ++it) {
            childrenEncoding += it->getEncoding() + " ";
        }
        return (tagEncoding + " " + attrsEncoding + e.getEncoding() + " " +
                childrenEncoding + e.getEncoding());
    }
};

int main() {
    std::unordered_map<std::string, int> table ({
        {"family", 1},
        {"person", 2},
        {"firstName", 3},
        {"lastName", 4},
        {"state", 5}
    });
    Tag tFamily("family", table);
    Tag tPerson("person", table);
    Tag tFirstName("firstName", table);
    Tag tLastName("lastName", table);
    Tag tState("state", table);
    Element family;
    Value vLName("Zhang");
    Value vState("NY");
    Attribute attr1(tLastName, vLName);
    Attribute attr2(tState, vState);
    family.t = tFamily;
    family.attrs.push_back(attr1);
    family.attrs.push_back(attr2);
    Element person;
    Value vFName("Christopher");
    Attribute attr3(tFirstName, vFName);
    person.t = tPerson;
    person.attrs.push_back(attr3);
    family.children.push_back(person);
    std::cout << family.getEncoding() << std::endl;
    return 0;
}
