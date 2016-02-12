#include <iostream>
#include <string>
#include <map>
#include <cassert>

// Convert ints into Roman numerals

std::string toRoman(int x) {
    std::map<int , std::string> romanNums =
        {{1, "I"}, {4, "IV"}, {5, "V"}, {9, "IX"}, {10, "X"}, {40, "XL"},
         {50, "L"}, {90, "XC"}, {100, "C"}, {400, "CD"}, {500, "D"},
         {900, "CM"}, {1000, "M"}}; // Must store offsets of one before each
                                    // letter to avoid cases like 9 == VIV
    std::string result;
    while (x > 0) {
        auto rit = romanNums.rbegin();
        for (; rit != romanNums.rend(); ++rit) {
            if (rit->first <= x) {
                break;
            }
        }
        if (x / rit->first > 3) {
            // Use subtraction rule because it would take more than 3 letters
            auto after = std::prev(rit);
            if (rit == romanNums.rbegin()) {
                std::cout << x << " is too large to convert.\n";
            }
            int bigger = after->first;
            while (bigger > x) {
                result.append(rit->second);
                bigger -= rit->first;
                x += rit->first;
            }
            result.append(after->second);
            x -= after->first;
        } else {
            // Use addition because it takes 1 to 3 letters
            for (int i = 0; i < x / rit->first; ++i) {
                result.append(rit->second);
            }
            x -= (x / rit->first) * rit->first;
        }
    }
    return result;
}

int main() {
   assert(toRoman(123) == "CXXIII");
   assert(toRoman(4) == "IV");
   assert(toRoman(9) == "IX");
   assert(toRoman(40) == "XL");
   assert(toRoman(90) == "XC");
   assert(toRoman(400) == "CD");
   assert(toRoman(900) == "CM");
   assert(toRoman(1954) == "MCMLIV");
   assert(toRoman(1990) == "MCMXC");
   assert(toRoman(2014) == "MMXIV");
   assert(toRoman(2) == "II");
   assert(toRoman(22) == "XXII");
   assert(toRoman(44) == "XLIV");
   assert(toRoman(31) == "XXXI");
   assert(toRoman(78) == "LXXVIII");
   assert(toRoman(80) == "LXXX");
   assert(toRoman(86) == "LXXXVI");
   assert(toRoman(99) == "XCIX");
   assert(toRoman(96) == "XCVI");
   assert(toRoman(55) == "LV");
   assert(toRoman(89) == "LXXXIX");
   assert(toRoman(14) == "XIV");
   return 0;
}
