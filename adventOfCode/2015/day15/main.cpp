#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <fstream>
#include <regex>
#include <cassert>
#include <map>
#include <climits>
#include <algorithm>

/*
From http://adventofcode.com/2015/day/15

--- Day 15: Science for Hungry People ---

Today, you set out on the task of perfecting your milk-dunking cookie recipe.
All you have to do is find the right balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
list of the remaining ingredients you could use to finish the recipe (your
puzzle input) and their properties per teaspoon:

    capacity (how well it helps the cookie absorb milk)
    durability (how well it keeps the cookie intact when full of milk)
    flavor (how tasty it makes the cookie)
    texture (how it improves the feel of the cookie)
    calories (how many calories it adds to the cookie)

You can only measure ingredients in whole-teaspoon amounts accurately, and you
have to be accurate so you can reproduce your results in the future. The total
score of a cookie can be found by adding up each of the properties (negative
totals become 0) and then multiplying together everything except calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon
(because the amounts of each ingredient must add up to 100) would result in a
cookie with the following properties:

    A capacity of 44*-1 + 56*2 = 68
    A durability of 44*-2 + 56*3 = 80
    A flavor of 44*6 + 56*-2 = 152
    A texture of 44*3 + 56*-1 = 76

Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
results in a total score of 62842880, which happens to be the best score
possible given these ingredients. If any properties had produced a negative
total, it would have instead become zero, causing the whole score to multiply
to zero.

Given the ingredients in your kitchen and their properties, what is the total
score of the highest-scoring cookie you can make?

--- Part Two ---

Your cookie recipe becomes wildly popular! Someone asks if you can make another
recipe that has exactly 500 calories per cookie (so they can use it as a meal
replacement). Keep the rest of your award-winning process the same (100
teaspoons, same ingredients, same scoring system).

For example, given the ingredients above, if you had instead selected 40
teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds to
100), the total calorie count would be 40*8 + 60*3 = 500. The total score would
go down, though: only 57600000, the best you can do in such trying
circumstances.

Given the ingredients in your kitchen and their properties, what is the total
score of the highest-scoring cookie you can make with a calorie total of 500?
*/

class Ingredient {

    std::string m_name;
    int m_capacity;
    int m_durability;
    int m_flavor;
    int m_texture;
    int m_calories;

    public:

        Ingredient(const std::string & name, int capacity, int durability,
                   int flavor, int texture, int calories)
        : m_name(name), m_capacity(capacity),
          m_durability(durability), m_flavor(flavor), m_texture(texture),
          m_calories(calories) {
        }

        const std::string & name() const {
            return m_name;
        }

        const int capacity() const {
            return m_capacity;
        }

        const int durability() const {
            return m_durability;
        }

        const int flavor() const {
            return m_flavor;
        }

        const int texture() const {
            return m_texture;
        }

        const int calories() const {
            return m_calories;
        }

        friend bool operator<(const Ingredient & i1, const Ingredient & i2);

};

bool operator<(const Ingredient & i1, const Ingredient & i2) {
    return i1.m_name < i2.m_name;
}

int toInt(const std::string & s) {
    std::stringstream ss(s);
    int r;
    if (!(ss >> r)) {
        throw std::runtime_error("Cannot interpret " + s + " as a number.");
    }
    return r;
}

static int maxScore = INT_MIN;

struct CalorieTarget {

    int amount;
    bool active;

    CalorieTarget()
    : amount(0), active(false) {
    }

    CalorieTarget(int amount)
    : amount(amount), active(true) {
    }

};

int getScore(const std::map<Ingredient, int> & ingredients_to_amounts,
             const CalorieTarget & calorieTarget) {
    int capacityScoreSum = 0;
    int durabilityScoreSum = 0;
    int flavorScoreSum = 0;
    int textureScoreSum = 0;
    int calorieScoreSum = 0;
    for (auto p : ingredients_to_amounts) {
        const auto & ingredient = p.first;
        const auto & amount = p.second;
        capacityScoreSum += ingredient.capacity() * amount;
        durabilityScoreSum += ingredient.durability() * amount;
        flavorScoreSum += ingredient.flavor() * amount;
        textureScoreSum += ingredient.texture() * amount;
        calorieScoreSum += ingredient.calories() * amount;
    }
    capacityScoreSum = std::max(0, capacityScoreSum);
    durabilityScoreSum = std::max(0, durabilityScoreSum);
    flavorScoreSum = std::max(0, flavorScoreSum);
    textureScoreSum = std::max(0, textureScoreSum);
    if (calorieTarget.active && calorieScoreSum != calorieTarget.amount) {
        return INT_MIN;
    }
    return capacityScoreSum * durabilityScoreSum * flavorScoreSum *
           textureScoreSum;
}

void solve(std::map<Ingredient, int>::iterator it, int amountLeft,
           const std::map<Ingredient, int> & ingredients_to_amounts,
           const CalorieTarget & calorieTarget = CalorieTarget()) {
    if (it == ingredients_to_amounts.end()) {
        return;
    }
    if (std::next(it) == ingredients_to_amounts.end()) {
        it->second = amountLeft;
        // Entertain these amounts of ingredients
        maxScore = std::max(maxScore, getScore(ingredients_to_amounts,
                                               calorieTarget));
        return;
    }
    for (int currAmt = 0; currAmt <= amountLeft; ++currAmt) {
        it->second = currAmt;
        solve(std::next(it), amountLeft - currAmt, ingredients_to_amounts,
              calorieTarget);
    }
}

int main(int argc, char * argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream f(argv[1]);
    if (!f.is_open()) {
        std::cerr << argv[1] << " not opened.\n";
        return 1;
    }
    std::map<Ingredient, int> ingredients_to_amounts;
    std::string line;
    std::regex lineRegex("^(.*): capacity (.*), durability (.*), flavor (.*), "
                         "texture (.*), calories (.*)$");
    while (std::getline(f, line)) {
        std::smatch result;
        std::regex_search(line, result, lineRegex);
        assert(result.size() == 7);
        const std::string & name = result[1];
        int capacity = toInt(result[2]);
        int durability = toInt(result[3]);
        int flavor = toInt(result[4]);
        int texture = toInt(result[5]);
        int calories = toInt(result[6]);
        ingredients_to_amounts[Ingredient(name, capacity, durability, flavor,
                                          texture, calories)] = 0;
    }
    solve(ingredients_to_amounts.begin(), 100, ingredients_to_amounts);
    std::cout << "Best cookie possible has score: " << maxScore << std::endl;
    maxScore = INT_MIN;
    solve(ingredients_to_amounts.begin(), 100, ingredients_to_amounts,
          CalorieTarget(500));
    std::cout << "Best cookie with 500 calories has score: " << maxScore
              << std::endl;
    return 0;
}
