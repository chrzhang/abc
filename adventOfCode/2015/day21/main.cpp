#include <iostream>
#include <vector>
#include <string>
#include <climits>
#include <cassert>

/*
From http://adventofcode.com/2015/day/21

--- Day 21: RPG Simulator 20XX ---

Little Henry Case got a new video game for Christmas. It's an RPG, and he's
stuck on a boss. He needs to know what equipment to buy at the shop. He hands
you the controller.

In this game, the player (you) and the enemy (the boss) take turns attacking.
The player always goes first. Each attack reduces the opponent's hit points by
at least 1. The first character at or below 0 hit points loses.

Damage dealt by an attacker each turn is equal to the attacker's damage score
minus the defender's armor score. An attacker always does at least 1 damage.
So, if the attacker has a damage score of 8, and the defender has an armor
score of 3, the defender loses 5 hit points. If the defender had an armor score
of 300, the defender would still lose 1 hit point.

Your damage score and armor score both start at zero. They can be increased by
buying items in exchange for gold. You start with no items and have as much
gold as you need. Your total damage or armor is equal to the sum of those stats
from all of your items. You have 100 hit points.

Here is what the item shop is selling:

Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3

You must buy exactly one weapon; no dual-wielding. Armor is optional, but you
can't use more than one.  You can buy 0-2 rings (at most one for each hand).
You must use any items you buy. The shop only has one of each item, so you
can't buy, for example, two rings of Damage +3.

For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the
boss has 12 hit points, 7 damage, and 2 armor:

The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

You have 100 hit points. The boss's actual stats are in your puzzle input. What
is the least amount of gold you can spend and still win the fight?

--- Part Two ---

Turns out the shopkeeper is working with the boss, and can persuade you to buy
whatever items he wants. The other rules still apply, and he still only has one
of each item.

What is the most amount of gold you can spend and still lose the fight?
*/

struct Item {
    std::string name;
    int cost, damage, armor;
    Item(const std::string & name, int cost, int damage, int armor)
    : name(name), cost(cost), damage(damage), armor(armor) {
    }
};

struct Stats {
    int damage, hp, armor;
    Stats(int damage, int hp, int armor)
    : damage(damage), hp(hp), armor(armor) {
    }
};

size_t amountOn(const std::vector<bool> & mask) {
    size_t count = 0;
    for (const auto & bit : mask) {
        if (bit) {
            count += 1;
        }
    }
    return count;
}

void getBitMasks(size_t position, std::vector<bool> & mask,
                 const size_t targetAmountOn,
                 std::vector<std::vector<bool>> & returnMasks) {
    if (amountOn(mask) == targetAmountOn) {
        returnMasks.push_back(mask);
        return;
    }
    if (position >= mask.size()) {
        return;
    }
    mask[position] = true;
    getBitMasks(position + 1, mask, targetAmountOn, returnMasks);
    mask[position] = false;
    getBitMasks(position + 1, mask, targetAmountOn, returnMasks);
}

std::vector<std::vector<bool>>
bitMasks(size_t amount, size_t capacity) {
    /*
    Return a list of bit masks of length `capacity` with `amount` number of bits
    flipped on.
    */
    std::vector<std::vector<bool>> masks;
    std::vector<bool> baseMask(capacity, false);
    getBitMasks(0, baseMask, amount, masks);
    return masks;
}

std::vector<std::vector<Item>>
subsets(size_t size, const std::vector<Item> & choices) {
    if (size <= 0 || size > choices.size()) {
        return {{}};
    }
    if (size == choices.size()) {
        return { choices };
    }
    std::vector<std::vector<Item>> ss;
    if (size == 1) {
        for (const auto & item : choices) {
            ss.push_back({item});
        }
        return ss;
    }
    const auto masks = bitMasks(size, choices.size());
    for (const auto & mask : masks) {
        std::vector<Item> selection;
        for (size_t i = 0; i < mask.size(); ++i) {
            if (mask[i]) {
                selection.push_back(choices[i]);
            }
        }
        ss.push_back(selection);
    }
    return ss;
}

std::vector<std::vector<Item>>
allSubsets(size_t min, size_t max, const std::vector<Item> & choices) {
    std::vector<std::vector<Item>> allSubsets;
    for (size_t currentSize = min; currentSize <= max; ++currentSize) {
        std::vector<std::vector<Item>> subsetsOfCurrentSize =
            subsets(currentSize, choices);
        allSubsets.insert(allSubsets.end(), subsetsOfCurrentSize.begin(),
                          subsetsOfCurrentSize.end());
    }
    return allSubsets;
}

std::vector<Item> weaponChoices() {
    return {
        Item("dagger", 8, 4, 0),
        Item("shortsword", 10, 5, 0),
        Item("warhammer", 25, 6, 0),
        Item("longsword", 40, 7, 0),
        Item("greataxe", 74, 8, 0)
    };
}

std::vector<Item> armorChoices() {
    return {
        Item("leather", 13, 0, 1),
        Item("chainmail", 31, 0, 2), Item("splintmail", 53, 0, 3),
        Item("bandedmail", 75, 0, 4),
        Item("platemail", 102, 0, 5)
    };
}

std::vector<Item> ringChoices() {
    return {
        Item("ringDamage1", 25, 1, 0),
        Item("ringDamage2", 50, 2, 0),
        Item("ringDamage3", 100, 3, 0),
        Item("ringDefense1", 20, 0, 1),
        Item("ringDefense2", 40, 0, 2),
        Item("ringDefense3", 80, 0, 3)
    };
}

void printBitMasks(const std::vector<std::vector<bool>> & masks) {
    for (const auto & mask : masks) {
        std::cout << "Mask: ";
        for (const auto & b : mask) {
            std::cout << b << " ";
        }
        std::cout << std::endl;
    }
}

Stats totalStats(const std::vector<Item> & weaponPick,
                 const std::vector<Item> & armorPick,
                 const std::vector<Item> & ringPick) {
    size_t totalDamage = 0;
    size_t totalArmor = 0;
    for (const auto & weapon : weaponPick) {
        totalDamage += weapon.damage;
        totalArmor += weapon.armor;
    }
    for (const auto & armor : armorPick) {
        totalDamage += armor.damage;
        totalArmor += armor.armor;
    }
    for (const auto & ring : ringPick) {
        totalDamage += ring.damage;
        totalArmor += ring.armor;
    }
    return Stats(totalDamage, 100, totalArmor);
}

size_t cost(const std::vector<Item> & weaponPick,
            const std::vector<Item> & armorPick,
            const std::vector<Item> & ringPick) {
    size_t totalCost = 0;
    for (const auto & weapon : weaponPick) {
        totalCost += weapon.cost;
    }
    for (const auto & armor : armorPick) {
        totalCost += armor.cost;
    }
    for (const auto & ring : ringPick) {
        totalCost += ring.cost;
    }
    return totalCost;
}

void printChoice(const std::vector<Item> & weaponPick,
                 const std::vector<Item> & armorPick,
                 const std::vector<Item> & ringPick) {
    std::cout <<  std::string(80, '-') << std::endl;
    std::cout << "Weapons:" << std::endl;
    for (const auto & weapon : weaponPick) {
        std::cout << "\t" << weapon.name << std::endl;
    }
    std::cout << "Armor:" << std::endl;
    for (const auto & armor : armorPick) {
        std::cout << "\t" << armor.name << std::endl;
    }
    std::cout << "Rings:" << std::endl;
    for (const auto & ring : ringPick) {
        std::cout << "\t" << ring.name << std::endl;
    }
    Stats stats = totalStats(weaponPick, armorPick, ringPick);
    std::cout << "Damage: " << stats.damage << std::endl;
    std::cout << "HP: " << stats.hp << std::endl;
    std::cout << "Armor: " << stats.armor << std::endl;
    std::cout << "Cost: " << cost(weaponPick, armorPick, ringPick) << std::endl;
}

bool playerWinsFight(const Stats & player, const Stats & boss) {
    Stats playerStats = player;
    Stats bossStats = boss;
    bool isPlayersTurn = true;
    while (playerStats.hp > 0 && bossStats.hp > 0) {
        if (isPlayersTurn) {
            bossStats.hp -= std::max(1, playerStats.damage - bossStats.armor);
            isPlayersTurn = false;
        } else {
            playerStats.hp -= std::max(1, bossStats.damage - playerStats.armor);
            isPlayersTurn = true;
        }
    }
    return playerStats.hp > 0 ? true : false;
}

int main() {
    const std::vector<Item> weapons = weaponChoices();
    const std::vector<Item> armor = armorChoices();
    const std::vector<Item> rings = ringChoices();
    const Stats bossStats(9, 103, 2);
    int currentLowestWinningCost = INT_MAX;
    int currentHighestLosingCost = INT_MIN;
    // Must buy exactly 1 weapon
    std::vector<std::vector<Item>> weaponPicks = allSubsets(1, 1, weapons);
    for (const auto & weaponPick : weaponPicks) {
        // Armor is optional
        std::vector<std::vector<Item>> armorPicks = allSubsets(0, 1, armor);
        for (const auto & armorPick : armorPicks) {
            // Can buy 0, 1, or 2 rings
            std::vector<std::vector<Item>> ringPicks = allSubsets(0, 2, rings);
            for (const auto & ringPick : ringPicks) {
                // Have enumerated a selection of gear
                //printChoice(weaponPick, armorPick, ringPick);
                const Stats stats = totalStats(weaponPick, armorPick, ringPick);
                if (playerWinsFight(stats, bossStats)) {
                    const int costOfGear =
                        cost(weaponPick, armorPick, ringPick);
                    if (costOfGear < currentLowestWinningCost) {
                        currentLowestWinningCost = costOfGear;
                    }
                } else { // Part 2
                    const int costOfGear =
                        cost(weaponPick, armorPick, ringPick);
                    if (costOfGear > currentHighestLosingCost) {
                        currentHighestLosingCost = costOfGear;
                    }
                }
            }
        }
    }
    std::cout << "Lowest cost of winning gearset: " << currentLowestWinningCost
              << std::endl;
    std::cout << "Highest cost of losing gearset: " << currentHighestLosingCost
              << std::endl;
    assert(currentLowestWinningCost = 121);
    assert(currentHighestLosingCost = 201);
    return 0;
}
