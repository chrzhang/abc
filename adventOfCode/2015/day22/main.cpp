#include <iostream>
#include <list>
#include <string>
#include <cassert>
#include <algorithm>
#include <climits>

/*
From http://adventofcode.com/2015/day/22

--- Day 22: Wizard Simulator 20XX ---

Little Henry Case decides that defeating bosses with swords and stuff is
boring. Now he's playing the game with a wizard. Of course, he gets stuck on
another boss and needs your help again.

In this version, combat still proceeds with the player and the boss taking
alternating turns. The player still goes first. Now, however, you don't get any
equipment; instead, you must choose one of your spells to cast. The first
character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you can't attack
normally. However, since you do magic damage, your opponent's armor is ignored,
and so the boss effectively has zero armor as well. As before, if armor (from a
spell, in this case) would reduce damage below 1, it becomes 1 instead - that
is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to cast. If you
cannot afford to cast any spell, you lose. Spells cost mana; you start with 500
mana, but have no maximum limit. You must have enough mana to cast a spell, and
its cost is immediately deducted when you cast it. Your spells are Magic
Missile, Drain, Shield, Poison, and Recharge.

Magic Missile costs 53 mana. It instantly does 4 damage.  Drain costs 73 mana.
It instantly does 2 damage and heals you for 2 hit points.  Shield costs 113
mana. It starts an effect that lasts for 6 turns. While it is active, your
armor is increased by 7.  Poison costs 173 mana. It starts an effect that lasts
for 6 turns. At the start of each turn while it is active, it deals the boss 3
damage.  Recharge costs 229 mana. It starts an effect that lasts for 5 turns.
At the start of each turn while it is active, it gives you 101 new mana.
Effects all work the same way. Effects apply at the start of both the player's
turns and the boss' turns. Effects are created with a timer (the number of
turns they last); at the start of each turn, after they apply any effect they
have, their timer is decreased by one. If this decreases the timer to zero, the
effect ends. You cannot cast a spell that would start an effect which is
already active. However, effects can be started on the same turn they end.

For example, suppose the player has 10 hit points and 250 mana, and that the
boss has 13 hit points and 8 damage:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.

-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.

-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

Now, suppose the same initial conditions, except that the boss has 14 hit
points instead:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.

-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!

-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.

-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.

-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.

-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points. The boss's actual stats are
in your puzzle input. What is the least amount of mana you can spend and still
win the fight? (Do not include mana recharge effects as "spending" negative
mana.)
*/

struct PlayerStatus {

    int current_armor;
    int current_hp;
    int current_mana;
    int mana_used;
    std::list<std::string> spell_log;
    // Any non-instant spells cannot be stacked
    int shield_turns_left;
    int poison_turns_left;
    int recharge_turns_left;

    PlayerStatus(int current_hp, int current_mana)
    : current_hp(current_hp), current_mana(current_mana) {
        current_armor = 0;
        mana_used = 0;
        shield_turns_left = poison_turns_left = recharge_turns_left = 0;
    }

    bool alive() const {
        return current_hp > 0;
    }

    bool shield_active() const {
        return shield_turns_left > 0;
    }

    bool poison_active() const {
        return poison_turns_left > 0;
    }

    bool recharge_active() const {
        return recharge_turns_left > 0;
    }

    void take_damage(int damage) {
        current_hp -= std::max(1, damage - current_armor);
    }

};

std::ostream & operator<<(std::ostream & os, const PlayerStatus & ps) {
    os << "Player: ";
    os << "( HP: " << ps.current_hp << ", Armor: " << ps.current_armor
       << ", Mana: " << ps.current_mana << " )" << std::endl;
    return os;
}

struct BossStatus {

    int current_hp;
    int damage;

    BossStatus(int current_hp, int damage)
    : current_hp(current_hp), damage(damage) {
    }

    bool alive() const {
        return current_hp > 0;
    }

    void take_damage(int damage) {
        current_hp -= std::max(1, damage);
    }

};

std::ostream & operator<<(std::ostream & os, const BossStatus & bs) {
    os << "Boss: ";
    os << "( HP: " << bs.current_hp << " )" << std::endl;
    return os;
}

bool game_over(const PlayerStatus & player, const BossStatus & boss) {
    if (!player.alive() || !boss.alive()) {
        return true;
    }
    return false;
}

int apply_effects(PlayerStatus & player, BossStatus & boss) {
    /* Return 1 if game ends after applying effects. */
    player.current_armor = 0;
    if (player.shield_active()) {
        player.current_armor = 7;
        player.shield_turns_left -= 1;
    }
    if (player.poison_active()) {
        boss.take_damage(3);
        player.poison_turns_left -= 1;
    }
    if (player.recharge_active()) {
        player.current_mana += 101;
        player.recharge_turns_left -= 1;
    }
    if (game_over(player, boss)) { return 1; }
    return 0;
}

int magic_missile(PlayerStatus & player, BossStatus & boss) {
    static const int MAGIC_MISSILE_COST = 53;
    if (player.current_mana < MAGIC_MISSILE_COST) {
        return -1;
    }
    player.current_mana -= MAGIC_MISSILE_COST;
    player.spell_log.push_back("magic missile");
    player.mana_used += MAGIC_MISSILE_COST;
    if (apply_effects(player, boss)) { return 1; }
    boss.take_damage(4);
    if (game_over(player, boss)) { return 1; }
    // Boss attacks!
    if (apply_effects(player, boss)) { return 1; }
    player.take_damage(boss.damage);
    if (game_over(player, boss)) { return 1; }
    return 0;
}

int drain(PlayerStatus & player, BossStatus & boss) {
    static const int DRAIN_COST = 73;
    if (player.current_mana < DRAIN_COST) {
        return -1;
    }
    player.current_mana -= DRAIN_COST;
    player.spell_log.push_back("drain");
    player.mana_used += DRAIN_COST;
    if (apply_effects(player, boss)) { return 1; }
    boss.take_damage(2);
    if (game_over(player, boss)) { return 1; }
    player.current_hp += 2;
    // Boss attacks!
    if (apply_effects(player, boss)) { return 1; }
    player.take_damage(boss.damage);
    if (game_over(player, boss)) { return 1; }
    return 0;
}

int shield(PlayerStatus & player, BossStatus & boss) {
    static const int SHIELD_COST = 113;
    if (player.current_mana < SHIELD_COST || player.shield_turns_left > 1) {
        return -1;
    }
    player.current_mana -= SHIELD_COST;
    player.spell_log.push_back("shield");
    player.mana_used += SHIELD_COST;
    if (apply_effects(player, boss)) { return 1; }
    player.shield_turns_left = 6;
    // Boss attacks!
    if (apply_effects(player, boss)) { return 1; }
    player.take_damage(boss.damage);
    if (game_over(player, boss)) { return 1; }
    return 0;
}

int poison(PlayerStatus & player, BossStatus & boss) {
    static const int POISON_COST = 173;
    if (player.current_mana < POISON_COST || player.poison_turns_left > 1) {
        return -1;
    }
    player.current_mana -= POISON_COST;
    player.spell_log.push_back("poison");
    player.mana_used += POISON_COST;
    if (apply_effects(player, boss)) { return 1; }
    player.poison_turns_left = 6;
    // Boss attacks!
    if (apply_effects(player, boss)) { return 1; }
    player.take_damage(boss.damage);
    if (game_over(player, boss)) { return 1; }
    return 0;
}

int recharge(PlayerStatus & player, BossStatus & boss) {
    static const int RECHARGE_COST = 229;
    if (player.current_mana < RECHARGE_COST || player.recharge_turns_left > 1) {
        return -1;
    }
    player.current_mana -= RECHARGE_COST;
    player.spell_log.push_back("recharge");
    player.mana_used += RECHARGE_COST;
    if (apply_effects(player, boss)) { return 1; }
    player.recharge_turns_left = 5;
    // Boss attacks!
    if (apply_effects(player, boss)) { return 1; }
    player.take_damage(boss.damage);
    if (game_over(player, boss)) { return 1; }
    return 0;
}

void exploreAux(const PlayerStatus & p, const BossStatus & b,
            int & lowest_mana_used_to_win_so_far,
            std::list<std::string> & spells) {
    if (game_over(p, b)) {
        if (p.alive()) {
            if (p.mana_used < lowest_mana_used_to_win_so_far) {
                spells = p.spell_log;
                lowest_mana_used_to_win_so_far = p.mana_used;
            }
        }
        return;
    }
    if (p.mana_used > lowest_mana_used_to_win_so_far) {
        return;
    }
    {
        PlayerStatus c_p = p;
        BossStatus c_b = b;
        if (-1 != magic_missile(c_p, c_b)) {
            exploreAux(c_p, c_b, lowest_mana_used_to_win_so_far, spells);
        }
    }
    {
        PlayerStatus c_p = p;
        BossStatus c_b = b;
        if (-1 != drain(c_p, c_b)) {
            exploreAux(c_p, c_b, lowest_mana_used_to_win_so_far, spells);
        }
    }
    {
        PlayerStatus c_p = p;
        BossStatus c_b = b;
        if (-1 != poison(c_p, c_b)) {
            exploreAux(c_p, c_b, lowest_mana_used_to_win_so_far, spells);
        }
    }
    {
        PlayerStatus c_p = p;
        BossStatus c_b = b;
        if (-1 != shield(c_p, c_b)) {
            exploreAux(c_p, c_b, lowest_mana_used_to_win_so_far, spells);
        }
    }
    {
        PlayerStatus c_p = p;
        BossStatus c_b = b;
        if (-1 != recharge(c_p, c_b)) {
            exploreAux(c_p, c_b, lowest_mana_used_to_win_so_far, spells);
        }
    }
}

int explore(const PlayerStatus & p, const BossStatus & b) {
    int lowest_mana_used_to_win_so_far = INT_MAX;
    std::list<std::string> spells;
    exploreAux(p, b, lowest_mana_used_to_win_so_far, spells);
    std::cout << "Mana: " << lowest_mana_used_to_win_so_far << std::endl;
    for (const auto & spell : spells) {
        std::cout << spell << std::endl;
    }
    return lowest_mana_used_to_win_so_far;
}

int main() {
    PlayerStatus p(50, 500);
    BossStatus b(71, 10);
    std::cout << p << b;
    assert(1824 == explore(p, b));
    return 0;
}
