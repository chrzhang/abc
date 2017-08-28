#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
#include <regex>
#include <map>

#define COMPARE_NUMBER_1 61
#define COMPARE_NUMBER_2 17

/*
From http://adventofcode.com/2016/day/10

--- Day 10: Balance Bots ---

You come upon a factory in which many robots are zooming around handing small
microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two
microchips, and once it does, it gives each one to a different bot or puts it
in a marked "output" bin. Sometimes, bots take microchips from "input" bins,
too.

Inspecting one of the microchips, it seems like they each contain a single
number; the bots must use some logic to decide what to do with each chip. You
access the local control computer and download the bots' instructions (your
puzzle input).

Some of the instructions specify that a specific-valued microchip should be
given to a specific bot; the rest of the instructions indicate what a given bot
should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2

Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2
chip and a value-5 chip.
Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its
higher one (5) to bot 0.
Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives
the value-3 chip to bot 0.
Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in
output 0.
In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a
value-2 microchip, and output bin 2 contains a value-3 microchip. In this
configuration, bot number 2 is responsible for comparing value-5 microchips
with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible
for comparing value-61 microchips with value-17 microchips?

--- Part Two ---

What do you get if you multiply together the values of one chip in each of
outputs 0, 1, and 2?
*/

int toInt(const std::string & someString) {
    std::stringstream ss(someString);
    int result;
    if (!(ss >> result)) {
        std::cerr << "Could not convert " << someString << " to int\n";
        return 0;
    }
    return result;
}

struct Chip {
    int value;
    Chip(int value) : value(value)
    {}
};

class Receiver {
    public:
        virtual void receiveChip(int value) = 0;
        virtual ~Receiver() {
        }
};

class OutputBin : public Receiver {
    int m_content;
    int binName;
    public:
    OutputBin(int name)
    : m_content(-1), binName(name)
    {}
    void receiveChip(int value) {
        assert(m_content == -1);
        m_content = value;
    }
    int content() const {
        return m_content;
    }
};

class Bot : public Receiver {
    Chip * m_lowChip;
    Chip * m_highChip;
    int botName;
    private:
        void handleGettingBothChips() {
            assert(m_lowChip && m_highChip);
            assert(m_lowChip->value <= m_highChip->value);
            assert(m_receiverOfLowChip && m_receiverOfHighChip);
            m_receiverOfLowChip->receiveChip(m_lowChip->value);
            m_receiverOfHighChip->receiveChip(m_highChip->value);
            delete m_lowChip;
            m_lowChip = nullptr;
            delete m_highChip;
            m_highChip = nullptr;
        }
        void considerComparison(int v1, int v2) {
            if ((v1 == COMPARE_NUMBER_1 && v2 == COMPARE_NUMBER_2) ||
                (v1 == COMPARE_NUMBER_1 && v2 == COMPARE_NUMBER_2)) {
                assert(botName == 141);
                std::cout << "Part 1: "  << botName << std::endl;
            }
        }
    public:
        Receiver * m_receiverOfLowChip;
        Receiver * m_receiverOfHighChip;
        Bot(int name)
        : m_lowChip(nullptr), m_highChip(nullptr), botName(name),
          m_receiverOfLowChip(nullptr), m_receiverOfHighChip(nullptr)
        {}
        virtual ~Bot() {
            delete m_lowChip;
            delete m_highChip;
        }
        virtual void receiveChip(int value) {
            if (m_lowChip == nullptr && m_highChip == nullptr) {
                m_lowChip = new Chip(value);
            } else if (m_lowChip == nullptr && m_highChip != nullptr) {
                considerComparison(m_highChip->value, value);
                if (m_highChip->value >= value) {
                    m_lowChip = new Chip(value);
                } else if (m_highChip->value < value) {
                    const int tempForSwap = m_highChip->value;
                    m_highChip->value = value;
                    m_lowChip = new Chip(tempForSwap);
                }
                handleGettingBothChips();
            } else if (m_highChip == nullptr && m_lowChip != nullptr) {
                considerComparison(m_lowChip->value, value);
                if (m_lowChip->value <= value) {
                    m_highChip = new Chip(value);
                } else if (m_lowChip->value > value) {
                    const int tempForSwap = m_lowChip->value;
                    m_lowChip->value = value;
                    m_highChip = new Chip(tempForSwap);
                }
                handleGettingBothChips();
            } else {
                std::cerr << "Bot " << botName
                          << " cannot receive chip if bot already has 2 chips.\n";
                assert(false);
            }
        }
};

void tests() {
    std::map<int, Bot> bots;
    bots.insert({0, Bot(0)});
    bots.insert({1, Bot(1)});
    bots.insert({2, Bot(2)});
    std::map<int, OutputBin> bins;
    bins.insert({0, OutputBin(0)});
    bins.insert({1, OutputBin(1)});
    bins.insert({2, OutputBin(2)});
    bots.at(2).m_receiverOfLowChip = &bots.at(1);
    bots.at(2).m_receiverOfHighChip = &bots.at(0);
    bots.at(1).m_receiverOfLowChip = &bins.at(1);
    bots.at(1).m_receiverOfHighChip = &bots.at(0);
    bots.at(0).m_receiverOfLowChip = &bins.at(2);
    bots.at(0).m_receiverOfHighChip = &bins.at(0);
    bots.at(2).receiveChip(5);
    bots.at(1).receiveChip(3);
    bots.at(2).receiveChip(2);
    assert(5 == bins.at(0).content());
    assert(2 == bins.at(1).content());
    assert(3 == bins.at(2).content());
}

void addIfDoesNotExist(std::map<int, Bot> & bots, const int botName) {
    if (bots.find(botName) == bots.end()) {
        bots.insert({botName, Bot(botName)});
    }
}

void addIfDoesNotExist(std::map<int, OutputBin> & bins, const int binName) {
    if (bins.find(binName) == bins.end()) {
        bins.insert({binName, OutputBin(binName)});
    }
}

int main(int argc, char * argv[]) {
    tests();
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::map<int, Bot> bots;
    std::map<int, OutputBin> bins;
    { // Read in bot behavior (who receives what) first
        std::ifstream inputFile(argv[1]);
        if (!inputFile.is_open()) {
            std::cerr << "Cannot open " << argv[1] << std::endl;
            return 1;
        }
        std::string currentLine;
        std::regex botBehaviorRegex("^bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)$");
        while (std::getline(inputFile, currentLine)) {
            std::smatch result;
            std::regex_search(currentLine, result, botBehaviorRegex);
            if (0 != result.size()) {
                assert(6 == result.size());
                const auto & botSrcNumber = toInt(result[1]);
                addIfDoesNotExist(bots, botSrcNumber);
                const auto & lowDestType = result[2];
                const auto & lowDestName = toInt(result[3]);
                if (lowDestType == "bot") {
                    addIfDoesNotExist(bots, lowDestName);
                    bots.at(botSrcNumber).m_receiverOfLowChip =
                        &bots.at(lowDestName);
                } else {
                    assert(lowDestType == "output");
                    addIfDoesNotExist(bins, lowDestName);
                    bots.at(botSrcNumber).m_receiverOfLowChip =
                        &bins.at(lowDestName);
                }
                const auto & highDestType = result[4];
                const auto & highDestName = toInt(result[5]);
                if (highDestType == "bot") {
                    addIfDoesNotExist(bots, highDestName);
                    bots.at(botSrcNumber).m_receiverOfHighChip =
                        &bots.at(highDestName);
                } else {
                    assert(highDestType== "output");
                    addIfDoesNotExist(bins, highDestName);
                    bots.at(botSrcNumber).m_receiverOfHighChip =
                        &bins.at(highDestName);
                }
            }
        }
    }
    { // Feed the numbers
        std::ifstream inputFile(argv[1]);
        if (!inputFile.is_open()) {
            std::cerr << "Cannot open " << argv[1] << std::endl;
            return 1;
        }
        std::string currentLine;
        std::regex botActionRegex("^value ([0-9]+) goes to bot ([0-9]+)$");
        while (std::getline(inputFile, currentLine)) {
            std::smatch result;
            std::regex_search(currentLine, result, botActionRegex);
            if (0 != result.size()) {
                assert(3 == result.size());
                const auto & value = toInt(result[1]);
                const auto & botDestName = toInt(result[2]);
                bots.at(botDestName).receiveChip(value);
            }
        }
    }
    const auto  & product = bins.at(0).content() *
                            bins.at(1).content() *
                            bins.at(2).content();
    assert(product == 1209);
    std::cout << "Part 2: " << product << std::endl;
    return 0;
}
