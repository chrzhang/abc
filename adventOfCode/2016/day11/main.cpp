#include <iostream>
#include <string>
#include <sstream>
#include <cassert>
#include <set>
#include <algorithm>

/*
From http://adventofcode.com/2016/day/11

--- Day 11: Radioisotope Thermoelectric Generators ---

You come upon a column of four floors that have been entirely sealed off from
the rest of the building except for a small dedicated lobby. There are some
radiation warnings and a big sign which reads "Radioisotope Testing Facility".

According to the project status board, this facility is currently being used to
experiment with Radioisotope Thermoelectric Generators (RTGs, or simply
"generators") that are designed to be paired with specially-constructed
microchips. Basically, an RTG is a highly radioactive rock that generates
electricity through heat.

The experimental RTGs have poor radiation containment, so they're dangerously
radioactive. The chips are prototypes and don't have normal radiation
shielding, but they do have the ability to generate an electromagnetic
radiation shield when powered. Unfortunately, they can only be powered by their
corresponding RTG. An RTG powering a microchip is still dangerous to other
microchips.

In other words, if a chip is ever left in the same area as another RTG, and
it's not connected to its own RTG, the chip will be fried. Therefore, it is
assumed that you will follow procedure and keep chips connected to their
corresponding RTG when they're in the same room, and away from other RTGs
otherwise.

These microchips sound very interesting and useful to your current activities,
and you'd like to try to retrieve them. The fourth floor of the facility has an
assembling machine which can make a self-contained, shielded computer for you
to take with you - that is, if you can bring it all of the RTGs and microchips.

Within the radiation-shielded part of the facility (in which it's safe to have
these pre-assembly RTGs), there is an elevator that can move between the four
floors. Its capacity rating means it can carry at most yourself and two RTGs or
microchips in any combination. (They're rigged to some heavy diagnostic
equipment - the assembling machine will detach it for you.) As a security
measure, the elevator will only function if it contains at least one RTG or
microchip. The elevator always stops on each floor to recharge, and this takes
long enough that the items within it and the items on that floor can irradiate
each other. (You can prevent this if a Microchip and its Generator end up on
the same floor in this way, as they can be connected while the elevator is
recharging.)

You make some notes of the locations of each component of interest (your puzzle
input). Before you don a hazmat suit and start moving things around, you'd like
to have an idea of what you need to do.

When you enter the containment area, you and the elevator will start on the
first floor.

For example, suppose the isolated area has the following arrangement:

The first floor contains a hydrogen-compatible microchip and a
lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for
Lithium, M for Microchip, and G for Generator), the initial state looks like
this:

F4 .  .  .  .  .
F3 .  .  .  LG .
F2 .  HG .  .  .
F1 E  .  HM .  LM

Then, to get everything up to the assembling machine on the fourth floor, the
following steps could be taken:

Bring the Hydrogen-compatible Microchip to the second floor, which is safe
because it can get power from the Hydrogen Generator:

F4 .  .  .  .  .
F3 .  .  .  LG .
F2 E  HG HM .  .
F1 .  .  .  .  LM

Bring both Hydrogen-related items to the third floor, which is safe because the
Hydrogen-compatible microchip is getting power from its generator:

F4 .  .  .  .  .
F3 E  HG HM LG .
F2 .  .  .  .  .
F1 .  .  .  .  LM

Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible
Microchip back down with you so you can still use the elevator:

F4 .  .  .  .  .
F3 .  HG .  LG .
F2 E  .  HM .  .
F1 .  .  .  .  LM

At the first floor, grab the Lithium-compatible Microchip, which is safe
because Microchips don't affect each other:

F4 .  .  .  .  .
F3 .  HG .  LG .
F2 .  .  .  .  .
F1 E  .  HM .  LM

Bring both Microchips up one floor, where there is nothing to fry them:

F4 .  .  .  .  .
F3 .  HG .  LG .
F2 E  .  HM .  LM
F1 .  .  .  .  .

Bring both Microchips up again to floor three, where they can be temporarily
connected to their corresponding generators while the elevator recharges,
preventing either of them from being fried:

F4 .  .  .  .  .
F3 E  HG HM LG LM
F2 .  .  .  .  .
F1 .  .  .  .  .

Bring both Microchips to the fourth floor:

F4 E  .  HM .  LM
F3 .  HG .  LG .
F2 .  .  .  .  .
F1 .  .  .  .  .

Leave the Lithium-compatible microchip on the fourth floor, but bring the
Hydrogen-compatible one so you can still use the elevator; this is safe because
although the Lithium Generator is on the destination floor, you can connect
Hydrogen-compatible microchip to the Hydrogen Generator there:

F4 .  .  .  .  LM
F3 E  HG HM LG .
F2 .  .  .  .  .
F1 .  .  .  .  .

Bring both Generators up to the fourth floor, which is safe because you can
connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:

F4 E  HG .  LG LM
F3 .  .  HM .  .
F2 .  .  .  .  .
F1 .  .  .  .  .

Bring the Lithium Microchip with you to the third floor so you can use the
elevator:

F4 .  HG .  LG .
F3 E  .  HM .  LM
F2 .  .  .  .  .
F1 .  .  .  .  .

Bring both Microchips to the fourth floor:

F4 E  HG HM LG LM
F3 .  .  .  .  .
F2 .  .  .  .  .
F1 .  .  .  .  .

In this arrangement, it takes 11 steps to collect all of the objects at the
fourth floor for assembly. (Each elevator stop counts as one step, even if
nothing is added to or removed from it.)

In your situation, what is the minimum number of steps required to bring all of
the objects to the fourth floor?

--- Part Two ---

You step into the cleanroom separating the lobby from the isolated area and put
on the hazmat suit.

Upon entering the isolated containment area, however, you notice some extra
parts on the first floor that weren't listed on the record outside:

An elerium generator.
An elerium-compatible microchip.
A dilithium generator.
A dilithium-compatible microchip.

These work just like the other generators and microchips. You'll have to get
them up to assembly as well.

What is the minimum number of steps required to bring all of the objects,
including these four new ones, to the fourth floor?
*/

enum Location {
    ELEVATOR = 0,
    FLOOR1 = 1,
    FLOOR2 = 2,
    FLOOR3 = 3,
    FLOOR4 = 4
};

static const auto & floors = { ELEVATOR, FLOOR1, FLOOR2, FLOOR3, FLOOR4 };

enum Element {
    THULIUM = 0,
    PLUTONIUM = 1,
    STRONTIUM = 2,
    PROMETHIUM = 3,
    RUTHENIUM = 4,
    ELERIUM = 5,
    DILITHIUM = 6
};

std::string floorToString(const size_t location) {
    assert(location >= ELEVATOR && location <= FLOOR4);
    static const std::vector<std::string> floorNames({ "E", "F1", "F2", "F3",
                                                       "F4" });
    return floorNames[location];
}

std::string elementToString(const size_t element) {
    assert(element >= THULIUM && element <= DILITHIUM);
    static const std::vector<std::string> elementNames({ "thulium",
                                                         "plutonium",
                                                         "strontium",
                                                         "promethium",
                                                         "ruthenium",
                                                         "elerium",
                                                         "dilithium" });
    return elementNames[element];
}

static const int ELEVATOR_CAPACITY = 2;

struct Subset {
    std::vector<bool> chips;
    std::vector<bool> generators;
};

class FacilityStatus {

    Location elevatorFloor;
    std::vector<std::vector<bool>> locations_to_microchips;
    std::vector<std::vector<bool>> locations_to_generators;

    std::vector<std::vector<bool>>
    getAllSubsetsOfSize(const size_t amount,
                        const std::vector<bool> & items) const {
        if (amount == 0 || amount > numberTrueIn(items)) {
            return {};
        }
        if (amount == numberTrueIn(items)) {
            return { items };
        }
        std::vector<Element> elements;
        for (size_t element = 0; element < items.size(); ++element) {
            if (items[element]) {
                elements.push_back((Element) element);
            }
        }
        std::vector<bool> chosen(elements.size(), false);
        for (size_t i = 0; i < amount; ++i) {
            chosen[chosen.size() - 1 - i] = true;
        }
        std::vector<std::vector<bool>> result;
        do {
            std::vector<bool> subset(items.size(), false);
            for (size_t choiceIndex = 0; choiceIndex < chosen.size();
                 ++choiceIndex) {
                if (chosen[choiceIndex]) {
                    subset[elements[choiceIndex]] = true;
                }
            }
            assert(numberTrueIn(subset) == amount);
            result.push_back(subset);
        } while (std::next_permutation(chosen.begin(), chosen.end()));
        return result;
    }

    bool chipsDecayFromGenerators(const std::vector<bool> & chips,
                                  const std::vector<bool> & generators) const {
        if (allFalse(chips) || allFalse(generators)) {
            return false;
        }
        for (size_t chip = 0; chip < chips.size(); ++chip) {
            if (chips[chip] && !generators[chip]) {
                return true; // Could not find shield, thus decaying
            }
        }
        return false; // Everything was shielded
    }

    size_t numberTrueIn(const std::vector<bool> & someVector) const {
        size_t result = 0;
        for (const auto & b : someVector) {
            if (b) {
                ++result;
            }
        }
        return result;
    }

    bool allFalse(const std::vector<bool> & someVector) const {
        for (const auto & b : someVector) {
            if (b) {
                return false;
            }
        }
        return true;
    }

    public:

        std::string representation() const {
            /*
            Get a representation of the current status, each character
            representing the location of the object corresponding to the index.

            Since there are 5 pairs of chip-generators, 10 bytes are used as
            well as an additional byte to represent the floor of the elevator.
            _ _ _ _ _ _ _ _ _ _ _
            0 1 2 3 4 5 6 7 8 9 A

            0 - Thulium Chip
            1 - Thulium Generator
            2 - Plutonium Chip
            3 - Plutonium Generator
            4 - Strontium Chip
            5 - Strontium Generator
            6 - Promethium Chip
            7 - Promethium Generator
            8 - Ruthenium Chip
            9 - Ruthenium Generator
            A - Elevator Floor
            */
            std::string result(1 + 2 * numberOfElements(), ' ');
            for (size_t loc = ELEVATOR; loc <= FLOOR4; ++loc) {
                for (size_t element = 0; element < numberOfElements();
                     ++element) {
                    if (locations_to_microchips[loc][element]) {
                        result[element * 2] = '0' + (char) loc;
                    }
                    if (locations_to_generators[loc][element]) {
                        result[element * 2 + 1] = '0' + (char) loc;
                    }
                }
            }
            std::vector<std::string> pairs(numberOfElements(), "  ");
            for (size_t pairsIndex = 0;
                 pairsIndex < numberOfElements(); ++pairsIndex) {
                pairs[pairsIndex][0] = result[pairsIndex * 2];
                pairs[pairsIndex][1] = result[pairsIndex * 2 + 1];
            }
            std::sort(pairs.begin(), pairs.end());
            for (size_t index = 0;
                 index < 2 * numberOfElements(); ++index) {
                result[index] = pairs[index / 2][index % 2];
            }
            result[2 * numberOfElements()] =
                '0' + (char) elevatorFloor;
            return result;
        }

        bool isSafeConfiguration() const {
            // Check if every chip is safe
            for (size_t floor = ELEVATOR; floor <= FLOOR4; ++floor) {
                std::vector<bool> allChips(numberOfElements(), false);
                std::vector<bool> allGenerators(numberOfElements(), false);
                if (elevatorFloor == floor) {
                    for (size_t element = 0; element < numberOfElements();
                         ++element) {
                        allChips[element] =
                            locations_to_microchips[ELEVATOR][element];
                        allGenerators[element] =
                            locations_to_generators[ELEVATOR][element];
                    }
                }
                for (size_t element = 0; element < numberOfElements();
                     ++element) {
                    if (locations_to_microchips[floor][element]) {
                        allChips[element] = true;
                    }
                    if (locations_to_generators[floor][element]) {
                        allGenerators[element] = true;
                    }
                }
                if (allFalse(allChips) || allFalse(allGenerators)) {
                    continue;
                }
                for (size_t chip = 0; chip < allChips.size(); ++chip) {
                    if (allChips[chip]) {
                        if (!allGenerators[chip]) { // Not shielded
                            return false;
                        }
                    }
                }
            }
            return true;
        }

        bool isSolvedConfiguration() const {
            assert(numberOfChipsIn(ELEVATOR) +
                   numberOfChipsIn(FLOOR1) +
                   numberOfChipsIn(FLOOR2) +
                   numberOfChipsIn(FLOOR3) +
                   numberOfChipsIn(FLOOR4) == numberOfElements());
            assert(numberOfGeneratorsIn(ELEVATOR) +
                   numberOfGeneratorsIn(FLOOR1) +
                   numberOfGeneratorsIn(FLOOR2) +
                   numberOfGeneratorsIn(FLOOR3) +
                   numberOfGeneratorsIn(FLOOR4) == numberOfElements());
            return numberOfItemsIn(FLOOR4) == 2 * numberOfElements();
        }

        FacilityStatus(const std::vector<std::vector<bool>> & chips,
                       const std::vector<std::vector<bool>> & generators) {
            locations_to_microchips = chips;
            locations_to_generators = generators;
            elevatorFloor = FLOOR1;
            assert(isSafeConfiguration());
            assert(!isSolvedConfiguration());
            assert(numberOfGeneratorsIn(ELEVATOR) +
                   numberOfGeneratorsIn(FLOOR1) +
                   numberOfGeneratorsIn(FLOOR2) +
                   numberOfGeneratorsIn(FLOOR3) +
                   numberOfGeneratorsIn(FLOOR4) ==
                   numberOfChipsIn(ELEVATOR) +
                   numberOfChipsIn(FLOOR1) +
                   numberOfChipsIn(FLOOR2) +
                   numberOfChipsIn(FLOOR3) +
                   numberOfChipsIn(FLOOR4));
        }

        size_t numberOfChipsIn(const Location location) const {
            return numberTrueIn(locations_to_microchips[location]);
        }

        size_t numberOfGeneratorsIn(const Location location) const {
            return numberTrueIn(locations_to_generators[location]);
        }

        size_t numberOfItemsIn(const Location location) const {
            return numberOfChipsIn(location) + numberOfGeneratorsIn(location);
        }

        size_t numberOfElements() const {
            return numberOfChipsIn(ELEVATOR) +
                   numberOfChipsIn(FLOOR1) +
                   numberOfChipsIn(FLOOR2) +
                   numberOfChipsIn(FLOOR3) +
                   numberOfChipsIn(FLOOR4);
        }

        const Location getElevatorFloor() const {
            return elevatorFloor;
        }

        std::vector<Subset>
        getAllSubsetsOfSize_On_(const size_t amount,
                                const Location location) const {
            if (amount == 0 || numberOfItemsIn(location) < amount) {
                Subset emptySubset;
                emptySubset.chips =
                    std::vector<bool>(numberOfElements(), false);
                emptySubset.generators =
                    std::vector<bool>(numberOfElements(), false);
                return { emptySubset };
            }
            std::vector<Subset> result;
            for (size_t amountChips = 0;
                 (amountChips <= amount) &&
                 (amountChips <= numberOfChipsIn(location));
                 ++amountChips) {
                const auto amountGenerators = amount - amountChips;
                if (amountGenerators > numberOfGeneratorsIn(location)) {
                    continue;
                }
                assert(amountGenerators + amountChips == amount);
                const auto & chipsAtLocation =
                    locations_to_microchips[location];
                const auto & generatorsAtLocation =
                    locations_to_generators[location];
                const auto & chipSubsets =
                    getAllSubsetsOfSize(amountChips, chipsAtLocation);
                const auto & generatorSubsets =
                    getAllSubsetsOfSize(amountGenerators, generatorsAtLocation);
                for (const auto & chipSubset : chipSubsets) {
                    assert(numberTrueIn(chipSubset) == amountChips);
                    for (const auto & generatorSubset : generatorSubsets) {
                        assert(numberTrueIn(generatorSubset) ==
                               amountGenerators);
                        Subset itemSubset;
                        itemSubset.chips = chipSubset;
                        itemSubset.generators = generatorSubset;
                        result.push_back(itemSubset);
                    }
                }
                if (chipSubsets.size() == 0) {
                    for (const auto & generatorSubset : generatorSubsets) {
                        assert(numberTrueIn(generatorSubset) == amount);
                        Subset itemSubset;
                        itemSubset.chips =
                            std::vector<bool>(numberOfElements(), false);
                        itemSubset.generators = generatorSubset;
                        result.push_back(itemSubset);
                    }
                }
                if (generatorSubsets.size() == 0) {
                    for (const auto & chipSubset : chipSubsets) {
                        assert(numberTrueIn(chipSubset) == amount);
                        Subset itemSubset;
                        itemSubset.chips = chipSubset;
                        itemSubset.generators =
                            std::vector<bool>(numberOfElements(), false);
                        result.push_back(itemSubset);
                    }
                }
            }
            return result;
        }

        bool wouldDecayIfFloorLost(const std::vector<bool> & chips,
                                   const std::vector<bool> & generators) const {
            assert(chips.size() == numberOfElements());
            assert(chips.size() == generators.size());
            for (size_t generator = 0; generator < generators.size();
                 ++generator) {
                if (generators[generator]) { // Generator is leaving
                    if (chips[generator] == true) {
                        continue; // Chip is also leaving on elevator
                    } else {
                        if (locations_to_microchips[elevatorFloor][generator]) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        void loadIntoElevator(const std::vector<bool> & chips,
                              const std::vector<bool> & generators) {
            assert(chips.size() == numberOfElements());
            assert(chips.size() == generators.size());
            assert(numberTrueIn(chips) + numberTrueIn(generators) <=
                   ELEVATOR_CAPACITY);
            for (size_t chip = 0; chip < chips.size(); ++chip) {
                if (chips[chip]) {
                    locations_to_microchips[elevatorFloor][chip] = false;
                    locations_to_microchips[ELEVATOR][chip] = true;
                }
                const size_t generator = chip;
                if (generators[generator]) {
                    locations_to_generators[elevatorFloor][generator] = false;
                    locations_to_generators[ELEVATOR][generator] = true;
                }
            }

        }

        void unloadFromElevator(const std::vector<bool> & chips,
                                const std::vector<bool> & generators) {
            assert(chips.size() == numberOfElements());
            assert(chips.size() == generators.size());
            assert(numberTrueIn(chips) + numberTrueIn(generators)
                   <= ELEVATOR_CAPACITY);
            for (size_t chip = 0; chip < chips.size(); ++chip) {
                if (chips[chip]) {
                    locations_to_microchips[elevatorFloor][chip] = true;
                    locations_to_microchips[ELEVATOR][chip] = false;
                }
                const size_t generator = chip;
                if (generators[generator]) {
                    locations_to_generators[elevatorFloor][generator] = true;
                    locations_to_generators[ELEVATOR][generator] = false;
                }
            }
        }

        void sendElevatorUp() {
            assert(elevatorFloor != FLOOR4);
            assert(numberOfItemsIn(ELEVATOR) > 0);
            if (elevatorFloor == FLOOR1) {
                elevatorFloor = FLOOR2;
            } else if (elevatorFloor == FLOOR2) {
                elevatorFloor = FLOOR3;
            } else if (elevatorFloor == FLOOR3) {
                elevatorFloor = FLOOR4;
            } else {
                assert(false);
            }
        }

        void sendElevatorDown() {
            assert(elevatorFloor != FLOOR1);
            assert(numberOfItemsIn(ELEVATOR) > 0);
            if (elevatorFloor == FLOOR2) {
                elevatorFloor = FLOOR1;
            } else if (elevatorFloor == FLOOR3) {
                elevatorFloor = FLOOR2;
            } else if (elevatorFloor == FLOOR4) {
                elevatorFloor = FLOOR3;
            } else {
                assert(false);
            }
        }

        friend void test();
        friend std::ostream & operator<<(std::ostream & os,
                                         const FacilityStatus & facilityStatus);

};

std::ostream & operator<<(std::ostream & os, const FacilityStatus & fs) {
    for (size_t floor = 0; floor <= FLOOR4; ++floor) {
        std::cout << floorToString(floor) << ": ";
        for (size_t element = 0; element < fs.numberOfElements(); ++element) {
            if (fs.locations_to_microchips[floor][element]) {
                os << elementToString(element) << "-chip" << " ";
            }
            if (fs.locations_to_generators[floor][element]) {
                os << elementToString(element) << "-generator" << " ";
            }
        }
        os << std::endl;
    }
    os << "elevator on " << floorToString(fs.elevatorFloor) << std::endl;

    return os;
}

void test() {
    std::vector<std::vector<bool>>
        locations_to_microchips(5, std::vector<bool>(2, false));
    std::vector<std::vector<bool>>
        locations_to_generators(5, std::vector<bool>(2, false));
    locations_to_microchips[FLOOR1][THULIUM] = true;
    locations_to_microchips[FLOOR1][PLUTONIUM] = true;
    locations_to_generators[FLOOR2][THULIUM] = true;
    locations_to_generators[FLOOR3][PLUTONIUM] = true;
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    assert(facilityStatus.representation() == "12131");
    assert(facilityStatus.locations_to_microchips ==
           std::vector<std::vector<bool>>({ { false, false },
                                            { true, true },
                                            { false, false},
                                            { false, false},
                                            { false, false} }));
    assert(facilityStatus.locations_to_generators ==
           std::vector<std::vector<bool>>({ { false, false },
                                            { false, false },
                                            { true, false},
                                            { false, true},
                                            { false, false} }));
    assert(facilityStatus.numberOfChipsIn(ELEVATOR) == 0);
    assert(facilityStatus.numberOfChipsIn(FLOOR1) == 2);
    assert(facilityStatus.numberOfChipsIn(FLOOR2) == 0);
    assert(facilityStatus.numberOfChipsIn(FLOOR3) == 0);
    assert(facilityStatus.numberOfChipsIn(FLOOR4) == 0);
    assert(facilityStatus.numberOfGeneratorsIn(ELEVATOR) == 0);
    assert(facilityStatus.numberOfGeneratorsIn(FLOOR1) == 0);
    assert(facilityStatus.numberOfGeneratorsIn(FLOOR2) == 1);
    assert(facilityStatus.numberOfGeneratorsIn(FLOOR3) == 1);
    assert(facilityStatus.numberOfGeneratorsIn(FLOOR4) == 0);
    assert(facilityStatus.numberOfItemsIn(ELEVATOR) == 0);
    assert(facilityStatus.numberOfItemsIn(FLOOR1) == 2);
    assert(facilityStatus.numberOfItemsIn(FLOOR2) == 1);
    assert(facilityStatus.numberOfItemsIn(FLOOR3) == 1);
    assert(facilityStatus.numberOfItemsIn(FLOOR4) == 0);
    {
        const auto & allSubsetsOfSize2OnFloor1 =
            facilityStatus.getAllSubsetsOfSize_On_(2, FLOOR1);
        assert(allSubsetsOfSize2OnFloor1.size() == 1);
        assert(allSubsetsOfSize2OnFloor1[0].chips ==
               std::vector<bool>({ true, true }));
        assert(allSubsetsOfSize2OnFloor1[0].generators ==
               std::vector<bool>({ false, false}));
    }
    {
        const auto & allSubsetsOfSize1OnFloor1 =
            facilityStatus.getAllSubsetsOfSize_On_(1, FLOOR1);
        assert(allSubsetsOfSize1OnFloor1.size() == 2);
        assert(allSubsetsOfSize1OnFloor1[0].chips ==
               std::vector<bool>({ false, true}));
        assert(allSubsetsOfSize1OnFloor1[1].chips ==
               std::vector<bool>({ true, false}));
    }
    facilityStatus.loadIntoElevator(std::vector<bool>({ true, false }),
                                    std::vector<bool>({ false, false }));
    // Loaded in thulium chip
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.loadIntoElevator(std::vector<bool>({ false, false }),
                                    std::vector<bool>({ true, false }));
    // Loaded in thulium generator
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    facilityStatus.unloadFromElevator(std::vector<bool>({ false, false}),
                                      std::vector<bool>({ true, false}));
    // Unloaded thulium generator
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorDown();
    // Went down
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorDown();
    // Went down
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.loadIntoElevator(std::vector<bool>({false, true}),
                                    std::vector<bool>({false, false}));
    // Loaded in plutonium chip
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.unloadFromElevator(std::vector<bool>({false, true}),
                                      std::vector<bool>({false, false}));
    // Unload plutonium chip
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorDown();
    // Went down
    facilityStatus.loadIntoElevator(std::vector<bool>({false, false}),
                                    std::vector<bool>({true, true}));
    // Loaded in thulium generator and plutonium generator
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.unloadFromElevator(std::vector<bool>({false, false}),
                                      std::vector<bool>({false, true}));
    // Unloaded plutonium generator
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorDown();
    // Went down
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.loadIntoElevator(std::vector<bool>({true, false}),
                                    std::vector<bool>({false, false}));
    // Loaded in thulium chip
    assert(facilityStatus.isSafeConfiguration());
    assert(!facilityStatus.isSolvedConfiguration());
    facilityStatus.sendElevatorUp();
    // Went up
    facilityStatus.unloadFromElevator(std::vector<bool>({true, false}),
                                      std::vector<bool>({true, false}));
    // Unloaded thulium chip and thulium generator
    assert(facilityStatus.isSafeConfiguration());
    assert(facilityStatus.isSolvedConfiguration());
}

static std::set<std::string> oldStatuses;

void addStatus(const FacilityStatus facilityStatus,
               std::vector<FacilityStatus> & nextStatuses) {
    const auto statusRepr = facilityStatus.representation();
    if (oldStatuses.find(statusRepr) == oldStatuses.end()) {
        nextStatuses.push_back(facilityStatus);
        oldStatuses.insert(statusRepr);
    }
}

void unloadElevator(const FacilityStatus facilityStatus,
                    const size_t amountOfThingsToUnloadFromElevator,
                    std::vector<FacilityStatus> & nextStatuses) {
    // Unload 0, 1, or 2 things from the elevator
    const auto & allSubsetsOfTargetSize =
        facilityStatus.getAllSubsetsOfSize_On_(amountOfThingsToUnloadFromElevator,
                                               ELEVATOR);
    for (const auto & chosenSubset : allSubsetsOfTargetSize) {
        const auto & chosenChips = chosenSubset.chips;
        const auto & chosenGenerators = chosenSubset.generators;
        auto unloadedFacilityStatus = facilityStatus;
        unloadedFacilityStatus.unloadFromElevator(chosenChips,
                                                  chosenGenerators);
        addStatus(unloadedFacilityStatus, nextStatuses);
    }
}

void moveElevator(const FacilityStatus facilityStatus,
                  std::vector<FacilityStatus> & nextStatuses) {
    // Move the elevator up or down in a valid direction
    if (facilityStatus.getElevatorFloor() < FLOOR4 &&
        facilityStatus.numberOfItemsIn(ELEVATOR) > 0) { // Can go up
        auto upFacilityStatus = facilityStatus;
        upFacilityStatus.sendElevatorUp();
        if (upFacilityStatus.isSafeConfiguration()) {
            // Prune: If moving to new floor would cause decay, stop
            for (size_t amountOfThingsToUnloadFromElevator = 0;
                 amountOfThingsToUnloadFromElevator <=
                 facilityStatus.numberOfItemsIn(ELEVATOR);
                 ++amountOfThingsToUnloadFromElevator) {
                unloadElevator(upFacilityStatus,
                               amountOfThingsToUnloadFromElevator,
                               nextStatuses);
            }
        }
    }
    if (facilityStatus.getElevatorFloor() > FLOOR1 &&
        facilityStatus.numberOfItemsIn(ELEVATOR) > 0) { // Can go down
        auto downFacilityStatus = facilityStatus;
        downFacilityStatus.sendElevatorDown();
        if (downFacilityStatus.isSafeConfiguration()) {
            // Prune: If moving to new floor would cause decay, stop
            for (size_t amountOfThingsToUnloadFromElevator = 0;
                 amountOfThingsToUnloadFromElevator <=
                 facilityStatus.numberOfItemsIn(ELEVATOR);
                 ++amountOfThingsToUnloadFromElevator) {
                unloadElevator(downFacilityStatus,
                               amountOfThingsToUnloadFromElevator,
                               nextStatuses);
            }
        }
    }
}

void loadElevator(const FacilityStatus facilityStatus,
                  const size_t amountOfThingsToLoadIntoElevator,
                  std::vector<FacilityStatus> & nextStatuses) {
    const auto & allSubsetsOfTargetSize =
        facilityStatus.getAllSubsetsOfSize_On_(amountOfThingsToLoadIntoElevator,
                                               facilityStatus.getElevatorFloor());
    for (const auto & chosenSubset : allSubsetsOfTargetSize) {
        const auto & chosenChips = chosenSubset.chips;
        const auto & chosenGenerators = chosenSubset.generators;
        if (facilityStatus.wouldDecayIfFloorLost(chosenChips,
                                                 chosenGenerators)) {
            // Prune: Chips left on the floor must not decay (since the elevator is moving away)
            continue;
        }
        auto loadedFacilityStatus = facilityStatus;
        loadedFacilityStatus.loadIntoElevator(chosenChips, chosenGenerators);
        moveElevator(loadedFacilityStatus, nextStatuses);
    }
}

int explore(const FacilityStatus facilityStatus) {
    std::vector<FacilityStatus> nextStatuses = { facilityStatus };
    for (int stepCounter = 0; !nextStatuses.empty(); ++stepCounter) {
        std::cout << "\tStep: " << stepCounter << ", considering "
                  << nextStatuses.size() << " statuses.\n";
        std::vector<FacilityStatus> freshNextStatuses;
        for (const auto & currentStatus : nextStatuses) {
            if (currentStatus.isSolvedConfiguration()) {
                return stepCounter;
            }
            // Prune: Elevator and floor cannot have a chip that would decay
            if (!currentStatus.isSafeConfiguration()) {
                continue;
            }
            // Load 0, 1, or 2 things into the elevator
            for (size_t amountOfThingsToLoadIntoElevator = 0;
                (int) amountOfThingsToLoadIntoElevator <=
                std::max(0, ELEVATOR_CAPACITY -
                (int) currentStatus.numberOfItemsIn(ELEVATOR));
                ++amountOfThingsToLoadIntoElevator) {
                if (amountOfThingsToLoadIntoElevator == 0 &&
                    currentStatus.numberOfItemsIn(ELEVATOR) == 0) {
                    // Prune: Elevator must have at least 1 chip or generator
                    continue;
                }
                if (amountOfThingsToLoadIntoElevator >
                    currentStatus.numberOfItemsIn(currentStatus.getElevatorFloor())) {
                    // Prune: Cannot load more items than there are on the floor
                    continue;
                }
                loadElevator(currentStatus, amountOfThingsToLoadIntoElevator,
                             freshNextStatuses);
            }
        }
        nextStatuses = freshNextStatuses;
    }
    return -1;
}

void example() {
    oldStatuses.clear();
    std::vector<std::vector<bool>>
        locations_to_microchips(5, std::vector<bool>(2, false));
    std::vector<std::vector<bool>>
        locations_to_generators(5, std::vector<bool>(2, false));
    locations_to_microchips[FLOOR1][THULIUM] = true;
    locations_to_microchips[FLOOR1][PLUTONIUM] = true;
    locations_to_generators[FLOOR2][THULIUM] = true;
    locations_to_generators[FLOOR3][PLUTONIUM] = true;
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    assert(11 == explore(facilityStatus));
    oldStatuses.clear();
}

void part1() {
    oldStatuses.clear();
    std::vector<std::vector<bool>>
        locations_to_microchips(5, std::vector<bool>(5, false));
    std::vector<std::vector<bool>>
        locations_to_generators(5, std::vector<bool>(5, false));
    /* Input
       =====
       - The first floor contains a thulium generator, a thulium-compatible
       microchip, a plutonium generator, and a strontium generator.
       - The second floor contains a plutonium-compatible microchip and a
       strontium-compatible microchip.
       - The third floor contains a promethium generator, a promethium-compatible
       microchip, a ruthenium generator, and a ruthenium-compatible microchip.
       - The fourth floor contains nothing relevant.
    */
    locations_to_microchips[FLOOR1][THULIUM] = true;
    locations_to_microchips[FLOOR2][PLUTONIUM] = true;
    locations_to_microchips[FLOOR2][STRONTIUM] = true;
    locations_to_microchips[FLOOR3][PROMETHIUM] = true;
    locations_to_microchips[FLOOR3][RUTHENIUM] = true;
    locations_to_generators[FLOOR1][THULIUM] = true;
    locations_to_generators[FLOOR1][PLUTONIUM] = true;
    locations_to_generators[FLOOR1][STRONTIUM] = true;
    locations_to_generators[FLOOR3][PROMETHIUM] = true;
    locations_to_generators[FLOOR3][RUTHENIUM] = true;
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    const auto result = explore(facilityStatus);
    assert(31 == result);
    std::cout << "Part 1: " << result << std::endl;
    oldStatuses.clear();
}

void part2() {
    oldStatuses.clear();
    std::vector<std::vector<bool>>
        locations_to_microchips(5, std::vector<bool>(7, false));
    std::vector<std::vector<bool>>
        locations_to_generators(5, std::vector<bool>(7, false));
    locations_to_microchips[FLOOR1][THULIUM] = true;
    locations_to_microchips[FLOOR2][PLUTONIUM] = true;
    locations_to_microchips[FLOOR2][STRONTIUM] = true;
    locations_to_microchips[FLOOR3][PROMETHIUM] = true;
    locations_to_microchips[FLOOR3][RUTHENIUM] = true;
    locations_to_generators[FLOOR1][THULIUM] = true;
    locations_to_generators[FLOOR1][PLUTONIUM] = true;
    locations_to_generators[FLOOR1][STRONTIUM] = true;
    locations_to_generators[FLOOR3][PROMETHIUM] = true;
    locations_to_generators[FLOOR3][RUTHENIUM] = true;
    locations_to_microchips[FLOOR1][ELERIUM] = true;
    locations_to_microchips[FLOOR1][DILITHIUM] = true;
    locations_to_generators[FLOOR1][ELERIUM] = true;
    locations_to_generators[FLOOR1][DILITHIUM] = true;
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    const auto result = explore(facilityStatus);
    assert(55 == result);
    std::cout << "Part 2: " << result << std::endl;
    oldStatuses.clear();
}

int main() {
    test();
    example();
    part1();
    part2();
}
