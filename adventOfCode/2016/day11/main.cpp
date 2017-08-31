#include <iostream>
#include <string>
#include <sstream>
#include <cassert>
#include <map>
#include <set>
#include <algorithm>

enum Location {
    ELEVATOR,
    FLOOR1,
    FLOOR2,
    FLOOR3,
    FLOOR4
};

static const auto & floors = { FLOOR1, FLOOR2, FLOOR3, FLOOR4, ELEVATOR };

static const std::map<Location, std::string> & floorNames =
    { { FLOOR1, "Floor 1" },
      { FLOOR2, "Floor 2" },
      { FLOOR3, "Floor 3" },
      { FLOOR4, "Floor 4" },
      { ELEVATOR, "Elevator" } };

enum Element {
    THULIUM,
    PLUTONIUM,
    STRONTIUM,
    PROMETHIUM,
    RUTHENIUM,
    ELERIUM,
    DILITHIUM
};

static const int ELEVATOR_CAPACITY = 2;

template <typename T>
bool contains(const std::set<T> someSet, const T someElement) {
    return someSet.find(someElement) != someSet.end();
}

std::string toString(const Element e) {
    switch (e) {
        case THULIUM:
            return "thulium";
        case PLUTONIUM:
            return "plutonium";
        case STRONTIUM:
            return "strontium";
        case PROMETHIUM:
            return "promethium";
        case RUTHENIUM:
            return "ruthenium";
        default:
            return "unknown";
    }
}

struct Subset {
    std::set<Element> chips;
    std::set<Element> generators;
};

std::vector<std::set<Element>>
getAllSubsetsOfSize(const size_t amount,
                    const std::set<Element> & items) {
    if (amount == 0 || amount > items.size()) {
        return {};
    }
    if (amount == items.size()) {
        return { std::set<Element>(items.begin(), items.end()) };
    }
    std::vector<bool> chosen(items.size(), false);
    for (size_t i = 0; i < amount; ++i) {
        chosen[chosen.size() - 1 - i] = true;
    }
    std::vector<std::set<Element>> result;
    do {
        std::set<Element> subset;
        for (size_t choiceIndex = 0; choiceIndex < chosen.size(); ++choiceIndex) {
            if (chosen[choiceIndex]) {
                subset.insert(*std::next(items.begin(), choiceIndex));
            }
        }
        assert(subset.size() == amount);
        result.push_back(subset);
    } while (std::next_permutation(chosen.begin(), chosen.end()));
    return result;
}


class FacilityStatus {

    Location elevatorFloor;
    std::map<Location, std::set<Element>> locations_to_microchips;
    std::map<Location, std::set<Element>> locations_to_generators;

    bool chipsDecayFromGenerators(const std::set<Element> & chips,
                                  const std::set<Element> & generators) const {
        if (chips.empty() || generators.empty()) {
            return false;
        }
        for (const auto & chip : chips) {
            if (!contains(generators, chip)) {
                return true; // Could not find shield, thus decaying
            }
        }
        return false; // Everything was shielded
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
            std::string result(1 + 2 * numberOfChipGeneratorPairs(), ' ');
            for (const auto & floor : floors) {
                for (const auto & chip : locations_to_microchips.at(floor)) {
                    if (chip == THULIUM) {
                        result[0] = '0' + (char) floor;
                    } else if (chip == PLUTONIUM) {
                        result[2] = '0' + (char) floor;
                    } else if (chip == STRONTIUM) {
                        result[4] = '0' + (char) floor;
                    } else if (chip == PROMETHIUM) {
                        result[6] = '0' + (char) floor;
                    } else if (chip == RUTHENIUM) {
                        result[8] = '0' + (char) floor;
                    } else if (chip == ELERIUM) {
                        result[10] = '0' + (char) floor;
                    } else if (chip == DILITHIUM) {
                        result[12] = '0' + (char) floor;
                    } else {
                        assert(0);
                    }
                }
                for (const auto & generator : locations_to_generators.at(floor)) {
                    if (generator == THULIUM) {
                        result[1] = '0' + (char) floor;
                    } else if (generator == PLUTONIUM) {
                        result[3] = '0' + (char) floor;
                    } else if (generator == STRONTIUM) {
                        result[5] = '0' + (char) floor;
                    } else if (generator == PROMETHIUM) {
                        result[7] = '0' + (char) floor;
                    } else if (generator == RUTHENIUM) {
                        result[9] = '0' + (char) floor;
                    } else if (generator == ELERIUM) {
                        result[11] = '0' + (char) floor;
                    } else if (generator == DILITHIUM) {
                        result[13] = '0' + (char) floor;
                    } else {
                        assert(0);
                    }
                }
            }
            std::vector<std::string> pairs(numberOfChipGeneratorPairs(), "  ");
            for (size_t pairsIndex = 0;
                 pairsIndex < numberOfChipGeneratorPairs(); ++pairsIndex) {
                pairs[pairsIndex][0] = result[pairsIndex * 2];
                pairs[pairsIndex][1] = result[pairsIndex * 2 + 1];
            }
            std::sort(pairs.begin(), pairs.end());
            for (size_t index = 0;
                 index < 2 * numberOfChipGeneratorPairs(); ++index) {
                result[index] = pairs[index / 2][index % 2];
            }
            result[2 * numberOfChipGeneratorPairs()] =
                '0' + (char) elevatorFloor;
            return result;
        }

        bool isSafeConfiguration() const {
            // Check if every chip is safe
            for (const auto & floor : floors) {
                std::set<Element> allGenerators;
                std::set<Element> allChips;
                if (elevatorFloor == floor) {
                    const auto & elevatorChips =
                        locations_to_microchips.at(ELEVATOR);
                    allChips.insert(elevatorChips.begin(),
                                    elevatorChips.end());
                    const auto & elevatorGenerators =
                        locations_to_generators.at(ELEVATOR);
                    allGenerators.insert(elevatorGenerators.begin(),
                                         elevatorGenerators.end());
                }
                const auto & floorChips = locations_to_microchips.at(floor);
                allChips.insert(floorChips.begin(),
                                floorChips.end());
                const auto & floorGenerators = locations_to_generators.at(floor);
                allGenerators.insert(floorGenerators.begin(),
                                     floorGenerators.end());
                for (const auto & chip : allChips) {
                    bool isShielded = false;
                    if (contains(allGenerators, chip)) {
                        isShielded = true;
                    }
                    if (!isShielded && !allGenerators.empty()) {
                        return false;
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
                   numberOfChipsIn(FLOOR4) == numberOfChipGeneratorPairs());
            assert(numberOfGeneratorsIn(ELEVATOR) +
                   numberOfGeneratorsIn(FLOOR1) +
                   numberOfGeneratorsIn(FLOOR2) +
                   numberOfGeneratorsIn(FLOOR3) +
                   numberOfGeneratorsIn(FLOOR4) == numberOfChipGeneratorPairs());
            return numberOfItemsIn(FLOOR4) == 2 * numberOfChipGeneratorPairs();
        }

        FacilityStatus(const std::map<Location, std::set<Element>> & chips,
                       const std::map<Location, std::set<Element>> & generators) {
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
            return locations_to_microchips.at(location).size();
        }

        size_t numberOfGeneratorsIn(const Location location) const {
            return locations_to_generators.at(location).size();
        }

        size_t numberOfItemsIn(const Location location) const {
            return numberOfChipsIn(location) + numberOfGeneratorsIn(location);
        }

        size_t numberOfChipGeneratorPairs() const {
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
                emptySubset.chips = {};
                emptySubset.generators = {};
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
                const auto & chipSubsets =
                    getAllSubsetsOfSize(amountChips,
                                        locations_to_microchips.at(location));
                const auto & generatorSubsets =
                    getAllSubsetsOfSize(amountGenerators,
                                        locations_to_generators.at(location));
                for (const auto & chipSubset : chipSubsets) {
                    assert(chipSubset.size() == amountChips);
                    for (const auto & generatorSubset : generatorSubsets) {
                        assert(generatorSubset.size() == amountGenerators);
                        Subset itemSubset;
                        itemSubset.chips = chipSubset;
                        itemSubset.generators = generatorSubset;
                        result.push_back(itemSubset);
                    }
                }
                if (chipSubsets.size() == 0) {
                    for (const auto & generatorSubset : generatorSubsets) {
                        assert(generatorSubset.size() == amount);
                        Subset itemSubset;
                        itemSubset.chips = {};
                        itemSubset.generators = generatorSubset;
                        result.push_back(itemSubset);
                    }
                }
                if (generatorSubsets.size() == 0) {
                    for (const auto & chipSubset : chipSubsets) {
                        assert(chipSubset.size() == amount);
                        Subset itemSubset;
                        itemSubset.chips = chipSubset;
                        itemSubset.generators = {};
                        result.push_back(itemSubset);
                    }
                }
            }
            return result;
        }

        bool wouldDecayIfFloorLost(const std::set<Element> & chips,
                                   const std::set<Element> & generators) const {
            for (const auto & chipToConsiderRemoving : chips) {
                assert(contains(locations_to_microchips.at(elevatorFloor),
                                chipToConsiderRemoving));
            }
            for (const auto & generatorToConsiderRemoving : generators) {
                assert(contains(locations_to_generators.at(elevatorFloor),
                                generatorToConsiderRemoving));
                for (const auto & chip : locations_to_microchips.at(elevatorFloor)) {
                    if (chip == generatorToConsiderRemoving) {
                        if (!contains(chips, chip)) { // Chip isn't leaving
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        void loadIntoElevator(const std::set<Element> & chips,
                              const std::set<Element> & generators) {
            assert(chips.size() + generators.size() <= ELEVATOR_CAPACITY);
            for (const auto & chip : chips) {
                assert(contains(locations_to_microchips[elevatorFloor], chip));
                locations_to_microchips[elevatorFloor].erase(chip);
                locations_to_microchips[ELEVATOR].insert(chip);
            }
            for (const auto & generator : generators) {
                assert(contains(locations_to_generators[elevatorFloor], generator));
                locations_to_generators[elevatorFloor].erase(generator);
                locations_to_generators[ELEVATOR].insert(generator);
            }
            for (const auto & chip : chips) {
                assert(!contains(locations_to_microchips[elevatorFloor], chip));
                assert(contains(locations_to_microchips[ELEVATOR], chip));
            }
            for (const auto & generator : generators) {
                assert(!contains(locations_to_generators[elevatorFloor], generator));
                assert(contains(locations_to_generators[ELEVATOR], generator));
            }
        }

        void unloadFromElevator(const std::set<Element> & chips,
                                const std::set<Element> & generators) {
            assert(chips.size() + generators.size() <= ELEVATOR_CAPACITY);
            for (const auto & chip : chips) {
                assert(contains(locations_to_microchips[ELEVATOR], chip));
                locations_to_microchips[ELEVATOR].erase(chip);
                locations_to_microchips[elevatorFloor].insert(chip);
            }
            for (const auto & generator : generators) {
                assert(contains(locations_to_generators[ELEVATOR], generator));
                locations_to_generators[ELEVATOR].erase(generator);
                locations_to_generators[elevatorFloor].insert(generator);
            }
            for (const auto & chip : chips) {
                assert(!contains(locations_to_microchips[ELEVATOR], chip));
                assert(contains(locations_to_microchips[elevatorFloor], chip));
            }
            for (const auto & generator : generators) {
                assert(!contains(locations_to_generators[ELEVATOR], generator));
                assert(contains(locations_to_generators[elevatorFloor], generator));
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


};

bool addStatus(const FacilityStatus facilityStatus,
               std::vector<FacilityStatus> & nextStatuses) {
    static std::set<std::string> oldStatuses;
    const auto statusRepr = facilityStatus.representation();
    if (!contains(oldStatuses, statusRepr)) {
        nextStatuses.push_back(facilityStatus);
        oldStatuses.insert(statusRepr);
    }
    return false;
}

bool unloadElevator(const FacilityStatus facilityStatus,
                    const size_t amountOfThingsToUnloadFromElevator,
                    std::vector<FacilityStatus> & nextStatuses) {
    // Unload 0, 1, or 2 things from the elevator
    const auto & allSubsetsOfTargetSize =
        facilityStatus.getAllSubsetsOfSize_On_(amountOfThingsToUnloadFromElevator, ELEVATOR);
    for (const auto & chosenSubset : allSubsetsOfTargetSize) {
        const auto & chosenChips = chosenSubset.chips;
        const auto & chosenGenerators = chosenSubset.generators;
        auto unloadedFacilityStatus = facilityStatus;
        unloadedFacilityStatus.unloadFromElevator(chosenChips, chosenGenerators);
        if (addStatus(unloadedFacilityStatus, nextStatuses)) {
            return true;
        }
    }
    return false;
}

bool moveElevator(const FacilityStatus facilityStatus,
                  std::vector<FacilityStatus> & nextStatuses) {
    // Move the elevator up or down in a valid direction
    if (facilityStatus.getElevatorFloor() < FLOOR4 &&
        facilityStatus.numberOfItemsIn(ELEVATOR) > 0) { // Can go up
        auto upFacilityStatus = facilityStatus;
        upFacilityStatus.sendElevatorUp();
        if (upFacilityStatus.isSafeConfiguration()) {
            // Prune: If moving to new floor would cause decay, stop
            for (size_t amountOfThingsToUnloadFromElevator = 0;
                 amountOfThingsToUnloadFromElevator <= facilityStatus.numberOfItemsIn(ELEVATOR);
                 ++amountOfThingsToUnloadFromElevator) {
                if (unloadElevator(upFacilityStatus,
                                   amountOfThingsToUnloadFromElevator,
                                   nextStatuses)) {
                    return true;
                }
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
                 amountOfThingsToUnloadFromElevator <= facilityStatus.numberOfItemsIn(ELEVATOR);
                 ++amountOfThingsToUnloadFromElevator) {
                if (unloadElevator(downFacilityStatus,
                                   amountOfThingsToUnloadFromElevator,
                                   nextStatuses)) {
                    return true;
                }
            }
        }
    }
    return false;
}

bool loadElevator(const FacilityStatus facilityStatus,
                  const size_t amountOfThingsToLoadIntoElevator,
                  std::vector<FacilityStatus> & nextStatuses) {
    const auto & allSubsetsOfTargetSize =
        facilityStatus.getAllSubsetsOfSize_On_(amountOfThingsToLoadIntoElevator, facilityStatus.getElevatorFloor());
    for (const auto & chosenSubset : allSubsetsOfTargetSize) {
        const auto & chosenChips = chosenSubset.chips;
        const auto & chosenGenerators = chosenSubset.generators;
        if (facilityStatus.wouldDecayIfFloorLost(chosenChips, chosenGenerators)) {
            // Prune: Chips left on the floor must not decay (since the elevator is moving away)
            continue;
        }
        auto loadedFacilityStatus = facilityStatus;
        loadedFacilityStatus.loadIntoElevator(chosenChips, chosenGenerators);
        if (moveElevator(loadedFacilityStatus, nextStatuses)) {
            return true;
        }
    }
    return false;
}

int explore(const FacilityStatus facilityStatus) {
    std::vector<FacilityStatus> nextStatuses = { facilityStatus };
    for (int stepCounter = 0; !nextStatuses.empty(); ++stepCounter) {
        std::cout << "Step: " << stepCounter << ", considering " << nextStatuses.size() << " statuses.\n";
        std::vector<FacilityStatus> freshNextStatuses;
        for (const auto & currentStatus : nextStatuses) {
            if (currentStatus.isSolvedConfiguration()) {
                return stepCounter;
            }
            // Prune: Elevator and floor cannot have a chip that would decay
            if (!currentStatus.isSafeConfiguration()) {
                continue;
            }
            // Load 0, 1, or 2 things into the elevator (that was not unloaded because that would be redundant)
            for (size_t amountOfThingsToLoadIntoElevator = 0;
                (int) amountOfThingsToLoadIntoElevator <= std::max(0, ELEVATOR_CAPACITY - (int) currentStatus.numberOfItemsIn(ELEVATOR));
                ++amountOfThingsToLoadIntoElevator) {
                if (amountOfThingsToLoadIntoElevator == 0 && currentStatus.numberOfItemsIn(ELEVATOR) == 0) {
                    // Prune: Elevator must have at least 1 chip or generator
                    continue;
                }
                if (amountOfThingsToLoadIntoElevator > currentStatus.numberOfItemsIn(currentStatus.getElevatorFloor())) {
                    // Prune: Cannot load more items than there are on the floor
                    continue;
                }
                loadElevator(currentStatus, amountOfThingsToLoadIntoElevator, freshNextStatuses);
            }
        }
        nextStatuses = freshNextStatuses;
    }
    return -1;
}

void example() {
    std::cout << "> EXAMPLE\n";
    std::map<Location, std::set<Element>> locations_to_microchips =
        { { FLOOR1, {} },
          { FLOOR2, {} },
          { FLOOR3, {} },
          { FLOOR4, {} },
          { ELEVATOR, {} } };
    std::map<Location, std::set<Element>> locations_to_generators = locations_to_microchips;
    locations_to_microchips[FLOOR1].insert(THULIUM);
    locations_to_microchips[FLOOR1].insert(PLUTONIUM);
    locations_to_generators[FLOOR2].insert(THULIUM);
    locations_to_generators[FLOOR3].insert(PLUTONIUM);
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    assert(11 == explore(facilityStatus));
}

void part1() {
    std::cout << "> PART 1\n";
    std::map<Location, std::set<Element>> locations_to_microchips =
        { { FLOOR1, {} },
          { FLOOR2, {} },
          { FLOOR3, {} },
          { FLOOR4, {} },
          { ELEVATOR, {} } };
    std::map<Location, std::set<Element>> locations_to_generators = locations_to_microchips;
    locations_to_microchips[FLOOR1].insert(THULIUM);
    locations_to_microchips[FLOOR2].insert(PLUTONIUM);
    locations_to_microchips[FLOOR2].insert(STRONTIUM);
    locations_to_microchips[FLOOR3].insert(PROMETHIUM);
    locations_to_microchips[FLOOR3].insert(RUTHENIUM);
    locations_to_generators[FLOOR1].insert(THULIUM);
    locations_to_generators[FLOOR1].insert(PLUTONIUM);
    locations_to_generators[FLOOR1].insert(STRONTIUM);
    locations_to_generators[FLOOR3].insert(PROMETHIUM);
    locations_to_generators[FLOOR3].insert(RUTHENIUM);
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    assert(31 == explore(facilityStatus));
}

void part2() {
    std::cout << "> PART 2\n";
    std::map<Location, std::set<Element>> locations_to_microchips =
        { { FLOOR1, {} },
          { FLOOR2, {} },
          { FLOOR3, {} },
          { FLOOR4, {} },
          { ELEVATOR, {} } };
    std::map<Location, std::set<Element>> locations_to_generators = locations_to_microchips;
    locations_to_microchips[FLOOR1].insert(THULIUM);
    locations_to_microchips[FLOOR2].insert(PLUTONIUM);
    locations_to_microchips[FLOOR2].insert(STRONTIUM);
    locations_to_microchips[FLOOR3].insert(PROMETHIUM);
    locations_to_microchips[FLOOR3].insert(RUTHENIUM);
    locations_to_generators[FLOOR1].insert(THULIUM);
    locations_to_generators[FLOOR1].insert(PLUTONIUM);
    locations_to_generators[FLOOR1].insert(STRONTIUM);
    locations_to_generators[FLOOR3].insert(PROMETHIUM);
    locations_to_generators[FLOOR3].insert(RUTHENIUM);
    locations_to_microchips[FLOOR1].insert(ELERIUM);
    locations_to_microchips[FLOOR1].insert(DILITHIUM);
    locations_to_generators[FLOOR1].insert(ELERIUM);
    locations_to_generators[FLOOR1].insert(DILITHIUM);
    FacilityStatus facilityStatus(locations_to_microchips,
                                  locations_to_generators);
    std::cout <<  explore(facilityStatus) << std::endl;
}

int main() {
    example();
    part1();
//  part2();
}
