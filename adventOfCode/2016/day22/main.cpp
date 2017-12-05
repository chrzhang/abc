#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <regex>
#include <string>
#include <cassert>

/*
From http://adventofcode.com/2016/day/22

--- Day 22: Grid Computing ---

You gain access to a massive storage cluster arranged in a grid; each storage
node is only connected to the four nodes directly adjacent to it (three if the
node is on an edge, two if it's in a corner).

You can directly access data only on node /dev/grid/node-x0-y0, but you can
perform some limited actions on the other nodes:

You can get the disk usage of all nodes (via df). The result of doing this is
in your puzzle input.  You can instruct a node to move (not copy) all of its
data to an adjacent node (if the destination node has enough space to receive
the data). The sending node is left empty after this operation.  Nodes are
named by their position: the node named node-x10-y10 is adjacent to nodes
node-x9-y10, node-x11-y10, node-x10-y9, and node-x10-y11.

Before you begin, you need to understand the arrangement of data on these
nodes. Even though you can only move data between directly connected nodes,
you're going to need to rearrange a lot of the data to get access to the data
you need. Therefore, you need to work out how you might be able to shift data
around.

To do this, you'd like to count the number of viable pairs of nodes. A viable
pair is any two nodes (A,B), regardless of whether they are directly connected,
such that:

Node A is not empty (its Used is not zero).
Nodes A and B are not the same node.
The data on node A (its Used) would fit on node B (its Avail).
How many viable pairs of nodes are there?

--- Part Two ---

Now that you have a better understanding of the grid, it's time to get to work.

Your goal is to gain access to the data which begins in the node with y=0 and
the highest x (that is, the node in the top-right corner).

For example, suppose you have the following grid:

Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%

In this example, you have a storage grid 3 nodes wide and 3 nodes tall. The
node you can access directly, node-x0-y0, is almost full. The node containing
the data you want to access, node-x2-y0 (because it has y=0 and the highest x
value), contains 6 terabytes of data - enough to fit on your node, if only you
could make enough space to move it there.

Fortunately, node-x1-y1 looks like it has enough free space to enable you to
move some of this data around. In fact, it seems like all of the nodes have
enough space to hold any node's data (except node-x0-y2, which is much larger,
very full, and not moving any time soon). So, initially, the grid's capacities
and connections look like this:

( 8T/10T) --  7T/ 9T -- [ 6T/10T]
    |           |           |
  6T/11T  --  0T/ 8T --   8T/ 9T
    |           |           |
 28T/32T  --  7T/11T --   6T/ 9T

The node you can access directly is in parentheses; the data you want starts in
the node marked by square brackets.

In this example, most of the nodes are interchangable: they're full enough that
no other node's data would fit, but small enough that their data could be moved
around. Let's draw these nodes as .. The exceptions are the empty node, which
we'll draw as _, and the very large, very full node, which we'll draw as #.
Let's also draw the goal data as G. Then, it looks like this:

(.) .  G
 .  _  .
 #  .  .

The goal is to move the data in the top right, G, to the node in parentheses.
To do this, we can issue some commands to the grid and rearrange the data:

Move data from node-y0-x1 to node-y1-x1, leaving node node-y0-x1 empty:

(.) _  G
 .  .  .
 #  .  .

Move the goal data from node-y0-x2 to node-y0-x1:

(.) G  _
 .  .  .
 #  .  .

At this point, we're quite close. However, we have no deletion command, so we
have to move some more data around. So, next, we move the data from node-y1-x2
to node-y0-x2:

(.) G  .
 .  .  _
 #  .  .

Move the data from node-y1-x1 to node-y1-x2:

(.) G  .
 .  _  .
 #  .  .

Move the data from node-y1-x0 to node-y1-x1:

(.) G  .
 _  .  .
 #  .  .

Next, we can free up space on our node by moving the data from node-y0-x0 to
node-y1-x0:

(_) G  .
 .  .  .
 #  .  .

Finally, we can access the goal data by moving the it from node-y0-x1 to
node-y0-x0:

(G) _  .
 .  .  .
 #  .  .

So, after 7 steps, we've accessed the data we want. Unfortunately, each of
these moves takes time, and we need to be efficient:

What is the fewest number of steps required to move your goal data to
node-x0-y0?
*/

int toInt(const std::string & someNumber) {
    int result = 0;
    std::stringstream ss(someNumber);
    if (!(ss >> result)) {
        std::cerr << "Cannot convert " << someNumber << " to int\n";
        abort();
    }
    return result;
}

struct Node {

    int d_size;
    int d_usedSize;
    int d_available;
    int d_usePercentage;

    Node(const int sizeOfNode, const int usedSize, const int availableSize,
         const int usePercentage)
    : d_size(sizeOfNode), d_usedSize(usedSize), d_available(availableSize),
      d_usePercentage(usePercentage)
    {}

};

bool isViable(const Node & node1, const Node & node2) {
    if (&node1 == &node2) {
        std::cerr << "A node cannot be viable with itself." << std::endl;
        abort();
    }
    return (node1.d_usedSize != 0 && node1.d_usedSize <= node2.d_available) ||
           (node2.d_usedSize != 0 && node2.d_usedSize <= node1.d_available);
}

size_t getGridCount(const std::vector<std::vector<Node>> & grid) {
    size_t counter = 0;
    for (const auto & row : grid) {
        counter += row.size();
    }
    return counter;
}

int part1(const std::vector<std::vector<Node>> & grid) {
    int viableCounter = 0;
    // Consider all pairings of nodes
    const size_t totalNodeCount = getGridCount(grid);
    const size_t nodesPerRow = grid[0].size();
    std::vector<bool> chosen(totalNodeCount, false);
    chosen[totalNodeCount - 1] = chosen[totalNodeCount - 2] = true;
    do {
        int indexOfNodeA = -1;
        int indexOfNodeB = -1;
        for (size_t ii = 0; ii < chosen.size(); ++ii) {
            if (chosen[ii]) {
                if (indexOfNodeA == -1) {
                    indexOfNodeA = ii;
                } else if (indexOfNodeB == -1) {
                    indexOfNodeB = ii;
                    break;
                }
            }
        }
        assert(indexOfNodeA != -1 && indexOfNodeB != -1
               && indexOfNodeA != indexOfNodeB);
        const int nodeARow = indexOfNodeA / nodesPerRow;
        const int nodeACol = indexOfNodeA  % nodesPerRow;
        const int nodeBRow = indexOfNodeB / nodesPerRow;
        const int nodeBCol = indexOfNodeB  % nodesPerRow;
        if (isViable(grid[nodeARow][nodeACol],
                     grid[nodeBRow][nodeBCol])) {
            ++viableCounter;
        }
    } while (std::next_permutation(chosen.begin(), chosen.end()));
    return viableCounter;
}

int part2(const std::vector<std::vector<Node>> & grid) {
    std::cout << "Legend:\n\t_ - hole\n\tG - goal\n\t# - wall\n"
              << "This problem is trivially solved by hand.\n"
              << "Move the hole above the goal data, avoiding wall nodes.\n"
              << "\tMove hole up 11.\n"
              << "\tMove hole left 32.\n"
              << "\tMove hole down 31.\n"
              << "Perform the following 5 step sequence 36 times.\n"
              << "\tSwap hole with goal node underneath.\n"
              << "\tMove hole right, up, up, left.\n"
              << "The hole is in the destination, with the goal underneath.\n"
              << "Swap the hole and the goal.\n"
              << "Total steps: 11 + 23 + 31 + 5 * 36 + 1 steps.\n";
    std::cout << "      ";
    for (size_t i = 0; i < grid[0].size(); ++i) {
        std::cout << std::setw(4) << i;
    }
    std::cout << std::endl << std::string(grid[0].size() * 4 + 6, '_') << std::endl;
    for (size_t rowIndex = 0; rowIndex < grid.size(); ++rowIndex) {
        std::cout << std::setw(4) << rowIndex << "| ";
        for (size_t colIndex = 0; colIndex < grid[rowIndex].size(); ++colIndex) {
            const auto & node = grid[rowIndex][colIndex];
            std::string node_marker = std::to_string(node.d_usePercentage);
            if (node.d_usePercentage == 0) {
                node_marker = "_"; // Node is empty
            }
            if (node.d_size > 500) {
                node_marker = "#"; // Node is very large, very full
            }
            if (colIndex == 0 && rowIndex == grid.size() - 1) {
                node_marker = "G"; // Goal node (y=0, highest x)
            }
            std::cout << std::setw(4) << node_marker;
        }
        std::cout << std::endl;
    }
    return 11 + 23 + 31 + 5 * 36 + 1;
}

int main(int argc, char * argv[]) {
    if (2 != argc) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream inputFile(argv[1]);
    if (!inputFile.is_open()) {
        std::cerr << "Cannot open " << argv[1] << std::endl;
        return 1;
    }
    std::string currentLine;
    std::getline(inputFile, currentLine);
    assert(currentLine == "root@ebhq-gridcenter# df -h");
    std::getline(inputFile, currentLine);
    assert(currentLine == "Filesystem              Size  Used  Avail  Use%");
    std::regex lineRegex("^/dev/grid/node\\-x([0-9]+)\\-y([0-9]+)[ ]+([0-9]+)T[ ]+([0-9]+)T[ ]+([0-9]+)T[ ]+([0-9]+)%$");
    std::vector<std::vector<Node>> grid;
    while (std::getline(inputFile, currentLine)) {
        std::smatch result;
        if (!std::regex_search(currentLine, result, lineRegex)) {
            std::cerr << "Line is malformed: " << currentLine << std::endl;
            abort();
        }
        assert(result.size() == 7);
        const int & xNumber = toInt(result[1]);
        if (xNumber >= (int) grid.size()) {
            grid.push_back({});
        }
        assert(xNumber < (int) grid.size());
        const int & yNumber = toInt(result[2]);
        assert(yNumber == (int) grid[xNumber].size());
        const int & sizeOfNode = toInt(result[3]);
        const int & usedSizeOfNode = toInt(result[4]);
        const int & availableSizeOfNode = toInt(result[5]);
        const int & usePercentageOfNode = toInt(result[6]);
        grid[xNumber].push_back(Node(sizeOfNode, usedSizeOfNode, availableSizeOfNode, usePercentageOfNode));
    }
    const auto & part1Result = part1(grid);
    assert(955 == part1Result);
    std::cout << "Part 1: " << part1Result << std::endl;
    const auto & part2Result = part2(grid);
    assert(246 == part2Result);
    std::cout << "Part 2: " << part2Result << std::endl;
    return 0;
}
