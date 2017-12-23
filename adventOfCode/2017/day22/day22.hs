import Data.List ((\\))
import qualified Data.Set as Set
import Control.Exception

{-
From http://adventofcode.com/2017/day/22

--- Day 22: Sporifica Virus ---
Diagnostics indicate that the local grid computing cluster has been
contaminated with the Sporifica Virus. The grid computing cluster is a
seemingly-infinite two-dimensional grid of compute nodes. Each node is either
clean or infected by the virus.

To prevent overloading the nodes (which would render them useless to the virus)
or detection by system administrators, exactly one virus carrier moves through
the network, infecting or cleaning nodes as it moves. The virus carrier is
always located on a single node in the network (the current node) and keeps
track of the direction it is facing.

To avoid detection, the virus carrier works in bursts; in each burst, it wakes
up, does some work, and goes back to sleep. The following steps are all
executed in order one time each burst:

If the current node is infected, it turns to its right. Otherwise, it turns to
its left. (Turning is done in-place; the current node does not change.)
If the current node is clean, it becomes infected. Otherwise, it becomes
cleaned. (This is done after the node is considered for the purposes of
changing direction.)
The virus carrier moves forward one node in the direction it is facing.

Diagnostics have also provided a map of the node infection status (your puzzle
input). Clean nodes are shown as .; infected nodes are shown as #. This map
only shows the center of the grid; there are many more nodes beyond those
shown, but none of them are currently infected.

The virus carrier begins in the middle of the map facing up.

For example, suppose you are given a map like this:

..#
#..
...

Then, the middle of the infinite grid looks like this, with the virus carrier's
position marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on a clean node, so it turns left, infects the node, and
moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]# . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on an infected node, so it turns right, cleans the node,
and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . . # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Four times in a row, the virus carrier finds a clean, infects it, turns left,
and moves forward, ending in the same place and still facing up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . #[#]. # . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Now on the same node as before, it sees an infection, which causes it to turn
right, clean the node, and move forward:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . # .[.]# . . .
. . # # # . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

After the above actions, a total of 7 bursts of activity had taken place. Of
them, 5 bursts of activity caused an infection.

After a total of 70, the grid looks like this, with the virus carrier facing
up:

. . . . . # # . .
. . . . # . . # .
. . . # . . . . #
. . # . #[.]. . #
. . # . # . . # .
. . . . . # # . .
. . . . . . . . .
. . . . . . . . .

By this time, 41 bursts of activity caused an infection (though most of those
nodes have since been cleaned).

After a total of 10000 bursts of activity, 5587 bursts will have caused an
infection.

Given your actual map, after 10000 bursts of activity, how many bursts cause a
node to become infected? (Do not count nodes that begin infected.)

--- Part Two ---
As you go to remove the virus from the infected nodes, it evolves to resist
your attempt.

Now, before it infects a clean node, it will weaken it to disable your
defenses. If it encounters an infected node, it will instead flag the node to
be cleaned in the future. So:

Clean nodes become weakened.
Weakened nodes become infected.
Infected nodes become flagged.
Flagged nodes become clean.
Every node is always in exactly one of the above states.

The virus carrier still functions in a similar way, but now uses the following
logic during its bursts of action:

Decide which way to turn based on the current node:
If it is clean, it turns left.
If it is weakened, it does not turn, and will continue moving in the same
direction.
If it is infected, it turns right.
If it is flagged, it reverses direction, and will go back the way it came.
Modify the state of the current node, as described above.
The virus carrier moves forward one node in the direction it is facing.
Start with the same map (still using . for clean and # for infected) and still
with the virus carrier starting in the middle and facing up.

Using the same initial state as the previous example, and drawing weakened as W
and flagged as F, the middle of the infinite grid looks like this, with the
virus carrier's position again marked with [ ]:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . #[.]. . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

This is the same as before, since no initial nodes are weakened or flagged. The
virus carrier is on a clean node, so it still turns left, instead weakens the
node, and moves left:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . .[#]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The virus carrier is on an infected node, so it still turns right, instead
flags the node, and moves up:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . .[.]. # . . .
. . . F W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

This process repeats three more times, ending on the previously-flagged node
and facing right:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. . W[F]W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Finding a flagged node, it reverses direction and cleans the node:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
. .[W]. W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

The weakened node becomes infected, and it continues in the same direction:

. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . W W . # . . .
.[.]# . W . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .

Of the first 100 bursts, 26 will result in infection. Unfortunately, another
feature of this evolved virus is speed; of the first 10000000 bursts, 2511944
will result in infection.

Given your actual map, after 10000000 bursts of activity, how many bursts cause
a node to become infected? (Do not count nodes that begin infected.)
-}

data Dir = N | W | E | S deriving (Show)
data State = State (Int, Int) Dir [(Int, Int)] Int deriving (Show)
type PosS = Set.Set (Int, Int)
data RState = RState (Int, Int) Dir PosS PosS PosS Int deriving (Show)

turnE, turnW, rev :: Dir -> Dir
turnE N = E
turnE E = S
turnE S = W
turnE W = N
turnW N = W
turnW E = N
turnW S = E
turnW W = S
rev N = S
rev S = N
rev E = W
rev W = E

advDir :: (Int, Int) -> Dir -> (Int, Int)
advDir (r, c) N = (r - 1, c)
advDir (r, c) S = (r + 1, c)
advDir (r, c) W = (r, c - 1)
advDir (r, c) E = (r, c + 1)

next :: State -> State
next (State pos dir infected ibursts)
    | pos `elem` infected = State (advDir pos rd) rd clean ibursts
    | otherwise = State (advDir pos ld) ld infect (1 + ibursts)
    where rd = turnE dir
          ld = turnW dir
          clean = infected \\ [pos]
          infect = pos : infected

day22a_solve :: [(Int, Int)] -> Int
day22a_solve i = c
                 where (State _ _ _ c) = iterate next (State (0, 0) N i 0) !! 10000

rnext :: RState -> RState
rnext (RState pos dir weakened infected flagged ibursts)
    | Set.member pos weakened = RState (advDir pos dir) dir unweak infect flagged (1 + ibursts)
    | Set.member pos infected = RState (advDir pos rd) rd weakened uninfect flag ibursts
    | Set.member pos flagged = RState (advDir pos fd) fd weakened infected unflag ibursts
    | otherwise = RState (advDir pos ld) ld weak infected flagged ibursts
    where rd = turnE dir
          ld = turnW dir
          fd = rev dir
          weak = Set.insert pos weakened
          infect = Set.insert pos infected
          flag = Set.insert pos flagged
          unweak = Set.delete pos weakened
          uninfect = Set.delete pos infected
          unflag = Set.delete pos flagged

day22b_solve :: [(Int, Int)] -> Int
day22b_solve i = c
                 where (RState _ _ _ _ _ c) = iterate rnext istate !! 10000000
                       istate = RState (0, 0) N emps (Set.fromList i) emps 0
                       emps = Set.fromList []

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let readLines = lines contents
    let (startr, startc) = ((length readLines - 1) `div` 2,
                            (length (head readLines) - 1) `div` 2)
    let infected = [(r, c) | r <- [(-startr)..startr], c <- [(-startc)..startc],
                             (readLines !! (r + startr)) !! (c + startc) /= '.']
    let day22a_result = day22a_solve infected
    let day22b_result = day22b_solve infected
    putStrLn $ unwords [ assert (5352 == day22a_result) "+",
                         assert (2511475 == day22b_result) "+" ]
    putStrLn $ "Part 1: " ++ show day22a_result
    putStrLn $ "Part 2: " ++ show day22b_result
