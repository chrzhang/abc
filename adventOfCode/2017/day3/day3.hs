import Control.Exception
import Data.Maybe
import Data.List

{-
From http://adventofcode.com/2017/day/3

--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite
two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location
marked 1 and then counting up while spiraling outward. For example, the first
few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data
must be carried back to square 1 (the location of the only access port for this
memory system) by programs that can only move up, down, left, or right. They
always take the shortest path: the Manhattan Distance between the location of
the data and square 1.

For example:

Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in
your puzzle input all the way to the access port?

--- Part Two ---

As a stress test on the system, the programs here clear the grid and then store
the value 1 in square 1. Then, in the same allocation order as shown above,
they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores
1.
Square 3 has both of the above squares as neighbors and stores the sum of their
values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores
the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the
value 5.
Once a square is written, its value does not change. Therefore, the first few
squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
-}

pos :: Int -> (Int, Int)
pos 1 = (0, 0)
pos 2 = (1, 0)
pos n
    | n < 1 = error "Input must be >= 1."
    | py > 0 && px <= py && px > (-py)  = left
    | py < 0 && px <= (-py) && px >= py = right
    | px > 0 && px > py                 = up
    | px < 0 && px < py                 = down
    | otherwise                         = error "Unsupported case."
    where p = pos (n - 1)
          px = fst p
          py = snd p
          left = (px - 1, py)
          right = (px + 1, py)
          down = (px, py - 1)
          up = (px, py + 1)

day3a_solve :: Int -> Int
day3a_solve n = abs (fst p) + abs (snd p)
                where p = pos n

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p = [(fst p + xd, snd p + yd) | xd <- [-1..1],
                                          yd <- [-1..1],
                                          xd /= 0 || yd /= 0]

spiral_values :: [Int]
spiral_values = 1 : [s | i <- [1..],
                         let currp = allp !! i,
                         let priorp = take i allp,
                         let nbrs = filter (\x -> elem x priorp) (neighbors currp),
                         let nbrinds = map (\n -> fromMaybe (error "Position must exist") (elemIndex n allp)) nbrs,
                         let s = sum (map (\ni -> spiral_values !! ni) nbrinds)
                    ]
    where allp = map pos [1..]

day3b_solve :: Int -> Int
day3b_solve l = head (filter (>l) spiral_values)

main :: IO ()
main = do
    let input = 368078
    putStrLn (unwords [ assert (day3a_solve input == 371) "+",
                        assert (day3b_solve input == 369601) "+"
                      ])
    putStrLn ("Part 1: " ++ show (day3a_solve input))
    putStrLn ("Part 2: " ++ show (day3b_solve input))
