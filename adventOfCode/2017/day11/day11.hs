import Control.Exception

{-
From http://adventofcode.com/2017/day/11

--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream when a
program comes up to you, clearly in distress. "It's my child process," she
says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be
found to the north, northeast, southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \

You have the path the child process took. Starting where he started, you need
to determine the fewest number of steps required to reach him. (A "step" means
to move from the hex you are in to any adjacent hex.)

For example:

ne,ne,ne is 3 steps away.
ne,ne,sw,sw is 0 steps away (back where you started).
ne,ne,s,s is 2 steps away (se,se).
se,sw,se,sw,sw is 3 steps away (s,s,sw).

--- Part Two ---

How many steps away is the furthest he ever got from his starting position?
-}

-- y axis runs NW (+) and SE (-) from the origin
-- x axis runs NE (+) and SW (-) from the origin
move :: String -> (Int, Int) -> (Int, Int)
move "n" (x, y) = (x + 1, y + 1)
move "ne" (x, y) = (x + 1, y)
move "nw" (x, y) = (x, y + 1)
move "s" (x, y) = (x - 1, y - 1)
move "se" (x, y) = (x, y - 1)
move "sw" (x, y) = (x - 1, y)
move _ _ = error "Unknown direction."

movepath :: [String] -> (Int, Int)
movepath = foldl (flip move) (0, 0)

getdist :: Int -> Int -> Int
getdist dstx dsty
    -- Can use North or South to move (1, 1) or (-1, 1) in one step
    -- e.g. If the point is at (-2, -5), moving s twice will go to (-2, -2)
    -- which minimizes steps taken
    | (dstx < 0 && dsty < 0) || (dstx > 0 && dsty > 0) = mxadst
    | otherwise = adstx + adsty
    where (adstx, adsty) = (abs dstx, abs dsty)
          mxadst = max adstx adsty

day11a_solve :: [String] -> Int
day11a_solve ds = getdist dstx dsty
                  where (dstx, dsty) = movepath ds

entirepath :: [String] -> [(Int, Int)]
entirepath = scanl (flip move) (0, 0)

day11b_solve :: [String] -> Int
day11b_solve ds = maximum ed
                  where ep = entirepath ds
                        ed = map (uncurry getdist) ep

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let dirs = words $ map (\x -> if x == ',' then ' ' else x) $ head $ lines contents
    let day11a_result = day11a_solve dirs
    let day11b_result = day11b_solve dirs
    putStrLn (unwords [ assert (3 == day11a_solve ["ne", "ne", "ne"]) "+",
                        assert (0 == day11a_solve ["ne", "ne", "sw", "sw"]) "+",
                        assert (2 == day11a_solve ["ne", "ne", "s", "s"]) "+",
                        assert (3 == day11a_solve ["se", "sw", "se", "sw", "sw"]) "+",
                        assert (794 == day11a_result) "+",
                        assert (1524 == day11b_result) "+" ])
    putStrLn $ "Part 1: " ++ show day11a_result
    putStrLn $ "Part 2: " ++ show day11b_result
