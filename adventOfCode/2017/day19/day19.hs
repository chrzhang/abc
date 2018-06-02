import Control.Exception

{-
From http://adventofcode.com/2017/day/19

--- Day 19: A Series of Tubes ---
Somehow, a network packet got lost and ended up here. It's trying to follow a
routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -,
and +) show the path it needs to take, starting by going down onto the only
line connected to the top of the diagram. It needs to follow this path until it
reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to
continue going the same direction, and only turn left or right when there's no
other option. In addition, someone has left letters on the line; these also
don't change its direction, but it can use them to keep track of where it's
been. For example:

     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+

Given this diagram, the packet needs to take the following path:

Starting at the only line touching the top of the diagram, it must go down,
pass through A, and continue onward to the first +.
Travel right, up, and right, passing through B in the process.
Continue down (collecting C), right, and up (collecting D).
Finally, go all the way left through E and stopping at F.
Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What
letters will it see (in the order it would see them) if it follows the path?
(The routing diagram is very wide; make sure you view it without line
wrapping.)

--- Part Two ---
The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

     |
     |  +--+
     A  |  C
 F---|--|-E---+
     |  |  |  D
     +B-+  +--+

...the packet would go:

6 steps down (including the first line at the top of the diagram).
3 steps right.
4 steps up.
3 steps right.
4 steps down.
3 steps right.
2 steps up.
13 steps left (including the F it stops on).

This would result in a total of 38 steps.

How many steps does the packet need to go?
-}

type Pos = (Int, Int)
type Grid = [String]

data Dir = North | East | West | South deriving (Eq)

dirs :: [Dir]
dirs = [North, East, West, South]

data State = State Dir Pos String

orthodirs :: Dir -> [Dir]
orthodirs d
    | d == North || d == South = [East, West]
    | d == East || d == West = [North, South]
    | otherwise = error "Impossible direction."

validpos :: Pos -> Grid -> Bool
validpos p@(r, c) g = inbounds && valatpos p g /= ' '
                      where inbounds = r >= 0 && r < length g && c >= 0 && c < length (head g)

validst :: Grid -> State -> Bool
validst gd (State _ ps' _) = validpos ps' gd

posindir :: Pos -> Dir -> Pos
posindir (r, c) North = (r - 1, c)
posindir (r, c) South = (r + 1, c)
posindir (r, c) East = (r, c + 1)
posindir (r, c) West = (r, c - 1)

valatpos :: Pos -> Grid -> Char
valatpos (r, c) g = (g !! r) !! c

next :: Grid -> State -> State
next gd (State cd' ps' ls')
    | currv == '|' || currv == '-' = State cd' (posindir ps' cd') ls'
    | currv == '+' = if null vodnbrs then State cd' (-1, -1) ls'
                     else State turndir turnpos ls'
    | currv `elem` ['A'..'Z'] = State cd' (posindir ps' cd') (ls' ++ [currv])
    | otherwise = error ("Unsupported character: " ++ show currv)
    where nbrs = [(d, posindir ps' d) | d <- dirs]
          currv = valatpos ps' gd
          od = orthodirs cd'
          odnbrs = filter (\(d, _) -> elem d od) nbrs
          vodnbrs = filter (\(_, p) -> validpos p gd) odnbrs
          (turndir, turnpos) = head vodnbrs

findstart :: Grid -> Pos
findstart gd = (0, head $ filter (\c -> head gd !! c == '|') [0..width - 1])
               where width = length (head gd)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

day19a_solve :: Grid -> String
day19a_solve gd = getls $ last vstates
    where states = iterate (next gd) (State South (findstart gd) [])
          vstates = takeUntil (validst gd) states
          getls (State _ _ ls) = ls

day19b_solve :: Grid -> Int
day19b_solve gd = length $ tail vstates
    where states = iterate (next gd) (State South (findstart gd) [])
          vstates = takeUntil (validst gd) states

main :: IO ()
main = do
    sample_contents <- readFile "sample_input.txt"
    let sample_grid = lines sample_contents
    let samplea_result = day19a_solve sample_grid
    let sampleb_result = day19b_solve sample_grid
    contents <- readFile "input.txt"
    let grid = lines contents
    let day19a_result = day19a_solve grid
    let day19b_result = day19b_solve grid
    putStrLn $ unwords [ assert (samplea_result == "ABCDEF") "+",
                         assert (sampleb_result == 38) "+",
                         assert (day19a_result == "NDWHOYRUEA") "+",
                         assert (day19b_result == 17540) "+"
                       ]
    putStrLn $ "Part 1: " ++ day19a_result
    putStrLn $ "Part 2: " ++ show day19b_result
