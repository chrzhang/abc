import Data.List.Split (chunksOf)
import Data.List (transpose)
import Control.Exception

{-
From http://adventofcode.com/2017/day/21

--- Day 21: Fractal Art ---
You find a program trying to generate some art. It uses a strange process that
involves repeatedly enhancing the detail of an image through a set of rules.

The image consists of a two-dimensional square grid of pixels that are either
on (#) or off (.). The program always begins with this pattern:

.#.
..#
###

Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have
a size of 3.

Then, the program repeats the following process:

If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and
convert each 2x2 square into a 3x3 square by following the corresponding
enhancement rule.  Otherwise, the size is evenly divisible by 3; break the
pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by
following the corresponding enhancement rule.  Because each square of pixels is
replaced by a larger one, the image gains pixels and so its size increases.

The artist's book of enhancement rules is nearby (your puzzle input); however,
it seems to be missing rules. The artist explains that sometimes, one must
rotate or flip the input pattern to find a match. (Never rotate or flip the
output pattern, though.) Each pattern is written concisely: rows are listed as
single units, ordered top-down, and separated by slashes. For example, the
following rules correspond to the adjacent patterns:

../.#  =  ..
          .#

                .#.
.#./..#/###  =  ..#
                ###

                        #..#
#..#/..../#..#/.##.  =  ....
                        #..#
                        .##.

When searching for a rule to use, rotate and flip the pattern as necessary. For
example, all of the following patterns match the same rule:

.#.   .#.   #..   ###
..#   #..   #.#   ..#
###   ###   ##.   .#.

Suppose the book contained the following two rules:

../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#

As before, the program begins with this pattern:

.#.
..#
###

The size of the grid (3) is not divisible by 2, but it is divisible by 3. It
divides evenly into a single square; the square matches the second rule, which
produces:

#..#
....
....
#..#

The size of this enhanced grid (4) is evenly divisible by 2, so that rule is
used. It divides evenly into four squares:

#.|.#
..|..
--+--
..|..
#.|.#

Each of these squares matches the same rule (../.# => ##./#../...), three of
which require some flipping and rotation to line up with the rule. The output
for the rule is the same in all four cases:

##.|##.
#..|#..
...|...
---+---
##.|##.
#..|#..
...|...

Finally, the squares are joined into a new grid:

##.##.
#..#..
......
##.##.
#..#..
......

Thus, after 2 iterations, the grid contains 12 pixels that are on.

How many pixels stay on after 5 iterations?

--- Part Two ---
How many pixels stay on after 18 iterations?
-}

type Grid = [String]
type Pos = (Int, Int)
type Rules = [(String, String)]

initGrid :: Grid
initGrid = [".#.",
            "..#",
            "###"]

sizeOf :: Grid -> Int
sizeOf = length

numOn :: Grid -> Int
numOn g = length $ filter (=='#') $ concat g

genNxN :: Int -> Pos -> [Pos]
genNxN n (r, c) = [(r + x, c + y) | x <- ds, y <- ds]
                  where ds = [0..n - 1]

breakNxN :: Int -> Int -> [[Pos]]
breakNxN n s = [genNxN n (r, c) | br <- bis, bc <- bis, let r = n * br, let c = n * bc]
               where bis = [0..s `div` n - 1]

gridChunks :: Grid -> [String]
gridChunks g
    | s `mod` 2 == 0 = map getVals (breakNxN 2 s)
    | s `mod` 3 == 0 = map getVals (breakNxN 3 s)
    | otherwise = error "Grid size is invalid."
    where s = sizeOf g
          getVals = map (\(r, c) -> (g !! r) !! c)

intSqrt :: Int -> Int
intSqrt x = floor . sqrt $ (fromIntegral x :: Double)

mergeChunks :: [String] -> Grid
mergeChunks [] = error "Cannot merge without chunks."
mergeChunks [c]
    | l == 3 * 3 = chunksOf (l `div` 3) c
    | l == 4 * 4 = chunksOf (l `div` 4) c
    | otherwise = error "Single chunk size not supported."
    where l = length c
mergeChunks cks@(c:_) = map concat rc
                        where rc = concat $ transpose $ map (chunksOf gs) ro
                              ro = transpose $ map (chunksOf cs) cks
                              cs = intSqrt $ length c
                              gs = intSqrt $ length cks

fliph2x2, fliph3x3 :: [a] -> [a]
fliph2x2 [tl, tr, bl, br] = [bl, br, tl, tr]
fliph2x2 _ = error "Wrong size to flip, not 2x2."
fliph3x3 [tl, tm, tr, ml, mm, mr, bl, bm, br] = [bl, bm, br, ml, mm, mr, tl, tm, tr]
fliph3x3 _ = error "Wrong size to flip, not 3x3."

rotn2x2, rotn3x3 :: Int -> [a] -> [a]
rotn2x2 n = rot2x2 n'
            where n' = n `mod` 4
                  rot2x2 0 x = x
                  rot2x2 1 [tl, tr, bl, br] = [bl, tl, br, tr]
                  rot2x2 2 [tl, tr, bl, br] = [br, bl, tr, tl]
                  rot2x2 3 [tl, tr, bl, br] = [tr, br, tl, bl]
                  rot2x2 _ _ = error "Wrong size to rotate, not 2x2."
rotn3x3 n = rot3x3 n'
            where n' = n `mod` 4
                  rot3x3 0 x = x
                  rot3x3 1 [tl, tm, tr, ml, mm, mr, bl, bm, br]
                    = [bl, ml, tl, bm, mm, tm, br, mr, tr]
                  rot3x3 2 [tl, tm, tr, ml, mm, mr, bl, bm, br]
                    = [br, bm, bl, mr, mm, ml, tr, tm, tl]
                  rot3x3 3 [tl, tm, tr, ml, mm, mr, bl, bm, br]
                    = [tr, mr, br, tm, mm, bm, tl, ml, bl]
                  rot3x3 _ _ = error "Wrong size to roate, not 3x3."

eqNxN, eq2x2, eq3x3 :: String -> String -> Bool
eqNxN m1 m2
    | lm1 /= lm2 = False
    | lm1 == 2 * 2 = eq2x2 m1 m2
    | lm1 == 3 * 3 = eq3x3 m1 m2
    | otherwise = error ("Cannot compare chunks of size: " ++ show lm1)
    where lm1 = length m1
          lm2 = length m2

eq2x2 m1 m2 = m1 `elem` transforms m2
    where transforms m = [rotn2x2 ramt tm | tm <- [m, fliph2x2 m],
                                            ramt <- [0..3]]
eq3x3 m1 m2 = m1 `elem` transforms m2
    where transforms m = [rotn3x3 ramt tm | tm <- [m, fliph3x3 m],
                                            ramt <- [0..3]]

trans :: [(String, String)] -> String -> String
trans table c = seek c table

seek :: String -> [(String, String)] -> String
seek _ [] = error "No rule for transformation."
seek n ((hk, hv):hs) = if eqNxN n hk then hv else seek n hs


next :: Rules -> Grid -> Grid
next r g = mergeChunks gks
           where gks = map (trans r) (gridChunks g)

states :: Rules -> [Grid]
states r = iterate (next r) initGrid

toRule :: [String] -> (String, String)
toRule ws = (rems from, rems to)
            where from = head ws
                  to = ws !! 2
                  rems = filter (/='/')

day21a_solve, day21b_solve :: Rules -> Int
day21a_solve r = numOn $ states r !! 5
day21b_solve r = numOn $ states r !! 18

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let wordLines = map words $ lines contents
    let rules = map toRule wordLines
    let day21a_result = day21a_solve rules
    let day21b_result = day21b_solve rules
    putStrLn $ unwords [ assert (day21a_result == 142) "+",
                         assert (day21b_result == 1879071) "+"
                       ]
    putStrLn $ "Part 1: " ++ show day21a_result
    putStrLn $ "Part 2: " ++ show day21b_result
