import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Control.Exception

{-
From http://adventofcode.com/2017/day/16

--- Day 16: Permutation Promenade ---
You come upon a very unusual sight; a group of programs here appear to be
dancing.

There are sixteen programs in total, named a through p. They start by standing
in a line: a stands in position 0, b stands in position 1, and so on until p,
which stands in position 15.

The programs' dance consists of a sequence of dance moves:

Spin, written sX, makes X programs move from the end to the front, but maintain
their order otherwise. (For example, s3 on abcde produces cdeab).
Exchange, written xA/B, makes the programs at positions A and B swap places.
Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do
the following dance:

s1, a spin of size 1: eabcd.
x3/4, swapping the last two programs: eabdc.
pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle
input). In what order are the programs standing after their dance?

--- Part Two ---
Now that you're starting to get a feel for the dance moves, you turn your
attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance, the programs
perform it again and again: including the first dance, a total of one billion
(1000000000) times.

In the example above, their second dance would begin with the order baedc, and
use the same dance moves:

s1, a spin of size 1: cbaed.
x3/4, swapping the last two programs: cbade.
pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?
-}

spin :: Int -> String -> String
spin n s
    | n > length s = error "n cannot be > length."
    | otherwise = drop r s ++ take r s
                  where r = length s - n

exchange :: Int -> Int -> String -> String
exchange i j s
    | i < 0 || i >= l || j < 0 || j >= l = error "Index out of bounds."
    | otherwise = left ++ [s !! m] ++ middle ++ [s !! n] ++ right
    where l = length s
          (n, m) = (min i j, max i j)
          left = take n s
          middle = take (m - n - 1) $ drop (n + 1) s
          right = drop (m + 1) s

findindex :: Char -> String -> Int
findindex c s = case elemIndex c s of Nothing -> error "Not in string."
                                      Just x -> x

partner :: Char -> Char -> String -> String
partner a b s = exchange (findindex a s) (findindex b s) s

tofunc :: String -> (String -> String)
tofunc s
    | h == 's' = spin (read t :: Int)
    | h == 'x' = exchange (read spt0 :: Int) (read spt1 :: Int)
    | h == 'p' = partner (spt0 !! 0) (spt1 !! 0)
    | otherwise = error "Unsupported dance move."
    where (h, t) = (head s, tail s)
          spt = splitOn "/" t
          spt0 = spt !! 0
          spt1 = spt !! 1

lineup :: String
lineup = ['a'..'p']

dance :: [(String -> String)] -> String -> String
dance m s = foldl (\x y -> y x) s m

day16a_solve :: [(String -> String)] -> String
day16a_solve moves = dance moves lineup

cycle_length :: Eq a => [a] -> Int
cycle_length [] = 0
cycle_length (x:xs) = length $ takeWhile (/=x) xs

day16b_solve :: [(String -> String)] -> String
day16b_solve moves = am !! (cl + (nm - 1))
                     where cl = 1 + cycle_length am
                           am = iterate (dance moves) lineup
                           b = 1000000000
                           nm = (b + 1) `mod` cl

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let moves = map tofunc readlines
                where readlines = splitOn "," $ lines contents !! 0
    let day16a_result = day16a_solve moves
    let day16b_result = day16b_solve moves
    putStrLn $ unwords [ assert ("kpfonjglcibaedhm" == day16a_result) "+",
                         assert ("odiabmplhfgjcekn" == day16b_result) "+" ]
    putStrLn $ "Part 1: " ++ show day16a_result
    putStrLn $ "Part 2: " ++ show day16b_result
