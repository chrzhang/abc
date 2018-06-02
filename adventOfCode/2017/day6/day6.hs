import Data.List
import Control.Exception
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

{-
From http://adventofcode.com/2017/day/6

--- Day 6: Memory Reallocation ---

A debugger program here is having an issue: it is trying to repair a memory
reallocation routine, but it keeps getting stuck in an infinite loop.

In this area, there are sixteen memory banks; each memory bank can hold any
number of blocks. The goal of the reallocation routine is to balance the blocks
between the memory banks.

The reallocation routine operates in cycles. In each cycle, it finds the memory
bank with the most blocks (ties won by the lowest-numbered memory bank) and
redistributes those blocks among the banks. To do this, it removes all of the
blocks from the selected bank, then moves to the next (by index) memory bank and
inserts one of the blocks. It continues doing this until it runs out of blocks;
if it reaches the last memory bank, it wraps around to the first one.

The debugger would like to know how many redistributions can be done before a
blocks-in-banks configuration is produced that has been seen before.

For example, imagine a scenario with only four memory banks:

The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks,
so it is chosen for redistribution.
Starting with the next bank (the fourth bank) and then continuing to the first
bank, the second bank, and so on, the 7 blocks are spread out over the memory
banks. The fourth, first, and second banks get two blocks each, and the third
bank gets one back. The final result looks like this: 2 4 1 2.
Next, the second bank is chosen because it contains the most blocks (four).
Because there are four memory banks, each gets one block.
The result is: 3 1 2 3.
Now, there is a tie between the first and fourth memory banks, both of which
have three blocks. The first bank wins the tie, and its three blocks are
distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
The fourth bank is chosen, and its four blocks are distributed such that each of
the four banks receives one: 1 3 4 1.
The third bank is chosen, and the same thing happens: 2 4 1 2.
At this point, we've reached a state we've seen before: 2 4 1 2 was already
seen. The infinite loop is detected after the fifth block redistribution cycle,
and so the answer in this example is 5.

Given the initial block counts in your puzzle input, how many redistribution
cycles must be completed before a configuration is produced that has been seen
before?

--- Part Two ---

Out of curiosity, the debugger would also like to know the size of the loop:
starting from a state that has already been seen, how many block redistribution
cycles must be performed before that same state is seen again?

In the example above, 2 4 1 2 is seen again after four cycles, and so the answer
in that example would be 4.

How many cycles are in the infinite loop that arises from the configuration in
your puzzle input?
-}

dup :: Ord a => [a] -> Maybe Int
dup xs = dup' 0 xs Set.empty
  where dup' _ [] _ = Nothing
        dup' i (y:ys) s = if Set.member y s
                          then Just i
                          else dup' (i + 1) ys (Set.insert y s)

day6a_solve :: Ord a => [a] -> Int
day6a_solve xs = fromMaybe (error "Failure.") (dup xs)

next :: [Int] -> [Int]
next s = map (\(x, xi) -> x + dlt xi) (zip s [0..])
          where (m, mi) = maximumBy cmp (zip s [0..])
                cmp (a, ai) (b, bi) = if a == b then compare bi ai
                                      else compare a b
                pds = (mi, -m) : [(j `mod` length s, 1) | j <- [mi + 1..mi + m]]
                occ e = filter (\(i, _) -> i == e) pds
                dlt e = foldr (\x y -> snd x + y) 0 (occ e)

states :: [Int] -> [[Int]]
states = iterate next

indata :: [Int]
indata = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]

dupi :: [Int] -> Int
dupi ds = fst ins
          where ins = until (\(_, s) -> s == ds) (\(i, s) -> (i + 1, next s)) (0, indata)

day6b_solve :: [Int] -> Int -> Int
day6b_solve ds ar = ar - dupi ds

main :: IO ()
main = do
    let day6a_result = day6a_solve (states indata)
    let dupst = states indata !! day6a_result
    let day6b_result = day6b_solve dupst day6a_result
    putStrLn (unwords [ assert (6681 == day6a_result) "+",
                        assert (2392 == day6b_result) "+"
                      ])
    putStrLn $ "Part 1: " ++ show day6a_result
    putStrLn $ "Part 2: " ++ show day6b_result
