import Data.List ((\\), maximumBy)
import Data.List.Split (splitOn)
import Control.Exception

{-
From http://adventofcode.com/2017/day/24

--- Day 24: Electromagnetic Moat ---
The CPU itself is a large, black building surrounded by a bottomless pit.
Enormous metal tubes extend outward from the side of the building at regular
intervals and descend down into the void. There's no way to cross, but you need
to get inside.

No way, of course, other than building a bridge out of the magnetic components
strewn about nearby.

Each component has two ports, one on each end. The ports come in all different
types, and only matching types can be connected. You take an inventory of the
components by their port types (your puzzle input). Each port is identified by
the number of pins it uses; more pins mean a stronger connection for your
bridge. A 3/7 component, for example, has a type-3 port on one side, and a
type-7 port on the other.

Your side of the pit is metallic; a perfect surface to connect a magnetic,
zero-pin port. Because of this, the first port you use must be of type 0. It
doesn't matter what type of port you end with; your goal is just to make the
bridge as strong as possible.

The strength of a bridge is the sum of the port types in each component. For
example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge
has a strength of 0+3 + 3+7 + 7+4 = 24.

For example, suppose you had the following components:

0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10

With them, you could make the following valid bridges:

0/1
0/1--10/1
0/1--10/1--9/10
0/2
0/2--2/3
0/2--2/3--3/4
0/2--2/3--3/5
0/2--2/2
0/2--2/2--2/3
0/2--2/2--2/3--3/4
0/2--2/2--2/3--3/5

(Note how, as shown by 10/1, order of ports within a component doesn't matter.
However, you may only use each port on a component once.)

Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of
0+1 + 1+10 + 10+9 = 31.

What is the strength of the strongest bridge you can make with the components
you have available?

--- Part Two ---
The bridge you've built isn't long enough; you can't jump the rest of the way.

In the example above, there are two longest bridges:

0/2--2/2--2/3--3/4
0/2--2/2--2/3--3/5

Of them, the one which uses the 3/5 component is stronger; its strength is 0+2
+ 2+2 + 2+3 + 3+5 = 19.

What is the strength of the longest bridge you can make? If you can make
multiple bridges of the longest length, pick the strongest one.
-}

type Comp = (Int, Int)

other :: Int -> Comp -> Int
other x (a, b)
    | x == a = b
    | x == b = a
    | otherwise = error "Not in pairing."

allPaths :: [Comp] -> Comp -> [[Comp]]
allPaths [] _ = error "No components."
allPaths es r = map init $ allPaths' es r [r, r]
allPaths' :: [Comp] -> Comp -> [Comp] -> [[Comp]]
allPaths' es r@(r1, r2) vs
    | null uvs = [vs]
    | otherwise = concatMap (\u -> allPaths' es u (u:vs)) uvs
    where uvs = filter (\(x, y) -> x == unsh || y == unsh) $ es \\ vs
          unsh
            | r1 == vs1 && r2 == vs2 && (r1 == 0 || r2 == 0) = other 0 r
            | r1 == vs1 || r1 == vs2 = r2
            | r2 == vs1 || r2 == vs2 = r1
            | otherwise = error "Cannot be connected."
          (vs1, vs2) = head $ tail vs

allBridges :: [Comp] -> [[Comp]]
allBridges comps = concatMap (allPaths comps) roots
                   where roots = filter (\(x, y) -> x == 0 || y == 0) comps

sumBridge :: [Comp] -> Int
sumBridge [] = 0
sumBridge ((c1, c2):cs) = c1 + c2 + sumBridge cs

day24a_solve :: [Comp] -> Int
day24a_solve comps = maximum $ map sumBridge $ allBridges comps

day24b_solve :: [Comp] -> Int
day24b_solve comps = sumBridge $ maximumBy ordr $ allBridges comps
                     where ordr b1 b2 = if length b1 == length b2 then compare (sumBridge b1) (sumBridge b2)
                                        else compare (length b1) (length b2)

sampleComponents :: [Comp]
sampleComponents = [(0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let readLines = lines contents
    let comps = map ((\[x0, x1] -> (read x0 :: Int, read x1 :: Int)) . splitOn "/") readLines
    let samplea_result = day24a_solve sampleComponents
    let sampleb_result = day24b_solve sampleComponents
    let day24a_result = day24a_solve comps
    let day24b_result = day24b_solve comps
    putStrLn $ unwords [ assert (31 == samplea_result) "+",
                         assert (19 == sampleb_result) "+",
                         assert (1906 == day24a_result) "+",
                         assert (1824 == day24b_result) "+"
                       ]
    putStrLn $ "Part 1: " ++ show day24a_result
    putStrLn $ "Part 2: " ++ show day24b_result
