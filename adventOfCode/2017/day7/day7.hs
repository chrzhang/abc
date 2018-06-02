import Data.List (nub)
import Data.Char (isAsciiLower)
import Control.Exception

{-
From http://adventofcode.com/2017/day/7

--- Day 7: Recursive Circus ---

Wandering further through the circuits of the computer, you come upon a tower of
programs that have gotten themselves into a bit of trouble. A recursive
algorithm has gotten out of hand, and now they're balanced precariously in a
large tower.

One program at the bottom supports the entire tower. It's holding a large disc,
and on the disc are balanced several more sub-towers. At the bottom of these
sub-towers, standing on the bottom disc, are other programs, each holding their
own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many
programs stand simply keeping the disc below them balanced but with no disc of
their own.

You offer to help, but first you need to understand the structure of these
towers. You ask each program to yell out their name, their weight, and (if
they're holding a disc) the names of the programs immediately above them
balancing on that disc. You write this information down (your puzzle input).
Unfortunately, in their panic, they don't do this in an orderly fashion; by the
time you're done, you're not sure which program gave which information.

For example, if your list is the following:

pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)

...then you would be able to recreate the structure of the towers that looks
like this:

                gyxo
              /
         ugml - ebii
       /      \
      |         jptl
      |
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |
      |         ktlj
       \      /
         fwft - cntj
              \
                xhth

In this example, tknk is at the bottom of the tower (the bottom program), and is
holding up ugml, padx, and fwft. Those programs are, in turn, holding up other
programs; in this example, none of those programs are holding up any other
programs, and are all the tops of their own towers. (The actual tower balancing
in front of you is much larger.)

Before you're ready to help them, you need to make sure your information is
correct. What is the name of the bottom program?

--- Part Two ---

The programs explain the situation: they can't get down. Rather, they could get
down, if they weren't expending all of their energy trying to keep the tower
balanced. Apparently, one program has the wrong weight, and until it's fixed,
they're stuck here.

For any program holding a disc, each program standing on that disc forms a
sub-tower. Each of those sub-towers are supposed to be the same weight, or the
disc itself isn't balanced. The weight of a tower is the sum of the weights of
the programs in that tower.

In the example above, this means that for ugml's disc to be balanced, gyxo,
ebii, and jptl must all have the same weight, and they do: 61.

However, for tknk to be balanced, each of the programs standing on its disc and
all programs above it must each match. This means that the following sums must
all be the same:

ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the
other two. Even though the nodes above ugml are balanced, ugml itself is too
heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the
towers balanced. If this change were made, its weight would be 60.

Given that exactly one program is the wrong weight, what would its weight need
to be to balance the entire tower?
-}

type WeightChildren = (Int, [String])
type NodeData = (String, WeightChildren)

nodeinfo :: String -> [NodeData] -> WeightChildren
nodeinfo n ns = snd $ head $ filter (\(na, (_, _)) -> na == n) ns

weight :: String -> [NodeData] -> Int
weight n ns = fst (nodeinfo n ns) + sum (map (`weight` ns) (children n ns))

weightonly :: String -> [NodeData] -> Int
weightonly n ns = fst $ nodeinfo n ns

children :: String -> [NodeData] -> [String]
children n ns = snd $ nodeinfo n ns

allchildren :: [NodeData] -> [String]
allchildren = concatMap (\(_, (_, ch)) -> ch)

empty :: [a] -> Bool
empty [] = True
empty _ = False

bfs :: [NodeData] -> String -> [[String]]
bfs ns r = takeWhile (not . empty) layers
           where layers = iterate (concatMap (`children` ns)) [r]

bfsFlat :: [NodeData] -> String -> [String]
bfsFlat ns r = concat $ bfs ns r

day7a_solve :: [NodeData] -> String
day7a_solve ns = fst $ head $ filter (\(na, (_, ch)) -> not (ischild na) &&
                                                        ch /= []) ns
                 where ischild n = n `elem` allchildren ns

anomaly :: [[(Int, String)]] -> [[(Int, String)]]
anomaly = filter (\x -> length (nub (map fst x)) > 1)

day7b_solve :: [NodeData] -> String -> Int
day7b_solve ns r = weightonly (snd oddity) ns + discrep
                   where ans = anomaly $ map (\n -> map wn (children n ns)) nord
                         wn c = (weight c ns, c)
                         an = head ans
                         -- All parent nodes of the unbalanced node will be
                         -- unbalanced so need to change the deepest node
                         nord = reverse $ bfsFlat ns r
                         (minw, maxw) = (minimum $ map fst an,
                                         maximum $ map fst an)
                         mins = filter (\x -> fst x == minw) an
                         minc = length mins
                         maxs = filter (\x -> fst x == maxw) an
                         (oddity, normie) = if minc == 1 then (head mins,
                                                               head maxs)
                                            else (head maxs, head mins)
                         discrep = fst normie - fst oddity

toNodeData :: String -> [NodeData]
toNodeData c = [(na, (wt, ch)) | cwl <- wordlines,
                                 let na =  filter isAsciiLower (head cwl),
                                 let wt = read (cwl !! 1) :: Int,
                                 let ch = map (filter isAsciiLower) $ drop 3 cwl]
                where wordlines = map words $ lines c

main :: IO ()
main = do
    sample_contents <- readFile "sample_input.txt"
    let sample_nodes = toNodeData sample_contents
    contents <- readFile "input.txt"
    let indata = toNodeData contents
    let day7a_result = day7a_solve indata
    let day7b_result = day7b_solve indata day7a_result
    putStrLn (unwords [ assert (day7a_solve sample_nodes == "tknk") "+",
                        assert (day7b_solve sample_nodes "tknk" == 60) "+",
                        assert (day7a_result == "veboyvy") "+",
                        assert (day7b_result == 749) "+"
                      ])
    putStrLn $ "Part 1: " ++ show day7a_result
    putStrLn $ "Part 2: " ++ show day7b_result
