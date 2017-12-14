import Control.Exception
import Data.List

{-
From http://adventofcode.com/2017/day/12

--- Day 12: Digital Plumber ---

Walking along the memory banks of the stream, you find a small village that is
experiencing a little confusion: some programs can't communicate with each
other.

Programs in this village communicate using a fixed system of pipes. Messages
are passed between programs using these pipes, but most programs aren't
connected to each other directly. Instead, programs pass messages between each
other until the message reaches the intended recipient.

For some reason, though, some of these messages aren't ever reaching their
intended recipient, and the programs suspect that some pipes are missing. They
would like you to investigate.

You walk through the village and record the ID of each program and the IDs with
which it can communicate directly (your puzzle input). Each program has one or
more programs with which it can communicate, and these pipes are bidirectional;
if 8 says it can communicate with 11, then 11 will say it can communicate with
8.

You need to figure out how many programs are in the group that contains program
ID 0.

For example, suppose you go door-to-door like a travelling salesman and record
the following list:

0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5

In this example, the following programs are in the group that contains program
ID 0:

Program 0 by definition.
Program 2, directly connected to program 0.
Program 3 via program 2.
Program 4 via program 2.
Program 5 via programs 6, then 4, then 2.
Program 6 via programs 4, then 2.

Therefore, a total of 6 programs are in this group; all but program 1, which
has a pipe that connects it to itself.

How many programs are in the group that contains program ID 0?

--- Part Two ---

There are more programs than just the ones in the group containing program ID
0. The rest of them have no way of reaching that group, and still might have no
way of reaching each other.

A group is a collection of programs that can all communicate via pipes either
directly or indirectly. The programs you identified just a moment ago are all
part of the same group. Now, they would like you to determine the total number
of groups.

In the example above, there were 2 groups: one consisting of programs
0,2,3,4,5,6, and the other consisting solely of program 1.

How many groups are there in total?
-}

type Graph = [(Int, [Int])]

data SearchState = SearchState { nodes :: [Int],
                                 visited :: [Int],
                                 graph :: Graph } deriving (Show)

next :: SearchState -> SearchState
next ss = SearchState nnodes nvisited (graph ss)
          where children n = case lookup n (graph ss) of Just x -> x
                                                         Nothing -> error "Node not in graph."
                nlayer = concat $ map children (nodes ss)
                nnodes = filter (\x -> not $ elem x (visited ss)) nlayer
                nvisited = visited ss ++ nnodes

bfs :: Int -> Graph -> [[Int]]
bfs n g = map nodes $ takeWhile (\ss -> nodes ss /= []) layers
          where layers = iterate next (SearchState [n] [n] g)

day12a_solve :: [(Int, [Int])] -> Int
day12a_solve g = length $ concat $ bfs 0 g


groups :: Graph -> [[Int]]
groups g = takeWhile (\x -> x /= []) groups'
           where groups' = iterate (\x -> x \\ (concat $ bfs (head x) g)) allnodes
                 allnodes = map fst g

day12b_solve :: Graph -> Int
day12b_solve g = length $ groups g

tograph :: String -> Graph
tograph cts = [(read n :: Int, map (\x -> read x:: Int) c) | cw <- readlines,
                                                             let n = cw !! 0,
                                                             let c = drop 2 cw]
              where readlines = map words $ map (filter (\x -> x /= ',')) l
                    l = lines cts

main :: IO ()
main = do
    sample_contents <- readFile "sample_input.txt"
    let sample_graph = tograph sample_contents
    let sample_result = day12a_solve sample_graph
    contents <- readFile "input.txt"
    let in_graph = tograph contents
    let day12a_result = day12a_solve in_graph
    let day12b_result = day12b_solve in_graph
    putStrLn (unwords [ assert (6 == sample_result) "+",
                        assert (128 == day12a_result) "+",
                        assert (209 == day12b_result) "+"
                      ])
    putStrLn $ "Part 1: " ++ show day12a_result
    putStrLn $ "Part 2: " ++ show day12b_result
