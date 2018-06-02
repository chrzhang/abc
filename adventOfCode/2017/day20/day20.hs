import Data.List (transpose, minimumBy)
import Data.List.Split (splitOn)
import Data.MultiSet (fromList, toOccurList)
import Data.Maybe (fromMaybe)
import Control.Exception

{-
From http://adventofcode.com/2017/day/20

--- Day 20: Particle Swarm ---
Suddenly, the GPU contacts you, asking for help. Someone has asked it to
simulate too many particles, and it won't be able to finish them all in time to
render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order
(starting with particle 0, then particle 1, particle 2, and so on). For each
particle, it provides the X, Y, and Z coordinates for the particle's position
(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties
are updated in the following order:

Increase the X velocity by the X acceleration.
Increase the Y velocity by the Y acceleration.
Increase the Z velocity by the Z acceleration.
Increase the X position by the X velocity.
Increase the Y position by the Y velocity.
Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would
like to know which particle will stay closest to position <0,0,0> in the long
term. Measure this using the Manhattan distance, which in this situation is
simply the sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay
entirely on the X-axis (for simplicity). Drawing the current states of
particles 0 and 1 (in that order) with an adjacent a number line and diagram of
current X positions (marked in parenthesis), the following would take place:

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and
so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?

--- Part Two ---
To simplify the problem further, the GPU would like to remove any particles
that collide. Particles collide if their positions ever exactly match. Because
particles are updated simultaneously, more than two particles can collide at
the same time and place. Once particles collide, they are removed and cannot
collide with anything else after that tick.

For example:

p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

------destroyed by collision------
------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
------destroyed by collision------                      (3)
p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

In this example, particles 0, 1, and 2 are simultaneously destroyed at the time
and place marked X. On the next tick, particle 3 passes through unharmed.

How many particles are left after all collisions are resolved?
-}

data Pos = Pos Int Int Int deriving (Eq, Show, Ord)
data Vel = Vel { dx :: Int, dy :: Int, dz :: Int } deriving (Eq, Show)
data Acc = Acc Int Int Int deriving (Eq, Show)
data PState = PState Pos Vel Acc deriving (Show, Eq)

tick :: PState -> PState
tick (PState (Pos x' y' z') (Vel dx' dy' dz') a@(Acc ddx' ddy' ddz')) = PState np nv a
    where nv = Vel (dx' + ddx') (dy' + ddy') (dz' + ddz')
          np = Pos (x' + dx nv) (y' + dy nv) (z' + dz nv)

states :: PState -> [PState]
states = iterate tick

distVec :: Int -> Int -> Int -> Int
distVec a b c = abs a + abs b + abs c

samesign :: Int -> Int -> Bool
samesign _ 0 = True
samesign 0 _ = True
samesign a b = signum a == signum b

normal :: PState -> Bool
normal (PState (Pos x' y' z') (Vel dx' dy' dz') (Acc ddx' ddy' ddz'))
    = normalax x' dx' ddx' && normalax y' dy' ddy' && normalax z' dz' ddz'
      where normalax a b c = samesign a b && samesign b c

normalall :: [PState] -> [PState]
normalall pss = head $ dropWhile (any (not . normal)) ticks
                where ticks = transpose $ map states pss

pstCmp :: (Int, PState) -> (Int, PState) -> Ordering
pstCmp (_, PState (Pos px1 py1 pz1) v1@(Vel vx1 vy1 vz1) a1@(Acc ax1 ay1 az1))
       (_, PState (Pos px2 py2 pz2) v2@(Vel vx2 vy2 vz2) a2@(Acc ax2 ay2 az2))
    | a1 /= a2  = compare (distVec ax1 ay1 az1) (distVec ax2 ay2 az2)
    | v1 /= v2  = compare (distVec vx1 vy1 vz1) (distVec vx2 vy2 vz2)
    | otherwise = compare (distVec px1 py1 pz1) (distVec px2 py2 pz2)

day20a_solve :: [PState] -> Int
day20a_solve pss = fst $ minimumBy pstCmp pssi
                   where pssi = zip [0..] (normalall pss)

toPState :: [String] -> PState
toPState [ps, vs, as] = PState (toPos $ soc tps) (toVel $ soc tvs) (toAcc $ soc tas)
                        where tps = take (length ps - 5) $ drop 3 ps
                              tvs = take (length vs - 5) $ drop 3 vs
                              tas = take (length as - 4) $ drop 3 as
                              soc a = map toi $ splitOn "," a
                              toPos a = Pos (head a) (a !! 1) (a !! 2)
                              toVel a = Vel (head a) (a !! 1) (a !! 2)
                              toAcc a = Acc (head a) (a !! 1) (a !! 2)
                              toi a = read a :: Int
toPState _ = error "Cannot convert to PState."

nocollide :: [PState] -> [PState]
nocollide pss = filter alone pss
                where allpos = map (\(PState pos _ _) -> pos) pss
                      freqpos = toOccurList . fromList $ allpos
                      alone (PState pos _ _) = fromMaybe (error "Position must exist.") (lookup pos freqpos) == 1

day20b_solve :: [PState] -> Int
day20b_solve pss = nps !! 200 -- Heuristic guess
                   where nps = map length $ iterate (map tick . nocollide) pss

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let readlines = map words $ lines contents
    let allPStates = map toPState readlines
    let day20a_result = day20a_solve allPStates
    let day20b_result = day20b_solve allPStates
    putStrLn $ unwords [ assert (308 == day20a_result) "+",
                         assert (504 == day20b_result) "+"
                       ]
    putStrLn $ "Part 1: " ++ show day20a_result
    putStrLn $ "Part 2: " ++ show day20b_result
