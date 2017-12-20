import Control.Exception
import Data.Maybe (fromMaybe)

{-
From http://adventofcode.com/2017/day/8

--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent assistance
with jump instructions, it would like you to compute the result of a series of
unusual register instructions.

Each instruction consists of several parts: the register to modify, whether to
increase or decrease that register's value, the amount by which to increase or
decrease it, and a condition. If the condition fails, skip the instruction
without modifying the register. The registers all start at 0. The instructions
look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10

These instructions would be processed as follows:

Because a starts at 0, it is not greater than 1, and so b is not modified.
a is increased by 1 (to 1) because b is less than 5 (it is 0).
c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is
1).
c is increased by -20 (to -10) because c is equal to 10.
After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the registers
are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions in
your puzzle input?

--- Part Two ---

To be safe, the CPU also needs to know the highest value held in any register
during this process so that it can decide how much memory to allocate to these
operations. For example, in the above instructions, the highest value ever held
was 10 (in register c after the third instruction was evaluated).
-}

data Instr = Incr String Int String String Int |
             Decr String Int String String Int deriving (Show)

type Regs = [(String, Int)]

type CState = ([Instr], Int, Regs)

next :: CState -> CState
next (is, ii, rs) = (is, ii + 1, upd ci)
    where ci = is !! ii
          upd (Incr s d lo op ro) =
              if eval (lo, op, ro) rs then chgreg s d rs else rs
          upd (Decr s d lo op ro) =
              if eval (lo, op, ro) rs then chgreg s (-d) rs else rs

tocmp :: Ord a => String -> a -> a -> Bool
tocmp x = case x of "<"  -> (<)
                    "<=" -> (<=)
                    ">"  -> (>)
                    ">=" -> (>=)
                    "==" -> (==)
                    "!=" -> (/=)
                    _    -> error "No such operator."

eval :: (String, String, Int) -> Regs -> Bool
eval (lo, op, ro) rs = ord rv ro
            where ord = tocmp op
                  rv = getval lo rs

chgreg :: String -> Int -> Regs -> Regs
chgreg r d = chgreg' (r, d)
             where chgreg' (k, v) [] = [(k, v)]
                   chgreg' (k, v) ((a, b):xs) = if k == a then (k, b + v) : xs
                                                else (a, b) : chgreg' (k, v) xs

getval :: String -> Regs -> Int
getval r rs = fromMaybe 0 (lookup r rs)

toInstrs :: [[String]] -> [Instr]
toInstrs = map to_instr
            where to_instr [r, "inc", d, "if", lo, op, ro] = Incr r (toint d) lo op (toint ro)
                  to_instr [r, "dec", d, "if", lo, op, ro] = Decr r (toint d) lo op (toint ro)
                  to_instr _ = error "Not valid instruction."
                  toint d = read d :: Int

day8a_solve :: [Instr] -> Int
day8a_solve instrs = get_mx fs
                     where ss = iterate next (instrs, 0, [])
                           vs = takeWhile (\(_, i, _) -> i < length instrs) ss
                           fs = last vs
                           get_mx (_, _, ld) = maximum $ map snd ld

day8b_solve :: [Instr] -> Int
day8b_solve instrs = max' $ map get_mx vs
                     where ss = iterate next (instrs, 0, [])
                           vs = takeWhile (\(_, i, _) -> i < length instrs) ss
                           get_mx (_, _, ld) = max' $ map snd ld
                           max' [] = 0
                           max' xs = maximum xs

main :: IO ()
main = do
    sample_contents <- readFile "sample_input.txt"
    let sample_instrs = toInstrs $ map words $ lines sample_contents
    contents <- readFile "input.txt"
    let instrs = toInstrs $ map words $ lines contents
    let day8a_result = day8a_solve instrs
    let day8b_result = day8b_solve instrs
    putStrLn (unwords [ assert (day8a_solve sample_instrs == 10) "+",
                        assert (day8a_result == 5966) "+",
                        assert (day8b_result == 6347) "+"
                      ])
    putStrLn $ "Part 1: " ++ show day8a_result
    putStrLn $ "Part 2: " ++ show day8b_result
