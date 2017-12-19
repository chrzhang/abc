import Control.Exception

{-
From http://adventofcode.com/2017/day/18

--- Day 18: Duet ---
You discover a tablet containing some strange assembly code labeled simply
"Duet". Rather than bother the sound card with it, you decide to run the code
yourself. Unfortunately, you don't see any documentation, so you're left to
figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are
each named with a single letter and that can each hold a single integer. You
suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what
they do. Here's what you determine:

snd X plays a sound with a frequency equal to the value of X.
set X Y sets register X to the value of Y.
add X Y increases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in
register X by the value of Y.
mod X Y sets register X to the remainder of dividing the value contained in
register X by the value of Y (that is, it sets X to the result of X modulo Y).
rcv X recovers the frequency of the last sound played, but only when the value
of X is not zero. (If it is zero, the command does nothing.)
jgz X Y jumps with an offset of the value of Y, but only if the value of X is
greater than zero. (An offset of 2 skips the next instruction, an offset of -1
jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a
number. The value of a register is the integer it contains; the value of a
number is that number.

After each jump instruction, the program continues with the instruction to
which the jump jumped. After any other instruction, the program continues with
the next instruction. Continuing (or jumping) off either end of the program
terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2

The first four instructions set a to 1, add 2 to it, square it, and then set it
to itself modulo 5, resulting in a value of 4.
Then, a sound with frequency 4 (the value of a) is played.
After that, a is set to 0, causing the subsequent rcv and jgz instructions to
both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
Finally, a is set to 1, causing the next jgz instruction to activate, jumping
back two instructions to another jump, which jumps again to the rcv, which
ultimately triggers the recover operation.
At the time the recover operation is executed, the frequency of the last sound
played is 4.

What is the value of the recovered frequency (the value of the most recently
played sound) the first time a rcv instruction is executed with a non-zero
value?

--- Part Two ---
As you congratulate yourself for a job well done, you notice that the
documentation has been on the back of the tablet this entire time. While you
actually got most of the instructions correct, there are a few key differences.
This assembly code isn't about sound at all - it's meant to be run twice at the
same time.

Each running copy of the program has its own set of registers and follows the
code independently - in fact, the programs don't even necessarily run at the
same speed. To coordinate, they use the send (snd) and receive (rcv)
instructions:

snd X sends the value of X to the other program. These values wait in a queue
until that program is ready to receive them. Each program has its own message
queue, so a program can never receive a message it sent.
rcv X receives the next value and stores it in register X. If no values are in
the queue, the program waits for a value to be sent to it. Programs do not
continue to the next instruction until they have received a value. Values are
received in the order they are sent.
Each program also has its own program ID (one 0 and the other 1); the register
p should begin with this value.

For example:

snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d

Both programs begin by sending three values to the other. Program 0 sends 1, 2,
0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and
stores it in a, receives another value (both 2) and stores it in b, and then
each receives the program ID of the other program (program 0 receives 1;
program 1 receives 0) and stores it in c. Each program now sees a different
value in its own copy of register c.

Finally, both programs try to rcv a fourth time, but no data is waiting for
either of them, and they reach a deadlock. When this happens, both programs
terminate.

It should be noted that it would be equally valid for the programs to run at
different speeds; for example, program 0 might have sent all three values and
then stopped at the first rcv before program 1 executed even its first
instruction.

Once both of your programs have terminated (regardless of what caused them to
do so), how many times did program 1 send a value?
-}

type Registers = [(Char, Int)]

upd :: Char -> Int -> Registers -> Registers
upd r v rs = case l of Just _ -> map (\(ky, ve) -> if ky == r then (ky, v) else (ky, ve)) rs
                       Nothing -> (r, v):rs
             where l = lookup r rs

getvalue :: String -> Registers -> Int
getvalue x rs
    | length x == 1 && elem (head x) ['a'..'z'] = getvaluereg (head x) rs
    | otherwise = read x :: Int
    where getvaluereg y ys = case l of Nothing -> 0
                                       Just z -> z
                             where l = lookup y ys

setreg :: Char -> String -> Registers -> Registers
setreg r x rs = upd r (getvalue x rs) rs

opreg :: Char -> String -> Registers -> (Int -> Int -> Int) -> Registers
opreg r x rs op = upd r (op (getvalue [r] rs) (getvalue x rs)) rs

addreg :: Char -> String -> Registers -> Registers
addreg r x rs = opreg r x rs (+)

mulreg :: Char -> String -> Registers -> Registers
mulreg r x rs = opreg r x rs (*)

modreg :: Char -> String -> Registers -> Registers
modreg r x rs = opreg r x rs (mod)

jmp :: String -> String -> Int -> Registers -> Int
jmp x y ci rs
    | getvalue x rs > 0 = ci + getvalue y rs
    | otherwise = ci + 1

type Instrs = [String]
type State = (Registers, Instrs, Int, Int)

isvalid1 :: State -> Bool
isvalid1 (_, instrs, ii, _) = ii >= 0 && ii < length instrs

next1 :: State -> State
next1 st
    | not $ isvalid1 st = error "State is not valid."
    | otherwise = next1' st

next1' :: State -> State
next1' (rs, instrs, ii, ls)
    | currcmd == "snd" = (rs, instrs, ii + 1, getvalue val1 rs)
    | currcmd == "set" = (setreg reg val2 rs, instrs, ii + 1, ls)
    | currcmd == "add" = (addreg reg val2 rs, instrs, ii + 1, ls)
    | currcmd == "mul" = (mulreg reg val2 rs, instrs, ii + 1, ls)
    | currcmd == "mod" = (modreg reg val2 rs, instrs, ii + 1, ls)
    | currcmd == "rcv" = (rs, instrs, ii + 1, ls)
    | currcmd == "jgz" = (rs, instrs, jmp val1 val2 ii rs, ls)
    | otherwise = error "Unsupported command."
    where curri = words $ instrs !! ii
          currcmd = curri !! 0
          reg = head $ (curri !! 1)
          val1 = curri !! 1
          val2 = curri !! 2

recvwithnzero :: State -> Bool
recvwithnzero (rs, instrs, ii, _)
    | currcmd == "rcv" && getvalue val1 rs /= 0 = True
    | otherwise = False
    where curri = words $ instrs !! ii
          currcmd = curri !! 0
          val1 = curri !! 1

day18a_solve :: Instrs -> Int
day18a_solve instrs = ls
    where states = takeWhile isvalid1 (iterate next1 ([], instrs, 0, 0))
          (_, _, _, ls) = last $ takeWhile (not . recvwithnzero) states

data Choice = Prog1 | Prog2 deriving (Eq, Show)

data DState = DState { r1 :: Registers,
                       r2 :: Registers,
                       is :: Instrs,
                       i1 :: Int,
                       i2 :: Int,
                       wp :: Choice,
                       ctr :: Int,
                       pipe1 :: [Int],
                       pipe2 :: [Int] } deriving (Show)

next2p1 :: DState -> DState
next2p1 ds@(DState r1' r2' is' i1' i2' wp' ctr' pipe1' pipe2')
    | currcmd == "set" =
        DState (setreg reg val2 r1') r2' is' (i1' + 1) i2' wp' ctr' pipe1' pipe2'
    | currcmd == "add" =
        DState (addreg reg val2 r1') r2' is' (i1' + 1) i2' wp' ctr' pipe1' pipe2'
    | currcmd == "mul" =
        DState (mulreg reg val2 r1') r2' is' (i1' + 1) i2' wp' ctr' pipe1' pipe2'
    | currcmd == "mod" =
        DState (modreg reg val2 r1') r2' is' (i1' + 1) i2' wp' ctr' pipe1' pipe2'
    | currcmd == "jgz" =
        DState r1' r2' is' (jmp val1 val2 i1' r1') i2' wp' ctr' pipe1' pipe2'
    | currcmd == "rcv" = next2rcv1 ds
    | otherwise = error "Unsupported command."
    where curri = words $ is ds !! i1 ds
          currcmd = curri !! 0
          reg = head $ (curri !! 1)
          val1 = curri !! 1
          val2 = curri !! 2

next2p2 :: DState -> DState
next2p2 ds = flipst fs
             where fs = next2p1 $ flipst ds
                   flipst s = DState (r2 s) (r1 s) (is s) (i2 s) (i1 s)
                                     (if wp s == Prog1 then Prog2 else Prog1)
                                     (ctr s) (pipe2 s) (pipe1 s)

next2snd1 :: DState -> DState
next2snd1 (DState r1' r2' is' i1' i2' wp' ctr' pipe1' pipe2') =
    DState r1' r2' is' (i1' + 1) i2' wp' ctr' ((getvalue sndval r1'):pipe1') pipe2'
    where sndval = words (is' !! i1') !! 1

next2snd2 :: DState -> DState
next2snd2 (DState r1' r2' is' i1' i2' wp' ctr' pipe1' pipe2') =
    DState r1' r2' is' i1' (i2' + 1) wp' (ctr' + 1) pipe1' ((getvalue sndval r2'):pipe2')
    where sndval = words (is' !! i2') !! 1

isvalid2 :: Choice -> DState -> Bool
isvalid2 Prog1 (DState _ _ is' i1' _ _ _ _ _) = i1' >= 0 && i1' < length is'
isvalid2 Prog2 (DState _ _ is' _ i2' _ _ _ _) = i2' >= 0 && i2' < length is'

next2rcv1 :: DState -> DState
next2rcv1 ds@(DState r1' r2' is' i1' i2' wp' ctr' pipe1' pipe2')
    | pipe2' /= [] = -- Consume
        DState (setreg rcvval (show $ last pipe2') r1') r2' is' (i1' + 1) i2' wp' ctr' pipe1' (init pipe2')
    | not (isvalid2 Prog2 ds) = -- Cannot consume, other program died
        DState r1' r2' is' (-1) (-1) Prog1 ctr' pipe1' pipe2'
    | pipe1' == [] && othercmd == "rcv" = -- Deadlock
        DState r1' r2' is' (-1) (-1) Prog1 ctr' pipe1' pipe2'
    | otherwise = DState r1' r2' is' i1' i2' Prog2 ctr' pipe1' pipe2' -- Switch programs
    where rcvval = head $ words (is' !! i1') !! 1
          othercmd = head $ words (is' !! i2')

next2 :: DState -> DState
next2 ds@(DState _ _ is' i1' i2' wp' _ _ _)
    | currcmd == "snd" && wp' == Prog1 = next2snd1 ds
    | currcmd == "snd" && wp' == Prog2 = next2snd2 ds
    | wp' == Prog1 = next2p1 ds
    | wp' == Prog2 = next2p2 ds
    | otherwise = error "Impossible state."
    where curri = words $ is' !! (if wp' == Prog1 then i1' else i2')
          currcmd = curri !! 0

initial_dstate :: [String] -> DState
initial_dstate instrs = DState [('p', 0)] [('p', 1)] instrs 0 0 Prog1 0 [] []

day18b_solve :: Instrs -> Int
day18b_solve instrs = ctr $ last vstates
                      where dstates = iterate next2 (initial_dstate instrs)
                            vstates = takeWhile (\s -> isvalid2 Prog1 s || isvalid2 Prog2 s) dstates

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instrs = lines contents
    let day18a_result = day18a_solve instrs
    let day18b_result = day18b_solve instrs
    putStrLn $ unwords [ assert (8600 == day18a_result) "+",
                         assert (7239 == day18b_result) "+"
                       ]
    putStrLn $ "Part 1: " ++ show day18a_result
    putStrLn $ "Part 2: " ++ show day18b_result
