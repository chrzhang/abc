import Control.Exception

{-
From http://adventofcode.com/2017/day/9

--- Day 9: Stream Processing ---

A large stream blocks your path. According to the locals, it's not safe to
cross the stream at the moment because it's full of garbage. You look down at
the stream; rather than water, you discover that it's a stream of characters.

You sit for a while and record part of the stream (your puzzle input). The
characters represent groups - sequences that begin with { and end with }.
Within a group, there are zero or more other things, separated by commas:
either another group or garbage. Since groups can contain other groups, a }
only closes the most-recently-opened unclosed group - that is, they are
nestable. Your puzzle input represents a single, large group which itself
contains many smaller ones.

Sometimes, instead of a group, you will find garbage. Garbage begins with < and
ends with >. Between those angle brackets, almost any character can appear,
including { and }. Within garbage, < has no special meaning.

In a futile attempt to clean up the garbage, some program has canceled some of
the characters within it using !: inside garbage, any character that comes
after ! should be ignored, including <, >, and even another !.

You don't see any characters that deviate from these rules. Outside garbage,
you only find well-formed groups, and garbage always terminates according to
the rules above.

Here are some self-contained pieces of garbage:

<>, empty garbage.
<random characters>, garbage containing random characters.
<<<<>, because the extra < are ignored.
<{!>}>, because the first > is canceled.
<!!>, because the second ! is canceled, allowing the > to terminate the garbage.
<!!!>>, because the second ! and the first > are canceled.
<{o"i!a,<{i<a>, which ends at the first >.
Here are some examples of whole streams and the number of groups they contain:

{}, 1 group.
{{{}}}, 3 groups.
{{},{}}, also 3 groups.
{{{},{},{{}}}}, 6 groups.
{<{},{},{{}}>}, 1 group (which itself contains garbage).
{<a>,<a>,<a>,<a>}, 1 group.
{{<a>},{<a>},{<a>},{<a>}}, 5 groups.
{{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).

Your goal is to find the total score for all groups in your input. Each group
is assigned a score which is one more than the score of the group that
immediately contains it. (The outermost group gets a score of 1.)

{}, score of 1.
{{{}}}, score of 1 + 2 + 3 = 6.
{{},{}}, score of 1 + 2 + 2 = 5.
{{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
{<a>,<a>,<a>,<a>}, score of 1.
{{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.

What is the total score for all groups in your input?

--- Part Two ---

Now, you're ready to remove the garbage.

To prove you've removed it, you need to count all of the characters within the
garbage. The leading and trailing < and > don't count, nor do any canceled
characters or the ! doing the canceling.

<>, 0 characters.
<random characters>, 17 characters.
<<<<>, 3 characters.
<{!>}>, 2 characters.
<!!>, 0 characters.
<!!!>>, 0 characters.
<{o"i!a,<{i<a>, 10 characters.

How many non-canceled characters are within the garbage in your puzzle input?
-}

data StreamState = StreamState { currgroup :: Int,
                                 counter1 :: Int,
                                 counter2 :: Int,
                                 ingarbage :: Bool } deriving (Show)

step :: (StreamState, String) -> (Int, Int)
step (ss, []) = (counter1 ss, counter2 ss)
step (ss, '!':xs) = if ingarbage ss then step (ss, drop 1 xs)
                    else step (ss, xs)
step (ss, '<':xs) = if ingarbage ss then stepGarbage (ss, xs)
                    else step (StreamState (currgroup ss)
                                           (counter1 ss)
                                           (counter2 ss)
                                           True, xs)
step (ss, '>':xs) = if ingarbage ss then step (StreamState (currgroup ss)
                                                           (counter1 ss)
                                                           (counter2 ss)
                                                           False, xs)
                    else step (ss, xs)
step (ss, '{':xs) = if ingarbage ss then stepGarbage (ss, xs)
                    else step (StreamState (1 + currgroup ss)
                                           (counter1 ss)
                                           (counter2 ss)
                                           False, xs)
step (ss, '}':xs) = if ingarbage ss then stepGarbage (ss, xs)
                    else step (StreamState (currgroup ss - 1)
                                           (counter1 ss + currgroup ss)
                                           (counter2 ss)
                                           False, xs)
step (ss, _:xs) = if ingarbage ss then stepGarbage (ss, xs)
                  else step (ss, xs)

stepGarbage :: (StreamState, String) -> (Int, Int)
stepGarbage (ss, xs) =  step (StreamState (currgroup ss)
                                          (counter1 ss)
                                          (1 + counter2 ss)
                                          (ingarbage ss), xs)

defaultstate :: StreamState
defaultstate = StreamState 0 0 0 False

day9a_solve :: String -> Int
day9a_solve s = fst $ step (defaultstate, s)

day9b_solve :: String -> Int
day9b_solve s = snd $ step (defaultstate, s)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = head (lines contents)
    let day9a_result = day9a_solve input
    let day9b_result = day9b_solve input
    putStrLn $ unwords [ assert (day9a_solve "{}" == 1) "+",
                         assert (day9a_solve "{{{}}}" == 6) "+",
                         assert (day9a_solve "{{},{}}" == 5) "+",
                         assert (day9a_solve "{{{},{},{{}}}}" == 16) "+",
                         assert (day9a_solve "{<a>,<a>,<a>,<a>}" == 1) "+",
                         assert (day9a_solve "{{<ab>},{<ab>},{<ab>},{<ab>}}" == 9) "+",
                         assert (day9a_solve "{{<!!>},{<!!>},{<!!>},{<!!>}}" == 9) "+",
                         assert (day9a_solve "{{<a!>},{<a!>},{<a!>},{<ab>}}" == 3) "+",
                         assert (day9a_result == 14204) "+",
                         assert (day9b_solve "<>" == 0) "+",
                         assert (day9b_solve "<random characters>" == 17) "+",
                         assert (day9b_solve "<<<<>" == 3) "+",
                         assert (day9b_solve "<{!>}>" == 2) "+",
                         assert (day9b_solve "<!!>" == 0) "+",
                         assert (day9b_solve "<!!!>>" == 0) "+",
                         assert (day9b_solve "<{o\"i!a,<{i<a>>" == 10) "+",
                         assert (day9b_result == 6622) "+"
                       ]
    putStrLn $ "Part 1: " ++ show day9a_result
    putStrLn $ "Part 2: " ++ show day9b_result
