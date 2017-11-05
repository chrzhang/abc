import System.IO
import Control.Exception
import Data.Char

-- 2.2.1 Define a function nextlet that takes a letter of the alphabet and
-- returns the letter coming immediately after it. Assume that letter 'A'
-- follows 'Z'.
nextlet :: Char -> Char
nextlet c
    | c >= 'a' && c < 'z' = chr (1 + ord c)
    | c == 'z'            = 'a'
    | c >= 'A' && c < 'Z' = chr (1 + ord c)
    | c == 'Z'            = 'A'
    | otherwise           = error "Input is not in the alphabet."

-- 2.2.2 Define a function digitval that converts a digit character to its
-- corresponding numerical value
digitval :: Char -> Int
digitval c
    | c >= '0' && c <= '9' = (ord c) - ord '0'
    | otherwise            = error "Input is not a digit."

main = do
    let testResults = [ (assert (nextlet 'a' == 'b') "+"),
                        (assert (nextlet 'b' == 'c') "+"),
                        (assert (nextlet 'z' == 'a') "+"),
                        (assert (nextlet 'A' == 'B') "+"),
                        (assert (nextlet 'B' == 'C') "+"),
                        (assert (nextlet 'Z' == 'A') "+"),
                        (assert (digitval '0' == 0) "+"),
                        (assert (digitval '2' == 2) "+"),
                        (assert (digitval '9' == 9) "+"),
                        (assert (digitval '1' == 1) "+")
                      ]
    putStrLn (unwords testResults)
