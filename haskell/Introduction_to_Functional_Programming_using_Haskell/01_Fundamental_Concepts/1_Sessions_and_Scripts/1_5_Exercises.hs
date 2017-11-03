import System.IO
import Control.Exception

-- 1.5.1 The Fibonacci numbers f0, f1, ... are defined by the rule
-- that f0 = 0, f1 = 1, and fn+2 = fn + fn+1. Give a definition of the
-- function fib that takes an integer n and returns fn.
fib :: Integer -> Integer
fib n
    | n < 0 = error "negative argument to fib"
    | n == 0 = 0
    | n == 1 = 1
    | n > 1 = fib (n - 1) + fib (n - 2)

-- 1.5.2 Define a function abs that returns the absolute value of an integer.
my_abs :: Integer -> Integer
my_abs n = if n >= 0 then n else -n

main = do
    let testResults = [ (assert (fib 0 == 0) "+"),
                        (assert (fib 1 == 1) "+"),
                        (assert (fib 2 == 1) "+"),
                        (assert (fib 3 == 2) "+"),
                        (assert (fib 4 == 3) "+"),
                        (assert (fib 5 == 5) "+"),
                        (assert (fib 6 == 8) "+"),
                        (assert (my_abs (-1) == 1) "+"),
                        (assert (my_abs 1 == 1) "+"),
                        (assert (my_abs 0 == 0) "+")
                      ]
    putStrLn (unwords testResults)
