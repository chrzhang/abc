import System.IO
import Control.Exception

square :: Integer -> Integer
square x = x * x

-- 1.7.1 Give another definition of increase that meets its specification
increase :: Integer -> Integer
-- increase x > square x, whenever x >= 0
increase x
    | x < 0 = error "Undefined specification for negative input."
    | x >= 0 = 1 + square x

main = do
    let testResults = [ (assert (increase 0 > square 0) "+"),
                        (assert (increase 1 > square 1) "+")
                      ]
    putStrLn (unwords testResults)
