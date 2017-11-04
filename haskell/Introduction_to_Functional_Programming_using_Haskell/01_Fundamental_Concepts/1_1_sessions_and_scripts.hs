import System.IO
import Control.Exception

-- 1.1.1 Using the function square, design a function quad that raises its
-- argument to the fourth power
square :: Integer -> Integer
square x = x * x
quad :: Integer -> Integer
quad x = square (square x)

-- 1.1.2 Define a function greater that returns the greater of its two arguments
greater :: Integer -> Integer -> Integer
greater x y = if x <= y then y else x

-- 1.1.3 Define a function for computing the area ofa  circle with given radius
circleArea :: Float -> Float
circleArea radius = pi * (radius ^ 2)

main = do
    let testResults = [ (assert (1 == quad 1) "+"),
                        (assert (16 == quad 2) "+"),
                        (assert (81 == quad 3) "+"),
                        (assert (2 == greater 1 2) "+"),
                        (assert (2 == greater 2 1) "+"),
                        (assert (2 == greater 2 2) "+"),
                        (assert (pi == circleArea 1) "+"),
                        (assert (0 == circleArea 0) "+"),
                        (assert (25 * pi == circleArea 5) "+")
                      ]
    putStrLn (unwords testResults)
