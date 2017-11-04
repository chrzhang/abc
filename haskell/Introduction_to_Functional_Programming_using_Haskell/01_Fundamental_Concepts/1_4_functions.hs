import System.IO
import Control.Exception

-- 1.4.1 Suppose f and g have the following types:
f :: Integer -> Integer
f x = x + 1
g :: Integer -> (Integer -> Integer)
g y = (\x -> x + y)
-- Let h be defined by
h :: Integer -> Integer -> Integer
h x y = f (g x y)
-- Fill in the correct type assignment for h.
-- Now determine which, if any, of the following statements is true:
-- h = f . g
-- h x y ?= (f . g) x y
-- f (g x y) ?= f (g x y)
-- True
-- h x = f . (g x)
-- h x y ?=  (f . (g x)) y
-- h x y ?=  f (g x y)
-- True
-- h x y = (f . g) x y
-- f . g is h => True

-- 1.4.2 Suppose we curry the arguments of the function delta so that we can
-- write delta a b c rather than delta (a, b, c). What is the type of the
-- curried version?
square :: Float -> Float
square x = x * x
delta :: (Float, Float, Float) -> Float
delta (a, b, c) = sqrt(square b - 4 * a * c)
deltaCurried :: Float -> Float -> Float -> Float
deltaCurried a b c = sqrt (square b - 4 * a * c)

-- 1.4.3. In mathematics one often uses logarithms to various bases; for
-- example, log_2, log_e, and log_10. Give an appropriate type of a function
-- log that takes a base and returns the logarithm function for that base.
my_log :: Float -> (Float -> Float)
my_log base = logBase base

-- 1.4.4 Describe one appropriate type for the definite integral function of
-- mathematical analysis, as used in the phrase 'the integral of f from a to b'
integral :: Float -> Float -> (Float -> Float) -> Float
integral lower upper f = 0

-- 1.4.5 Give examples of functions with the following types
example1 :: (Integer -> Integer) -> Integer
example1 f = f 0

example2 :: (Integer -> Integer) -> (Integer -> Integer)
example2 f x = f (f (f x))

-- 1.4.6 Which, if any, of the following statements is true?
-- (*) x = (*x) is True since * is commutative
-- (+) x = (x+) is True since + it is the definition
-- (-) x = (-x) is False since (-x) y is defined as y - x

-- 1.4.7 Define a function uncurry that converts a curried function into a
-- noncurried version. Show that
-- curry (uncurry f) x y = f x y
-- uncurry (curry f) (x, y) = f (x, y)
-- for all x and y.
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y
-- curry (uncurry f) x y
-- = (uncurry f) (x, y)
-- = f x y
-- uncurry (curry f) (x, y)
-- = (curry f) x y
-- = f (x y)

main = do
    let testResults = [ (assert (h 1 2 == f (g 1 2)) "+"),
                        (assert (h 3 1 == f (g 3 1)) "+"),
                        (assert (h 0 10 == f (g 0 10)) "+"),
                        (assert (delta (1, 3, 2) == 1.0) "+"),
                        (assert (deltaCurried 1 3 2 == 1.0) "+"),
                        (assert ((my_log 2) 32 == 5) "+")
                      ]
    putStrLn (unwords testResults)
