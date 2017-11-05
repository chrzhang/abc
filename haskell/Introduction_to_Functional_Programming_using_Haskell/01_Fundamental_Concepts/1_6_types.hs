import System.IO
import Control.Exception

-- 1.6.1 Give suitable polymorphic type assignments for the following:
my_const :: a -> b -> a
my_const x y = x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

apply :: (a -> b) -> a -> b
apply f x = f x

flip :: (a -> a -> b) -> a -> a -> b
flip f x y = f y x

-- 1.6.2 Define a function swap so that
-- flip (curry f) = curry (f . swap)
-- for all f :: (a, b) -> c
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
-- flip (curry f) x y
-- = (curry f) y x
-- = f y x
-- curry (f . swap) x y
-- = f (swap x y)
-- = f y x

-- 1.6.3 Can you find polymorphic type assignments for the following functions?
strange :: ((a -> b) -> a) -> (a -> b) -> b
strange f g = g (f g)
-- stranger f = f f
-- No this has an infinite type in Haskell.
-- If f :: a -> b, then f f would mean a = (a -> b) which is recursive.

-- 1.6.4 Find a polymorphic type assignment for
square :: Num a => a -> a
square x = x * x

main = do
    let testResults = [ (assert (swap (1, 2) == (2, 1)) "+")
                      ]
    putStrLn (unwords testResults)
