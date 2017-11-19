import System.IO()
import Control.Exception

type Cbool a = a -> a -> a

true, false :: Cbool a
true x y = x
false x y = y

c_not :: Cbool (Cbool a) -> Cbool a
c_not x = x false true

c_and, c_or :: Cbool (Cbool a) -> Cbool a -> Cbool a
c_and x y = x y false -- If x is true then return y, else return false
c_or x y = x true y -- If x is true then return true, else return y

type Cnum a = (a -> a) -> (a -> a)
zero, one, two :: Cnum a
zero f = id
one f = f
two f = f . f

c_succ :: Cnum a -> Cnum a
c_succ cn f = f . cn f

from_church :: Cnum Int -> Int
from_church cn = cn (+1) 0

plus1 :: Cnum a -> Cnum a -> Cnum a
plus1 cn dn f = cn f . dn f

plus2 :: Cnum (Cnum a) -> Cnum a -> Cnum a
plus2 cn = cn c_succ

times1 :: Cnum a -> Cnum a -> Cnum a
times1 cn dn = cn . dn

-- 3.7.1 Write a definition of isZero that determines whether its argument is
-- zero or not.
isZero :: Cnum (Cbool a) -> Cbool a
isZero cn = cn (\x -> false) true

-- 3.7.2 Consider the definition arrow2 m n = m n. Why does this give the Church
-- number for n^m? What is the type of arrow2?
-- m n means applying n for m times.
-- 'applying n' means multiplying by n.
-- m n means multiplying by n for m times, or n^m.
arrow2 :: (Cnum a -> Cnum a) -> Cnum a -> Cnum a
arrow2 m n = m n

-- 3.7.3 Use a computer to discover the type of arrow1.
arrow1 :: ((Cnum a -> Cnum a) -> c) -> Cnum a -> c
arrow1 cn = cn . times1

-- 3.7.4 Define a Church version of Either a b.
type Ceither a b c = (a -> c) -> (b -> c) -> c

main :: IO ()
main = do
    putStrLn (unwords [ (assert (true 1 0 == 1) "+"),
                        (assert (false 1 0 == 0) "+"),
                        (assert ((c_not true) 1 0 == 0) "+"),
                        (assert ((c_not false) 1 0 == 1) "+"),
                        (assert (c_and true true 1 0 == 1) "+"),
                        (assert (c_and true false 1 0 == 0) "+"),
                        (assert (c_and false true 1 0 == 0) "+"),
                        (assert (c_and false false 1 0 == 0) "+"),
                        (assert (c_or true true 1 0 == 1) "+"),
                        (assert (c_or true false 1 0 == 1) "+"),
                        (assert (c_or false true 1 0 == 1) "+"),
                        (assert (c_or false false 1 0 == 0) "+"),
                        (assert (from_church zero == 0) "+"),
                        (assert (from_church one == 1) "+"),
                        (assert (from_church two == 2) "+"),
                        (assert (from_church (plus1 one two) == 3) "+"),
                        (assert (from_church (plus2 one two) == 3) "+"),
                        (assert (from_church (times1 two two) == 4) "+"),
                        (assert (from_church (times1 one two) == 2) "+"),
                        (assert ((isZero zero) 1 0 == 1) "+"),
                        (assert ((isZero one) 1 0 == 0) "+"),
                        (assert ((isZero two) 1 0 == 0) "+")
                      ])
