import System.IO
import Control.Exception

-- 3.1.1 Construct the positive numbers as a recursive data type.
data Pos = One | SuccP Pos

-- 3.1.2 Define a function that converts a natural number to an integer.
data Nat = Zero | SuccN Nat
           deriving (Eq, Ord, Show)

convert :: Nat -> Integer
convert Zero = 0
convert (SuccN n) = 1 + convert n

-- 3.1.3 Define versions of + and * that use pattern matching on the first
-- argument. Can the same be done for ^?
plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (SuccN n) m = SuccN (plus m n)
times :: Nat -> Nat -> Nat
times Zero m = Zero
times (SuccN n) m = plus (times m n) m
-- Pattern matching on the first argument works for plus and times since they
-- are commutative. For ^, pattern matching on a first argument of Zero is
-- trivial, resulting in an outcome of Zero. Matching on raised (SuccN n) m
-- is not able to be defined in this scope.

-- 3.1.4 How many evaluation steps does it take to evaluate m * n?
-- m * n
-- n steps to evaluate all the * that will expand out
-- n steps to evaluate all the + that were also expanded out by above
-- = 2n

-- 3.1.5 Define a total version of subtraction so that totalSub m n = Zero
-- if m < n
totalSub :: Nat -> Nat -> Nat
totalSub m Zero = m
totalSub Zero m = Zero
totalSub (SuccN m) (SuccN n) = totalSub m n


main = do
    putStrLn (unwords [ (assert (convert Zero == 0) "+"),
                        (assert (convert (SuccN Zero) == 1) "+"),
                        (assert (convert (SuccN (SuccN Zero)) == 2) "+"),
                        (assert (plus Zero Zero == Zero) "+"),
                        (assert (plus Zero (SuccN Zero) == SuccN Zero) "+"),
                        (assert (plus (SuccN Zero) (SuccN Zero) ==
                                 (SuccN (SuccN Zero))) "+"),
                        (assert (plus (SuccN Zero) Zero == SuccN Zero) "+"),
                        (assert (times Zero Zero == Zero) "+"),
                        (assert (times (SuccN Zero) Zero == Zero) "+"),
                        (assert (times (SuccN (SuccN Zero))
                                       (SuccN (SuccN (SuccN Zero)))
                                 == SuccN (SuccN (SuccN (SuccN (SuccN
                                    (SuccN Zero)))))) "+"),
                        (assert (totalSub Zero (SuccN Zero) == Zero) "+"),
                        (assert (totalSub (SuccN Zero) Zero == SuccN Zero) "+"),
                        (assert (totalSub (SuccN Zero) (SuccN (SuccN Zero)) ==
                                 Zero) "+")
                      ])
