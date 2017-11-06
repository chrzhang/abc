import System.IO
import Control.Exception

-- 2.4.1 Prove that cross (f, g) . cross (h, k) = cross (f . h, g . k)
-- cross (f . h, g . k) (x, y)
-- = pair (f . h . fst, g . k . snd) (x, y)
-- = (f . h . fst (x, y), g . k . snd (x, y))
-- = (f . h . x         , g . k . y)
-- cross (f, g) . cross (h, k) (x, y)
-- = cross (f, g) . pair (h . fst, k . snd) (x, y)
-- = cross (f, g) . (h . x, k . y)
-- = pair (f . fst, g . snd) . (h . x, k . y)
-- = (f . h . x         , g . k . y)
-- QED


-- 2.4.2 Give a datatype declaration of a type Triple that corresponds to
-- triplets.
data Triple a b c =  MkTriple a b c
                     deriving (Eq)

valid_date :: Triple Int Int Int -> Bool
valid_date (MkTriple d m y)
    | d < 1 = False
    | d > 31 = False
    | m < 1 = False
    | m > 12 = False
    | otherwise = True

compare_dates :: Triple Int Int Int -> Triple Int Int Int -> Int
compare_dates (MkTriple d1 m1 y1) (MkTriple d2 m2 y2)
    | not (valid_date (MkTriple d1 m1 y1)) = error "Invalid date input."
    | not (valid_date (MkTriple d2 m2 y2)) = error "Invalid date input."
    | y1 < y2 = (-1)
    | y1 > y2 = 1
    | m1 < m2 = (-1)
    | m1 > m2 = 1
    | d1 < d2 = (-1)
    | d1 > d2 = 1
    | otherwise = 0

-- 2.4.3 Suppose a date is represented by a triple (d, m, y) of three integers,
-- where d is the day, m is the month, and y the year. Define a function age
-- that takes two dates, the first being the current date, and the second being
-- the birthdate of some person P, and returns the age of P as a whole number
-- of years.
age :: Triple Int Int Int -> Triple Int Int Int -> Int
age (MkTriple currD currM currY) (MkTriple birthD birthM birthY)
    | compare_dates (MkTriple currD currM currY)
                    (MkTriple birthD birthM birthY) == -1
      = error "Person not born yet."
    | otherwise = currY - birthY + offset
      where offset = if currM < birthM then (-1) else
                     if currM == birthM && currD < birthD then (-1) else 0

-- 2.4.4 Is it possible to declare (a, b) as a member of the type class Enum,
-- given that both a and b are instances of Enum?
-- No. From https://www.haskell.org/onlinereport/derived.html: Derived instance
-- declarations for the class Enum are only possible for enumerations (data
-- types with only nullary constructors). Since (a, b) takes arguments, this
-- cannot be done.

-- Demonstrate the nullary type ()
pifun :: () -> Float
pifun () = 3.14159

main = do
    let testResults = [ (assert ((MkTriple 1 2 3) == MkTriple 1 2 3) "+"),
                        (assert ((MkTriple 1 2 3) /= MkTriple 2 2 3) "+"),
                        (assert (age (MkTriple 5 11 1705)
                                     (MkTriple 5 11 1605) == 100) "+"),
                        (assert (age (MkTriple 5 10 1705)
                                     (MkTriple 5 11 1605) == 99) "+"),
                        (assert (age (MkTriple 4 11 1705)
                                     (MkTriple 5 11 1605) == 99) "+"),
                        (assert (age (MkTriple 5 12 1605)
                                     (MkTriple 5 11 1605) == 0) "+"),
                        (assert (pifun () == 3.14159) "+")
                      ]
    putStrLn (unwords testResults)
