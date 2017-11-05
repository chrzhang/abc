import System.IO
import Control.Exception

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
           deriving (Eq, Ord, Enum)

workday :: Day -> Bool
workday d = d >= Mon && d <= Fri

restday :: Day -> Bool
restday d = not (workday d)

dayAfter :: Day -> Day
dayAfter d = toEnum ((1 + fromEnum d) `mod` 7)

-- 2.3.1 Define a function dayBefore that returns the day before a given day.
dayBefore :: Day -> Day
dayBefore Sun = Sat
dayBefore d   = toEnum (fromEnum d - 1)

-- 2.3.2 Define a datatype Direction whose values describe the four major points
-- of the compass, and define a function reverse for reversing direction.
data Direction = N | E | W | S
                 deriving (Eq)
my_reverse :: Direction -> Direction
my_reverse d
    | d == N = S
    | d == S = N
    | d == E = W
    | d == W = E

-- 2.3.3 Declare Bool as a member of the type class Enum by giving an explicit
-- instance declaration.
data MyBool = MyFalse | MyTrue
instance Enum MyBool where
    fromEnum MyFalse = 0
    fromEnum MyTrue = 1
    toEnum 0 = MyFalse
    toEnum 1 = MyTrue

main = do
    let testResults = [ (assert (dayBefore Mon == Sun) "+"),
                        (assert (dayBefore Tue == Mon) "+"),
                        (assert (dayBefore Wed == Tue) "+"),
                        (assert (dayBefore Thu == Wed) "+"),
                        (assert (dayBefore Fri == Thu) "+"),
                        (assert (dayBefore Sat == Fri) "+"),
                        (assert (dayBefore Sun == Sat) "+"),
                        (assert (my_reverse N == S) "+"),
                        (assert (my_reverse E == W) "+"),
                        (assert (my_reverse W == E) "+"),
                        (assert (my_reverse S == N) "+")
                      ]
    putStrLn (unwords testResults)
