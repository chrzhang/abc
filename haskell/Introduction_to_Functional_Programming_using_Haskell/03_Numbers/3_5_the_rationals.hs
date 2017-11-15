import System.IO()
import Control.Exception

data MyRational = MyRat Integer Integer

mkMyRat :: Integer -> Integer -> MyRational
mkMyRat x y = MyRat (u `div` d) (v `div` d)
              where u = (signum y) * x
                    v = abs y
                    d = gcd u v

instance Eq MyRational where
    MyRat x y == MyRat u v = (x * v) == (u * y)

instance Ord MyRational where
    MyRat x y < MyRat u v = (x * v) < (y * u)
    MyRat x y <= MyRat u v = MyRat x y == MyRat u v || MyRat x y < MyRat u v

showMyRat :: MyRational -> String
showMyRat (MyRat x y) = if y == 1 then show x else show x ++ "/" ++ show y

instance Num MyRational where
    MyRat x y + MyRat u v = mkMyRat (x * v + u * y) (y * v)
    MyRat x y - MyRat u v = mkMyRat (x * v - u * y) (y * v)
    MyRat x y * MyRat u v = mkMyRat (x * u) (y * v)
    negate (MyRat x y)    = mkMyRat (-x) y
    fromInteger x         = mkMyRat x 1
    abs (MyRat x y)       = if y == 1 then mkMyRat (abs x) 1 else
                            error "Cannot get abs"
    signum (MyRat x _)    = mkMyRat (signum x) 1

-- 3.5.1 Using the fact that for positive integers x and y , if x divides y and
-- y divides x, then x = y, prove that each rational number has a unique
-- representation (x, y) with y > 0 and gcd (x, y) = 1.
-- Prove by contradiction.
-- Consider a Rational with two representations: MyRat x y and MyRat u v
-- MyRat x y == MyRat u v
-- By the definition of == on Rational, this means:
-- x = (u * y) / v
-- x = u * (y / v) by associative property of * and /
-- => implies v divides y (in other words, no remainder)
-- u = (v * x) / y
-- u = x * (v / y) by associative property of * and /
-- => implies y divides v (in other words, no remainder)
-- Using the fact suggested, v = y.
-- Since v = y, v / y and y / v evaluate to 1, the left/right unit of * so:
-- x = u
-- This contradicts the fact that there can be different representations.
-- QED

-- 3.5.2 An integer x can be represented by a pair of integers (y, z) with
-- x = 10 * y + z. For example, 27 can be represented by (2, 7), (3, -3),
-- and (1, 17), among others. Among possible representations we can choose one
-- in which abs z < 5 and abs y is as small as possible (subject to abs z < 5).
-- Show that each integer has a unique representation of this form, and define
-- a function repint so that repint x returns this canonical representation.
-- Non-zero multiples of 5 cannot be expressed via this form!
-- To prove that if there is a representation of this form, then it is unique:
-- Prove by contradiction.
-- Case 1: y1 = y2 and z1 /= z2
    -- 10 * y1 + z1 = 10 * y2 + z2
    -- Since y1 = y2, this reduces to:
    -- z1 = z2, contradicting our current case.
-- Case 2: y1 /= y2 and z1 = z2
    -- 10 * y1 + z1 = 10 * y2 + z2
    -- Since z1 = z2, this reduces to:
    -- y1 = y2, contradicting our current case.
-- Case 3: y1 /= y2 and z1 /= z2
    -- 10 * y1 + z1 = 10 * y2 + z2
    -- 10 * y1 = 10 * y2 + z2 - z1
    -- Since abs z1 < 5 and abs z2 < 5,
    -- -10 < z2 - z1 < 10
    -- z2 - z1 is an integer in [-9, 9]
    -- y1 = y2 + (z2 - z1) / 10
    -- The only way for y1, an integer, to be defined in terms of adding
    -- a difference to another integer y2 is if that difference is also an
    -- integer. Hence (z2 - z1) / 10 must be an integer and this only occurs
    -- if (z2 - z1) = 0. Reducing this equation yields:
    -- z2 = z1, contradicting our current case.
-- QED

repint :: Integer -> (Integer, Integer)
repint x = (y, z)
           where y = (x `div` 10) + offset
                 offset = if x `mod` 10 < 5 then 0 else 1
                 z = x - (10 * y)

main :: IO ()
main = do
    putStrLn (unwords [ (assert (MyRat 1 2 == MyRat 1 2) "+"),
                        (assert (MyRat 11 22 == MyRat 1 2) "+"),
                        (assert (MyRat 1 2 < MyRat 2 3) "+"),
                        (assert (MyRat 2 3 > MyRat 1 2) "+"),
                        (assert (showMyRat (MyRat 1 2) == "1/2") "+"),
                        (assert (showMyRat (MyRat 123 1) == "123") "+"),
                        (assert (mkMyRat 2 4 == MyRat 1 2) "+"),
                        (assert (MyRat 3 4 + MyRat 1 2 == MyRat 5 4) "+"),
                        (assert (MyRat 3 4 - MyRat 1 2 == MyRat 1 4) "+"),
                        (assert (MyRat 3 4 * MyRat 1 2 == MyRat 3 8) "+"),
                        (assert (negate (MyRat 1 5) == MyRat (-1) 5) "+"),
                        (assert (fromInteger 12 == MyRat 12 1) "+"),
                        (assert (repint 27 == (3, -3)) "+"),
                        (assert (repint 14 == (1, 4)) "+"),
                        (assert (repint 19 == (2, -1)) "+"),
                        (assert (repint 2 == (0, 2)) "+"),
                        (assert (repint 23 == (2, 3)) "+")
                      ])
