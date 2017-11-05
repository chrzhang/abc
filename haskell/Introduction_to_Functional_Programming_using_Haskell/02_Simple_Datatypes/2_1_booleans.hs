import System.IO
import Control.Exception
import Prelude

-- 2.1.1 Define conjunction and disjunction using conditional expressions.
conjunction :: Bool -> Bool -> Bool
conjunction x y = if x == False then False else
                  if y == False then False else
                  True
disjunction :: Bool -> Bool -> Bool
disjunction x y = if x == True then True else
                  if y == True then True else
                  False

-- 2.1.2 The definition of (<) given in the text for arguments of type Bool has
-- False < True. This reflects the fact that False comes before True in the
-- declaration of the values of type Bool. Naturally, one also expects that both
-- False < False and True < True evaluate to False.
-- Now consider the alternative definition (x < y) = not x V y, where V is
-- defined by pattern matching on the left argument. Why is this definition of
-- (<) not correct?
-- If x was False and y was False, the left argument would evaluate to True and
-- pattern matching would short circuit (x < y) into True.

-- 2.1.3 In logic, implication, denoted by =>, is defined by the condition that
-- x => y is false only if x is true and y is false. Give a formal definition of
-- implication as an operation on Bool.
implication :: Bool -> Bool -> Bool
implication True False = False
implication _ _ = True

-- 2.1.4 Rewrite the declaration of the type Eq by giving a default definition
-- of (!=).
class MyEq a where
    equality :: a -> a -> Bool
    inequality :: a -> a -> Bool
    inequality x y = not (equality x y)

-- 2.1.5 Rewrite the definition of analyse so that the cases in the case
-- analysis do not depend on the order in which they are given.
data Triangle = Failure | Isosceles | Equilateral | Scalene
instance Eq Triangle where
    Failure == Failure = True
    Isosceles == Isosceles = True
    Equilateral == Equilateral = True
    Scalene == Scalene = True
    _ == _ = False
instance Show Triangle where
    show Failure = "Failure"
    show Isosceles = "Isosceles"
    show Equilateral = "Equilateral"
    show Scalene = "Scalene"

analyse :: (Int, Int, Int) -> Triangle
analyse (x, y, z)
    | x + y <= z = Failure
    | (x + y > z) && x == y && y == z = Equilateral
    | (x + y > z) && ((x == y && y /= z) ||
                      (x == z && z /= y) ||
                      (y == z && x /= z)) = Isosceles
    | (x + y > z) && x /= y && y /= z && x /= z = Scalene

-- 2.1.6 Define a function sort3 that sorts three integers into nondecreasing
-- order. Hence define a function, analyse' say, that does not depend on the
-- assumption that its arguments are in nondecreasing order.
sort3 :: (Int, Int, Int) -> (Int, Int, Int)
sort3 (x, y, z)
    | x <= y && y <= z = (x, y, z)
    | x <= z && z <= y = (x, z, y)
    | y <= x && x <= z = (y, x, z)
    | y <= z && z <= x = (y, z, x)
    | z <= x && x <= y = (z, x, y)
    | z <= y && y <= x = (z, y, x)
analyse' :: (Int, Int, Int) -> Triangle
analyse' (x, y, z) = analyse (sort3 (x, y, z))

-- 2.1.7 How many equations would you have to write to define Triangle as an
-- instance of the type class Ord?
-- There are 4 * 4 possible pairings of Triangle values in <, from which the
-- default operations can be formed. But first, Triangle needs to be made an
-- instance of the Eq class and that requires 5 equations. In total, 4 * 4 + 5
-- = 21.

-- 2.1.8 Are there any numbers that can be compared by (==) but cannot sensibly
-- be compared by <?
-- Comparing infinite numbers may not make sense. Imaginary numbers can also
-- not be compared against real numbers.

-- 2.1.9 The definition of (==) on a datatype should ensure that this operation
-- is: (i) reflexive, that is, x == x for all x; (ii) transitive, that is,
-- x == y and y == z imply x == z; and (ii) symmetric, that is x == y implies
-- y == x. Show that these properties hold for the definition of (==) on Bool.
-- Show x == x for all x
-- Let x be True.
-- True == True
-- = (True ^ True) v (not True ^ not True)
-- = (True) v (False ^ False)
-- = True v False
-- = True
-- Let x be False.
-- False == False
-- = (False ^ False) v (not False ^ not False)
-- = False v (True ^ True)
-- = False v True
-- = True
-- Show x == y and y == z => x == z
-- Assume x == y and y == z.
-- Since x == y, we know y is True if x is True and y is False if x is False
-- because we just proved == is reflexive.
-- Since y == z, we know z is True if y is True and z is False if y is False
-- because we just proved == is reflexive.
-- Since x == y then x == z.
-- Show x == y => y == x
-- Assume x == y.
-- Expand the expressions on either side to their definitions, which are
-- comprised of ^ and v operations.
-- Since the ^ and v operations do not care about the order of their arguments,
-- y == x.

-- 2.1.10 What properties of (<) would you expect to hold in any instance
-- declaration?
-- a < b => b >= a
-- !(a < b) && !(b > a) => b == a

main = do
    let testResults = [ (assert (disjunction True True == True) "+"),
                        (assert (disjunction True False == True) "+"),
                        (assert (disjunction False True == True) "+"),
                        (assert (disjunction False False == False) "+"),
                        (assert (conjunction True True == True) "+"),
                        (assert (conjunction True False == False) "+"),
                        (assert (conjunction False True == False) "+"),
                        (assert (conjunction False False == False) "+"),
                        (assert (implication True False == False) "+"),
                        (assert (implication True True == True) "+"),
                        (assert (implication False True == True) "+"),
                        (assert (implication False False == True) "+"),
                        (assert (Failure == analyse (1, 2, 3)) "+"),
                        (assert (Equilateral == analyse (5, 5, 5)) "+"),
                        (assert (Isosceles == analyse (2, 2, 3)) "+"),
                        (assert (Scalene == analyse (5, 10, 12)) "+"),
                        (assert ((1, 2, 3) == sort3 (1, 2, 3)) "+"),
                        (assert ((1, 2, 3) == sort3 (1, 3, 2)) "+"),
                        (assert ((1, 2, 3) == sort3 (2, 3, 1)) "+"),
                        (assert ((1, 2, 3) == sort3 (2, 1, 3)) "+"),
                        (assert ((1, 2, 3) == sort3 (3, 2, 1)) "+"),
                        (assert ((1, 2, 3) == sort3 (3, 1, 2)) "+")
                      ]
    putStrLn (unwords testResults)
