import System.IO()
import Control.Exception

data MyList a = MyNil | MyCons a (MyList a)

instance (Eq a) => Eq (MyList a) where
    MyNil == MyNil             = True
    MyNil == MyCons y ys       = False
    MyCons x xs == MyNil       = False
    MyCons x xs == MyCons y ys = (x == y) && (xs == ys)

my_null :: MyList a -> Bool
my_null MyNil = True
my_null (MyCons x xs) = False

-- 4.1.1 Give an example of an expression that contains two occurrences of the
-- empty list, the first occurrence having type [Bool] and the second having
-- type [Char].
both_null :: MyList Char -> MyList Bool -> Bool
both_null xs ys = (my_null xs) && (my_null ys)

-- 4.1.2 Complete the following instance declaration:
instance (Ord a) => Ord (MyList a) where
-- Recall from Chapter 2 that, for the particular type [Char], the ordering on
-- strings is lexicographic. The general definition of <= on lists should be
-- lexicographic too.
    MyNil < MyNil = False
    MyNil < MyCons y ys = True
    MyCons x xs < MyNil = False
    MyCons x xs < MyCons y ys = if x == y then xs < ys else x < y
    xs <= ys = (xs < ys) || (xs == ys)

-- 4.1.3 Consider the following two definitions of the function last that
-- return the last element of a nonempty list:
my_last :: MyList a -> a
my_last MyNil = error "List is empty."
my_last (MyCons x xs) = if my_null xs then x else my_last xs
my_last' :: (Eq a) => MyList a -> a
my_last' MyNil = error "List is empty."
my_last' (MyCons x xs) = if xs == MyNil then x else my_last' xs
-- The difference between my_last and my_last' is that they have slightly
-- different types. Since the definition of my_last' involves an explicit
-- equality test, its type is restricted to lists whose elements are drawn
-- from an equality type. Give an expression e such that the evaluator
-- response differently to last e and last' e.
-- For a type without Eq defined
data Color = Red | Blue
-- my_last will work here:
    -- my_last (MyCons Red MyNil)
-- but my_last' will not work
    -- my_last' (MyCons Red MyNil)

-- 4.1.4 The dual view of lists is to construct them by adding elements to the
-- end of the list:
data MyList' a = MyNil' | MySnoc (MyList' a) a
instance (Eq a) => Eq (MyList' a) where
    MyNil' == MyNil'           = True
    MyNil' == MySnoc ys y      = False
    MySnoc xs x == MyNil'      = False
    MySnoc xs x == MySnoc ys y = (x == y) && (xs == ys)
-- MySnoc, of course, is just Cons backwards. With this view of lists, [1, 2, 3]
-- would be represented by an element of MyList' Int by
-- MySnoc (MySnoc (MySnoc MyNil' 1) 2) 3
-- Exactly the same information is provided by the two views, but it is
-- organised differently. For example, the function my_head, which returns the
-- first element of a nonempty list, is easy to define with the datatype
-- MyList a, but more complicated with MyList' a. Give the definitions of
-- my_head for the two types.
my_head :: MyList a -> a
my_head MyNil = error "List is empty."
my_head (MyCons x xs) = x
my_head' :: MyList' a -> a
my_head' MyNil' = error "List is empty."
my_head' (MySnoc MyNil' x) = x
my_head' (MySnoc xs x) = my_head' xs
-- Give a function for converting from one datatype to the other.
my_convert :: MyList' a -> MyList a
my_convert MyNil' = MyNil
my_convert (MySnoc xs x) = cat (my_convert xs) (MyCons x MyNil)
    where cat MyNil MyNil = MyNil
          cat xs MyNil = xs
          cat MyNil ys = ys
          cat (MyCons x xs) ys = MyCons x (cat xs ys)
my_convert' :: MyList a -> MyList' a
my_convert' MyNil = MyNil'
my_convert' (MyCons x xs) = cat (MySnoc MyNil' x) (my_convert' xs)
    where cat MyNil' MyNil' = MyNil'
          cat xs MyNil' = xs
          cat MyNil' ys = ys
          cat xs (MySnoc ys y) = MySnoc (cat xs ys) y

main :: IO ()
main = do
    putStrLn (unwords [
        (assert (MyCons 1 MyNil == MyCons 1 MyNil) "+"),
        (assert (MyCons 1 MyNil /= MyCons 0 MyNil) "+"),
        (assert (MyCons 1 (MyCons 2 MyNil) == MyCons 1 (MyCons 2 MyNil)) "+"),
        (assert (MyCons 1 (MyCons 2 MyNil) /= MyCons 1 (MyCons 0 MyNil)) "+"),
        (assert (both_null MyNil MyNil) "+"), -- 4.1.1
        (assert (MyCons 1 (MyCons 2 MyNil) < MyCons 2 (MyCons 1 MyNil)) "+"),
        (assert (MyCons 1 (MyCons 2 MyNil) < MyCons 2 (MyCons 2 MyNil)) "+"),
        (assert (MyCons 1 (MyCons 2 MyNil) < MyCons 2 (MyCons 3 MyNil)) "+"),
        (assert (MyCons 3 (MyCons 2 MyNil) < MyCons 3 (MyCons 3 MyNil)) "+"),
        (assert (MyCons 2 MyNil < MyCons 2 (MyCons 1 MyNil)) "+"),
        (assert (my_last (MyCons 2 MyNil) == 2) "+"),
        (assert (my_last (MyCons 2 (MyCons 3 MyNil)) == 3) "+"),
        (assert (my_last' (MyCons 2 MyNil) == 2) "+"),
        (assert (my_last' (MyCons 2 (MyCons 3 MyNil)) == 3) "+"),
        (assert (my_head (MyCons 2 (MyCons 3 MyNil)) == 2) "+"),
        (assert (my_head (MyCons 2 MyNil) == 2) "+"),
        (assert (my_head' (MySnoc MyNil' 2) == 2) "+"),
        (assert (my_head' (MySnoc (MySnoc MyNil' 2) 3) == 2) "+"),
        (assert (my_convert (MySnoc (MySnoc MyNil' 2) 3) ==
                             MyCons 2 (MyCons 3 MyNil)) "+"),
        (assert (my_convert' (MyCons 2 (MyCons 3 MyNil)) ==
                              MySnoc (MySnoc MyNil' 2) 3) "+")

        ])
