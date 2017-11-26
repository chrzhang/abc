import System.IO
import Control.Exception

-- 4.2.1 Which of the following equations are true for all xs and which are
-- false?
-- [] : xs = xs
    -- False, the result will have an empty list prepended
-- [] : xs = [[], xs]
    -- False, xs does not get nested
-- xs : [] = xs
    -- False, because the result is [xs]
-- xs : [] = [xs]
    -- True
-- xs : xs = [xs, xs]
    -- False, the result is not valid because of the type of the : operator

-- 4.2.2 Which of the following equations are true for all xs and which are
-- false?
-- [[]] ++ xs = xs
    -- False, the result will have an empty list prepended
-- [[]] ++ xs = [xs]
    -- False, the result will have an empty list prepended
-- [[]] ++ xs = [[], xs]
    -- False, xs does not get nested
-- [[]] ++ [xs] = [[], xs]
    -- True
-- [xs] ++ [] = [xs]
    -- True
-- [xs] ++ [xs] = [xs, xs]
    -- True

-- 4.2.3 Prove by induction that xs ++ [] = xs for all lists xs.
-- Prove by induction over xs.
-- Case xs = undefined
    -- By case exhaustion, the result is undefined.
-- Case xs = []
    -- xs ++ []
    -- = [] ++ [] by replacement
    -- = [] by definition of ++
-- Case xs = (x:xs)
    -- Assume xs ++ [] = xs
    -- (x:xs) ++ []
    --- = x : (xs ++ []) by definition of ++
    -- = x : xs by the induction hypothesis
-- QED

-- 4.2.4 Prove that concat (xss ++ yss) = concat xss ++ concat yss.
-- Prove by induction over xs.
-- Case xss = undefined
    -- By case exhaustion, the result is undefined.
-- Case xss = []
    -- Left-hand side:
        -- concat (xss ++ yss)
        -- = concat ([] ++ yss) by replacement
        -- = concat yss by definition of ++
    -- Right-hand side:
        -- concat xss ++ concat yss
        -- = concat [] ++ concat yss by replacement
        -- = [] ++ concat yss by definition of concat
        -- = concat yss by definition of ++
-- Case xss = (xs:xss)
    -- Assume concat (xss ++ yss) = concat xss ++ concat yss
    -- Left-hand side:
        -- concat ((xs:xss) ++ yss)
        -- = concat (xs : (xss ++ yss)) by definition of ++
        -- = xs ++ concat (xss ++ yss) by definition of concat
        -- = xs ++ concat xss ++ concat yss by the induction hypothesis
    -- Right-hand side:
        -- concat (xs:xss) ++ concat yss
        -- = xs ++ concat xss ++ concat yss by definition of concat
-- QED

-- 4.2.5 Prove that length (xs ++ ys) = length xs + length ys.
-- Prove by induction over xs.
-- Case xs = undefined
    -- By case exhaustion, the result is undefined.
-- Case xs = []
    -- Left-hand side:
        -- length (xs ++ ys)
        -- = length ([] ++ ys) by replacement
        -- = length ys by definition of ++
    -- Right-hand side:
        -- length xs + length ys
        -- = length [] + length ys by replacement
        -- = 0 + length ys by definition of length
        -- = length ys by definition of +
-- Case xs = (x:xs)
    -- Assume length (xs ++ ys) = length xs + length ys
    -- Left-hand side:
        -- length ((x:xs) ++ ys)
        -- = length (x : (xs ++ ys)) by definition of ++
        -- = 1 + length (xs ++ ys) by definition of length
        -- = 1 + length xs + length ys by the induction hypothesis
    -- Right-hand side:
        -- length (x:xs) + length ys
        -- = 1 + length xs + length ys by definition of length
-- QED

-- 4.2.6 Prove that reverse xs = undefined for all infinite lists and
-- init xs = undefined for all infinite where init = reverse . tail . reverse
-- reverse (x:xs) = reverse xs ++ [x] is the definition of reverse
-- Since the list is infinite, xs will also be infinite and the process will
-- never terminate, thus being undefined.
-- Since the definition of init relies on application of reverse, the result
-- of init will also be undefined.

-- 4.2.7 Consider again the assertion reverse (reverse xs) = xs. Since
-- reverse (reverse undefined) = reverse undefined = undefined
-- it seems that the assertion is proved for all lists, not just the finite
-- ones. But one cannot reverse an infinite list (see the previous exercise),
-- so what has gone wrong?
-- The first claim that reverse (reverse xs) = xs is based on the premise that
-- the list is finite. The claim cannot be used without this premise and thus,
-- cannot be applied for infinite lists.

-- 4.2.8 Using pattern matching with (:), define a function rev2 that reverses
-- all lists of length 2, but leaves others unchanged. Ensure that the patterns
-- are exhaustive and disjoint.
rev2 :: [a] -> [a]
rev2 [] = []
rev2 (x : []) = [x]
rev2 (x : y : []) = [y, x]
rev2 (x : y : z : zs) = x:y:z:zs

-- 4.2.9 Give an informal characterisation of those finite lists xs and ys which
-- satisfy xs ++ ys = ys ++ xs.
-- xs ++ ys forms a palindrome.

-- 4.2.10 What is the value of [head xs] ++ tail xs when xs = []?
-- undefined since head xs will exhaust all cases

-- 4.2.11 Is it the case that (drop m xs) !! n = xs !! (m + n) for all
-- finite lists xs and all natural numbers m and n?
-- Yes and here is the proof.
-- Prove by induction over xs.
-- Case xs = undefined
    -- Through case exhaustion, the result will be undefined.
-- Case xs = []
    -- The result will be undefined because of the index operator.
-- Case xs = (x:xs)
    -- Assume (drop m xs) !! n = xs !! (m + n)
    -- Case m = 0 and n = 0
        -- Left-hand side:
            -- drop 0 (x:xs) !! 0
            -- = x
        -- Right-hand side:
            -- (x:xs) !! (0 + 0)
            -- = x
    -- Case m = 0 and n > 0
        -- Left-hand side:
            -- drop 0 (x:xs) !! n
            -- = (x:xs) !! n
        -- RIght-hand side:
            -- (x:xs) !! (0 +_n)
            -- = (x:xs) !! n
    -- Case m > 0 (This means m - 1 is >= 0)
        -- Left-hand side:
            -- (drop m (x:xs)) !! n
            -- = (drop m - 1 xs) !! n by definition of drop
        -- Right-hand side:
            -- (x:xs) !! (m + n)
            -- = (x:xs) !! (m - 1 + n + 1)
            -- = xs !! (m - 1 + n) by definition of !!
        -- Using the induction hypothesis, the left and right sides are equal.
        -- Because we are in case m > 0, m - 1 is a valid number covered in the
        -- cases. The induction hypothesis's m can be generalized to this
        -- equation's (m - 1) without loss of generality.
-- QED

-- 4.2.12 Prove that
-- (xs ++ ys) !! k = if k < n then xs !! k else ys !! (k - n)
--                   where n = length xs
-- Prove by induction over xs.
-- Case xs = undefined
    -- Through case exhaustion of !!, the result will be undefined.
-- Case xs = []
    -- In this case, n = 0, so k cannot be < n.
    -- Left-hand side:
        -- ([] ++ ys) !! k
        -- = ys !! k by definition of +
    -- Right-hand side:
        -- ys !! (k - 0)
        -- = ys !! k
-- Case xs = (x:xs)
    -- Assume
        -- (xs ++ ys) !! k = if k < n then xs !! k else ys !! (k - n)
        --                   where n = length xs
    -- ((x:xs) ++ ys) !! k
    -- = x:(xs ++ ys) !! k by definition of ++
    -- = xs ++ ys !! (k - 1) by definition of !!
    -- By the induction hypothesis, there are two cases:
    -- if k < n
        -- = xs !! (k - 1) by induction hypothesis
        -- = (x:xs) !! k by definition of !!
    -- else
        -- ys !! (k - 1 - n)
        -- = (y:ys) !! (k - n) by definition of !!
-- QED

-- 4.2.13 Since conatenation seems such a basic operation on lists, we can try
-- to construct a datatype that captures concatenation as a primitive. For
-- example,
data CatList a = Nil | Wrap a | Cat (CatList a) (CatList a)
-- The intention is that Nil represents [], and Wrap x represents [x], and
-- Cat xs ys represents xs ++ ys. However, since ++ is an associative operation,
-- the expressions
-- Cat xs (Cat ys zs) and Cat (Cat xs ys) zs
-- should be regarded as equal. Define appropriate instances of Eq and Ord for
-- CatList.
head' :: CatList a -> a
head' (Wrap a) = a
head' (Cat Nil b) = head' b
head' (Cat a b) = head' a

tail' :: CatList a -> CatList a
tail' (Wrap a) = Nil
tail' (Cat Nil b) = tail' b
tail' (Cat a b) = Cat (tail' a) b

instance (Eq a) => Eq (CatList a) where
    Nil == Nil           = True
    Nil == _             = False
    _ == Nil             = False
    Wrap a == Wrap b     = a == b
    Wrap a == Cat Nil c  = Wrap a == c
    Cat Nil c == Wrap a  = Wrap a == c
    Wrap a == Cat b Nil  = Wrap a == b
    Cat b Nil == Wrap a  = Wrap a == b
    Wrap a == Cat b c    = False
    Cat b c == Wrap a    = False
    Cat a b == Cat c Nil = (Cat a b) == c
    Cat c Nil == Cat a b = (Cat a b) == c
    Cat a b == Cat Nil c = (Cat a b) == c
    Cat Nil c == Cat a b = (Cat a b) == c
    Cat a b == Cat c d   = head' (Cat a b) == head' (Cat c d) &&
                           tail' (Cat a b) == tail' (Cat c d)

main :: IO ()
main = do
    putStrLn (unwords [ assert (rev2 ([] :: [Int]) == []) "+",
                        assert (rev2 [1] == [1]) "+",
                        assert (rev2 [1, 2] == [2, 1]) "+",
                        assert (rev2 [1, 2, 3] == [1, 2, 3]) "+",
                        assert (head' (Wrap 3) == 3) "+",
                        assert (head' (Cat (Wrap 3) (Wrap 4)) == 3) "+",
                        assert (head' (Cat Nil (Wrap 4)) == 4) "+",
                        assert ((Nil :: CatList Int) == Nil) "+",
                        assert (Wrap 1 == Wrap 1) "+",
                        assert (Wrap 1 /= Wrap 2) "+",
                        assert (Nil /= Wrap 2) "+",
                        assert (Wrap 2 /= Nil) "+",
                        assert (Wrap 1 == Cat Nil (Wrap 1)) "+",
                        assert (Wrap 1 /= Cat Nil (Wrap 2)) "+",
                        assert (Cat Nil (Wrap 1) == Wrap 1) "+",
                        assert (Cat Nil (Wrap 2) /= Wrap 1) "+",
                        assert (Wrap 2 /= Cat (Wrap 1) (Wrap 2)) "+",
                        assert (Cat (Wrap 1) (Wrap 2) == Cat (Wrap 1) (Wrap 2)) "+",
                        assert (Cat (Wrap 1) (Wrap 2) /= Cat (Wrap 2) (Wrap 2)) "+",
                        assert (Cat (Wrap 1) (Wrap 2) /= Cat Nil (Wrap 2)) "+",
                        assert (Cat (Wrap 1) (Wrap 2) == Cat Nil (Cat (Wrap 1) (Wrap 2))) "+",
                        assert (Cat (Wrap 1) (Wrap 2) == Cat (Cat (Wrap 1) (Wrap 2)) Nil) "+",
                        assert (Cat (Wrap 1) (Wrap 2) == Cat (Cat (Wrap 1) (Wrap 2)) Nil) "+",
                        assert (Cat (Wrap 1) (Wrap 2) == Cat (Cat (Cat (Wrap 1) (Wrap 2)) Nil) Nil) "+",
                        assert (Cat (Wrap 1) (Cat (Wrap 2) (Wrap 3)) == Cat (Cat (Wrap 1) (Wrap 2)) (Wrap 3)) "+"
                      ])
