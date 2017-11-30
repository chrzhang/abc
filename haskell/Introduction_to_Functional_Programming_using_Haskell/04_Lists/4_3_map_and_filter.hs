import System.IO
import Control.Exception

-- 4.3.1 Evaluate map (map square) [[1, 2], [3, 4, 5]].
-- [(map square [1, 2]), (map square [3, 4, 5])]
-- [            [1, 4] ,             [9, 16, 25]]

-- 4.3.2 What is the value of map f undefined?
-- undefined (case exhaustion)
-- What is the value of map undefined []?
-- [] (definition of map)

-- 4.3.3 What is the type of map map?
-- The argument to map map is a list, xs, by the definition of the first map.
-- map map xs
-- If we imagine xs as a list of, say 3, elements:
-- map map [a, b, c]
-- = [map a, map b, map c] by definition of map
-- Since the items of xs are the first inputs to map, they are of type
-- (a -> b) and the list as a whole is of type [ a -> b ]
-- What is returned is the type of [map a, map b, map c]
-- The type of the individual items can be found by looking at the type of map a
-- map a accepts a list and returns a list, i.e. [a] -> [b]
-- Thus, the overall type of the return is [ [a] -> [b] ]
-- And the type of map map is
-- [a -> b] -> [[a] -> [b]]

-- 4.3.4 Prove that map f (xs ++ ys) = map f xs ++ map f ys.
-- Prove by induction over xs.
-- Case xs = undefined
    -- map f (undefined ++ ys)
    -- = map f undefined by case exhaustion
    -- = undefined by case exhaustion
    -- map f undefined ++ map f ys
    -- = undefined ++ map f ys by case exhaustion
    -- = undefined by case exhaustion
-- Case xs = []
    -- Left-hand side:
        -- map f ([] ++ ys)
        -- = map f ys by definition of ++
    -- Right-hand side:
        -- map f [] ++ map f ys
        -- = [] ++ map f ys by definition of map
        -- = map f ys by definition of ++
-- Case xs = (x:xs)
    -- Assume map f (xs ++ ys) = map f xs ++ map f ys
    -- Left-hand side:
        -- map f ((x:xs) ++ ys)
        -- = map f (x:(xs ++ ys) by definition of ++
        -- = f x : map f (xs ++ ys) by definition of map
        -- = f x : map f xs ++ map f ys by replacing with the induction hypothesis
    -- Right-hand side:
        -- map f (x:xs) ++ map f ys
        -- = f x : map f xs ++ map f ys by definition of map
-- QED
-- Hence prove that map f . concat = concat . map (map f).
-- Recall that concat [] = [], concat (xs:xss) = xs ++ concat xss
-- Prove by induction over xs.
-- Case xs = undefined
    -- Left-hand side:
        -- map f . concat undefined
        -- = map f . undefined by case exhaustion
        -- = undefined by case exhaustion
    -- Right-hand side:
        -- concat . map (map f) undefined
        -- = concat . undefined by case exhaustion
        -- = undefined by case exhaustion
-- Case xs = []
    -- Left-hand side:
        -- map f . concat []
        -- = map f . [] by definition of concat
        -- = [] by definition of map
    -- Right-hand side:
        -- concat . map (map f) []
        -- = concat . [] by definition of map
        -- = [] by definition of concat
-- Case xs = (x:xs)
    -- Assume map f . concat xs = concat . map (map f) xs
    -- Left-hand side:
        -- map f . concat (x:xs)
        -- = map f . (x ++ (concat xs)) by definition of concat
        -- = f x : map f (concat xs) by definition of map
        -- = f x : (concat (map (map f) xs)) by replacing with the induction
        -- hypothesis
    -- Right-hand side:
        -- concat . map (map f) (x:xs)
        -- = concat . (map f x : map (map f) xs) by definition of map
        -- = map f x ++ concat map (map f) xs by definition of concat
        -- = f x : (concat map (map f) xs) by simplifying
-- QED

-- 4.3.5 The function inits computes the list of initial segments of a list; its
-- type is inits :: [a] -> [[a]]. What is the appropriate naturality condition
-- for inits?
-- map (map f) inits = inits map f

-- 4.3.6 The function filter can be defined in terms of concat and map. Give the
-- definition of box.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . map box
              where box x = if p x then [x] else []

-- 4.3.7 The useful functions takeWhile and dropWhile' are similar to take and
-- drop except that they both take a boolean function as first argument instead
-- of a natural number. In this respect, they are both similar to filter too.
-- The value takeWhile p xs is the longest initial segment of xs all of whose
-- elements satisfy p. For example
-- takeWhile even [2, 4, 6, 1, 5, 6] = [2, 4, 6]
-- The value dropWhile' p xs gives what remains; for example,
-- dropWhile' even [2, 4, 6, 1, 5, 6] = [1, 5, 6]
-- Give recursive definitions of takeWhile and dropWhile'.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs
                      else []
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs
                      else (x:xs)

-- 4.3.8 Under what conditions on xs and ys does the following equation hold?
-- [x | x <- xs, y <= ys] = [x | y <- ys, x <= xs]
-- xs and ys are of equal length or xs has exactly one unique element

-- 4.3.9 Define a function pairs so that pairs n is a list of all distinct pairs
-- of integers 1 <= x, y, <= n.
pairs' :: Int -> [(Int, Int)]
pairs' n = [(x, y) | x <- [1..n], y <- [x..n]]

-- 4.3.10 Write a program to find all essentially distinct quadruples
-- (a, b, c, d) in the range 0 < a, b, c, d <= n such that
-- a^2 + b^2 = c^2 + d^2.
sameSumSquareQuads :: Int -> [(Int, Int, Int, Int)]
sameSumSquareQuads n = [(a, b, c, d) | a <- [1..n], b <- [a..n],
                                       c <- [1..n], d <- [c..n],
                                       a^2 + b^2 == c^2 + d^2]

-- 4.3.11 Convert the following list comprehensions to combinatory style:
-- [(x, y) | x <- [1..n], odd x, y <- [1..n]]
-- [(x, y) | x <- [1..n], y <- [1..n], odd x]
-- Are they equal? Compare the costs of evaluating the two expressions.
-- Recall the generator rule, where Q is a qualifier:
    -- [e | x <- xs, Q] = concat (map f xs) where f x = [e | Q]
-- Recall the guard rule, where p is a guard:
   -- [e | p, Q] = if p then [e | Q] else []
-- Now, for the first list comprehension:
    -- [(x, y) | x <- [1..n], odd x, y <- [1..n]]
    -- [e      | x <- xs,     Q                 ] fits the generator rule
    -- = concat (map f [1..n]) where f x = [(x, y) | odd x, y <- [1..n]]
    --                                     [e      | p,     Q] fits the guard rule
    -- = concat (map f [1..n]) where f x = if odd x then [(x, y) | y <- [1..n]] else []
    --                                                   [e      | x <- xs, Q] fits the generator rule
    -- = concat (map f [1..n]) where f x = if odd x then (concat map f' [1..n] where f' x = [(x, y)]) else []
-- And for the second list comprehension:
    -- [(x, y) | x <- [1..n], y <- [1..n], odd x]
    -- [e      | x <- xs,     Q                 ] fits the generator rule
    -- = concat (map f [1..n]) where f x = [(x, y) | y <- [1..n], odd x]
    --                                     [e      | x <- xs,     Q    ] fits the generator rule
    -- = concat (map f [1..n]) where f x = concat (map f' [1..n]) where f' x = [(x, y) | odd x]
    --                                                                         [e      | p, Q] fits the guard rule
    -- = concat (map f [1..n]) where f x = concat (map f' [1..n]) where f' x = if odd x then [(x, y)] else []
-- The comprehensions are equal because they both generate a subset of all possible pairings of the numbers [1..n] where the first is odd
-- The order of the individual steps taken to achieve the result is what differs.
-- By filtering on odd x earlier in the first comprehension, there are fewer pairings generated so the first is less costly than the second,
-- which generates all possible pairings before filtering.


main :: IO ()
main = do
    putStrLn (unwords [ assert (map (map (^2)) [[1, 2], [3, 4, 5]] == [[1, 4], [9, 16, 25]]) "+",
                        assert (filter' (>3) [1..5] == [4, 5]) "+",
                        assert (filter' (<3) [1..5] == [1, 2]) "+",
                        assert (takeWhile' (\x -> x `mod` 2 == 0) [2, 4, 6, 1, 5, 6] == [2, 4, 6]) "+",
                        assert (dropWhile' (\x -> x `mod` 2 == 0) [2, 4, 6, 1, 5, 6] == [1, 5, 6]) "+",
                        assert (pairs' 3 == [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]) "+",
                        assert (sameSumSquareQuads 3 == [(1, 1, 1, 1), (1, 2, 1, 2), (1, 3, 1, 3),
                                                         (2, 2, 2, 2), (2, 3, 2, 3), (3, 3, 3, 3)]) "+"
                      ])
