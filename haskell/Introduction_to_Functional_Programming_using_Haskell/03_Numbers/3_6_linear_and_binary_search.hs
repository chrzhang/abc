import System.IO()
import Control.Exception

my_until :: (a -> Bool) -> (a -> a) -> a -> a
my_until p f x = if p x then x else my_until p f (f x)

floor_linear :: Float -> Integer
floor_linear x = searchFrom 0
    where searchFrom = decrease . upper . lower
          lower      = my_until (\a -> (realToFrac a) <= x) decrease
          upper      = my_until (\a -> (realToFrac a) > x) increase
          decrease n = n - 1
          increase n = n + 1

floor_binary :: Float -> Integer
floor_binary x = searchFrom (-1, 1)
    where searchFrom = fst . middle . cross (lower, upper)
          cross (f, g) (a, b) = (f a, g b)
          double = (2 *)
          lower = my_until (\a -> (realToFrac a) <= x) double
          upper = my_until (\a -> (realToFrac a) > x) double
          middle = my_until done improve
          done (m, n) = m + 1 == n
          improve (m, n) = if (realToFrac p) <= x then (p, n) else (m, p)
                           where p = (m + n) `div` 2

-- 3.6.1 Justify the claim that the second program for floor x takes
-- 2 log_2 (abs x) steps.
-- It takes log_2 (abs x) to find the lower and upper bounds since each
-- lower and upper step multiplies by 2. The improve routine then
-- repeatedly divides by 2 until the bound is as close to x without exceeding
-- it. The repeated division also takes exactly log_2 (abs x) steps.
-- The total is therefore 2 log_2 (abs x) steps.

-- 3.6.2 Let p = (m + n) div 2. Show that m < p < n provided m + 1 < n. What
-- happens if m + 1 = n?
-- (1) m < n since m + 1 < n
-- Given that p = (m + n) / 2,
-- 2p = m + n
-- m + n < 2n since m + 1 < n
-- 2p < 2n
-- (2) p < n
-- Since m + 1 < n,
-- n = m + 1 + q where q is a positive integer
-- m + n = m + (m + 1 + q)
-- m + n = 2m + 1 + q
-- 2p = 2m + 1 + q
-- p = m + 1/2 + q/2
-- (3) m < p
-- Given (1), (2), and (3), m < p < n
-- If m + 1 = n, then m < p < n does not always hold.
-- For example, m = 4, n = 5 would mean p = 9 div 2 = 4, thus p = m

my_sqrt :: Float -> Float
my_sqrt x = until done improve x
    where done y = abs (y * y - x) < eps
          eps = 0.0001
          improve y = (y + x / y) / 2 -- Newton's method

-- 3.6.3 In Newton's method, the test for determining whether an approximation
-- y to sqrt(x) is good enough was taken to be
-- abs(y * y - x) < eps
-- Another test is
-- abs(y * y - x) < eps * x
-- Rewrite the sqrt function to use this test.
my_sqrt' :: Float -> Float
my_sqrt' x = until done improve x
    where done y = abs (y * y - x) < eps * x
          eps = 0.0001
          improve y = (y + x / y) / 2 -- Newton's method

-- 3.6.4 Yet another test for convergence is to stop when two successive
-- approximations y and y' are sufficiently close:
-- abs (y - y') < eps * abs y
-- Rewrite the definition of sqrt to use this new test. Give reasons why these
-- new tests are likely to be superior in practice.
my_sqrt'' :: Float -> Float
my_sqrt'' x = until done improve x
    where done y = abs (y - improve y) < eps * abs y
          eps = 0.0001
          improve y = (y + x / y) / 2 -- Newton's method

main :: IO ()
main = do
    putStrLn (unwords [ (assert (floor_linear 1 == 1) "+"),
                        (assert (floor_linear 1.1 == 1) "+"),
                        (assert (floor_linear 1.2 == 1) "+"),
                        (assert (floor_linear 1.5 == 1) "+"),
                        (assert (floor_linear 1.9 == 1) "+"),
                        (assert (floor_linear 10.1 == 10) "+"),
                        (assert (floor_linear 10.5 == 10) "+"),
                        (assert (floor_linear 10.9 == 10) "+"),
                        (assert (floor_linear 10.999 == 10) "+"),
                        (assert (floor_binary 1 == 1) "+"),
                        (assert (floor_binary 1.9 == 1) "+"),
                        (assert (floor_binary 1.8 == 1) "+"),
                        (assert (floor_binary 1.5 == 1) "+"),
                        (assert (floor_binary 1.3 == 1) "+"),
                        (assert (floor_binary 1.1 == 1) "+"),
                        (assert (floor_binary 10.9 == 10) "+"),
                        (assert (floor_binary 12.22 == 12) "+"),
                        (assert (floor_binary 34.4 == 34) "+"),
                        (assert (floor_binary 2129.01 == 2129) "+"),
                        (assert (my_sqrt 2 == 1.4142157) "+"),
                        (assert (my_sqrt' 2 == 1.4142157) "+"),
                        (assert (my_sqrt'' 2 == 1.4142157) "+")
                      ])
