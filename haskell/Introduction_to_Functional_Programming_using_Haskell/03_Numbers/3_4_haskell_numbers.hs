import System.IO
import Control.Exception

-- 3.4.1 What is the value of 3 mod (-4)?
-- 3 = (3 div (-4)) * (-4) + (3 mod (-4))
-- 3 = (floor (-3/4)) * (-4) + (3 mod (-4))
-- 3 = (-1) * (-4) + (3 mod (-4))
-- 3 = 4 + (3 mod (-4))
-- -1 = 3 mod (-4)

-- 3.4.2 Show that when y is negative, y < x mod y <= 0
-- Recall x mod y = x - (x div y) * y
-- Since x / y >= floor (x / y) by the definition of floor,
-- x / y * y <= floor (x / y) * y (Notice the flipping of signs since y is < 0)
-- x / y * y <= (x div y) * y
-- x <= (x div y) * y
-- -x >= -((x div y) * y)
-- (-x) + x >= x - ((x div y) * y)
-- 0 >= x - ((x div y) * y)
-- 0 >= x mod y
-- As for x mod y being > y, that is a consequence of the definition of
-- division.

-- 3.4.3 Is it the case that floor (floor x) = floor x?
-- Yes since floor is the largest integer n such that n <= x,
-- floor (floor x) = floor n = n where n is an integer

main = do
    putStrLn (unwords [ (assert (3 `mod` (-4) == (-1)) "+")
                      ])
