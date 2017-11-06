import System.IO
import Control.Exception

data MyEither = MyLeft Bool | MyRight Char

-- 2.5.1 Define a function with source type Either Bool Char that behaves
-- differently on the three arguments Left Bottom, Right Bottom, and Bottom
some_func :: MyEither -> Int
some_func (MyLeft undefined) = 1
some_func (MyRight undefined) = 2
-- some_func undefined will cause an exception which is different behavior


-- 2.5.2 Prove that case (f, g) . plus (h, k) = case (f . h, g . k)
data MyEitherG a b = MyLeftG a | MyRightG b

my_case :: (a -> c, b-> c) -> MyEitherG a b -> c
my_case (f, g) (MyLeftG x) = f x
my_case (f, g) (MyRightG y) = g y

my_plus :: (a -> c, b -> d) -> MyEitherG a b -> MyEitherG c d
my_plus (f, g) = my_case (MyLeftG . f, MyRightG . g)
-- case (f . h, g . k) Either a b
-- Consider Either a b is Left a
-- case (f . h, g . k) Left a
-- = f . h . a
-- Consider Either a b is Right b
-- case (f . h, g . k) Right b
-- = g . k . b
--case (f, g) . plus (h, k) Either a b
-- = case (f, g) . case (Left . h, Right . k) Either a b
-- Consider Either a b is Left a
-- = case (f, g) . case (Left . h, Right . k) Left a
-- = case (f, g) . Left . h . a
-- = f . h . a
-- Consider Either a b is Right b
-- = case (f, g) . case (Left . h, Right . k) Right b
-- = case (f, g) . Right . k . b
-- = g . k . b
-- QEDJ

main = do
    let testResults = [ (assert (some_func (MyLeft undefined) == 1) "+"),
                        (assert (some_func (MyRight undefined) == 2) "+")
                      ]
    putStrLn (unwords testResults)
