fib :: Integer -> Integer
fib n = fibGen 0 1 n

fibGen :: Integer -> Integer -> Integer -> Integer
fibGen a b n = case n of
    0 -> a
    n -> fibGen b (a + b) (n - 1)

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]
