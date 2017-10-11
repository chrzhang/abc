fib :: (Num a, Num b, Eq a) => a -> b
fib n = fibGen 0 1 n

fibGen :: (Num a, Num b, Eq a) => b -> b -> a -> b
fibGen a _ 0 = a
fibGen a b n = fibGen b (a + b) (n - 1)
