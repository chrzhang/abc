-- 1.3.1 Suppose we define multiply by
multiply :: (Integer, Integer) -> Integer
multiply (x, y) = if x == 0 then 0 else x * y
-- The symbol == is used for an equality test between two integers. Assume that
-- evaluation of e1 == e2 proceeds by reducing e1 and e2 to normal form and
-- testing whether the two results are identical. Under lazy evaluation,
-- what would be the value of multiply (0, infinity)?
-- 0 since y need not be evaluated.
-- What would be the value of multiply (infinity, 0)?
-- undefined

-- 1.3.2 Suppose we define the function h by the equation h x = f (g x). Show
-- that if f and g are both strict, then so is h.
-- Assume f and g are both strict. Let b stand for undefined. Since f and g are
-- both strict, f b = b and g b = b.
-- h x = f (g x) means h b = f (g b)
-- h b = f (b)
-- h b = b
-- Thus h is strict.

infinity :: Integer
infinity = infinity + 1

main = do
    putStrLn(show (multiply (0, infinity)))
    putStrLn(show (multiply (infinity, 0)))
