-- 4.4.1 What is unzip [undefined]?
-- unzip [undefined]
-- = pair (map fst, map snd) [undefined]
-- = (undefined, undefined)
-- Using the fact that (undefined, undefined) /= undefined, construct a list
-- xys such that zipp (unzip xys) /= xys.
-- If xys = [undefined, undefined],
-- zipp (unzip [undefined, undefined])
-- = zipp undefined
-- = undefined by case exhaustion

-- 4.4.2 Does the converse equation unzip . zipp = id hold?
-- No because zipp [] undefined = [] and unzip [] = ([], [])

-- 4.4.3 Provided certain other laws are assumed, the equation
-- cross (map f, map g) . unzip = unzip . map (cross (f, g))
-- can be proved by simple calculation with functions. The necessary additional
-- laws are
-- 1) map (f . g) = map f . map g
-- 2) cross (f, g) . pair (h, k) = pair (f . h, g . k)
-- 3) pair (f, g) . h = pair (f . h, g . h)
-- 4) fst . cross (f, g) = f . fst
-- 5) snd . cross (f, g) = g . snd
-- Use these laws, together with the definition of unzip, to prove the required
-- result.
-- Left-hand side:
    -- cross (map f, map g) . unzip
    -- = cross (map f, map g) . pair (map fst, map snd) by definition of unzip
    -- = pair (map f . map fst, map g . map snd) by (2)
    -- = pair (map (f . fst), map (g . snd)) by (1)
    -- = pair (map (fst . cross (f, g)), map (snd . cross (f, g))) by (3)
-- Right-hand side:
    -- unzip . map (cross (f, g))
    -- = pair (map fst, map snd) . map (cross (f, g)) by definition of unzip
    -- = pair (map fst . map (cross (f, g)), map snd . map (cross (f, g))) by (3)
    -- = pair (map (fst . cross (f, g)), map (snd . cross (f, g))) by (1)
-- QED

main :: IO ()
main = do
    return ()
