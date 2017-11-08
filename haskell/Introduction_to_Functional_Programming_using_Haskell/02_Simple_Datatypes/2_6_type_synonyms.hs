import System.IO
import Control.Exception

-- 2.6.1 Suppose you wanted to treat two distances as equal if they were less
-- than 10 miles apart. Can you define an equality test on Distance, when
-- Distance is a type synonym? If you can, can you call it (==)?
-- Yes I can define an equality test on Distance.
type Distance = Float
distEq :: Distance -> Distance -> Bool
distEq dist1 dist2 = diff < 10
    where diff = abs(dist1 - dist2)
-- No I cannot call it (==) since that is already defined for the underlying
-- Float type. However, if Distance were not a type synonym but a newtype
-- declaration, this would be possible.
newtype OtherDistance = MkDistance Float
instance Eq OtherDistance where
    MkDistance x == MkDistance y = distEq x y

-- 2.6.2 Consider the declarations
data Jane = MkJane Int
newtype Dick = MkDick Int
-- By defining appropriate functions, demonstrate that Jane and Dick are
-- different from one another.
janefunc :: Jane -> Int
janefunc (MkJane x) = x
-- Open GHCi, and create a variable of type Jane
-- let x = MkJane 4
-- Do the same for Dick
-- let y = MkDick 2
-- Running janefunc x is okay but janefunc y cannot be run.

main = do
    putStrLn (unwords [ (assert (distEq 1 1) "+"),
                        (assert (distEq 1 2) "+"),
                        (assert (not (distEq 1 11)) "+"),
                        (assert (MkDistance 1 == MkDistance 1) "+"),
                        (assert (MkDistance 1 == MkDistance 2) "+"),
                        (assert (MkDistance 1 /= MkDistance 11) "+"),
                        (assert (janefunc (MkJane 3) == 3) "+")
                      ])
