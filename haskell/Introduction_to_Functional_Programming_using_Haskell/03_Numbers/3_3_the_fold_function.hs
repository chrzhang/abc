import System.IO
import Control.Exception

data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (Succ n) = h (foldn h c n)

-- 3.3.1 Prove that foldn Succ Zero n = n for all elements n of Nat, finite,
-- partial, or infinite.
-- Induct over n.
-- Case n = Zero:
    -- foldn Succ Zero n
    -- = foldn Succ Zero Zero by substitution
    -- = Zero by pattern matching the first definition of foldn
-- Case n = n:
    -- Assume foldn Succ Zero n = n
    -- Evaluate if foldn Succ Zero (Succ n) = Succ n
    -- foldn Succ Zero (Succ n)
    -- = Succ (foldn Succ Zero n) by matching the second definition of foldn
    -- = Succ n by replacing with our assumption
-- QED

-- 3.3.2 Use the fusion law to prove that + is commutative.
-- Recall that the fusion law states
-- f . foldn g a = foldn h b
-- if f is strict, f a = b, and f . g = h . f
-- Let's evaluate w + n where n = foldn Succ Zero n
-- (w +) foldn Succ Zero
-- To see if we can use the fusion law, check each of the 3 conditions.
-- 1) strict        => w + bottom = bottom due to case exhaustion.
-- 2) f a = b       => w + Zero = w = b
-- 3) f . g = h . f => (w +) Succ n = Succ (w + n)
-- Because of these conditions, the fusion law states
-- foldn h b => foldn Succ w
-- Now let's see if foldn Succ w n = n + w
-- Induct over n.
-- Case n = Zero:
    -- foldn Succ w Zero
    -- = w by matching first definition of foldn
    -- Since Zero is a left unit of +, Zero + w = Zero
-- Case n = n:
    -- Assume foldn Succ w n = n + w
    -- Evaluate if foldn Succ w (Succ n) = (Succ n) + w
    -- The left-hand side can be evaluated:
    -- Succ (foldn Succ w n)
    -- = Succ (n + w) by replacing with our assumption
    -- = (Succ n) + w by definition of +
-- QED

-- 3.3.3 Division of natural numbers can be specified by the condition that
-- (n * m) / n = m for all positive n and all m. Construct a program for
-- division and prove that it meets the specification.
-- Induct over m to find conditions that define /
-- Case m = Zero:
    -- (n * Zero) / n
    -- = Zero / n
    -- Zero / n = Zero is our first rule of division
-- Case m = m
    -- Assume (n * m) / n = m
    -- n * Succ m / n = Succ m
    -- n * Succ m / n = Succ m
    -- = ((n * m) + n) / n = Succ m
    -- Based on our assumption, replace the right-hand side
    -- = ((n * m) + n) / n = Succ (n * m / n)
    -- Twiddle the right-hand side further
    -- = ((n * m) + n) / n = Succ (n * m + n - n / n)
    -- Replace (n * m) + n with m
    -- = m / n = Succ ((m - n) / n) is our second rule of division
my_sub :: Nat -> Nat -> Nat
my_sub m Zero = m
my_sub (Succ m) (Succ n) = m `my_sub` n

my_div :: Nat -> Nat -> Nat
my_div Zero _ = Zero
my_div m n = Succ ((m `my_sub` n) `my_div` n)

-- 3.3.4 The function log can be specified by the condition that log (2 ^ m) = m
-- for all m. Construct a program for log and prove that it meets the
-- specification.
-- Induct over m to find conditions that define log
-- Case m = Zero:
    -- log (2 ^ Zero) = Zero
    -- log (Succ Zero) = Zero is our first rule of log
-- Case m = m:
    -- Assume log (2 ^ m) = m
    -- log (2 ^ Succ m) = Succ m
    -- Recall the definition of ^
        -- m ^ Zero = Succ Zero
        -- m ^ Succ n = (m ^ n) * m
    -- log (2 ^ m * 2) = Succ m
    -- Twiddle the right-hand side by replacing with our assumption
    -- log (2 ^ m * 2) = Succ (log (2 ^ m))
    -- Replace 2 ^ m * 2 by m
    -- log m = Succ (log (m / 2)) is our second rule of log
my_log :: Nat -> Nat
my_log (Succ Zero) = Zero
my_log m = Succ (my_log (m `my_div` Succ (Succ Zero)))

main = do
    putStrLn (unwords [ (assert (my_div Zero (Succ Zero) == Zero) "+"),
                        (assert (my_div (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ Zero)) == Succ (Succ Zero)) "+"),
                        (assert (my_log (Succ Zero) == Zero) "+"),
                        (assert (my_log (Succ (Succ Zero)) == Succ Zero) "+"),
                        (assert (my_log (Succ (Succ (Succ (Succ Zero)))) == Succ (Succ Zero)) "+")
                      ])
