import System.IO
import Control.Exception

square :: Integer -> Integer
square x = x * x

infinity :: Integer
infinity = infinity + 1

-- 1.2.1 In order to evaluate x * y, the expressions x and y are reducted to
-- normal form and then multiplication is performed. Does evaluation of square
-- infinity terminate?
-- Yes, GHC will print out <<loop>> because it detected an infinite loop.

-- 1.2.2 How many terminating reduction sequences are there for the expression
-- square (3 + 4)?
-- 2
-- 1) (3 + 4) * (3 + 4) => 7 * (3 + 4) => 7 * 7 => 49
-- 2) square 7 => 7 * 7 => 49

-- 1.2.3 Imagine a language of expressions for representing integers defined by
-- the syntax rules: (i) zero is an expression; (ii) if e is an expression,
-- then so are succ (e) and pred (e). An evaluator reduces expressions in this
-- language by applying the following rules repeatedly until no longer possible:
-- succ (pred (e)) = e
-- pred (succ (e)) = e
-- Simplify the expression succ (pred (succ (pred (pred (zero))))).
-- pred zero
-- In how many ways can the reduction rules be applied to this expression? 3
-- 1)
-- succ (pred (succ (pred (pred (zero)))))
--            (succ (pred (pred (zero))))
--                         pred (zero)
-- 2)
-- succ (pred (succ (pred (pred (zero)))))
-- succ             (pred (pred (zero)))
--                         pred (zero)
-- 3)
-- succ (pred (succ (pred (pred (zero)))))
-- succ (pred             (pred (zero)))
--                         pred (zero)
-- Prove that the process of reduction must terminate for all given expressions.
-- Let e be an expression.
-- If e is zero, e is normal since the reduction rules are not possible.
-- Consider the following cases:
-- If e is succ (f) where f is reduced, then e is normal.
-- If e is succ (f) where f is not reduced, and f is pred (g) then the
-- reduction rule will apply and shorten e to g.
-- The converse for e being pred (f) and f being succ (g) is also true since
-- the reduction rules are interchangeable.
-- Since the reduction rules are applied only when it is possible to shorten
-- the length and e is a finite size, reduction must terminate.

-- 1.2.4 Carrying on from the previous question, suppose an extra syntactic rule
-- is added to the language: (iii) if e1 and e2 are expressions, then so is
-- add(e1, e2). The corresponding reduction rules are
-- add (zero, e2) = e2
-- add (succ (e1), e2) = succ (add (e1, e2))
-- add (pred (e1), e2) = pred (add (e1, e2))
-- Simplify the expression add (succ (pred (zero)), zero)
-- Count the number of different ways the reduction rules can be applied to the
-- above expression. Do they always lead to the same final result?
-- 1)
-- add (succ (pred (zero)), zero)
-- add (           (zero) , zero)
--                          zero
-- 2)
-- add (succ (pred (zero)), zero)
-- succ (add (pred (zero)), zero)
-- succ (pred (add (zero), (zero)))
-- succ (pred zero)
-- zero

-- 1.2.5 Now suppose we define the size of an expression by the following rules:
-- size (zero) = 1
-- size (succ (e)) = 1 + size (e)
-- size (pred  (e)) = 1 + size (e)
-- size (add (e1, e2)) = 1 + 2 * (size (e1) + size(e2))
-- Show that the application of any of the five reduction rules given above
-- reduces expression size. Why does this prove that the process of reduction
-- must always terminate for any given initial expression?
-- 1) succ (pred (e)) = e
--      size (succ (pred (e))) => 1 + size (pred (e)) => 1 + 1 + size (e)
--      => 2 + size(e) which decreases when it gets reduced to size(e)
-- 2) pred (succ (e)) = e
--      size (pred (succ (e))) => 1 + size (succ (e)) => 1 + 1 + size (e)
--      => 2 + size(e) which decreases when it gets reduced to size(e)
-- 3) add (zero, e2) = e2
--      => size (add (zero, e2)) => 1 + 2 * (size (zero) + size (e2))
--      => 1 + 2 * (1 + size (e2)) => 1 + 2 + 2 * size (e2) which decreases when
--      it gets reduced to size (e2)
-- 4) add (succ (e1), e2) = succ (add (e1, e2))
--      => size (add (succ (e1), e2))
--      => 1 + 2 * (size (succ (e1)) + size (e2))
--      => 1 + 2 * ((1 + size (e1)) + size (e2))
--      => 1 + 2 + 2 * size (e1) + 2 * size (e2)
--      => 3 + 2 * size (e1) + 2 * size (e2)
--      which decreases when it gets reduced to
--      => size (succ (add (e1, e2)))
--      => 1 + size (add (e1, e2))
--      => 1 + 1 + 2 * (size (e1) + size (e2))
--      => 2 + 2 * size (e1) + 2 * size (e2)
-- 5) add (pred (e1), e2) = pred (add (e1, e2))
--      => size (add (pred (e1), e2))
--      => 1 + 2 * (size (pred (e1)) + size (e2))
--      => 1 + 2 * ((1 + size (e1)) + size (e2)))
--      => 1 + 2 + 2 * size (e1) + 2 * size (e2)
--      => 3 + 2 * size (e1) + 2 * size (e2)
--      which decreases when it gets reduced to
--      => size (pred (add (e1, e2)))
--      => 1 + size (add (e1, e2))
--      => 1 + 1 + 2 * (size (e1) + size (e2))
--      => 2 + 2 * size (e1) + 2 * size (e2)
-- This proves that the reduction must terminate since the termination is
-- possible only if it is not possible to apply any more reduction rules.
-- Since every reduction rule can only decrease the expression size, termination
-- is assured either by the expression being too short to reduce further or
-- running out of further ways to reduce.


main = do
    putStrLn (show (square infinity))
