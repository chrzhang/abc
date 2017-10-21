import System.IO
import Control.Exception

type Mass = Double
type Position = (Double, Double, Double) --x, y, z
type Body = (Mass, Position)

{-
Input: two Positions
Return: distance between the positions
-}
distXYZ :: Position -> Position -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd ^ 2 + yd ^ 2 + zd ^ 2)
    where
    (xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)

{-
Input: a list of Body objects
Return: a list of (sum of ((mass times other object mass) over distance))
Preserves order of input.
-}
calcMassesOverDists :: [Body] -> [Double]
calcMassesOverDists objs = calcHelper objs objs

calcMMoD :: Body -> [Body] -> [Double]
calcMMoD o1@(m1, p1) ((m2, p2):rest) = safeValue : calcMMoD o1 rest
    where
    dist = distXYZ p1 p2
    safeValue = if p1 == p2 then 0 else m1 * m2 / dist
calcMMoD _ [] = []

calcHelper :: [Body] -> [Body] -> [Double]
calcHelper [] _ = []
calcHelper (o:os) objs = (sum (calcMMoD o objs)) : calcHelper os objs

main = do
    let result = calcMassesOverDists [(1, (0, 0, 0)),
                                      (2, (1, 1, 1)),
                                      (3, (-1, -1, -1))]
    putStrLn (show (2 / (sqrt 3) + (3 / (sqrt 3))))
    putStrLn (show (2 / (sqrt 3) + (6 / (sqrt 12))))
    putStrLn (show (3 / (sqrt 3) + (6 / (sqrt 12))))
    putStrLn (show result)
