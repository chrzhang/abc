import System.IO
import Control.Exception

type Mass = Double
type Position = (Double, Double, Double) --x, y, z
type Body = (Mass, Position)

calcMassesOverDists :: [Body] -> [Double]
calcMassesOverDists bodies = map (\body -> sum (calcMMoD body bodies)) bodies

calcMMoD :: Body -> [Body] -> [Double]
calcMMoD body bodies = map (mMoDHelper body) bodies

mMoDHelper :: Body -> Body -> Double
mMoDHelper (m1, p1) (m2, p2) = if p1 == p2 then 0 else m1 * m2 / (distXYZ p1 p2)

distXYZ :: Position -> Position -> Double
distXYZ (x1, y1, z1) (x2, y2, z2) = sqrt (xd ^ 2 + yd ^ 2 + zd ^ 2)
    where
    (xd, yd, zd) = (x1 - x2, y1 - y2, z1 - z2)

main = do
    let result = calcMassesOverDists [(1, (0, 0, 0)),
                                      (2, (1, 1, 1)),
                                      (3, (-1, -1, -1))]
    putStrLn (show (2 / (sqrt 3) + (3 / (sqrt 3))))
    putStrLn (show (2 / (sqrt 3) + (6 / (sqrt 12))))
    putStrLn (show (3 / (sqrt 3) + (6 / (sqrt 12))))
    putStrLn (show result)
