module IslandCounter
( countislands ) where

import Data.List (nub, (\\))

type Cell = (Int, Int)
type Grid = [String]

activecells :: Grid -> [Cell]
activecells g = [(snd r, snd c) | r <- zip g [0..],
                                  c <- zip (fst r) [0..],
                                  fst c == '1']

regionof :: Cell -> Grid -> [Cell]
regionof (r, c) g = fst $ regionof' (r, c) g []

regionof' :: Cell -> Grid -> [Cell] -> ([Cell], [Cell])
regionof' (r, c) g v
    | r > maxrow || r < 0 = ([], v)
    | c > maxcol || c < 0 = ([], v)
    | elem (r, c) v = ([], v)
    | (g !! r) !! c /= '1' = ([], v)
    | otherwise = ((r, c) : (cnr ++ csr ++ cer ++ cwr),
                   nub $ nv ++ cnv ++ csv ++ cev ++ cwv)
    where maxrow = length g - 1
          maxcol = length (g !! 0) - 1
          nv = (r, c) : v
          (cnr, cnv) = regionof' (r - 1, c) g nv
          (csr, csv) = regionof' (r + 1, c) g cnv
          (cer, cev) = regionof' (r, c + 1) g csv
          (cwr, cwv) = regionof' (r, c - 1) g cev

countislands :: Grid -> Int
countislands g = countislands' 0 ac
    where ac = activecells g
          countislands' c [] = c
          countislands' c a = countislands' (c + 1) remain
                              where remain = a \\ (regionof (head a) g)
