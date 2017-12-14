module KnotHash
( knothash ) where

import qualified Data.List
import qualified Data.List.Split
import qualified Data.Bits
import qualified Data.Char
import qualified Numeric

rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0 = take len $ drop amt $ cycle xs
    | n < 0 = drop (len - (-amt)) xs ++ take (len - (-amt)) xs
    | otherwise = error "Cannot rotate."
    where len = length xs
          amt = n `rem` len

revrot :: Int -> [a] -> Int -> [a]
revrot l xs i = rotate reved (-i)
                where len = length xs
                      shift = take len $ drop i $ cycle xs
                      reved = (reverse $ take l shift) ++ drop l shift

data ListState = ListState {ci :: Int, xss :: [Int], sk :: Int, lss :: [Int],
                            li :: Int}

next :: ListState -> ListState
next liststate = ListState nci nxs nsk (lss liststate) nli
    where nxs = revrot currlen (xss liststate) (ci liststate)
          nci = (ci liststate + currlen + sk liststate) `mod`
                (length $ xss liststate)
          nsk = sk liststate + 1
          nli = li liststate + 1
          currlen = (lss liststate) !! (li liststate)

finalstate :: [Int] -> [Int] -> ListState
finalstate a b = (last sts)
                  where sts = takeUntil (\x -> li x < length (lss x)) sts'
                        sts' = iterate next (ListState 0 a 0 b 0)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

finallengths :: String -> [Int]
finallengths xs = map Data.Char.ord xs ++ [17, 31, 73, 47, 23]

sparsehash :: [Int] -> [Int] -> [Int]
sparsehash xs ls = xss fs
                   where rounds = concat $ replicate 64 ls
                         fs = finalstate xs rounds

densehash :: [Int] -> [Int]
densehash xs
    | length xs /= 256 = error "Sparse hash length != 256"
    | otherwise = map (\c -> foldr (Data.Bits.xor) 0 c) chunks
                  where chunks = Data.List.Split.chunksOf 16 xs

tohex :: [Int] -> String
tohex xs = concat $ map (\x -> pad $ Numeric.showHex x "") xs
           where pad s = if length s == 1 then '0':s else s

knothash :: String -> String
knothash x = tohex dh
             where ls = finallengths x
                   sh = sparsehash [0..255] ls
                   dh = densehash sh
