import System.IO
import Control.Exception
import Data.List

-- 2.7.1 Put the following strings in ascending order:
sortNames :: [String]
sortNames = sort ["McMillan", "Macmillan", "MacMillan"]

-- 2.7.2 What are the values of the following expressions?
-- show (show 42)
-- show 42 ++ show 42
-- show "\n"
-- See assertions.

-- 2.7.3 Suppose a date is represented by a triple (d, m, y) of three Int
-- integers, where d is the day, m the month, and y the year. Define a function
-- showDate that takes a date and prints the corresponding date. For example,
--     showDate (10, 12, 1997) prints as "10 December, 1997"
-- As a more complicated version, revise the definition so that
--     showDate (10, 12 1997) prints as "10th December, 1997"
--     showDate (31, 12, 1997) prints as "31st December, 1997"
type Date = (Int, Int, Int)
showDate :: Date -> String
showDate (d, m, y) = show d ++ suffix ++ " " ++ monthStr ++ ", " ++ show y
    where monthStr = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November",
                      "December"] !! (m - 1)
          suffix
            | d `mod` 10 == 1 = "st"
            | d `mod` 10 == 2 = "nd"
            | d `mod` 10 == 3 = "rd"
            | otherwise = "th"

main = do
    putStrLn (unwords [ (assert (sortNames == ["MacMillan", "Macmillan", "McMillan"]) "+"),
                        (assert (show (show 42) == "\"42\"") "+"),
                        (assert (show 42 ++ show 42 == "4242") "+"),
                        (assert (show "\n" == "\"\\n\"") "+"),
                        (assert (showDate (1, 12, 1997) == "1st December, 1997") "+"),
                        (assert (showDate (2, 12, 1997) == "2nd December, 1997") "+"),
                        (assert (showDate (3, 12, 1997) == "3rd December, 1997") "+"),
                        (assert (showDate (4, 12, 1997) == "4th December, 1997") "+"),
                        (assert (showDate (5, 12, 1997) == "5th December, 1997") "+"),
                        (assert (showDate (6, 12, 1997) == "6th December, 1997") "+"),
                        (assert (showDate (7, 12, 1997) == "7th December, 1997") "+"),
                        (assert (showDate (8, 12, 1997) == "8th December, 1997") "+"),
                        (assert (showDate (9, 12, 1997) == "9th December, 1997") "+"),
                        (assert (showDate (10, 12, 1997) == "10th December, 1997") "+"),
                        (assert (showDate (31, 12, 1997) == "31st December, 1997") "+")
                      ])
