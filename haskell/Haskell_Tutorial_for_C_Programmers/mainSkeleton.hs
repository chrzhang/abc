import System.IO

main = do
    putStr "Enter a number: "
    hFlush stdout
    a <- getLine
    putStr "Enter another number: "
    hFlush stdout
    b <- getLine
    putStrLn (show (read a :: Int))
    putStrLn (show (read b :: Int))
