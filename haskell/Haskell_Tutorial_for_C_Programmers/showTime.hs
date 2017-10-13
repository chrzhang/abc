showTime :: Int -> Int -> String
showTime hours minutes
    | hours == 0 = "12:" ++ showMin ++ " am"
    | hours < 12 = (show hours) ++ ":" ++ showMin ++ " am"
    | hours == 12 = "12:" ++ showMin ++ " pm"
    | otherwise = (show (hours - 12)) ++ ":" ++ showMin ++ " pm"
    where
    showMin
        | minutes < 10 = "0" ++ show minutes
        | otherwise = show minutes
