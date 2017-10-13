showLen :: [a] -> String
showLen lst = (show (theLen)) ++ (if theLen == 1 then " item" else " items")
    where
    theLen = length lst
