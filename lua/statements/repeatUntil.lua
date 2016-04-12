local i = 1
repeat -- body always executed once (do-while)
    print(i)
    i = i + 1
until i == 10
-- scope of the condition includes inner block
repeat
    print("hi")
    local x = i + 1
until x == 11
