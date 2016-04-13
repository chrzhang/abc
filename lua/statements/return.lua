local a = { [1] = "1", [2] = "2", [3] = "3", [4] = "4" }
local i = 1
while a[i] do
    if (a[i] == "3") then
        print("Match!")
        return i
    end
    i = i + 1
end
-- return only allowed at the end of block
-- to return prematurely, put return in a do structure
do return end
print("I will not appear")
