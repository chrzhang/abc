a = -1
b = 5

if a < 0 then a = 0 end -- else is optional

if a < b then print(a) else print(b) end -- 0

if a == 0 then -- equivalent of switch statements in other languages
    print("a")
elseif a == 1 then
    print("b")
else
    print("c")
end
