j = 10 -- global
local i = 1 -- scope limited to enclosing block
x = 10
local i = 1
while i <= x do
    local x = i * 2
    print(x) -- multiples of 2
    i = i + 1
end
print(x) -- 10 since out of scope and is original x
if true then
    local x
    x = 20
    print(x + 2) -- 22
else
    print(x) -- 10
end
-- good practice to use locals if possible (faster, less clutter)
local foo = foo -- create a local copy of the global (preserve original global)
