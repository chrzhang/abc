--[[
^
not # - (unary)
* / %
+ -
..
< > <= >= ~= ==
and
or

all binary operators are left associative except ^ and ..

a + i < b/2 + 1 -- (a + i) < ((b / 2) + 1)
5 + x^2 * 8 -- 5 + ((x^2) * 8)
a < y and y <= z -- (a < y) and (y <= z)
-x^2 -- -(x^2)
x^y^z -- x^(y^z)

]]--

print(2 ^ 3 ^ 4) -- 2 ^ (3 ^ 4) = 2 ^ 81
print(2 ^ 81)

(x and y and (not z)) or ((not y) and x) -- parens are not needed but helpful
