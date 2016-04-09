-- expressions include numeric constants, string literals, variables,
-- operations, function calls, function definitions, table constructors
a = 25 ^ 0.5 -- sqrt(25) = 5
print(a)
-- a % b == a - math.floor(a/b) * b
a = 3.14159
print(a % 1) -- get the fractional part of a number
print(a - a % 1) --  get the integer part of a number
print(a - a % 0.01) -- get only 2 decimal digits
-- print -10 to 10, each element followed by its remainder when div by 3
for i = -10, 10 do
    print(i, i % 3)
end
