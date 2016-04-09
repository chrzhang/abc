-- and, or, not
print(4 and 5) -- returns first argument if false, else the second argument
print(nil and 13) -- returns first argument
print(false and 13) -- returns false
print(4 or 5) -- 4
print(false or 5) -- 5
-- both and + or use lazy evaluation
x = 3
v = 4
x = x or v -- idiom to set x to a default value v when x is not set
(a and b) or c -- idiom like the ternary operator
a = 5
b = 6
max = (a > b) and a or b -- parens are unnecessary (and has greater precedence)
print(max)
-- not is negation resulting in a boolean
print(not nil)
print((a == true) or (a == false)) -- without using type, can check if a value is a boolean
