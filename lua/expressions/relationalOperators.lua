a = 3
b = "3"
print(a == b) -- different types are not equal
print(a ~= b) -- true
-- references are considered equal only if they refer to the same object
a = {}
b = {}
print(a == b) -- false
b = a
print(a == b) -- true
-- remember comparing strings is alphabetic even if they contain numbers
