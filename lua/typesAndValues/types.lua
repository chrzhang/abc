print(type("Hello World")) -- string
print(type(10.4 * 3)) -- number
print(type(print)) -- function
print(type(type)) -- function
print(type(true)) -- boolean
print(type(nil)) -- nil
print(type(type(X))) -- string

-- Variables changing type
print(type(x)) -- nil
x = 10
print(type(x))
x = "hello"
print(type(x))
x = print -- functions are first-class
x(type(x))

-- nil is different from any other value (assign nil back to var to delete it)

-- nil and false are both considered false
-- 0 and "" are true

-- number represents real doubles
-- no integral type
-- IEEE 754 means errors happen when a number can't be represented exactly
-- any # up to 2^53 has an exact representation as a double
-- Lua's doubles can hold any 32-bit integer correctly
-- fractional numbers can have rounding errors 
x = 4
x = 0.4
x = 4.57e-3
x = 0.3e12
x = 5E+20
x = 0xff
x = 0x1A3
x = 0x0.2
x = 0x1p-1
x = 0xa.bp2

-- strings hold any binary data (incl. Unicode)
-- immutable so changes need to be done to a copy
a = "one string"
b = string.gsub(a, "one", "another")
print(a)
print(b)
-- get len with #
print(#a)
print(#b)

-- literal strings
a = "a line"
b = 'another line'
-- either " or ' are fine, however inside each you can use the other without
-- needing escapes
a = "a 'line'"
b = 'another "line"'

print(type(nil)==nil) -- false because a string is not nil
