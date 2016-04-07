-- tables are associative arrays indexable by #s and values (but not nil)
-- only data structuring mechanism in Lua
-- can represent arrays, sets, records, other DS simply
-- represent packages as well
-- tables are objects, not values or variables
-- can be thought of as dynamically allocated (and never copied implicitly)
a = {} -- make table and store a reference in the variable a
k = "x"
a[k] = 10
a[20] = "great"
print(a["x"]) -- or a[k] should be 10
k = 20
print(a[k]) -- now a[20] or "great"
a["x"] = a["x"] + 1 -- a["x"] is now 11
print(a["x"])
-- tables are always anonymous
-- no fixed relationship b/t variables and table itself
a = {}
a["x"] = 10
b = a -- there is still only one table
print(b["x"]) -- 10
b["x"] = 20
print(a["x"]) -- 20
-- when no references remain to the table, the table si deleted
a = {}
for i = 1, 1000 do a[i] = i * 2 end
print(a[9]) -- 18
a["x"] = 10
print(a["x"]) -- 10
print(a["y"]) -- nil
a["x"] = nil -- deletes table entry
-- a.name is syntactic sugar for a["name"]
a.x = 10
print(a.x)
print(a.y)
-- a.x is NOT a[x]

-- for an array or list, use a table with integer keys (no way to declare size)
-- Lua convention is to index arrays from 1

-- for tables without nil elements, the length operator returns the last index
-- of the sequence which can be used for size

a = {}
a.a = a -- a["a"] = a
-- So a.a.a.a = a.a.a == a.a == a
print(a.a.a.a == a) -- true
a.a.a.a = 3
-- So a.a.a.a = 3 => a.a = 3 => a["a"] = 3
print(a["a"]) -- 3
