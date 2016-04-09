-- length operator for strings (gets # bytes) and tables (length of sequence)
a = {}
print(a[#a]) -- get last value of sequence a (nil)
a[1] = 10
a[2] = 20
print(a[#a]) -- 20
a[#a] = nil -- remove last value
print(a[#a]) -- 10
-- a sequence is a table where the numeric keys comprise 1,...,n
-- length is unreliable for lists with holes so store the length as a separate
-- variable if needed
