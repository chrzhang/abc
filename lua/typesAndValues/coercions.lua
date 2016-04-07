print("10" + 1) -- 11
print("10 + 1") -- 10 + 1
print("-5.3e-10"*"2") -- -1.06e09
-- print("hello" + 1) will have an error
-- number to string will happen depending on context
print(10 .. 20)  -- .. for string concatenation becomes "1020"
-- remember 10 == "10" is false and coercions are unreliable
-- for valid conversions:
-- better to use tonumber which will return nil if unable to convert
-- tostring is the converse (or concatenate number with "")
