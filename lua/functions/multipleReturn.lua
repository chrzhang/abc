function foo()
    return "a", "b" -- return more than one value
end

x, y = foo() -- depending on the context of foo() call, values may be ignored
print(x)
print(y)

-- a function call that is not the last element only produces one result
x, y = foo(), "c"
print(x)
print(y) -- c

-- to force a function to return one value, enclose in parens
x, y = (foo())
print(x)
print(y) -- nil
