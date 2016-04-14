-- can be used as a statement or an expression
print(8 * 9, 9 / 8)
a = math.sin(3) + math.cos(10)
print(os.date())
-- if a function has one single argument that is a string or table ctor, parens
-- are optional
print "Hello world!"
print(type {})

-- number of parameters does not have to match number of arguments
-- extra arguments are tossed, just like in multiple assignment
function fnc(n)
    n = n or 1
    print(n)
end

fnc() -- 1
fnc(10) -- 10
fnc(10, 20) -- 10
