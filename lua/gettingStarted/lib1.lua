--[[ Use this library to compare using -llib1 as a flag to the interpreter
     or calling dofile("lib1.lua") in the interpreter. dofile is more on-the-fly
     and doesn't require restarting the interpreter. -llib1 is arguably cleaner
     and more portable (so a script won't need to call dofile on each library
     it might possibly need). ]]--

function norm(x, y)
    return (x^2 + y^2)^0.5
end

function twice(x)
    return 2 * x
end
