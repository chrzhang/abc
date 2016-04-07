-- factorial function def
function fact(n)
    if n == 0 then
        return 1
    elseif n < 0 then
        print("Input was negative. Error.")
        return -1
    else
        return n * fact(n - 1)
    end
end

print("Enter a number:")
x = io.read("*n")
print(fact(x))
