coeffs = {1, 2, 3, 4, 5}
x =  2
result = 0
for key,value in pairs(coeffs) do
    result = result + value * x^(key - 1)
end
print(result)
print(1 + 2 * x + 3 * x^2 + 4 * x^3 + 5 * x^4)
