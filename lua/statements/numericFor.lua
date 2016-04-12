for var = 1, 10 do -- third argument (step) is optional
    print(var)
end
for var = 1, 10, 2 do -- third argument (step) is optional
    print(var)
end
for var = 1, math.huge do -- infinite loop
    print(var)
    if (var > 10) then break end
end
