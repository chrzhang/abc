local i = 0
while i < 10 do
    ::redo:: -- a goto label
    i = i + 1
    print(i)
    if i % 2 then goto continue
    elseif n then goto redo
    end
    ::continue::
    i = i + 1
end
