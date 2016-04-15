t = {10, 20, 30}
for k,v in pairs(t) do
    print("key", k)
    print("value", v)
end

d, e, f = table.unpack(t)
print("d", d)
print("e", e)
print("f", f)

function fnc(a, b, c)
    print("Got", a, b, c)
end

fnc(table.unpack(t))
