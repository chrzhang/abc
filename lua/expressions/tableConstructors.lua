a = {}
days = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"}
print(days[4])
a = {x=10, y=20} -- a["x"] or a.x = 10
polyline = {color="blue", -- combine list and key initialization
            thickness=2,
            npoints=4,
            {x = 0, y = 0}, -- polyline[1]
            {x = -10, y = 0},
            {x = -10, y = 1},
            {x = 0, y = 1}
            }
print(polyline[2].x) -- -10
print(polyline[4].y) -- 1
-- the above ways of initializing tables does not allow negative indices
-- explicitly specify keys as follows
opnames = {["+"] = "add",
           ["-"] = "sub",
           ["*"] = "mul",
           ["/"] = "div"}
i = 20
s= "-"
print(opnames[s]) -- "sub"
a = {[i + 0] = s, [i + 1] = s..s, [i + 2] = s..s..s}
print(a[22]) -- "---"

sunday = "monday"
monday = "sunday"
t = {sunday = "monday", [sunday] = monday}
-- t["sunday"] = "monday"
-- t["monday"] = "sunday"
for key,value in pairs(t) do
    print(key, value)
end
-- t = {"monday" = "monday", "monday" = "sunday"}
print(t.sunday, t[sunday], t[t.sunday])
-- t.sunday is t["sunday"] or "monday"
-- t[sunday] is t["monday"] or "sunday"
-- t[t.sunday] is t["monday"] is "sunday"
