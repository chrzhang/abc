a = "hello" .. "world"
print(a) -- "helloworld"
t = {n = 3}
t.n = t.n + 1
print(t.n) -- 4
-- multiple assignment
x = 3
a, b = 10, 2 * x
-- swap
x, y = 1, 2
x, y = y, x
print(x) -- 2
print(y) -- 1
-- extra variables are assigned null, extra values are ignored
a, b, c = 0, 1
print(c) -- nil
