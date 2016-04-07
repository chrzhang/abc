print("one line\nnextline\n\"in quotes\", 'in quotes'")
print('a backlash inside quotes: \'\\\'')
print("a simpler way: '\\'")

-- Use [[ <text> ]] to ignore escape sequences
page = [[
<html>
<head>
    <title>HTML Page</title>
</head>
<body>Body</body>
</html>
]]
print(page)

-- Fill any # of ='s between [ and [ if text might contain ]] incidentally
page = [===[
a = b[c[i]]
]===]
print(page)

--[=[
    --[[The same applies for comments.]]--
]=]--

xml = [=[
<![CDATA[
    Hello world
]]>
]=]
print(xml)
