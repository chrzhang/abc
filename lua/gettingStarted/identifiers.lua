-- Identifiers/names for stuff in Lua can have letters, digits, and underscores
-- but cannot start with a digit

--[[
--- is not valid because the beginning two '--'s will make the rest of the line
a comment
]]--

_end = 3
print(_end)

End = 4;
print(End)

--[[
end is a reserved word and thus cannot be used as an identifier
]]--

--[[
until? has an illegal character ? and 'until' itself is reserved
]]--

--[[
nil is a reserved
]]--

NULL = 5 
print(NULL)
