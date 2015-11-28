#!/usr/bin/env lua

function concatenate(a1,a2)
   local result = {}
   for i = 1, #a1 do
      result[#result + 1] = a1[i]
   end
   for i = 1, #a2 do
      result[#result + 1] = a2[i]
   end
   return result
end

function print_array(ar)
   print("========")
   for i = 1, #ar do
      print(i .. ": " .. ar[i])
   end
   print("--------")
end

a = {1,2,3,4}
b = {4,5,6,7}

c = concatenate(a,b)
d = concatenate(b,c)

print_array(c)
print_array(d)

-- strict_write with deletion support
function make_strict_table()
   -- unlike the example in the book,
   -- we don't want to make just a single object
   -- that can use strict read and write, we want to make
   -- object that has this ability.
   -- in the book both function "strict_read" and "strict_write" are tied to
   -- the local variable "private", which means they cannot be shared with
   -- other objects (meaning that if we share "mt" with any other object,
   -- we end up just having one "private" object being shared by two wrappers,
   -- which is wrong)
   local private = {}

   function strict_read(table, key)
      if private[key] then
         return private[key]
      else
         error("Invalid key: " .. key)
      end 
   end

   function strict_write(table, key, value)
      if private[key] and value ~= nil then
         error("Duplicate key: " .. key)
      else 
         private[key] = value
      end
   end

   local mt = {
      __index = strict_read,
      __newindex = strict_write
   }

   local result = {}
   setmetatable(result,mt)
   return result
end

ta = make_strict_table()
tb = make_strict_table()

-- error
-- print(tb[1])

tb[1] = 10
print(tb[1])
-- error
-- if we share the metatable, we'll have ta[1] available
-- which is incorrect
-- print(ta[1])

tb[1] = nil

-- error
-- print(tb[1])
