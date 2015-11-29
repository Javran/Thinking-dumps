#!/usr/bin/env lua

function concatenate(a1,a2)
   print("customized concatenate")
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

-- turns out exercise description is a little bit confusing, see:
-- https://forums.pragprog.com/forums/351/topics/13237

local mt = {
   __newindex = function(t,k,v)
      -- the idea is that if "v" is a table (an array),
      -- we modify its metatable so that we can install "__add" to
      -- that object
      if type(v) == 'table' then
         local mt  = {
            __add = concatenate
         }
         print("Modifying metatable for this value..")
         setmetatable(v, mt)
      end
      rawset(t,k,v)
   end
}

setmetatable(_G,mt)

testNum = 1
testTable = {1,2,3,4}

print_array(testTable + testTable)
