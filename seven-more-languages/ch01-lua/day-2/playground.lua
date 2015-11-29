#!/usr/bin/env lua

function five_numbers_start_from(n)
   -- note that we must return a function object
   return function ()
      local v = n
      local c = 0
      while c < 5 do
         coroutine.yield(v)
         v = v+1
         c = c+1
      end
      -- when all values are yielded,
      -- the function's return value will be produced
      return "done"
   end 
end

function consume_and_print(g)
   local succeeded, v = coroutine.resume(g)
   print (succeeded, v)
end

g = coroutine.create(five_numbers_start_from(10))

for i = 1,10 do
   consume_and_print(g)
end

scheduler = require 'scheduler'

-- try using an external file
print(scheduler.test(10))

test_mt = {
   hello = function()
      print("hello")
   end
}

-- here is a trick to setup "fallback function",
-- actually it seems that the creation of OO-style
-- objects makes use of this trick
test_mt2 = {}
setmetatable(test_mt2, test_mt)
test_mt.__index = test_mt

test_mt2.hello()
