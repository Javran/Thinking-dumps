#!/usr/bin/env lua

-- need the following lines before using random module
math.randomseed(os.time())
for i = 1,10 do
   math.random()
end

function retry(count, body)
   -- TODO
   local co = coroutine.create(body)
   succeeded, value = coroutine.resume(co)
   print(succeeded)
   print(value)
end

retry(
   5,
   function()
      local v = math.random()
      if v > 0.2 then
         coroutine.yield('Something bad happened')
      end
      print('Succeeded')
   end )
