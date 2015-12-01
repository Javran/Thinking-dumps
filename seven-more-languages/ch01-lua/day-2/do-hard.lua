#!/usr/bin/env lua

-- need the following lines before using the random module
math.randomseed(os.time())
for i = 1,10 do
   math.random()
end

function retry(count, body)
   local remaining = count
   while remaining > 0 do
      -- we need to have the "body" restart from beginning,
      -- so the previous coroutine cannot be used (which will return
      -- control to where it yielded, and this is not our intention)
      local co = coroutine.create(body)
      succeeded, value = coroutine.resume(co)
      -- we need to examine the status to tell if the coroutine has yielded
      -- or returned
      if coroutine.status(co) == 'dead' then
         -- the function has returned normally so the coroutine is dead
         -- no further attempt is necessary
         break
      else
         print("Coroutine returned an error message:")
         print(value)
         remaining = remaining - 1
      end
   end
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
