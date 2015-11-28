local pending = {}

local function sort_by_time(array)
   local function compare(e1,e2)
      return e1.time < e2.time
   end 
   table.sort(array, compare)
end

local function schedule(time, action)
   pending[#pending+1] = {
      time = time,
      action = action
   }

   sort_by_time(pending)
end

local function wait(seconds)
   coroutine.yield(seconds)
end

-- TODO: I suspect this is not working because the order is not preserved
-- we need "pending" to be sorted otherwise sort_by_time is not necessary
local function remove_first(array)
   result = array[1]
   array[1] = array[#array]
   array[#array] = nil
   return result
end

local function run()
   while #pending > 0 do
      while os.clock() < pending[1].time do
         -- busy wait
      end

      local item = remove_first(pending)
      local _, seconds = coroutine.resume(item.action)

      if seconds then
         later = os.clock() + seconds
         schedule(later, item.action)
      end
   end
end

return {
   schedule = schedule,
   run = run,
   wait = wait,
   test = function(v)
      return v+1
   end 
}

