#!/usr/bin/env lua

function ends_in_3(num)
   return num % 10 == 3
end

print( ends_in_3(10) )
print( ends_in_3(23) )

function is_prime(num)
   for i = 2, num-1 do
      if num % i == 0 then
         return false
      end
   end
   return true
end

for i = 2, 20 do
   if is_prime(i) then
      print(i)
   end
end

function print_first_n(n)
   local count = n
   local curNum = 2

   -- this procedure bumps curNum until it's a prime that ends in 3
   function next_candidate()
      curNum = curNum + 1
      if not (is_prime(curNum) and ends_in_3(curNum)) then
         next_candidate()
      end
   end

   while count > 0 do
      -- find next candidate and print it
      next_candidate()
      print(curNum)
      count = count-1
   end
end

print("------")
print_first_n(10)
