#!/usr/bin/env lua
 
function reduce(max, init, f)
   local acc = init
   for i = 1, max do
      acc = f(acc,i)
   end
   return acc
end

function add(previous,next)
   return previous+next
end

print( reduce(5,0,add) )

function factorial(n)
   function mul(a,b)
      return a*b
   end
   return reduce(n,1,mul)
end

for i = 0,10 do
   print( i .. " " .. factorial (i))
end
