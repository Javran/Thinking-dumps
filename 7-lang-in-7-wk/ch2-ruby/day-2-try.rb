#!/bin/env ruby

def tell_the_truth
	true
end

def put_line
	puts "=" * 20
end

puts tell_the_truth

put_line

animals = ['lions', 'tigers', 'bears' ]
puts animals

put_line

puts animals[0]
# lions
puts animals[10]
# nil
puts animals[-1]
# bears

put_line
puts animals[0..-1]

put_line

puts [1].class
puts [1].methods.include?(:[])

a = []
a[0] = 'zero'
a[1] = 1
a[2] = ['two', 'things']
puts a.to_s

a.push( 'this!' )
while a.length > 0
	element = a.pop
	puts "Pop: #{element.to_s}"	
end

put_line
