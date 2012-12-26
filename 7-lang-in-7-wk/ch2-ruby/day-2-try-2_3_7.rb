#!/bin/env ruby

puts 'begin' <=> 'end'
# -1
puts 'same' <=> 'same'
# 0

a = [6,2,3,4,1]

puts a.to_s
puts a.sort.to_s

puts a.any? { |i| i > 6 }
# false
puts a.all? { |i| i > 0 }
# true

# hello map! I've found you!
puts a.collect{ |i| [i,i*2] }.to_s

# hello filter!
puts a.select{ |i| i%2 == 0 }.to_s

puts a.max
# 6

puts (1..6).select{ |i| a.member? i }.to_s
# [1,2,3,4,6]

# hello foldl!
result = a.inject(0) do |sum, i|
	puts "Current sum is #{sum}"
	puts "Will now add #{i}"
	sum + i
end

puts "result: #{result}"
