#!/bin/env ruby

puts "Task #1: Read Files"

puts "Read myself!"

puts "[1: without code block]"
puts "*" * 10 + " BEGIN " + "*" * 10
line_counter = 1
file = File.new("day-2-find-code.rb", "r")
while (line = file.gets)
	puts "#{line_counter}: #{line}"
	line_counter += 1
end
file.close
puts "*" * 10 + " END " + "*" * 10

puts "[2: with code block]"
puts "*" * 10 + " BEGIN " + "*" * 10
line_counter = 1
File.open("day-2-find-code.rb", "r") do |f|
	while (line = f.gets)
		puts "#{line_counter}: #{line}"
		line_counter += 1
	end
end
puts "*" * 10 + " END " + "*" * 10

puts "Task #2: Hash tables to arrays"

h = { "a" => "AAA", "b" => "BBB", "c"=>"CCC" }
puts "origin data:"
puts "\t#{h}"

puts "to (key,value) pairs:"
puts "\t#{h.to_a.to_s}"
puts "to values:"
puts "\t#{h.values.to_s}"
puts "to keys:"
puts "\t#{h.keys.to_s}"

puts "Task #3: Arrays to hash tables"

arr = ["a", "AAA", "b", "BBB"]
puts "origin data:"

puts "\t#{arr}"
puts "to hash table:"
puts "\t#{Hash[*arr]}"

arr = [["a", "AAA"], ["b", "BBB"]]
puts "origin data:"
puts "\t#{arr}"

puts "to hash table:"
puts "\t#{Hash[arr]}"

puts "Task #4: Traverse through hash table"
h = {1 => "one", 2 => "two", 3 => "three" }
puts "The data is #{h}"
h.each do | key, value |
	puts "#{key} => #{value}"
end


puts "Task #5: Use array as queue"
arr = []
puts "Insert items ..."
(1..5).each do | i |
	arr.insert(0, i)
	puts "push item: #{i}"
end
puts arr.to_s
while arr.length > 0
	item = arr.pop
	puts "pop item: #{item}"
end
