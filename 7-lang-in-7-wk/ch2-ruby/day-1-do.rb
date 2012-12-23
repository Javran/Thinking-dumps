#!/bin/env ruby

puts "Task #1: print!"
puts "nice boat!"

puts "Task #2: find ruby!"

str = "Nice Ruby."
pos = str.match(/Ruby\./).begin(0)
puts "found 'Ruby.' at the position: #{pos} of string '#{str}'"

puts "Task #3: print my name for 10 times!"
puts "Javran " * 10

puts "Task #4: print strings!"
(1..10).each{ |n| puts "This is sequence number #{n}" }

puts "Task #5: run ruby from file!"
puts "That's it!"

puts "Task #6: guess number!"

target = rand(100)

userGuess = nil
while userGuess != target
	userGuess = gets().to_i
	puts "The target is greater" if target > userGuess
	puts "The target is smaller" if target < userGuess
end
puts "You've got it!"
