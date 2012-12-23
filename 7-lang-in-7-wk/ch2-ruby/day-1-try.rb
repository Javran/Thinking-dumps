#!/bin/env ruby

puts "=============================="

properties = [ 'object oriented', 'duck typed', 'productive', 'fun' ]

properties.each {
	| property |
	puts "Ruby is #{property}"}

puts "=============================="

# ruby comment

=begin
	ruby block comment
=end

x = 4

[
	4, 4.class, 4.methods,
	x < 5, x >= 5, 
	false.class, true.class
].each {
	| obj |
	puts "#{obj}"}

puts "=============================="

# if and unless

trueString = "This appears to be true"
falseString = "This appears to be false"

if x == 4
	puts trueString
end

unless x == 4
	puts trueString
else
	puts falseString
end

puts "=============================="

puts trueString if x == 4
puts falseString unless x == 4

puts "=============================="

# while and until
x = 1
while x < 10
	puts x
	x = x + 1
end

x = x - 1 until x == 1
puts x

puts "=============================="

# duck type
a = ['123', 456.789] 
i = 0
while i < 2
	puts a[i].to_i
	i += 1
end
