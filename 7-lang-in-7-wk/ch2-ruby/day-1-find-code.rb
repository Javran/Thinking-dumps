#!/bin/env ruby

puts "Task #1: replace"

originString = "original content"
puts "the original content is '#{originString}'"

replaceOffset = "original".length

originString[0..replaceOffset-1] = "new"
puts "the original content is '#{originString}'"

puts "="*20

puts "Task #2: Regexp"

originString = "haystack"

puts "find 'hay' in '#{originString}':"
res = /hay/ =~ 'haystack'
puts "result=#{res}"

puts "find 'hey' in '#{originString}':"
res = /hey/ =~ 'haystack'
puts "result=#{res}"

originString = "the quick brown fox jumps over the lazy dog"
puts "find words in '#{originString}':"

curString = originString
m = curString.match(/\w+/)
while m
	word = curString[m.begin(0) .. m.end(0)-1]
	puts "found '#{word}'"
	curString = curString[m.end(0)..-1]
	m = curString.match(/\w+/)
end

puts "="*20
puts "Task #3: Range"

puts "Range: 1..3"
puts (1..3).to_a
puts "Range: 1...3"
puts (1...3).to_a
puts "Cover test: 0..9/0...9 covered?" 
puts (0..9).cover?(9), (0...9).cover?(9)
