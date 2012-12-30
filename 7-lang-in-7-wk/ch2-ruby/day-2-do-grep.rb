#!/bin/env ruby

def error(reason)
	puts "Error: #{reason}"
	exit(0)
end

if ARGV.length != 1
	error "args: <target phrase>"
end

target_phrase = ARGV[0]

if not /^[a-zA-Z0-9_ ]+$/ =~ target_phrase
	error "target is not a phrase"
end

line_counter = 0
$stdin.readlines.each do |l|
	line_counter += 1
	if /#{target_phrase}/ =~ l
		puts "#{line_counter}\t#{l}"
	end
end
