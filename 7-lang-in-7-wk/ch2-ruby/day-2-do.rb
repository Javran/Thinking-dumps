#!/bin/env ruby

puts "Task #1: each vs each_slice"
arr = (1..16).to_a.collect { |i| i*4 } 
puts arr.to_s
# nice boat

puts "Attempt #1: use Array#each"
_tmp_list = []
arr.each do | item |
	_tmp_list.push item
	if _tmp_list.length >= 4
		puts _tmp_list.join("\t")
		_tmp_list.clear
	end
end

# this is a nice boat
puts "Attempt #2: use Enumerable#each_slice"
arr.each_slice(4) { | four | puts four.join("\t") }

puts "="*20
puts "Task #2: make a tree"

in_data = {'grandpa' => {
	'dad' => {
		'child 1' => {},
		'child 2' => {},
	},
	'uncle' => {
		'child 3' => {},
		'child 4' => {},
	},
}}
puts "input structure: #{in_data}"

class MyTree
	attr_accessor :children, :node_name

	def initialize(node_name, children_dict)
		@node_name = node_name	
		@children = children_dict.collect { |k,v| MyTree.new(k,v) }
	end

	def visit(&block)
		block.call self
	end

	def visit_all(&block)
		visit &block
		children.each { |c| c.visit_all &block }
	end

end

myTree = MyTree.new(*in_data.flatten)
myTree.visit_all { |c| puts "Visiting node: #{c.node_name}" }
puts

# this is a really nice boat
puts "Task #3: a simple \"grep\" tool"
grep_script = "day-2-do-grep.rb"
grep_phrase = "Task"
grep_file = "day-2-do.rb"
puts "Now I'll call \"#{grep_script}\" to find lines"
puts "that contain \"#{grep_phrase}\" from \"#{grep_file}\""
cmd = "cat #{grep_file} | ruby #{grep_script} \"#{grep_phrase}\""
puts "The command line is [#{cmd}]"
puts "="*20 + "BEGIN" + "="*20
system cmd
puts "="*20 + " END " + "="*20

grep_phrase = "nice boat"
puts "Now, find lines that contains \"#{grep_phrase}\""
cmd = "cat #{grep_file} | ruby #{grep_script} \"#{grep_phrase}\""
puts "The command line is [#{cmd}]"
puts "="*20 + "BEGIN" + "="*20
system cmd
puts "="*20 + " END " + "="*20
