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

a = []
a.push( [0,1,2] )
a.push( [3,4,5] )
a.push( [6,7,8] )
puts a.to_s
puts a[2][2]

put_line

numbers = {1=>"one", 2=>"two"}
puts numbers
puts numbers[1]

stuff = {:array => [1,2,3], :string => "Hi, mom!"}
puts stuff[:array]
puts stuff[:string]

put_line
puts "Test symbol"
str = "aaa"
puts str.object_id
puts "aaa".object_id

puts :aaa.object_id
puts :aaa.object_id

put_line

def tell_the_truth(options={})
	if options[:profession] == :lawyer
		'it could be believed that this is almost certainly not false.'
	else
		true
	end
end

puts tell_the_truth
puts tell_the_truth :profession => :lawyer

put_line
puts "Block!"

3.times { puts "nice boat!" }
3.times do
	puts "nice BOAT!"
end

animals.each do
	|animal|
	puts "There are lots of #{animal}!"
end

put_line
puts "Modify class Fixnum!"

class Fixnum
	def my_times
		i = self
		while i > 1
			yield i, "Y", "!"
			i -= 1
		end
	end
end

10.my_times { |n, y, ed| puts y + "o"*n + ed }

puts "OK, What is <del>yoooo</del>yield?"
i = 9
def test_yield(range)
	i = 0
	while i < range 
		yield i * 2
		i += 1
	end
end
# just like a different form of 'foreach'?
test_yield(10) { |n| puts n }

put_line
puts "Call Block"

def call_block(&block)
	puts "block->"
	block.call
end

call_block { puts "block code here." }

def begin_transaction
	puts "Transaction Begin ==\\"

end

def end_transaction
	puts "Transaction End   ==/"
end

def emulate_transaction
	begin_transaction
	yield
	end_transaction
end

emulate_transaction do
	(0..4).each { |n| puts ">>> Operation ##{n}" }
end

put_line
it = 4.class
while it
	puts it
	it = it.superclass
end

put_line
it = 4.class.class
while it
	puts it
	it = it.superclass
end
