puts "Task #1: each vs each_slice"
arr = (1..16).to_a.collect { |i| i*4 } 
puts arr.to_s

puts "Attempt #1: use Array#each"
_tmp_list = []
arr.each do | item |
	_tmp_list.push item
	if _tmp_list.length >= 4
		puts _tmp_list.join("\t")
		_tmp_list.clear
	end
end

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
