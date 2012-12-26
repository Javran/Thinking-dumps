#!/bin/env ruby

module IdStr
	def object_id_str
		"Method from IdStr, object_id = #{object_id}"
	end
end

class Person
	include IdStr
	attr_accessor :name

	def initialize(name)
		@name = name
	end

	def to_s
		"Method from Person, name = #{name}"
	end
end

a = Person.new('Javran')

puts a
puts a.object_id_str

