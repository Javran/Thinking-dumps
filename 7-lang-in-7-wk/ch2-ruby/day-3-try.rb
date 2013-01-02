#!/bin/env ruby

def putline
	puts "=" * 20
end

class NilClass
	def blank?
		true
	end
end

class String
	def blank?
		0 == self.size
	end
end

puts "define methods for existing classes"

["", "person", nil].each do |element|
	puts element unless element.blank?
end
# prints "person" only

["", "person", nil]
	.select{|e| e.blank?}
	.each{|e| puts "Empty, class=#{e.class}"} 
# prints class names of "" and nil

putline

puts "Units"

class Numeric
	def inches
		self
	end
	
	def feet
		self * 12.inches
	end

	def yards
		self * 3.feet
	end

	def miles
		self * 5280.feet
	end

	def back
		self * -1
	end

	def forward
		self
	end
end

puts "10 miles, backward: #{10.miles.back}"
puts "2 feet, forward: #{2.feet.forward}"

putline

puts "Roman numbers, using method_missing"

class Roman
	def self.method_missing name, *args
		roman = name.to_s
		roman.gsub!("IV", "IIII" )
		roman.gsub!("IX", "VIIII")
		roman.gsub!("XL", "XXXX" )
		roman.gsub!("XC", "LXXXX")

		[ 
			["I",   1],
			["V",   5],
			["X",  10],
			["L",  50],
			["C", 100],
		]
			.collect{ | ch,w| roman.count(ch)*w }
			.inject{  |acc,i| acc + i }
	end
end


puts "X=   #{Roman.X}"
puts "XC=  #{Roman.XC}"
puts "XII= #{Roman.XII}"
puts "X=   #{Roman.X}"

putline

puts "class: ActsAsCsv"

def testClass cls
	m = cls.new
	puts "="*10 + " class: #{m.class.to_s} " + "="*10
	puts "The superclass of #{m.class.to_s} is #{m.class.superclass.to_s}"
	puts "Headers:"
	puts m.headers.inspect
	puts "Content:"
	puts m.csv_contents.inspect
	puts "Pretty print:"
	m.pretty_print
end


class ActsAsCsv

	def read
		filename = 'rubycsv.txt'
		puts "Make sure the file \"#{filename}\" exists and is formatted properly"

		file = File.new filename
		@headers = file.gets.chomp.split(', ')
		
		file.each do |row|
			# the operator "<<" seems to append things into array
			@result << row.chomp.split(', ')
		end
	end

	def headers
		@headers
	end

	def csv_contents
		@result
	end

	def pretty_print
		puts @headers.join("\t| ")
		@result.each { |row| puts row.join("\t| ") }
	end

	def initialize
		@result = []
		read
	end
end

class RubyCsv < ActsAsCsv
end

testClass RubyCsv

class ActsAsCsv2
	def self.acts_as_csv

		define_method 'read' do
			filename = 'rubycsv.txt'
			puts "Make sure the file \"#{filename}\" exists and is formatted properly"

			file = File.new filename
			@headers = file.gets.chomp.split(', ')

			file.each do |row|
				# the operator "<<" seems to append things into array
				@result << row.chomp.split(', ')
			end

		end

		define_method 'headers' do
			@headers
		end

		define_method 'csv_contents' do
			@result
		end

		define_method 'pretty_print' do
			puts @headers.join("\t| ")
			@result.each { |row| puts row.join("\t| ") }
		end

		define_method 'initialize' do
			@result = []
			read
		end

	end
end

class RubyCsv2 < ActsAsCsv2
	acts_as_csv
end

testClass RubyCsv2

module ActsAsCsv3

	def self.included(base)
		base.extend ClassMethods
	end

	module ClassMethods
		def acts_as_csv
			include InstanceMethods
		end
	end

	module InstanceMethods

		def read
			@csv_contents = []
			filename = 'rubycsv.txt'
			puts "Make sure the file \"#{filename}\" exists and is formatted properly"
			file = File.new filename
			@headers = file.gets.chomp.split(', ')

			file.each do |row|
				# the operator "<<" seems to append things into array
				@csv_contents << row.chomp.split(', ')
			end
		end

		attr_accessor :headers, :csv_contents

		def pretty_print
			puts @headers.join("\t| ")
			@csv_contents.each { |row| puts row.join("\t| ") }
		end

		def initialize
			read
		end
	end
end

class RubyCsv3
	include ActsAsCsv3
	acts_as_csv
end

testClass RubyCsv3
