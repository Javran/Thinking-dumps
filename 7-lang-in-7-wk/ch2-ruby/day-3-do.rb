#!/usr/bin/env ruby

class RubyCsv
	attr_accessor :headers, :csv_contents

	def read(filename)
		puts "Make sure the file \"#{filename}\" exists and is formatted properly"

		file = File.new filename
		@headers = file.gets.chomp.split(', ')
		
		@csv_contents = []
		file.each do |row|
			# the operator "<<" seems to append things into array
			@csv_contents << row.chomp.split(', ')
		end

		@csv_contents = @csv_contents.collect{ |row| CsvRow.new(@headers, row) }
	end

	def pretty_print
		puts "=" * 20
		puts @headers.join("\t| ")
		puts "=" * 20
		@csv_contents.each{ |row| row.pretty_print }
	end

	def initialize(filename)
		read filename
	end

	def each(&block)
		@csv_contents.each(&block)
	end

	class CsvRow
		attr_accessor :headers, :contents

		def initialize(headers, contents)
			@headers = headers
			@contents = contents
		end

		def pretty_print
			puts @contents.join("\t| ")
		end

		def method_missing name, *args
			col = name.to_s
			@contents[@headers.index col]
		end
	end
end

def runTestOn filename
	puts "/"*20 + "Test Case Begin" + "\\"*20
	csv = RubyCsv.new filename
	puts "Print csv content:"
	csv.pretty_print
	puts "Run test code:"

	col = csv.headers[0]
	puts "The first column is \"#{col}\""
	csv.each do |row| 
		content = eval "row.#{col}"
		print "class: #{row.class.to_s}, "
		print "row.#{col}: #{content}"
		puts
	end
	puts "\\"*20 + "Test Case End  " + "/"*20
end

["rubycsv.txt","ruby-exercise-csv.txt"].each{ |file| runTestOn file}
