#!/usr/bin/env python

import re
import sys

class HanoiVerifier:
	def __init__(self, level):
		self.level = level
		self.mv_pattern = re.compile(r'([abc]) -> ([abc])')

		# init stack
		init_rod = range(1,level+1)
		init_rod.reverse()
		self.rods = [init_rod, [], []]	


	def move(self, cmd):
		# verify if the command is valid
		results = self.mv_pattern.match(cmd)
		assert results is not None
		# parse result
		src, tgt = map(lambda x: self.toIndex(x), results.groups())

		# move disk
		disk = self.rods[src].pop()
		self.rods[tgt].append(disk)
		# TODO: pretty print
		print self.rods
		
	def toIndex(self, ch):
		assert ch in 'abc'
		return ord(ch) - ord('a')
	
	def done(self):
		final_rod = range(1, self.level + 1)
		final_rod.reverse()
		if self.rods == [[],[],final_rod]:
			print "Solution is correct."
		else:
			print "Solution is incorrect."


if __name__ == '__main__':
	if len(sys.argv) != 2:
		print "Please give me a potential solution."
		print "* first line is the level"
		print "* followed by one move per line"
		print "* each move should be given in form \"[abc] -> [abc]\""
		exit(0)

	solutionFile = open( sys.argv[1], 'r' )
	solution = solutionFile.readlines()
	solutionFile.close()

	level = int( solution.pop(0) )

	he = HanoiVerifier(level)
	for move in solution:
		he.move(move)
	he.done()
