// Package scrabble contains a function that computes Scrabble scores.
package scrabble

import (
	"unicode"
)

// letterScores represents a mapping from uppercase Scrabble letter
// to its corresponding score.
// - this map is initialized once and never mutated afterwards.
// - keys are all uppercase letters.
var letterScores map[rune]int

func init() {
	letterScores = make(map[rune]int)
	// register serves as a shorthand for assigning multiple letters same score.
	register := func(letters string, score int) {
		for _, ch := range letters {
			letterScores[ch] = score
		}
	}

	register("AEIOULNRST", 1)
	register("DG", 2)
	register("BCMP", 3)
	register("FHVWY", 4)
	register("K", 5)
	register("JX", 8)
	register("QZ", 10)
}

// Score returns Scrabble score for the given word.
func Score(word string) int {
	sum := 0
	for _, ch := range word {
		sum += letterScores[unicode.ToUpper(ch)]
	}
	return sum
}
