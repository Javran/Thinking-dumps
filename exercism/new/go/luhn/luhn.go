package luhn

import (
	"strings"
	"unicode"
)

// Valid checks whether a given string is valid per Luhn formula.
// No other character except '0' - '9' and space are allowed.
func Valid(input string) bool {
	input = strings.ReplaceAll(input, " ", "")
	if len(input) <= 1 {
		// input string is less than 2 bytes, which cannot be a valid Luhn string.
		return false
	}

	// padding '0' in front of a string of odd length,
	// this allows us to compute Luhn checksum from front of the string
	// while preserving original sum.
	if len(input)%2 == 1 {
		input = "0" + input
	}
	// INVARIANT: len(input) is even.

	sum := 0
	// INVARIANT of the following loop:
	// scanned part of the string can only have '0' - '9'.
	for i, ch := range input {
		if !unicode.IsDigit(ch) {
			return false
		}
		val := int(ch - '0')
		// check if we want to do the digit transformation
		// on the current digit.
		if i%2 == 0 {
			val *= 2
			if val >= 10 {
				val -= 9
			}
		}
		sum += val
	}
	// at this point the string is both checked to contain '0' - '9' only
	// and summed thanks to the loop invariant.
	return sum%10 == 0
}
