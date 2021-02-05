package luhn

import (
	"strings"
	"unicode"
)

// Valid checks whether a given string is valid per Luhn formula.
// No other character except '0' - '9' and space are allowed.
func Valid(input string) bool {
	input = strings.ReplaceAll(input, " ", "")
	if len(input) < 2 {
		// input string is less than 2 bytes, which cannot be a valid Luhn string.
		return false
	}

	// isSecondDigit keeps track of whether the current digit is
	// considered a second digit, counting from **right** side of the string.
	sum, isSecondDigit := 0, len(input)%2 == 0
	// INVARIANT of the following loop:
	// scanned part of the string can only have '0' - '9'.
	for _, ch := range input {
		if !unicode.IsDigit(ch) {
			return false
		}
		val := int(ch - '0')
		// check if we want to do the digit transformation
		// on the current digit.
		if isSecondDigit {
			val *= 2
			if val > 9 {
				val -= 9
			}
		}
		sum += val
		isSecondDigit = !isSecondDigit
	}
	// at this point the string is both checked to contain '0' - '9' only
	// and summed thanks to the loop invariant.
	return sum%10 == 0
}
