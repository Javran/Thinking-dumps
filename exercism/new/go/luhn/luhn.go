package luhn

import (
	"strings"
)

// Valid checks whether a given string is valid per Luhn formula.
// No other character except digits and space are allowed.
func Valid(input string) bool {
	input = strings.ReplaceAll(input, " ", "")
	if len(input) <= 1 {
		// input string is less than 2 bytes, which cannot be a valid string.
		return false
	}

	sum := 0
	for i, ch := range input {
		if ch < '0' || ch > '9' {
			return false
		}
		val := int(ch - '0')
		// check if we want to do the digit transformation
		// on the current digit.
		if (len(input)-i)%2 == 0 {
			val *= 2
			if val >= 10 {
				val -= 9
			}
		}
		sum += val
	}

	return sum%10 == 0
}
