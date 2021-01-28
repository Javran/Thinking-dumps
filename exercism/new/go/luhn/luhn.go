package luhn

import (
	"errors"
	"regexp"
	"strings"
)

// DigitStr is a string consists of only digits and its length longer than 1.
type DigitStr string

// removeSpaces removes spaces from a string
var removeSpaces func(string) string = strings.NewReplacer(" ", "").Replace

// isDigitStr checks whether a string qualifies as DigitStr after spaces removed.
var isDigitStr func(string) bool = regexp.MustCompile(`^\d{2,}$`).MatchString

// verifyInput verifies that the input string meets the precondition and
// returns a string that contains only digits
func verifyInput(input string) (DigitStr, error) {
	input = removeSpaces(input)
	if !isDigitStr(input) {
		return "", errors.New("input string contains non-digit characters or its length is less than 2")
	}
	return DigitStr(input), nil
}

// Valid checks whether a given string is valid per Luhn formula.
// No other character except digits and space are allowed.
func Valid(input string) bool {
	digitStr, err := verifyInput(input)
	if err != nil {
		return false
	}

	sum := 0
	for i, ch := range digitStr {
		val := int(ch - '0')
		// check if we want to do the digit transformation
		// on the current digit.
		if (len(digitStr)-i)%2 == 0 {
			val *= 2
			if val >= 10 {
				val -= 9
			}
		}
		sum += val
	}

	return sum%10 == 0
}
