// Package isogram exports a function that tests isogram.
package isogram

import (
	"strings"
)

// sanitizeInput returns the input string without ' ' or '-'.
var sanitizeInput func(string) string = strings.NewReplacer(
	" ", "",
	"-", "",
).Replace

// IsIsogram tests whether a word or phase is an isogram
// note that hyphens and spaces are allowed to appear multiple times.
func IsIsogram(input string) bool {
	// overwrite input with normalized version (sanitized & ToUpper)
	// since there is no need refering to original string after this.
	input = strings.ToUpper(sanitizeInput(input))
	for i, ch := range input {
		if strings.Contains(input[:i], string(ch)) {
			return false
		}
	}
	return true
}
