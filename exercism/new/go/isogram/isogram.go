// Package isogram exports a function that tests isogram.
package isogram

import (
	"strings"
	"unicode"
)

// sanitizeInput returns the input string without ' ' or '-'.
var sanitizeInput func(string) string = strings.NewReplacer(
	" ", "",
	"-", "",
).Replace

// IsIsogram tests whether a word or phase is an isogram
// note that hyphens and spaces are allowed to appear multiple times.
func IsIsogram(input string) bool {
	exist := make(map[rune]struct{})
	for _, ch := range sanitizeInput(input) {
		upper := unicode.ToUpper(ch)
		_, ok := exist[upper]
		if ok {
			return false
		}
		exist[upper] = struct{}{}
	}
	return true
}
