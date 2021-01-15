// Package isogram exports a function that tests isogram.
package isogram

import (
	"unicode"
)

// IsIsogram tests whether a word or phase is an isogram
// note that hyphens and spaces are allowed to appear multiple times.
func IsIsogram(input string) bool {
	exist := make(map[rune]struct{})
	for _, ch := range input {
		switch upper := unicode.ToUpper(ch); {
		case upper == ' ' || upper == '-':
			// NOOP
		default:
			_, ok := exist[upper]
			if ok {
				return false
			}
			exist[upper] = struct{}{}
		}
	}
	return true
}
