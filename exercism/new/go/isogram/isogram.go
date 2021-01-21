// Package isogram exports a function that tests isogram.
package isogram

import (
	"strings"
	"unicode"
)

// IsIsogram tests whether a word or phase is an isogram
// note that hyphens and spaces are allowed to appear multiple times.
func IsIsogram(input string) bool {
	exist := make(map[rune]struct{})
	input = strings.ReplaceAll(input, " ", "")
	input = strings.ReplaceAll(input, "-", "")
	for _, ch := range input {
		upper := unicode.ToUpper(ch)
		_, ok := exist[upper]
		if ok {
			return false
		}
		exist[upper] = struct{}{}
	}
	return true
}
