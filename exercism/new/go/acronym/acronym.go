// Package acronym contains a function that converts phases into acronyms.
package acronym

import (
	"regexp"
	"unicode"
)

// wordMatcher is a Regexp for extracting words from a string.
// here `'` is considered part of the word to address inputs like "Jim's"
var wordMatcher = regexp.MustCompile(`(?i)[a-z']+`)

// Abbreviate takes a phase and return a possible acronym.
func Abbreviate(s string) string {
	result := ""
	for _, w := range wordMatcher.FindAllString(s, -1) {
		word := []rune(w)
		result += string(unicode.ToUpper(word[0]))
	}
	return result
}
