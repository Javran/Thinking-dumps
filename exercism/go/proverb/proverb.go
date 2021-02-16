// Package proverb contains a function that generates proverb.
package proverb

import "fmt"

// Proverb returns a slice of proverb given a slice of any length.
func Proverb(rhyme []string) []string {
	if len(rhyme) == 0 {
		return nil
	}
	// Note: the exercise and its tests are not specific
	// about the use of indefinite articles,
	// here I choose to stick with "a" to keep it simple.
	var results = make([]string, len(rhyme))
	for i := 1; i < len(rhyme); i++ {
		results[i-1] = fmt.Sprintf("For want of a %s the %s was lost.", rhyme[i-1], rhyme[i])
	}
	results[len(rhyme)-1] = fmt.Sprintf("And all for the want of a %s.", rhyme[0])
	return results
}
