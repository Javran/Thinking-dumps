// Package etl contains a function to convert representation of old system to that of the new one.
package etl

import (
	"strings"
)

type oldTable = map[int][]string
type newTable = map[string]int

// Transform takes mapping used in old system
// and converts that to mapping supported by new system.
func Transform(old oldTable) newTable {
	result := make(newTable)
	for score, upChars := range old {
		for _, upChar := range upChars {
			result[strings.ToLower(upChar)] = score
		}
	}
	return result
}
