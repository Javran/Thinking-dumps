// Package raindrops contains a function that converts int to raindrops string.
package raindrops

import (
	"strconv"
)

// Convert converts an int to raindrops string.
func Convert(x int) string {
	// condValue tests whether fac is a factor of x and returns trueVal
	// otherwise an empty string is returned.
	condValue := func(fac int, trueVal string) string {
		if x%fac == 0 {
			return trueVal
		}
		return ""
	}

	result := condValue(3, "Pling") + condValue(5, "Plang") + condValue(7, "Plong")
	if result == "" {
		result = strconv.Itoa(x)
	}
	return result
}
