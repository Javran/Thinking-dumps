// Package twofer implements "Two-fer" exercise.
package twofer

// ShareWith returns a string with the "Two-fer" message.
func ShareWith(name string) string {
	result := "One for "

	if name != "" {
		result += name
	} else {
		result += "you"
	}

	result += ", one for me."
	return result
}
