// Package accumulate contains a function to visit and apply unary string function to a slice of strings.
package accumulate

// Accumulate takes a slice of string and returns the result of applying a unary string function to
// each of its elements.
func Accumulate(strs []string, transform func(string) string) []string {
	result := make([]string, len(strs))
	for i, v := range strs {
		result[i] = transform(v)
	}

	return result
}
