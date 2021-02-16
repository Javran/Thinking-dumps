// Package reverse contains a function that returns reversed string.
package reverse

// Reverse reverses the input string.
func Reverse (input string) string {
	result := ""
	for _, ch := range input {
		result = string(ch) + result 
	}
	return result
}
