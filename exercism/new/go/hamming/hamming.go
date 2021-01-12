// Package hamming contains function computing Hamming distances.
package hamming

import "errors"

// Distance computes Hamming distance between two strings.
// Two strings must have the same length, an error will occur otherwise.
func Distance(a, b string) (int, error) {
	// reinterpret as slices of runes
	// as this is intended for code point comparision.
	ar, br := []rune(a), []rune(b)
	if len(ar) != len(br) {
		return 0, errors.New("two input strings must have the same length")
	}

	distance := 0
	for i, aVal := range ar {
		if aVal != br[i] {
			distance++
		}
	}
	return distance, nil
}
