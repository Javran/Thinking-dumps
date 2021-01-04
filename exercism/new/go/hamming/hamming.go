// This package contains function computing Hamming distances.
package hamming

import "errors"

// Computes Hamming distance between two ASCII-encoded strings.
// Two strings must have the same length, an error will occur otherwise.
func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return 0, errors.New("two input strings must have the same length.")
	}

	distance := 0
	// Visiting by byte. Should not affect correctness assuming ASCII encoding.
	for i := 0; i < len(a); i += 1 {
		if a[i] != b[i] {
			distance += 1
		}
	}
	return distance, nil
}
