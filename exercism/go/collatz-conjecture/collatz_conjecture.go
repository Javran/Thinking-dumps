// Package collatzconjecture contains a function that verifies Collatz conjecture.
package collatzconjecture

import (
	"errors"
)

// CollatzConjecture takes a positive number and compute steps it will take
// to reach 1 in Collatz conjecture.
func CollatzConjecture(x int) (int, error) {
	if x <= 0 {
		return 0, errors.New("input number must be positive")
	}

	next := func(v int) int {
		if v%2 == 0 {
			return v / 2
		}
		return v*3 + 1
	}

	count := 0
	for ; x != 1; x = next(x) {
		count++
	}
	return count, nil
}
