// Package diffsquares contain functions to compute difference of squares.
// For this package it is assume that the imput is always non-negative.
// (However the fast approach does allow generalizing to negative numbers
// while the naive version doesn't)
package diffsquares

// fast determines which implementation to use.
var fast bool = true

// SquareOfSum computes (1 + 2 + ... + n)^2 given n.
func SquareOfSum(n int) int {
	var sum int
	if fast {
		sum = n * (n + 1) / 2
	} else {
		for i := 1; i <= n; i++ {
			sum += i
		}
	}
	return sum * sum
}

// SumOfSquares computes 1^2 + 2^2 + ... + n^2 given n.
func SumOfSquares(n int) int {
	if fast {
		// http://oeis.org/A000330
		return n * (n + 1) * (n + n + 1) / 6
	}

	result := 0
	for i := 1; i <= n; i++ {
		result += i * i
	}
	return result
}

// Difference computes the difference between SquareOfSum(n)
// and SumOfSquares(n) given n.
func Difference(n int) int {
	if fast {
		// http://oeis.org/A052149
		return n * (n - 1) * (n + 1) * (3*n + 2) / 12
	}

	return SquareOfSum(n) - SumOfSquares(n)
}
