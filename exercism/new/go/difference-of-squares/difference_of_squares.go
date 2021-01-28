// Package diffsquares contain functions to compute difference of squares.
// For this package it is assume that the imput is always non-negative.
// (However the fast approach does allow generalizing to negative numbers
// while the naive version doesn't)
package diffsquares

// SquareOfSum computes (1 + 2 + ... + n)^2 given n.
func SquareOfSum(n int) int {
	sum := n * (n + 1) / 2
	return sum * sum
}

// SumOfSquares computes 1^2 + 2^2 + ... + n^2 given n.
func SumOfSquares(n int) int {
	// http://oeis.org/A000330
	return n * (n + 1) * (n + n + 1) / 6
}

// Difference computes the difference between SquareOfSum(n)
// and SumOfSquares(n) given n.
func Difference(n int) int {
	// http://oeis.org/A052149
	return n * (n - 1) * (n + 1) * (3*n + 2) / 12
}
