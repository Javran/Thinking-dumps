// Package pythagorean contains functions to find Pythagorean triplets
// with certain criteria.
package pythagorean

import (
	"math"
)

// Triplet represents Pythagorean triplet,
// in this type Triplet{a,b,c} has an extra requirement that a < b <  c.
type Triplet [3]int

// checkSquare checks whether a number is a perfect square
// and returns an integer closest to its positive square root.
func checkSquare(x int) (int, bool) {
	v := int(math.Round(math.Sqrt(float64(x))))
	return v, v*v == x
}

// Range returns all Triplets whose all sides are between min and max.
func Range(min, max int) []Triplet {
	var results []Triplet
	// We require following invariant to ensure unique Triplets in the result.
	// INVARIANT: a < b < c
	for a := min; a <= max; a++ {
		aSq := a * a
		for b := a + 1; ; b++ {
			cSqExpect := aSq + b*b
			if cSqExpect > max*max {
				break
			}
			c, ok := checkSquare(cSqExpect)
			if ok {
				results = append(results, Triplet{a, b, c})
			}
		}
	}
	return results
}

// Sum returns all Triplets whose all side adds up to a specific number.
func Sum(p int) []Triplet {
	var results []Triplet
	// We require following invariant to ensure unique Triplets in the result.
	// INVARIANT: a < b < c
	for a := 3; a*3 < p; a++ {
		aSq := a * a
		for b := a + 1; ; b++ {
			c := p - a - b
			if c <= b {
				break
			}
			if aSq+b*b == c*c {
				results = append(results, Triplet{a, b, c})
			}
		}
	}
	return results
}
