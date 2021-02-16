// Package triangle contains function and type determining the type of a triangle.
package triangle

import "math"

// Kind represents return type of KindFromSides().
type Kind = string

const (
	NaT = "Not a triangle"
	Equ = "Equilateral"
	Iso = "Isosceles"
	Sca = "Scalene"
)

// KindFromSides determines the type of a triangle given its 3 sides in any order.
// Returns NaT (Not a triangle) if any of those side is not finite (i.e. NaN or +-Infinity)
// or if triangle inequality is violated.
func KindFromSides(a, b, c float64) Kind {
	// Eliminate Infinity and NaN: if any of a, b, c come out
	// non-finite, combining them (by addition) will result in NaN or +-Inf.
	sum := a + b + c
	if math.IsNaN(sum) || math.IsInf(sum, 0) {
		return NaT
	}

	// Apply sorting network on 3 variables.
	if b > c {
		b, c = c, b
	}

	if a > b {
		a, b = b, a
	}

	if b > c {
		b, c = c, b
	}
	// INVARIANT: a <= b <= c

	// Minimal side must be non-negative and triangle inequality must hold.
	if a <= 0 || a+b < c {
		return NaT
	}

	if a == c {
		return Equ
	}

	if a == b || b == c {
		return Iso
	}

	return Sca
}
