package pythagorean

import (
	"math"
)

type Triplet [3]int

func checkSquare(x int) (int, bool) {
	v := int(math.Round(math.Sqrt(float64(x))))
	return v, v*v == x
}

func Range(min, max int) []Triplet {
	var results []Triplet
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

func Sum(p int) []Triplet {
	var results []Triplet
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
