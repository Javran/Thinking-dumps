// Package darts exports a function that computes score given a coordinate.
package darts

import (
	"math"
)

const (
	innerRadiusSq  = 1 * 1
	middleRadiusSq = 5 * 5
	outerRadiusSq  = 10 * 10
)

// Score computes a score given a dart coordinate.
func Score(x, y float64) int {
	// exclude large numbers that multiplication might result in overflow issues.
	if math.Abs(x) > 10 || math.Abs(y) > 10 {
		return 0
	}

	switch sq := x*x + y*y; {
	case sq <= innerRadiusSq:
		return 10
	case sq <= middleRadiusSq:
		return 5
	case sq <= outerRadiusSq:
		return 1
	default:
		return 0
	}
}
