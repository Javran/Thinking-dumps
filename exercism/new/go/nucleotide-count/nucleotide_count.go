// Package dna contains definitions for counting nucleotides.
package dna

import (
	"errors"
)

// Histogram is a mapping from nucleotide to its count in given DNA.
type Histogram map[rune]int

// DNA is a list of nucleotides.
type DNA string

// Counts generates a histogram of valid nucleotides in the given DNA.
// Returns an error if d contains an invalid nucleotide.
func (d DNA) Counts() (Histogram, error) {
	// populate the map with only valid nucleotides so that
	// the same map can be used as a way to detect invalid input
	// (as invalid ones will fail to lookup).
	h := Histogram{'A': 0, 'C': 0, 'G': 0, 'T': 0}
	for _, n := range d {
		count, ok := h[n]
		if !ok {
			return nil, errors.New("unexpected input")
		}
		h[n] = count + 1
	}
	return h, nil
}
