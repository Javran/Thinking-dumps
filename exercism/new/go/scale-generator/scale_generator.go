// Package scale contains a function that generates scales.
package scale

import (
	"strings"
)

var sharpPitches = []string{
	"A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#",
}
var flatPitches = []string{
	"Ab", "A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G",
}

// noFlats represents the set of initial tonics
// that should not use flat group of pitches
var noFlats map[string]struct{}

func init() {
	noFlats = make(map[string]struct{})
	for _, tonic := range []string{
		// majors
		"C", "G", "D", "A", "E", "B", "F#",
		// minors
		"a", "e", "b", "f#", "c#", "g#", "d#",
	} {
		noFlats[tonic] = struct{}{}
	}
}

// findPitchGroup picks between sharpPitches and flatPitches
// depending on tonic, and return the location of the tonic within it.
func findPitchGroup(tonic string) (int, []string) {
	pitches := flatPitches
	_, noFlat := noFlats[tonic]
	if noFlat {
		pitches = sharpPitches
	}
	loc := -1
	for i, p := range pitches {
		if strings.EqualFold(tonic, p) {
			loc = i
			break
		}
	}
	if loc == -1 {
		panic("tonic not found")
	}
	return loc, pitches
}

// Scale generates scales depending on the initial tonic and an interval.
// the interval can be left empty, in which case a sequence of half steps is assumed.
func Scale(tonic string, interval string) []string {
	if interval == "" {
		interval = strings.Repeat("m", 12)
	}

	loc, pitches := findPitchGroup(tonic)
	var result []string
	for _, step := range interval {
		result = append(result, pitches[loc%len(pitches)])
		switch step {
		case 'm':
			loc += 1
		case 'M':
			loc += 2
		case 'A':
			loc += 3
		default:
			panic("unrecognized step")
		}
	}

	return result
}
