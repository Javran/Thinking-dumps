// Package strand contains a function that performs RNA transcription.
package strand

var rnaTable = map[rune]rune {
	'G': 'C',
	'C': 'G',
	'T': 'A',
	'A': 'U',
}

// ToRNA transcripts a DNA sequence to its complement RNA sequence.
func ToRNA(dna string) string {
	var result []rune
	for _, d := range dna {
		r, ok := rnaTable[d]
		if !ok {
			panic("unexpected input nucleotide")
		}
		result = append(result, r)
	}
	return string(result)
}
