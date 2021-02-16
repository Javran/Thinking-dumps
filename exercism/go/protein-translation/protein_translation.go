// Package protein contains utilities for decoding RNA into sequence of proteins.
package protein

import (
	"errors"
)

// proteins is a mapping from codon to protein or "STOP"
// which is initialized once and never mutated.
var proteins map[string]string

func init() {
	proteinTable := []struct {
		codons []string
		protein string
	}{
		{[]string{"AUG"}, "Methionine"},
		{[]string{"UUU", "UUC"}, "Phenylalanine"},
		{[]string{"UUA", "UUG"}, "Leucine"},
		{[]string{"UCU", "UCC", "UCA", "UCG"}, "Serine"},
		{[]string{"UAU", "UAC"}, "Tyrosine"},
		{[]string{"UGU", "UGC"}, "Cysteine"},
		{[]string{"UGG"}, "Tryptophan"},
		{[]string{"UAA", "UAG", "UGA"}, "STOP"},
	}
	proteins = make(map[string]string)
	for _, item := range proteinTable {
		for _, codon := range item.codons {
			proteins[codon] = item.protein
		}
	}
	
}

// ErrStop signals that a STOP instruction is issued.
var ErrStop = errors.New("STOP")
// ErrInvalidBase signals either the codon is not recognized or RNA is incomplete.
var ErrInvalidBase = errors.New("invalid codon or insufficient RNA code")

// FromCodon converts a single codon code to protein.
func FromCodon(codon string) (string, error) {
	protein, ok := proteins[codon]

	if !ok {
		return "", ErrInvalidBase
	}

	if protein == "STOP" {
		return "", ErrStop
	}

	return protein, nil
}

// FromRNA converts RNA code to slice of proteins.
func FromRNA(rna string) ([]string, error) {
	var result []string
	for i := 0; i < len(rna); i += 3 {
		protein, err := FromCodon(rna[i : i+3])
		switch {
		case err == ErrInvalidBase:
			return result, err
		case err == ErrStop:
			return result, nil
		default:
			result = append(result, protein)
		}
	}

	return result, nil
}
