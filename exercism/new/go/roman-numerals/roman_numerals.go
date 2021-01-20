// Package romannumerals contains function to convert ints to roman numeral.
package romannumerals

import (
	"errors"
)

type romanRepresent struct{ i, v, x rune }

var oneRep = romanRepresent{'I', 'V', 'X'}
var tenRep = romanRepresent{'X', 'L', 'C'}
var hundredRep = romanRepresent{'C', 'D', 'M'}
var thousandRep = romanRepresent{'M', '_', '_'}

// singleDigit converts a single digit to its roman numeral represetantion.
// a `rep` is given to allow `val` to be interpreted as
// first digit (lowest), tenth digit, hundreds digit, thousands digit.
func singleDigit(rep romanRepresent, val int) []rune {
	switch val {
	case 0:
		return nil
	case 1:
		return []rune{rep.i}
	case 2:
		return []rune{rep.i, rep.i}
	case 3:
		return []rune{rep.i, rep.i, rep.i}
	case 4:
		return []rune{rep.i, rep.v}
	case 5:
		return []rune{rep.v}
	case 6:
		return []rune{rep.v, rep.i}
	case 7:
		return []rune{rep.v, rep.i, rep.i}
	case 8:
		return []rune{rep.v, rep.i, rep.i, rep.i}
	case 9:
		return []rune{rep.i, rep.x}
	default:
		panic("cannot represent this digit")
	}
}

// ToRomanNumeral converts an int to its roman numeral representation.
// the input value must be between 1 and 3000,
// an error is raised otherwise.
func ToRomanNumeral(val int) (string, error) {
	if val <= 0 || val > 3000 {
		return "", errors.New("input out of representable range")
	}

	var result []rune

	result = append(result, singleDigit(thousandRep, val/1000)...)
	result = append(result, singleDigit(hundredRep, (val/100)%10)...)
	result = append(result, singleDigit(tenRep, (val/10)%10)...)
	result = append(result, singleDigit(oneRep, val%10)...)

	return string(result), nil
}
