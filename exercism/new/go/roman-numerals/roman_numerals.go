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

	result := singleDigit(oneRep, val%10)

	val /= 10
	result = append(singleDigit(tenRep, val%10), result...)

	val /= 10
	result = append(singleDigit(hundredRep, val%10), result...)

	val /= 10
	result = append(singleDigit(thousandRep, val%10), result...)

	return string(result), nil
}
