package encode

import (
	"strconv"
	"strings"
	"unicode"
)

// sameChars represents a consecutive sequence of the same rune.
// note that when count == 0, the value is meaningless.
type sameChars struct {
	value rune
	count int
}

// encodeSameChars outputs run length encoded string.
func encodeSameChars(sc *sameChars) string {
	switch sc.count {
	case 0:
		return ""
	case 1:
		return string(sc.value)
	default:
		return strconv.Itoa(sc.count) + string(sc.value)
	}
}

// RunLengthEncode run-length-encodes a string.
// Note that it is assumed but not checked that the input string does not contain
// any digit runes.
func RunLengthEncode(input string) string {
	curChars := sameChars{}
	result := ""
	for _, ch := range input {
		if curChars.count > 0 && curChars.value == ch {
			// accumulate existing group
			curChars.count++
		} else {
			// need a new group here, push existing one to result if any.
			if curChars.count > 0 {
				result += encodeSameChars(&curChars)
			}
			curChars = sameChars{ch, 1}
		}
	}
	// push existing group to result if any.
	if curChars.count > 0 {
		result += encodeSameChars(&curChars)
	}
	return result
}

// RunLengthDecode decodes a run-length-encoded string.
func RunLengthDecode(input string) string {
	charSeq := ([]rune)(input)
	result := ""
	for i := 0; i < len(charSeq); i++ {
		ch := charSeq[i]
		if unicode.IsDigit(ch) {
			// find maximum j so that charSeq[i:j] form a number.
			j := i + 1
			for ; ; j++ {
				if !unicode.IsDigit(charSeq[j]) {
					break
				}
			}
			count, err := strconv.Atoi(string(charSeq[i:j]))
			if err != nil {
				panic(err)
			}
			if j == len(charSeq) {
				panic("Encoded string terminated while a non-digit rune is expected.")
			}
			// advance i to j, which should now point to a non-digit rune.
			i = j
			result += strings.Repeat(string(charSeq[i]), count)
		} else {
			result += string(ch)
		}
	}

	return result
}
