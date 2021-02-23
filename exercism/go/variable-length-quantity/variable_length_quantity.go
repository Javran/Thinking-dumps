package variablelengthquantity

import (
	"errors"
	"math"
	"math/bits"
)

func decodeOne(input []byte) uint32 {
	var result uint32
	for _, v := range input {
		result = bits.RotateLeft32(result, 7) | (uint32(v) & 0b0111_1111)
	}
	return result
}

// ErrEndByteMissing indicates that a VLQ end byte is missing
var ErrEndByteMissing error = errors.New("end byte missing")

// ErrBitLengthTooLong indicates that current VLQ encoded section is longer than 32 bits.
var ErrBitLengthTooLong error = errors.New("bit length is too long")

// DecodeVarint decodes slices of encoded VLQ.
func DecodeVarint(input []byte) ([]uint32, error) {
	var result []uint32
	for i := 0; i < len(input); {
		j := i
		// identifies a uint32 section and decodes it via decodeOne().
		for ; j < len(input) && (input[j]&0b1000_0000) != 0; j++ {
		}
		if j == len(input) {
			return nil, ErrEndByteMissing
		}
		sectionLen := j - i + 1
		// ceil(32/7) = 5, which is longest possible section.
		if sectionLen > 5 {
			return nil, ErrBitLengthTooLong
		}
		// 5 bytes can encode 35 bits, so we need to make sure the first byte
		// does not use its most significant 3 bits.
		// i.e. bit pattern of first byte must match: `0b1XXX_????`,
		// in which X must be 0.
		if sectionLen == 5 && input[i] > 0b1000_1111 {
			return nil, ErrBitLengthTooLong
		}
		result = append(result, decodeOne(input[i:j+1]))
		i = j + 1
	}
	return result, nil
}

func encodeOne(input uint32) []byte {
	if input == 0 {
		return []byte{0}
	}
	reserve := int32(math.Ceil(float64(32-bits.LeadingZeros32(input)) / 7.0))
	result := make([]byte, reserve)
	for i := reserve - 1; i >= 0; i-- {
		result[i] = byte(input & 0b111_1111)
		if i != reserve-1 {
			result[i] |= 0b1000_0000
		}
		input = bits.RotateLeft32(input&0xFFFF_FFF8, -7)
	}
	return result
}

// EncodeVarint encodes slices of uint32.
func EncodeVarint(input []uint32) []byte {
	var result []byte
	for _, v := range input {
		result = append(result, encodeOne(v)...)
	}
	return result
}
