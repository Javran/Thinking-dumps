// Package leap contains a leap year testing function.
package leap

// IsLeapYear tests whether a year given by int is a leap year.
func IsLeapYear(year int) bool {
	if year%4 != 0 {
		return false
	}

	// year is divisible by 4 here.
	if year%100 == 0 {
		// this is not a leap year unless it is also divisible by 400.
		return year%400 == 0
	}

	// year is not divisible by 100 here.
	return true
}
