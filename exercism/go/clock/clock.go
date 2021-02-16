package clock

import (
	"fmt"
)

// Clock-related constants
const (
	MinutesPerHour = 60
	HoursPerDay    = 24
	MinutesPerDay  = HoursPerDay * MinutesPerHour
)

// Clock represents a clock that only has hour and minute components within a day.
// internally it stores as an int `v`, which is limited to `0 <= v < MinutesPerDay` at all time.
type Clock int

// nonNegMod performs x % d and wraps the result modulo d
// so it stays non-negative
func nonNegMod(x, d int) int {
	r := x % d
	if r >= 0 {
		return r
	}
	return d + r
}

// New creates a new Clock.
func New(hour, minute int) Clock {
	hourNorm := nonNegMod(hour, HoursPerDay) * MinutesPerHour
	minNorm := nonNegMod(minute, MinutesPerDay)
	return Clock(nonNegMod(hourNorm+minNorm, MinutesPerDay))
}

// Add returns a new Clock which is `m` minutes in advance.
func (c Clock) Add(minutes int) Clock {
	return New(0, int(c)+minutes)
}

// Subtract returns a new Clock which is `m` minutes early.
func (c Clock) Subtract(minutes int) Clock {
	return New(0, int(c)-minutes)
}

func (c Clock) String() string {
	tick := int(c)
	q, r := tick/MinutesPerHour, tick%MinutesPerHour
	return fmt.Sprintf("%02d:%02d", q, r)
}
