package clock

import (
	"fmt"
)

// Clock represents a clock that only has hour and minute components within a day.
// internally is is stored as a single field limited to `0 <= min < 24 * 60` at all time,
// to represent minutes relative to start of a day.
type Clock struct {
	min int
}

// New creates a new Clock.
func New(hour, minute int) Clock {
	m := minute + hour*60
	m %= 24 * 60
	if m < 0 {
		m += 24 * 60
	}
	return Clock{min: m}
}

// Add returns a new Clock which is `m` minutes in advance.
func (c Clock) Add(minutes int) Clock {
	return New(0, c.min+minutes)
}

// Subtract returns a new Clock which is `m` minutes early.
func (c Clock) Subtract(minutes int) Clock {
	return New(0, c.min-minutes)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.min/60, c.min%60)
}
