package clock

import (
	"fmt"
)

// Clock represents a clock that only has hour and minute components within a day.
// internally it stores as an int `v`, which is limited to `0 <= v < 24 * 60` at all time.
type Clock int

// New creates a new Clock.
func New(hour, minute int) Clock {
	m := minute + hour*60
	m %= 24 * 60
	if m < 0 {
		m += 24 * 60
	}
	return Clock(m)
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
	q, r := tick/60, tick%60
	return fmt.Sprintf("%02d:%02d", q, r)
}
