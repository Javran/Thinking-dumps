// Package gigasecond contains a function to add a gigasecond to an existing time.
package gigasecond

import (
	"time"
)

var gigasecond = time.Duration(1_000_000_000) * time.Second

// AddGigasecond returns input time plus 1 gigaseconds.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(gigasecond)
}
