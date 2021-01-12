// Package gigasecond contains a function to add a gigasecond to an existing time.
package gigasecond

import (
	"fmt"
	"time"
)

var gigasecond time.Duration

func init() {
	var err error
	gigasecond, err =
		time.ParseDuration(fmt.Sprintf("%ds", 1_000_000_000))
	if err != nil {
		panic(err)
	}
}

// AddGigasecond returns input time plus 1 gigaseconds.
func AddGigasecond(t time.Time) time.Time {
	return t.Add(gigasecond)
}
