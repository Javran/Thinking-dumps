// Package space contains definitions to compute age relative to a planet (in our solar system).
package space

// Planet represents name of all solar planets.
type Planet string

// earthYears is a mapping from Planets to their orbital periods relative to Earth.
var earthYears = map[Planet]float64{
	"Mercury": 0.2408467,
	"Venus":   0.61519726,
	"Earth":   1,
	"Mars":    1.8808158,
	"Jupiter": 11.862615,
	"Saturn":  29.447498,
	"Uranus":  84.016846,
	"Neptune": 164.79132,
}

const (
	earthYearInSeconds = 31557600
)

// Age computes years of a given age (in seconds) relative to a solar planet.
func Age(sec float64, p Planet) float64 {
	earthYear := earthYears[p]
	return sec / (earthYearInSeconds * earthYear)
}
