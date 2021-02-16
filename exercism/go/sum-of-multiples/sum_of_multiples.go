package summultiples

// SumMultiples computes sum of numbers from 0 to limit-1,
// whose divisible by any of divisors given.
func SumMultiples(limit int, divisors ...int) int {
	// With a longer list of divisors we can try to dedup and sort them in order
	// so that inner loop breaks sooner. But this seems good enough for current testcaess.
	sum := 0
	for i := 1; i < limit; i++ {
		for _, d := range divisors {
			if d != 0 && i % d == 0 {
				sum += i
				break
			}
		}
	}
	return sum
}
