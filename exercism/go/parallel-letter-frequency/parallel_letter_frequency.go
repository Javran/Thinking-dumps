package letter

// FreqMap records the frequency of each rune in a given text.
type FreqMap map[rune]int

// Frequency counts the frequency of each rune in a given text and returns this
// data as a FreqMap.
func Frequency(s string) FreqMap {
	m := FreqMap{}
	for _, r := range s {
		m[r]++
	}
	return m
}

// ConcurrentFrequency counts the frequency of each rune in a given slice of texts.
func ConcurrentFrequency(ss []string) FreqMap {
	m := FreqMap{}
	ch := make(chan FreqMap)
	todo := len(ss)
	for _, cur := range ss {
		go func(s string) {
			ch <- Frequency(s)
		}(cur)
	}

	for ; todo > 0; todo-- {
		for k, v := range <-ch {
			m[k] += v
		}
	}
	return m
}
