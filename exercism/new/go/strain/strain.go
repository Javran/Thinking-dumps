// Package strain operates on some type of slices and keeps or discards elements based on a predicate.
package strain

/*
  Before start commenting on definitions, I have a obvious question to ask, why?
  I almost feel like this exercise itself is a boycott on golang not having generics...
 */

type Ints []int
type Strings []string
type Lists [][]int

func (xs Ints) Keep(pred func(int) bool) Ints {
	var result Ints
	for _, x := range xs {
		if pred(x) {
			result = append(result, x)
		}
	}
	return result
}

func (xs Ints) Discard(pred func(int) bool) Ints {
	return xs.Keep(func(x int) bool { return !pred(x) })
}

func (xs Strings) Keep(pred func(string) bool) Strings {
	var result Strings
	for _, x := range xs {
		if pred(x) {
			result = append(result, x)
		}
	}
	return result
}

func (xs Strings) Discard(pred func(string) bool) Strings {
	return xs.Keep(func(x string) bool { return !pred(x) })
}

func (xs Lists) Keep(pred func([]int) bool) Lists {
	var result Lists
	for _, x := range xs {
		if pred(x) {
			result = append(result, x)
		}
	}
	return result
}

func (xs Lists) Discard(pred func([]int) bool) Lists {
	return xs.Keep(func(x []int) bool { return !pred(x) })
}
