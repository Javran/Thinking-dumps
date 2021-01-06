// This package contains functional list operations operating on []int.
// Note that all of those functions are functional, meaning they keep the receiver intact.
package listops

type IntList []int

type predFunc func(int) bool
type unaryFunc func(int) int
type binFunc func(int, int) int

// Appends another slice to the existing slice.
func (xs IntList) Append(ys IntList) IntList {
	result := xs
	for _, v := range ys {
		result = append(result, v)
	}
	return result
}

// Appends multiple slices to the existing slice.
func (xs IntList) Concat(yss []IntList) IntList {
	result := xs
	for _, ys := range yss {
		result = result.Append(ys)
	}
	return result
}

// Left-folds through a IntList and accumulates its result.
func (xs IntList) Foldl(f binFunc, seed int) int {
	acc := seed
	for _, v := range xs {
		acc = f(acc, v)
	}
	return acc
}

// Right-folds through a IntList and accumulates its result.
func (xs IntList) Foldr(f binFunc, seed int) int {
	acc := seed
	for i := len(xs) - 1; i >= 0; i -= 1 {
		acc = f(xs[i], acc)
	}
	return acc
}

// Returns a slice that only contains elements satisfying a given predicate.
func (xs IntList) Filter(pred predFunc) IntList {
	// Create an empty slice rather than allowing nil - although not
	// specified explicitly this behavior is enforced by testcases.
	result := IntList{}
	for _, v := range xs {
		if pred(v) {
			result = append(result, v)
		}
	}
	return result
}

// Returns the length of the slice.
func (xs IntList) Length() int {
	// While len() does the job,
	// Foldl is chosen here in the spirit of functional programming.
	return xs.Foldl(func(a, _ int) int { return a + 1 }, 0)
}

// Applies a function to every element of the slice and returns the resulting slice.
func (xs IntList) Map(f unaryFunc) IntList {
	result := make(IntList, len(xs))
	for i, v := range xs {
		result[i] = f(v)
	}
	return result
}

// Reverses the slice.
func (xs IntList) Reverse() IntList {
	l := len(xs)
	result := make(IntList, l)
	for i, v := range xs {
		result[l-1-i] = v
	}
	return result
}
