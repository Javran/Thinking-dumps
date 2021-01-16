// Package bob contains a function that mimics Bob's response.
package bob

import (
	"regexp"
	"strings"
)

var alphaTest = regexp.MustCompile(`(?i)[a-z]`)

// Hey mimics Bob's response to a sentence.
func Hey(remark string) string {
	remark = strings.TrimSpace(remark)
	if remark == "" {
		return "Fine. Be that way!"
	}

	question := remark[len(remark)-1] == '?'
	// test cases imply that a sentence is not consider in all caps if
	// it does not contain any alphabet.
	hasAlpha := alphaTest.Find([]byte(remark)) != nil
	allCap := hasAlpha && remark == strings.ToUpper(remark)

	switch {
		case question && !allCap:
		  return "Sure."
		case !question && allCap:
		  return "Whoa, chill out!"
		case question && allCap:
		  return "Calm down, I know what I'm doing!"
		default:
		  return "Whatever."
	}
}
