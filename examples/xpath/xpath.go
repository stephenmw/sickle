package xpath

import (
	"sickle"
)

// ignores optional whitespace between rules
func tokSeq(rules ...sickle.Rule) sickle.Rule {
	// make a new rule slice with enough space to put a separator between each
	// rule.
	newLen := 2*len(rules) - 1
	rs := make([]sickle.Rule, newLen)

	// fill in rules
	for i := 0; i < len(rs); i += 2 {
		rs[i] = rules[i/2]
	}

	// fill in separators
	for i := 1; i < len(rs); i += 2 {
		rs[i] = ignore_optional_ws
	}

	return sickle.Seq(rs...)
}
