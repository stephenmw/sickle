package sickle

import (
	"reflect"
	"unicode/utf8"
)

func Seq(rules ...Rule) Rule {
	return &seqRule{rules: rules}
}

func Choice(rules ...Rule) Rule {
	return &choiceRule{rules: rules}
}

func Ch(r rune) Rule {
	return &chRule{r: r}
}

func ChRange(min, max rune) Rule {
	return &chRangeRule{
		min: min,
		max: max,
	}
}

func Indirect() *IndirectRule {
	return &IndirectRule{}
}

func LengthValue(length, value Rule) Rule {
	return &lengthValueRule{
		length: length,
		value:  value,
	}
}

func Action(r Rule, f ActionFunc) Rule {
	return &actionRule{
		r: r,
		f: f,
	}
}

func Predicate(r Rule, f PredicateFunc) Rule {
	return &predicateRule{
		r: r,
		f: f,
	}
}

func Many(r Rule) Rule {
	return Choice(Many1(r), Seq())
}

func Many1(r Rule) Rule {
	return &many1Rule{r: r}
}

func End() Rule {
	return endRule{}
}

func Byte() Rule {
	return byteRule{}
}

func Left(left, right Rule) Rule {
	return &leftRule{
		left:  left,
		right: right,
	}
}

type seqRule struct {
	rules []Rule
}

func (s *seqRule) parse(p *parserState, pos Position) RuleResult {
	var start = pos
	var ret []Node

	for _, rule := range s.rules {
		res := p.applyRule(rule, pos)
		if res.Failed {
			return res
		}

		pos = res.Node.End

		if res.Node.Value != nil {
			ret = append(ret, res.Node)
		}
	}

	return RuleResult{
		Node: Node{
			Value: ret,
			Start: start,
			End:   pos,
		},
	}
}

type choiceRule struct {
	rules []Rule
}

func (c *choiceRule) parse(p *parserState, pos Position) RuleResult {
	for _, rule := range c.rules {
		res := p.applyRule(rule, pos)
		if !res.Failed {
			return res
		}
	}

	return RuleResult{Failed: true}
}

type chRule struct {
	r rune
}

func (c *chRule) parse(p *parserState, pos Position) RuleResult {
	if pos.Bytes >= uint64(len(p.input)) {
		// EOF reached
		return RuleResult{Failed: true}
	}

	r, n := utf8.DecodeRune(p.input[pos.Bytes:])
	if r != c.r || r == utf8.RuneError {
		// even if it is a rune error, we don't trust it.
		return RuleResult{Failed: true}
	}

	end := pos
	end.Bytes += uint64(n)
	end.Bits = 0

	return RuleResult{
		Node: Node{
			Value: r,
			Start: pos,
			End:   end,
		},
	}
}

type chRangeRule struct {
	min, max rune
}

func (c *chRangeRule) parse(p *parserState, pos Position) RuleResult {
	if pos.Bytes >= uint64(len(p.input)) {
		// EOF reached
		return RuleResult{Failed: true}
	}

	r, n := utf8.DecodeRune(p.input[pos.Bytes:])
	if r < c.min || r > c.max || r == utf8.RuneError {
		// even if it is a rune error, we don't trust it.
		return RuleResult{Failed: true}
	}

	end := pos
	end.Bytes += uint64(n)
	end.Bits = 0

	return RuleResult{
		Node: Node{
			Value: r,
			Start: pos,
			End:   end,
		},
	}
}

type IndirectRule struct {
	Rule
}

func (r *IndirectRule) Bind(rule Rule) {
	r.Rule = rule
}

type lengthValueRule struct {
	length Rule
	value  Rule
}

func (l *lengthValueRule) parse(p *parserState, pos Position) RuleResult {
	start := pos

	res := p.applyRule(l.length, pos)
	if res.Failed {
		return res
	}

	v := reflect.ValueOf(res.Node.Value)

	var length uint64
	switch v.Kind() {
	case reflect.Uint, reflect.Uintptr, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		length = v.Uint()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		i := v.Int()
		if i < 0 {
			return RuleResult{Failed: true}
		}
		length = uint64(i)
	default:
		return RuleResult{Failed: true}
	}

	var ret []Node

	for i := length; i > 0; i-- {
		res = p.applyRule(l.value, res.Node.End)
		if res.Failed {
			return res
		}

		pos = res.Node.End

		if res.Node.Value != nil {
			ret = append(ret, res.Node)
		}
	}

	return RuleResult{
		Node: Node{
			Value: ret,
			Start: start,
			End:   pos,
		},
	}
}

type actionRule struct {
	r Rule
	f ActionFunc
}

func (a *actionRule) parse(p *parserState, pos Position) RuleResult {
	res := p.applyRule(a.r, pos)
	if res.Failed {
		return res
	}

	res.Node = a.f(res.Node)

	return res
}

type predicateRule struct {
	r Rule
	f PredicateFunc
}

func (a *predicateRule) parse(p *parserState, pos Position) RuleResult {
	res := p.applyRule(a.r, pos)
	if res.Failed {
		return res
	}

	if !a.f(res.Node) {
		return RuleResult{Failed: true}
	}

	return res
}

type many1Rule struct {
	r Rule
}

func (m *many1Rule) parse(p *parserState, pos Position) RuleResult {
	var start = pos
	var ret []Node

	for {
		res := p.applyRule(m.r, pos)
		if res.Failed {
			break
		}

		pos = res.Node.End

		if res.Node.Value != nil {
			ret = append(ret, res.Node)
		}

	}

	if len(ret) < 1 {
		return RuleResult{Failed: true}
	}

	return RuleResult{
		Node: Node{
			Value: ret,
			Start: start,
			End:   pos,
		},
	}
}

type endRule struct{}

func (endRule) parse(p *parserState, pos Position) RuleResult {
	if pos.Bytes < uint64(len(p.input)) {
		return RuleResult{Failed: true}
	}

	return RuleResult{Node: Node{Start: pos, End: pos}}
}

type byteRule struct{}

func (byteRule) parse(p *parserState, pos Position) RuleResult {
	if pos.Bytes >= uint64(len(p.input)) {
		return RuleResult{Failed: true}
	}

	b := p.input[pos.Bytes]

	end := pos
	end.Bytes++
	end.Bits = 0

	return RuleResult{Node: Node{Value: b, Start: pos, End: end}}
}

type leftRule struct {
	left, right Rule
}

func (l *leftRule) parse(p *parserState, pos Position) RuleResult {
	resL := p.applyRule(l.left, pos)
	if resL.Failed {
		return resL
	}

	resR := p.applyRule(l.right, resL.Node.End)
	if resR.Failed {
		return resR
	}

	resL.Node.End = resR.Node.End

	return resL
}
