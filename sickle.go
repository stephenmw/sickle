package sickle

func Parse(input []byte, r Rule) RuleResult {
	return newParserState(input).applyRule(r, Position{})
}

type parserState struct {
	input []byte

	lrStack *lrStackEntry
	memos   map[memoIndex]*memoEntry
	heads   map[Position]*headEntry
}

func newParserState(input []byte) *parserState {
	return &parserState{
		input: input,
		memos: make(map[memoIndex]*memoEntry),
		heads: make(map[Position]*headEntry),
	}
}

func (p *parserState) applyRule(rule Rule, pos Position) RuleResult {
	if r, ok := rule.(*IndirectRule); ok {
		rule = r.Rule // dereference indirect
	}

	m := p.recall(rule, pos)
	if m == nil {
		lr := &lrStackEntry{
			seed: RuleResult{Failed: true},
			rule: rule,
			head: nil,
			next: p.lrStack,
		}

		p.lrStack = lr // push onto stack

		m := new(memoEntry)
		m.setLR(lr)

		p.memos[memoIndex{rule, pos}] = m

		res := rule.parse(p, pos)

		p.lrStack = p.lrStack.next // pop off stack

		if lr.head != nil {
			lr.seed = res
			return p.lrAnswer(rule, pos, m)
		}

		m.setResult(res)
		return res

	}

	if m.typ == lrMemo {
		p.setupLR(rule, m.lr)
	}

	return m.res
}

func (p *parserState) setupLR(rule Rule, lr *lrStackEntry) {
	if lr.head == nil {
		lr.head = newHeadEntry(rule)
	}

	s := p.lrStack
	for s.head != lr.head {
		s.head = lr.head
		lr.head.involvedSet.add(s.rule)
		s = s.next
	}
}

func (p *parserState) lrAnswer(rule Rule, pos Position, memo *memoEntry) RuleResult {
	if memo.typ != lrMemo {
		panic("programmer error: memo must be an lrMemo")
	}

	h := memo.lr.head
	if h.rule != rule {
		return memo.lr.seed
	} else {
		memo.setResult(memo.lr.seed)
		if memo.res.Failed {
			return memo.res
		}

		return p.growLR(rule, pos, memo, h)
	}
}

func (p *parserState) growLR(rule Rule, pos Position, memo *memoEntry, h *headEntry) RuleResult {
	if memo.typ != resultMemo {
		panic("programmer error: memo must be an resultMemo")
	}

	p.heads[pos] = h

	for {
		h.evalSet = h.involvedSet.copy()

		res := rule.parse(p, pos)
		if res.Failed || res.Node.End.Cmp(memo.res.Node.End) <= 0 {
			break
		}

		memo.setResult(res)
	}

	delete(p.heads, pos)
	return memo.res
}

func (p *parserState) recall(rule Rule, pos Position) *memoEntry {
	m := p.memos[memoIndex{rule, pos}]
	h := p.heads[pos]
	if h == nil {
		return m
	}

	if m == nil && (rule == h.rule || h.involvedSet.contains(rule)) {
		m := new(memoEntry)
		m.setResult(RuleResult{Failed: true})
		return m
	}

	if h.evalSet.contains(rule) {
		h.evalSet.remove(rule)
		res := rule.parse(p, pos)
		m.setResult(res)
	}

	return m
}

type Position struct {
	Bytes uint64
	Bits  uint8
}

// if p > pos: return 1; if p < pos: return -1; else: return 0
func (p Position) Cmp(pos Position) int8 {
	switch {
	case p.Bytes > pos.Bytes:
		return 1
	case p.Bytes < pos.Bytes:
		return -1
	}

	switch {
	case p.Bits > pos.Bits:
		return 1
	case p.Bits < pos.Bits:
		return -1
	}

	return 0
}

type Rule interface {
	parse(p *parserState, pos Position) RuleResult
}

type lrStackEntry struct {
	seed RuleResult
	rule Rule
	head *headEntry

	next *lrStackEntry
}

type RuleResult struct {
	// if Failed, node is invalid
	Failed bool

	Node Node
}

type Node struct {
	Value      interface{}
	Start, End Position
}

type headEntry struct {
	rule        Rule
	involvedSet *ruleSet
	evalSet     *ruleSet
}

func newHeadEntry(rule Rule) *headEntry {
	return &headEntry{
		rule:        rule,
		involvedSet: newRuleSet(),
		evalSet:     newRuleSet(),
	}
}

type ruleSet struct {
	entries map[Rule]struct{}
}

func newRuleSet() *ruleSet {
	return &ruleSet{
		entries: make(map[Rule]struct{}),
	}
}

func (r *ruleSet) add(rule Rule) {
	r.entries[rule] = struct{}{}
}

func (r *ruleSet) remove(rule Rule) {
	delete(r.entries, rule)
}

func (r *ruleSet) contains(rule Rule) bool {
	_, ok := r.entries[rule]
	return ok
}

func (r *ruleSet) copy() *ruleSet {
	newSet := newRuleSet()
	for k, v := range r.entries {
		newSet.entries[k] = v
	}

	return newSet
}

type memoType uint

const (
	_ memoType = iota
	lrMemo
	resultMemo
)

type memoIndex struct {
	rule Rule
	pos  Position
}

type memoEntry struct {
	typ memoType

	lr  *lrStackEntry
	res RuleResult
}

func (m *memoEntry) setLR(lr *lrStackEntry) {
	m.typ = lrMemo
	m.lr = lr
	m.res.Node.Value = nil
}

func (m *memoEntry) setResult(res RuleResult) {
	m.typ = resultMemo
	m.lr = nil
	m.res = res
}

type ActionFunc func(node Node) Node

type PredicateFunc func(node Node) bool
