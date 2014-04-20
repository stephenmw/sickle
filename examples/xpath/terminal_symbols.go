package xpath

import (
	"sickle"
)

func init() {
	comment.Bind(comment_actual)
}

// lexical fragments: these are not terminals themselves but are used to build
// terminals.
var (
	digits     = sickle.Many1(sickle.ChRange('0', '9'))
	digits_opt = sickle.Many(sickle.ChRange('0', '9'))

	ncname_start_char = sickle.Choice(
		sickle.ChRange('A', 'Z'),
		sickle.Ch('_'),
		sickle.ChRange('a', 'z'),
		sickle.ChRange(0xC0, 0xD6),
		sickle.ChRange(0xD8, 0xF6),
		sickle.ChRange(0xF8, 0x2FF),
		sickle.ChRange(0x370, 0x37D),
		sickle.ChRange(0x27F, 0x1FFF),
		sickle.ChRange(0x200C, 0x200D),
		sickle.ChRange(0x2070, 0x218F),
		sickle.ChRange(0x2C00, 0x2FEF),
		sickle.ChRange(0x3001, 0xD7FF),
		sickle.ChRange(0xF900, 0xFDCF),
		sickle.ChRange(0xFDF0, 0xFFFD),
		sickle.ChRange(0x10000, 0xEFFFF),
	)

	ncname_char = sickle.Choice(
		ncname_start_char,
		sickle.Ch('-'),
		sickle.Ch('.'),
		sickle.ChRange('0', '9'),
		sickle.Ch(0xB7),
		sickle.ChRange(0x0300, 0x036F),
		sickle.ChRange(0x203F, 0x2040),
	)
)

// Whitespace
var (
	ws = sickle.Many1(sickle.Choice(
		ws_char,
		comment,
	))

	ws_char = sickle.Choice(
		sickle.Ch(0x20),
		sickle.Ch(0x9),
		sickle.Ch(0xD),
		sickle.Ch(0xA),
	)

	comment        = sickle.Indirect()
	comment_actual = sickle.Seq(
		comment_start,
		sickle.Many(sickle.Choice(comment_contents, comment)),
		comment_end,
	)

	comment_contents = sickle.Many1(sickle.Right(
		sickle.Not(sickle.Choice(comment_start, comment_end)),
		char,
	))

	comment_start = sickle.TokenStr("(:")
	comment_end   = sickle.TokenStr(":)")

	char = sickle.Choice(
		ws_char,
		sickle.ChRange(0x20, 0xD7FF),
		sickle.ChRange(0x20, 0xD7FF),
		sickle.ChRange(0x20, 0xD7FF),
	)

	ignore_optional_ws = sickle.Ignore(sickle.Opt(ws))
)

// Delimiting terminal symbols
var (
	delimiting_terminal_symbol = sickle.Choice(
		bang,
		not_equal,
		pound_sign,
		dollar_sign,
		left_paren,
		right_paren,
		asterisk,
		plus_sign,
		comma,
		minus_sign,
		dot,
		double_dot,
		slash,
		double_slash,
		colon,
		double_colon,
		colon_equal,
		less_than,
		double_less_than,
		less_than_equal,
		equal,
		greater_than,
		greater_than_equal,
		double_greater_than,
		question_mark,
		at_sign,
		left_bracket,
		right_bracket,
		left_brace,
		pipe,
		double_pipe,
		right_brace,

		string_literal,
		braced_uri_literal,
	)

	bang                = sickle.TokenStr("!")
	not_equal           = sickle.TokenStr("!=")
	pound_sign          = sickle.TokenStr("#")
	dollar_sign         = sickle.TokenStr("$")
	left_paren          = sickle.TokenStr("(")
	right_paren         = sickle.TokenStr(")")
	asterisk            = sickle.TokenStr("*")
	plus_sign           = sickle.TokenStr("+")
	comma               = sickle.TokenStr(",")
	minus_sign          = sickle.TokenStr("-")
	dot                 = sickle.TokenStr(".")
	double_dot          = sickle.TokenStr("..")
	slash               = sickle.TokenStr("/")
	double_slash        = sickle.TokenStr("//")
	colon               = sickle.TokenStr(":")
	double_colon        = sickle.TokenStr("::")
	colon_equal         = sickle.TokenStr(":=")
	less_than           = sickle.TokenStr("<")
	double_less_than    = sickle.TokenStr("<<")
	less_than_equal     = sickle.TokenStr("<=")
	equal               = sickle.TokenStr("=")
	greater_than        = sickle.TokenStr(">")
	greater_than_equal  = sickle.TokenStr(">=")
	double_greater_than = sickle.TokenStr(">>")
	question_mark       = sickle.TokenStr("?")
	at_sign             = sickle.TokenStr("@")
	left_bracket        = sickle.TokenStr("[")
	right_bracket       = sickle.TokenStr("]")
	left_brace          = sickle.TokenStr("{")
	pipe                = sickle.TokenStr("|")
	double_pipe         = sickle.TokenStr("||")
	right_brace         = sickle.TokenStr("}")

	string_literal = sickle.Choice(
		sickle.Seq(
			sickle.TokenStr("\""),
			sickle.Many(
				sickle.Right(
					sickle.Not(sickle.TokenStr("\"\"")),
					sickle.NotIn("\""),
				),
			),
			sickle.TokenStr("\""),
		),
		sickle.Seq(
			sickle.TokenStr("'"),
			sickle.Many(
				sickle.Right(
					sickle.Not(sickle.TokenStr("''")),
					sickle.NotIn("'"),
				),
			),
			sickle.TokenStr("'"),
		),
	)

	braced_uri_literal = sickle.Seq(
		sickle.TokenStr("Q{"),
		sickle.Many(sickle.NotIn("{}")),
		sickle.TokenStr("}"),
	)
)

// ndt is used to wrap "non-delimiting terminal symbols". These are symbols
// which must be delimited by either a delimiting terminal symbol or whitespace.
func ndt(r sickle.Rule) sickle.Rule {
	return sickle.Left(
		r,
		sickle.And(sickle.Choice(ws, delimiting_terminal_symbol, sickle.End())),
	)
}

// Non-delimiting terminal symbols
var (
	integer_literal = ndt(digits)
	decimal_literal = ndt(sickle.Choice(
		sickle.Seq(dot, digits),
		sickle.Seq(digits, dot, digits_opt),
	))
	double_literal = ndt(sickle.Seq(
		sickle.Choice(
			sickle.Seq(dot, digits),
			sickle.Seq(
				digits, sickle.OptSeq(dot, digits_opt),
			),
			sickle.In("eE"),
			sickle.Opt(sickle.In("+-")),
			digits,
		),
	))

	uri_qualified_name = ndt(sickle.Seq(braced_uri_literal, nc_name))
	nc_name            = ndt(sickle.Seq(ncname_start_char, sickle.Many(ncname_char)))
	q_name             = ndt(sickle.Seq(sickle.OptSeq(nc_name, colon), nc_name))

	ancestor               = ndt(sickle.TokenStr("ancestor"))
	ancestor_or_self       = ndt(sickle.TokenStr("ancestor-or-self"))
	and                    = ndt(sickle.TokenStr("and"))
	as                     = ndt(sickle.TokenStr("as"))
	attribute              = ndt(sickle.TokenStr("attribute"))
	cast                   = ndt(sickle.TokenStr("cast"))
	castable               = ndt(sickle.TokenStr("castable"))
	child                  = ndt(sickle.TokenStr("child"))
	comment_token          = ndt(sickle.TokenStr("comment"))
	descendant             = ndt(sickle.TokenStr("descendant"))
	descendant_or_self     = ndt(sickle.TokenStr("descendant-or-self"))
	div                    = ndt(sickle.TokenStr("div"))
	document_node          = ndt(sickle.TokenStr("document-node"))
	element                = ndt(sickle.TokenStr("element"))
	else_token             = ndt(sickle.TokenStr("else"))
	empty_sequence         = ndt(sickle.TokenStr("empty-sequence"))
	eq                     = ndt(sickle.TokenStr("eq"))
	every                  = ndt(sickle.TokenStr("every"))
	except                 = ndt(sickle.TokenStr("except"))
	following              = ndt(sickle.TokenStr("following"))
	following_sibling      = ndt(sickle.TokenStr("following-sibling"))
	for_token              = ndt(sickle.TokenStr("for"))
	function               = ndt(sickle.TokenStr("function"))
	ge                     = ndt(sickle.TokenStr("ge"))
	gt                     = ndt(sickle.TokenStr("gt"))
	idiv                   = ndt(sickle.TokenStr("idiv"))
	if_token               = ndt(sickle.TokenStr("if"))
	in                     = ndt(sickle.TokenStr("in"))
	instance               = ndt(sickle.TokenStr("instance"))
	intersect              = ndt(sickle.TokenStr("intersect"))
	is                     = ndt(sickle.TokenStr("is"))
	item                   = ndt(sickle.TokenStr("item"))
	le                     = ndt(sickle.TokenStr("le"))
	let                    = ndt(sickle.TokenStr("let"))
	lt                     = ndt(sickle.TokenStr("lt"))
	mod                    = ndt(sickle.TokenStr("mod"))
	namespace              = ndt(sickle.TokenStr("namespace"))
	namespace_node         = ndt(sickle.TokenStr("namespace-node"))
	ne                     = ndt(sickle.TokenStr("ne"))
	node                   = ndt(sickle.TokenStr("node"))
	of                     = ndt(sickle.TokenStr("of"))
	or                     = ndt(sickle.TokenStr("or"))
	parent                 = ndt(sickle.TokenStr("parent"))
	preceding              = ndt(sickle.TokenStr("preceding"))
	preceding_sibling      = ndt(sickle.TokenStr("preceding-sibling"))
	processing_instruction = ndt(sickle.TokenStr("processing-instruction"))
	return_token           = ndt(sickle.TokenStr("return"))
	satisfies              = ndt(sickle.TokenStr("satisfies"))
	schema_attribute       = ndt(sickle.TokenStr("schema-attribute"))
	schema_element         = ndt(sickle.TokenStr("schema-element"))
	self                   = ndt(sickle.TokenStr("self"))
	some                   = ndt(sickle.TokenStr("some"))
	text                   = ndt(sickle.TokenStr("text"))
	then                   = ndt(sickle.TokenStr("then"))
	to                     = ndt(sickle.TokenStr("to"))
	treat                  = ndt(sickle.TokenStr("treat"))
	union                  = ndt(sickle.TokenStr("union"))
)
