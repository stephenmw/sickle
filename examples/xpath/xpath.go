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

func tokOptSeq(rules ...sickle.Rule) sickle.Rule {
	return sickle.Opt(tokSeq(rules...))
}

// sepBy1 but ignore whitespace around separator
func tokSepBy1(r, sep sickle.Rule) sickle.Rule {
	return sickle.SepBy1(
		r,
		sickle.Seq(ignore_optional_ws, sep, ignore_optional_ws),
	)
}

// many but ignore whitespace inbetween values
func tokMany(r sickle.Rule) sickle.Rule {
	return sickle.SepBy(r, ignore_optional_ws)
}

// binOp implementes a binary operator.
func binOp(r, op sickle.Rule) sickle.Rule {
	return tokSeq(
		r,
		tokMany(tokSeq(op, r)),
	)
}

func init() {
	expr.Bind(expr_actual)
	expr_single.Bind(expr_single_actual)
	item_type.Bind(item_type_actual)
	sequence_type.Bind(sequence_type_actual)
}

var (
	Rule = xpath

	xpath = expr

	param_list = tokSepBy1(param, comma)

	param = tokSeq(dollar_sign, eq_name, sickle.Opt(type_declaration))

	function_body = enclosed_expr

	enclosed_expr = tokSeq(left_brace, expr, right_brace)

	expr        = sickle.Indirect()
	expr_actual = tokSepBy1(expr_single, comma)

	expr_single        = sickle.Indirect()
	expr_single_actual = sickle.Choice(
		for_expr,
		let_expr,
		quantified_expr,
		if_expr,
		or_expr,
	)

	for_expr = tokSeq(simple_for_clause, return_token, expr_single)

	simple_for_clause = tokSeq(for_token, tokSepBy1(simple_for_binding, comma))

	simple_for_binding = tokSeq(
		tokSepBy1(tokSeq(dollar_sign, var_name, in, expr_single), comma),
		satisfies, expr_single,
	)

	let_expr = tokSeq(simple_let_clause, return_token, expr_single)

	simple_let_clause = tokSeq(let, tokSepBy1(simple_let_binding, comma))

	simple_let_binding = tokSeq(dollar_sign, var_name, colon_equal, expr_single)

	quantified_expr = tokSeq(
		sickle.Choice(some, every),
		tokSepBy1(tokSeq(dollar_sign, var_name, in, expr_single), comma),
		satisfies, expr_single,
	)

	if_expr = tokSeq(
		if_token, left_paren, expr, right_paren,
		then, expr_single, else_token, expr_single,
	)

	or_expr         = binOp(and_expr, or)
	and_expr        = binOp(comparison_expr, and)
	comparison_expr = tokSeq(
		string_concat_expr,
		sickle.Opt(tokSeq(
			sickle.Choice(
				value_comp,
				general_comp,
				node_comp,
			),
			string_concat_expr,
		)),
	)

	string_concat_expr    = binOp(range_expr, double_pipe)
	range_expr            = tokSeq(additive_expr, tokOptSeq(to, additive_expr))
	additive_expr         = binOp(multiplicative_expr, sickle.Choice(plus_sign, minus_sign))
	multiplicative_expr   = binOp(union_expr, sickle.Choice(asterisk, div, idiv, mod))
	union_expr            = binOp(intersect_except_expr, sickle.Choice(union, pipe))
	intersect_except_expr = binOp(instance_of_expr, sickle.Choice(intersect, except))
	instance_of_expr      = tokSeq(treat_expr, tokOptSeq(instance, of, sequence_type))
	treat_expr            = tokSeq(castable_expr, tokOptSeq(treat, as, sequence_type))
	castable_expr         = tokSeq(cast_expr, tokOptSeq(castable, as, single_type))
	cast_expr             = tokSeq(unary_expr, tokOptSeq(cast, as, single_type))
	unary_expr            = tokSeq(tokMany(sickle.Choice(plus_sign, minus_sign)), value_expr)
	value_expr            = simple_map_expr
	general_comp          = sickle.Choice(equal, not_equal, less_than, less_than_equal, greater_than, greater_than_equal)
	value_comp            = sickle.Choice(eq, ne, lt, le, gt, ge)
	node_comp             = sickle.Choice(is, double_less_than, double_greater_than)
	simple_map_expr       = binOp(path_expr, bang)

	path_expr = sickle.Choice(
		tokSeq(slash, sickle.Opt(relative_path_expr)),
		tokSeq(double_slash, relative_path_expr),
		relative_path_expr, // xgc: leading-lone-slash
	)

	relative_path_expr = binOp(step_expr, sickle.Choice(double_slash, slash))
	step_expr          = sickle.Choice(postfix_expr, axis_step)
	axis_step          = tokSeq(sickle.Choice(reverse_step, forward_step), predicate_list)

	forward_step = sickle.Choice(tokSeq(forward_axis, node_test), abbrev_forward_step)
	forward_axis = sickle.Choice(
		tokSeq(child, double_colon),
		tokSeq(descendant, double_colon),
		tokSeq(attribute, double_colon),
		tokSeq(self, double_colon),
		tokSeq(descendant_or_self, double_colon),
		tokSeq(following_sibling, double_colon),
		tokSeq(following, double_colon),
		tokSeq(namespace, double_colon),
	)
	abbrev_forward_step = tokSeq(sickle.Opt(at_sign), node_test)

	reverse_step = sickle.Choice(tokSeq(reverse_axis, node_test), abbrev_reverse_step)
	reverse_axis = sickle.Choice(
		tokSeq(parent, double_colon),
		tokSeq(ancestor, double_colon),
		tokSeq(preceding_sibling, double_colon),
		tokSeq(preceding, double_colon),
		tokSeq(ancestor_or_self, double_colon),
	)
	abbrev_reverse_step = double_dot

	node_test = sickle.Choice(kind_test, name_test)
	name_test = sickle.Choice(eq_name, wildcard)
	wildcard  = sickle.Choice(
		asterisk,
		tokSeq(nc_name, colon, asterisk),
		tokSeq(asterisk, colon, nc_name),
		sickle.Seq(braced_uri_literal, asterisk), // ws: explicit
	)

	postfix_expr = tokSeq(
		primary_expr,
		tokMany(sickle.Choice(predicate, argument_list)),
	)

	argument_list = tokSeq(
		left_paren,
		sickle.Opt(tokSepBy1(argument, comma)),
		right_paren,
	)

	predicate_list = tokMany(predicate)
	predicate      = tokSeq(left_bracket, expr, right_bracket)

	primary_expr = sickle.Choice(
		literal,
		var_ref,
		parenthesized_expr,
		context_item_expr,
		function_call,
		function_item_expr,
	)

	literal = sickle.Choice(numeric_literal, string_literal)

	numeric_literal = sickle.Choice(
		double_literal, decimal_literal, integer_literal,
	)

	var_ref  = tokSeq(dollar_sign, var_name)
	var_name = eq_name

	parenthesized_expr = tokSeq(left_paren, sickle.Opt(expr), right_paren)

	context_item_expr = dot

	function_call = tokSeq(eq_name, argument_list) // xgc: reserved-function-names

	argument             = sickle.Choice(expr_single, argument_placeholder)
	argument_placeholder = question_mark

	function_item_expr = sickle.Choice(named_function_ref, inline_function_expr)

	named_function_ref = tokSeq(eq_name, pound_sign, integer_literal) // xgc: reserved-function-names

	inline_function_expr = tokSeq(
		function, left_paren, sickle.Opt(param_list), right_paren,
		tokOptSeq(as, sequence_type), function_body,
	)

	single_type = tokSeq(simple_type_name, sickle.Opt(question_mark))

	type_declaration = tokSeq(as, sequence_type)

	sequence_type        = sickle.Indirect()
	sequence_type_actual = sickle.Choice(
		tokSeq(empty_sequence, left_paren, right_paren),
		tokSeq(item_type, sickle.Opt(occurance_indicator)),
	)

	occurance_indicator = sickle.Choice(question_mark, asterisk, plus_sign) // xgc: occurrence-indicators

	item_type        = sickle.Indirect()
	item_type_actual = sickle.Choice(
		kind_test,
		tokSeq(item, left_paren, right_paren),
		function_test,
		atomic_or_union_type,
		parenthesized_item_type,
	)

	atomic_or_union_type = eq_name

	kind_test = sickle.Choice(
		document_test,
		element_test,
		attribute_test,
		schema_element_test,
		pi_test,
		comment_test,
		text_test,
		namespace_node_test,
		any_kind_test,
	)

	any_kind_test = tokSeq(node, left_paren, right_paren)
	document_test = tokSeq(
		document_node, left_paren,
		sickle.Opt(sickle.Choice(element_test, schema_element_test)),
		right_paren,
	)
	text_test           = tokSeq(text, left_paren, right_paren)
	comment_test        = tokSeq(comment, left_paren, right_paren)
	namespace_node_test = tokSeq(namespace_node, left_paren, right_paren)
	pi_test             = tokSeq(
		processing_instruction, left_paren,
		sickle.Opt(sickle.Choice(nc_name, string_literal)),
		right_paren,
	)

	attribute_test = tokSeq(
		attribute, left_paren,
		tokOptSeq(attrib_name_or_wildcard, tokOptSeq(comma, type_name)),
		right_paren,
	)
	attrib_name_or_wildcard = sickle.Choice(attribute_name, asterisk)

	schema_attribute_test = tokSeq(schema_attribute, left_paren, attribute_declaration, right_paren)
	attribute_declaration = attribute_name
	element_test          = tokSeq(
		element, left_paren,
		tokOptSeq(
			comma,
			element_name_or_wildcard,
			tokOptSeq(comma, type_name, sickle.Opt(question_mark)),
		),
	)

	element_name_or_wildcard = sickle.Choice(element_name, asterisk)

	schema_element_test = tokSeq(schema_element, left_paren, element_declaration, right_paren)

	element_declaration = element_name

	attribute_name    = eq_name
	element_name      = eq_name
	simple_type_name  = type_name
	type_name         = eq_name
	function_test     = sickle.Choice(any_function_test, typed_function_test)
	any_function_test = tokSeq(function, left_paren, asterisk, right_paren)

	typed_function_test = tokSeq(
		function, left_paren,
		sickle.Opt(tokSepBy1(sequence_type, comma)),
		right_paren, as, sequence_type,
	)

	parenthesized_item_type = tokSeq(left_paren, item_type, right_paren)

	eq_name = sickle.Choice(q_name, uri_qualified_name)
)
